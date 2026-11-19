"""Prepare per-paper inputs for the markdown digests (evidence cards).

For every PDF in the archive folder this writes
  _build/text/<stem>.txt      pdftotext -layout output (form feeds between pages; see below)
  _build/context/<stem>.json  the works.json records for that paper: citation, repo locations, drugs, used_for
and writes ../digests/STATUS.md plus _build/digest_manifest.json listing every PDF with its digest state.

A digest is <cards.CARDS>/<stem>.md -- in the stanpumpR repository since 2026-10-29, read through a worktree
pinned to master (see _build/cards.py); this archive's ../digests/ now holds only the generated STATUS.md and
REVIEW.md. Run after update.bat files new PDFs (update.bat calls this), so a new paper
shows up as "needs digest" at once. Usage: python digest_prep.py

2026-10-27, after two audit errors:
- A form-feed chunk of the text is not always a PDF page: some PDFs carry literal form feeds inside a page's text
  (PMID_26257511's pages 5 and 6 split into 12 chunks), and an agent counting chunks cited "PDF p. 16" for p. 6.
  `pages` is now the PDF's own page count (pypdf); where the chunks differ, the context records `text_chunks` and
  `chunk_page`, the PDF page of each chunk.
- A replaced PDF can keep its old modification time (PMID_32667154: the erratum was replaced by the full article,
  and the text layer stayed the erratum's). Each PDF's SHA-256 is now recorded in _build/pdf_hashes.json; the text
  is re-extracted when the hash changes, and a digest written before that change is listed as stale (unless the
  change is an OCR rewrite logged by ocr_scans.py, which adds a text layer to the same pages).
"""
import hashlib, json, logging, os, re, subprocess, sys, time

try:
    import pypdf
    logging.getLogger("pypdf").setLevel(logging.ERROR)
except ImportError:
    pypdf = None

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
TEXT = os.path.join(HERE, "text")
CTX = os.path.join(HERE, "context")
import cards                            # the one place that says where the cards live (the repo worktree)
CARDS = cards.CARDS                     # papers/digests in the stanpumpR worktree: the cards themselves
DIG = cards.DIG                         # this archive's digests/: the generated STATUS.md and REVIEW.md only
PDFTOTEXT = r"C:\Program Files\Git\mingw64\bin\pdftotext.exe"
for d in (TEXT, CTX, DIG):
    os.makedirs(d, exist_ok=True)
if not os.path.isdir(CARDS):            # first run after the move, or the worktree was removed
    cards.sync()


AAM = re.compile(r"PMID_(\d+)_aam$")  # authors' accepted manuscript, filed by unpaywall_fetch.py (2026-10-28)


def stem_keys(stem):
    """Identifiers a works.json record can match a PDF stem by."""
    m = AAM.match(stem)
    if m:
        return ("pmid", m.group(1))  # the manuscript is the same work as PMID_n
    if stem.startswith("PMID_"):
        return ("pmid", stem[5:])
    if stem.startswith("DOI_"):
        return ("doi", stem[4:])
    return (None, None)


def norm_doi(d):
    return re.sub(r"[^a-z0-9]", "_", (d or "").lower())


works = json.load(open(os.path.join(HERE, "works.json"), encoding="utf-8"))
by_pmid, by_doi = {}, {}
for w in works:
    for r in w.get("records", []):
        pm = (r.get("res") or {}).get("pmid") or r.get("pmid") or (r.get("pm") or {}).get("pmid")
        doi = (r.get("res") or {}).get("doi") or r.get("doi") or (r.get("pm") or {}).get("doi")
        if pm:
            by_pmid.setdefault(str(pm), []).append(r)
        if doi:
            by_doi.setdefault(norm_doi(doi), []).append(r)

# Non-journal documents (labels, FDA reviews, guidelines, posters): NJ_<group>_... files, listed by group in
# nonjournal_manifest.json; the work for a group has key "g:<group>" or "t:<group>".
NJ_MAN = json.load(open(os.path.join(HERE, "nonjournal_manifest.json"), encoding="utf-8")) \
    if os.path.exists(os.path.join(HERE, "nonjournal_manifest.json")) else {}
nj_group = {e["file"][:-4]: g for g, es in NJ_MAN.items() for e in es}
nj_meta = {e["file"][:-4]: e for es in NJ_MAN.values() for e in es}
by_key = {w.get("key"): w for w in works}


def nj_records(stem):
    g = nj_group.get(stem)
    if not g:
        return []
    w = by_key.get("g:" + g) or by_key.get("t:" + g) or by_key.get("t:" + g.replace("-", ":", 1))
    if not w:
        return []
    return [{"locations": w.get("locations", []), "drugs": w.get("drugs", []),
             "used_for": w.get("used_for"), "notes": w.get("notes"), "citation_text": w.get("citation_text"),
             "pm": {"title": w.get("title"), "first_author": w.get("first_author"), "year": w.get("year")}}]


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()


def pdf_pages(pdf):
    """The PDF's own page reader, or None when pypdf is missing or cannot read it."""
    if pypdf is None:
        return None
    try:
        return pypdf.PdfReader(pdf).pages
    except Exception:
        return None


def chunk_page_map(pages):
    """PDF page (one-based) of each form-feed chunk of the pdftotext output, from the form feeds pypdf finds
    inside each page's own text."""
    out = []
    for i, page in enumerate(pages):
        try:
            inner = (page.extract_text() or "").count("\f")
        except Exception:
            inner = 0
        out += [i + 1] * (inner + 1)
    return out


# PDF hashes: {stem: {"sha256": ..., "changed": epoch seconds when these bytes were first seen, "content_changed":
# the same for the last change that was not an OCR rewrite}}. Any change re-extracts the text; only a non-OCR
# change makes the digest stale, since ocr_scans.py adds a text layer to the same pages (it logs SHA-256 before
# and after, and keeps the modification time for exactly this reason). A PDF with no record yet is recorded with
# its modification time, so the first run marks nothing stale.
HASHES = os.path.join(HERE, "pdf_hashes.json")
hashes = json.load(open(HASHES, encoding="utf-8")) if os.path.exists(HASHES) else {}
OCR_LOG = os.path.join(HERE, "ocr_log.json")
ocr_rewrites = {(e.get("sha_before"), e.get("sha_after"))
                for e in (json.load(open(OCR_LOG, encoding="utf-8")) if os.path.exists(OCR_LOG) else [])}
reextracted, changed = [], []


def card_write_times():
    """When each card was last WRITTEN, for the staleness test (2026-10-29).

    The cards are in a git worktree now, and git does not store modification times: a card's file mtime is
    whenever git last checked it out, not when it was authored, so comparing it with the PDF's would call an old
    card current after any checkout.  A committed card's time is the commit date of its last change, from one
    `git log` pass (newest first, the first date seen per path).  A card with uncommitted changes (a run
    writing, or one that crashed before committing) keeps its file mtime, which is then the true write time.
    """
    times, cur = {}, None
    log = cards.git("log", "--format=%x01%ct", "--name-only", "--", "papers/digests").stdout
    for line in log.splitlines():
        if line.startswith("\x01"):
            cur = int(line[1:])
        elif line.strip().endswith(".md") and cur is not None:
            times.setdefault(os.path.basename(line.strip())[:-3], cur)
    dirty = {os.path.basename(l[3:].strip().strip('"'))[:-3]
             for l in cards.git("status", "--porcelain", "--", "papers/digests").stdout.splitlines()
             if l.strip().endswith(".md")}
    return times, dirty


card_times, cards_dirty = card_write_times()
manifest = []
for fn in sorted(os.listdir(ARCH)):
    if not fn.lower().endswith(".pdf"):
        continue
    stem = fn[:-4]
    pdf = os.path.join(ARCH, fn)
    txt = os.path.join(TEXT, stem + ".txt")
    digest_sha = sha256(pdf)
    known = hashes.get(stem)
    unrecorded = known is None
    if unrecorded:
        first_seen = os.path.getmtime(pdf)
        hashes[stem] = known = {"sha256": digest_sha, "changed": first_seen, "content_changed": first_seen}
    elif known["sha256"] != digest_sha:
        now = time.time()
        known["previous"] = {"sha256": known["sha256"], "content_changed": known["content_changed"]}
        if (known["sha256"], digest_sha) not in ocr_rewrites:
            known["content_changed"] = now
            changed.append(stem)
        known.update(sha256=digest_sha, changed=now)
    elif ("previous" in known and (known["previous"]["sha256"], digest_sha) in ocr_rewrites
          and known["content_changed"] != known["previous"]["content_changed"]):
        # ocr_scans.py writes its log only when it finishes, so a run during OCR sees the change first: once the
        # log records the pair, it was an OCR rewrite after all and the digest is not stale.
        known["content_changed"] = known["previous"]["content_changed"]
    # A PDF seen for the first time is always re-extracted: a text file of unknown origin is not trusted (on the
    # first run, two text files held an erratum and a supplement in place of the articles now archived).
    if (unrecorded or not os.path.exists(txt) or os.path.getmtime(txt) < os.path.getmtime(pdf)
            or os.path.getmtime(txt) < known["changed"]):
        before = open(txt, "rb").read() if os.path.exists(txt) else None
        # pdftotext gets ANSI-code-page args; archive names are ASCII, so direct paths are safe here
        subprocess.run([PDFTOTEXT, "-layout", "-enc", "UTF-8", pdf, txt], capture_output=True)
        after = open(txt, "rb").read() if os.path.exists(txt) else None
        if before is not None and before != after:
            reextracted.append(stem)
    text = open(txt, encoding="utf-8", errors="replace").read() if os.path.exists(txt) else ""
    chunks = text.count("\f") + (1 if text and not text.endswith("\f") else 0)
    reader_pages = pdf_pages(pdf)
    pages = len(reader_pages) if reader_pages is not None else chunks
    chunk_page = chunk_page_map(reader_pages) if reader_pages is not None and chunks != pages else None
    if chunk_page is not None and len(chunk_page) != chunks:
        chunk_page = None   # embedded form feeds do not explain the difference: render the pages instead
    letters = len(re.findall(r"[A-Za-z]", text))
    kind, key = stem_keys(stem)
    recs = by_pmid.get(key, []) if kind == "pmid" else by_doi.get(norm_doi(key), []) if kind == "doi" \
        else nj_records(stem) if stem.startswith("NJ_") else []
    locs, drugs, used = [], [], []
    for r in recs:
        locs += [l for l in r.get("locations", []) if l not in locs]
        drugs += [d for d in r.get("drugs", []) if d not in drugs]
        if r.get("used_for") and r["used_for"] not in used:
            used.append(r["used_for"])
    pm = next((r.get("pm") for r in recs if r.get("pm")), None) or {}
    ctx = {
        "stem": stem,
        "pdf": pdf,
        "sha256": digest_sha,
        "text": txt,
        "pages": pages,                # the PDF's own page count (pypdf), not the number of form-feed chunks
        "text_chunks": chunks if chunks != pages else None,
        "chunk_page": chunk_page,      # PDF page of each text chunk, when the two counts differ
        "scan": letters < 500 * max(pages, 1) * 0.2,  # almost no text layer -> read page images instead
        "pubmed": pm,
        "citation_text": [r.get("citation_text") for r in recs if r.get("citation_text")],
        "drugs": drugs,
        "used_for": used,
        "locations": locs,
        "notes": [r.get("notes") for r in recs if r.get("notes")],
        "cited_in_repo": bool(recs),
        "nonjournal": nj_meta.get(stem),   # source URL, application number, date, kind, note (NJ_ files only)
    }
    json.dump(ctx, open(os.path.join(CTX, stem + ".json"), "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    dig = os.path.join(CARDS, stem + ".md")
    state = "missing"
    if os.path.exists(dig):
        # git's record of when the card was written, not the checkout's file time (card_write_times())
        written = card_times.get(stem) if stem in card_times and stem not in cards_dirty else os.path.getmtime(dig)
        state = "stale" if written < os.path.getmtime(pdf) or written < known["content_changed"] else "done"
    manifest.append({"stem": stem, "drugs": drugs, "pages": pages, "text_chunks": ctx["text_chunks"],
                     "scan": ctx["scan"], "edition": "accepted_manuscript" if AAM.match(stem) else "published",
                     "cited": bool(recs), "digest": state,
                     "title": pm.get("title"), "first_author": pm.get("first_author"), "year": pm.get("year")})

json.dump(manifest, open(os.path.join(HERE, "digest_manifest.json"), "w", encoding="utf-8"), indent=1,
          ensure_ascii=False)
json.dump(hashes, open(HASHES, "w", encoding="utf-8"), indent=1, sort_keys=True)
done = sum(m["digest"] == "done" for m in manifest)
todo = [m for m in manifest if m["digest"] != "done"]
aams = [m for m in manifest if m["edition"] == "accepted_manuscript"]
by_stem = {m["stem"]: m for m in manifest}
with open(os.path.join(DIG, "STATUS.md"), "w", encoding="utf-8") as f:
    f.write("# Digest status\n\nGenerated by `_build/digest_prep.py` (run by `update.bat`). "
            f"{done} of {len(manifest)} archived PDFs have a digest.\n\n")
    if any(m["edition"] == "published" for m in todo):
        f.write("## Needing a digest\n\n| File | State | First author | Year | Drugs | Scan |\n|---|---|---|---|---|---|\n")
        for m in todo:
            if m["edition"] == "published":
                f.write(f"| {m['stem']} | {m['digest']} | {m['first_author'] or ''} | {m['year'] or ''} | "
                        f"{', '.join(m['drugs'])} | {'yes' if m['scan'] else ''} |\n")
    # Accepted manuscripts (2026-10-28): "needs a digest" and "needs the published PDF" are two facts about one work,
    # so they get their own table; see digests/README.md for how such a card cites pages (PDF pages only).
    if aams:
        f.write("\n## Accepted manuscripts\n\nAuthors' accepted manuscripts filed by `_build/unpaywall_fetch.py` as "
                "`PMID_n_aam.pdf`. The work stays *To download* until the publisher's PDF is archived as `PMID_n.pdf`; "
                "both files are kept.\n\n| File | Digest | Published PDF | First author | Year | Drugs |\n"
                "|---|---|---|---|---|---|\n")
        for m in aams:
            pub = by_stem.get(m["stem"][:-4])
            f.write(f"| {m['stem']} | {m['digest']} | {'archived' if pub else 'not yet'} | {m['first_author'] or ''} | "
                    f"{m['year'] or ''} | {', '.join(m['drugs'])} |\n")
    # The transition where a proof correction could slip past: a manuscript was digested, then the published PDF
    # arrived. Listed until the published file has its own current digest.
    recheck = [m for m in aams if m["digest"] != "missing" and m["stem"][:-4] in by_stem
               and by_stem[m["stem"][:-4]]["digest"] != "done"]
    if recheck:
        f.write("\n## Published version arrived, accepted-manuscript digest to re-check\n\nThe publisher's PDF is now "
                "archived for these works. Re-walk the manuscript digest's numbers (those an implemented model pins "
                "first) against the published PDF and write its digest; the entry clears when `PMID_n.md` is current."
                "\n\n| Published | Manuscript digest | Published digest |\n|---|---|---|\n")
        for m in recheck:
            p = by_stem[m["stem"][:-4]]
            f.write(f"| {p['stem']} | {m['stem']}.md ({m['digest']}) | {p['digest']} |\n")
    # file_supplements.py (run first by update.bat) leaves here what it could not place (2026-10-27)
    wp = os.path.join(HERE, "supplements_waiting.json")
    waiting = json.load(open(wp, encoding="utf-8")) if os.path.exists(wp) else []
    if waiting:
        f.write("\n## Supplements waiting\n\nSupplementary files in Downloads that `_build/file_supplements.py` could "
                "not match to a reference. To file one, add `{\"sha256\", \"size\", \"parent\": \"PMID_n\", \"note\"}` "
                "to `_build/supplement_map.json`; to silence one, `{\"sha256\", \"size\", \"ignore\": true}`.\n\n"
                "| File | Size | Why | First line | sha256 |\n|---|---|---|---|---|\n")
        for w in waiting:
            fl = (w.get("first_line") or "").replace("|", "/")[:110]
            f.write(f"| {os.path.basename(w['file'])} | {w['size']} | {w['why']} | {fl} | {w['sha256'][:16]} |\n")
    split = [m for m in manifest if m["text_chunks"] is not None]
    if split:
        f.write("\n## Text chunks that are not PDF pages\n\nIn these files the text layer's form-feed chunks do "
                "not match the PDF's pages, so chunk N is not PDF page N. Use `chunk_page` in "
                "`_build/context/<stem>.json`, or render the page.\n\n| File | PDF pages | Text chunks | Digest |\n"
                "|---|---|---|---|\n")
        for m in split:
            f.write(f"| {m['stem']} | {m['pages']} | {m['text_chunks']} | {m['digest']} |\n")

# Machine-readability check (adversarial review 2026-10-14, section E): every card's frontmatter and fenced yaml
# blocks must parse, each card needs a yaml parameters block, and `verification:` must be one of the four fixed
# values. Needs PyYAML (C:\Python314 has it); without it the check is reported as skipped.
VERDICTS = {"matches", "discrepancy", "partly-verified", "not-applicable"}
REQUIRED_HEADINGS = ["Study", "Model and parameters", "Pharmacodynamics and end points", "Covariates",
                     "Caveats", "Used by stanpumpR", "Verification against stanpumpR"]
problems = []
cards = []   # (card file, frontmatter) for REVIEW.md
try:
    import yaml
except ImportError:
    yaml = None
if yaml:
    for fn in sorted(os.listdir(CARDS)):
        if not fn.endswith(".md") or fn in ("README.md", "STATUS.md", "REVIEW.md"):
            continue
        card = open(os.path.join(CARDS, fn), encoding="utf-8").read()
        fm_match = re.match(r"---\n(.*?)\n---\n", card, re.S)
        try:
            fm = yaml.safe_load(fm_match.group(1)) if fm_match else None
            if not isinstance(fm, dict):
                problems.append((fn, "no frontmatter"))
            else:
                if fm.get("verification") not in VERDICTS:
                    problems.append((fn, f"verification is {fm.get('verification')!r}"))
                # an accepted manuscript's card must say so, and only such a card may (2026-10-28)
                if AAM.match(fn[:-3]) and fm.get("edition") != "accepted_manuscript":
                    problems.append((fn, "an _aam card needs `edition: accepted_manuscript`"))
                elif not AAM.match(fn[:-3]) and fm.get("edition") == "accepted_manuscript":
                    problems.append((fn, "`edition: accepted_manuscript` on a card that is not an _aam file"))
                oq = fm.get("open_questions")
                if oq is not None and not (isinstance(oq, list) and all(isinstance(q, str) for q in oq)):
                    problems.append((fn, "`open_questions` must be a list of strings"))
                if fm.get("reviewed") is not None and not re.match(r"\d{4}-\d{2}-\d{2}", str(fm["reviewed"])):
                    problems.append((fn, "`reviewed` must start with a date, YYYY-MM-DD"))
                cards.append((fn, fm))
        except yaml.YAMLError as e:
            problems.append((fn, "frontmatter: " + str(e).splitlines()[0]))
        # A card cut off mid-write still parses, so also require every template heading (2026-10-27).
        missing = [h for h in REQUIRED_HEADINGS if not re.search(r"^## " + re.escape(h) + r"\b", card, re.M)]
        if missing:
            problems.append((fn, "missing sections: " + ", ".join(missing)))
        blocks = re.findall(r"```yaml\n(.*?)```", card, re.S)
        if not blocks:
            problems.append((fn, "no yaml parameters block"))
        for b in blocks:
            try:
                yaml.safe_load(b)
            except yaml.YAMLError as e:
                problems.append((fn, "yaml block: " + str(e).splitlines()[0]))
with open(os.path.join(DIG, "STATUS.md"), "a", encoding="utf-8") as f:
    if yaml is None:
        f.write("\n## Card format\n\nNot checked: PyYAML is not installed (`python -m pip install pyyaml`).\n")
    elif problems:
        f.write("\n## Card format problems\n\n| Card | Problem |\n|---|---|\n")
        for fn, why in problems:
            f.write(f"| {fn} | {why} |\n")
    else:
        f.write("\n## Card format\n\nEvery card's frontmatter and yaml blocks parse.\n")
# Review queue (2026-10-28): what a person still has to look at after the digests are written, above all after the
# unattended runs of digest_run.py.  A question is closed by revising its card (drop it from `open_questions`, record
# the answer under Verification, add `revised:`); a card is closed by `reviewed: YYYY-MM-DD (who)`, which neither the
# session that wrote it nor an unattended run may write (digests/README.md).
if yaml:
    def unattended(fm):
        return "unattended" in str(fm.get("digest_by") or "")

    KINDS = [("[decision]", "Only Shafer can settle (a dose basis or modelling choice the source leaves open)"),
             ("[source]", "Another paper or the full text would settle it"),
             ("[check]", "Re-reading this PDF's rendered page would settle it"),
             ("", "Not classified")]
    questions = [(fn, str(q)) for fn, fm in cards for q in (fm.get("open_questions") or [])]
    pending = [(fn, fm) for fn, fm in cards if unattended(fm) and not fm.get("reviewed")]
    older = [(fn, fm) for fn, fm in cards if not unattended(fm) and not fm.get("reviewed")
             and fm.get("verification") in ("discrepancy", "partly-verified")]
    with open(os.path.join(DIG, "REVIEW.md"), "w", encoding="utf-8") as f:
        f.write("# Digest review queue\n\nGenerated by `_build/digest_prep.py`. Close a question by revising its card "
                "(remove it from `open_questions`, record the answer under *Verification against stanpumpR*, add a "
                "`revised:` line); close a card with `reviewed: YYYY-MM-DD (who)`, which the session that wrote the card "
                "and the unattended runs never write.\n\n"
                f"Open questions: {len(questions)} · unattended cards not yet reviewed: {len(pending)} · earlier "
                f"discrepancy/partly-verified cards not marked reviewed: {len(older)}\n")
        f.write("\n## Open questions\n")
        for tag, what in KINDS:
            sub = [(fn, q) for fn, q in questions
                   if (q.lstrip().lower().startswith(tag) if tag else not re.match(r"\s*\[(decision|source|check)\]", q, re.I))]
            if sub:
                f.write(f"\n### {tag or 'Other'} {what} ({len(sub)})\n\n| Card | Question |\n|---|---|\n")
                for fn, q in sub:
                    f.write(f"| {fn[:-3]} | {q.replace('|', '/')} |\n")
        if not questions:
            f.write("\nNone.\n")
        f.write("\n## Unattended cards not yet reviewed, every verdict\n\nA wrong `matches` is silent: it tells later "
                "sessions a number was checked when it was not, so these are listed whatever their verdict.\n\n"
                "| Card | Verdict | Open questions | Source read | Digest by |\n|---|---|---|---|---|\n")
        for fn, fm in pending:
            f.write(f"| {fn[:-3]} | {fm.get('verification')} | {len(fm.get('open_questions') or [])} | "
                    f"{str(fm.get('source_read') or '').replace('|', '/')[:60]} | {fm.get('digest_by')} |\n")
        f.write("\n## Earlier cards with a discrepancy or partial verification, not marked reviewed\n\n"
                "| Card | Verdict | Checked against |\n|---|---|---|\n")
        for fn, fm in older:
            f.write(f"| {fn[:-3]} | {fm.get('verification')} | {fm.get('checked_against') or ''} |\n")
    with open(os.path.join(DIG, "STATUS.md"), "a", encoding="utf-8") as f:
        f.write(f"\n## Review queue\n\nSee `REVIEW.md`: {len(questions)} open questions, {len(pending)} unattended cards "
                f"not yet reviewed, {len(older)} earlier discrepancy/partly-verified cards.\n")
print(f"digests: {done} done, {len(todo)} needed (see digests\\STATUS.md)")
if pypdf is None:
    print("pypdf is not installed: page counts fall back to form-feed chunks (python -m pip install pypdf)")
if changed:
    print(f"PDFs whose content changed since last run: {', '.join(changed)}")
if reextracted:
    print(f"text files whose content changed on re-extraction: {', '.join(reextracted)}")
if problems:
    print(f"card format problems: {len(problems)} (see digests\\STATUS.md)")
