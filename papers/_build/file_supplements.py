"""File journals' supplementary material from Downloads into the archive beside its article.

file_downloads.py files articles, which are PDFs; a supplement is often a Word file, a .doc, a CSV, a spreadsheet
or a zip, and until 2026-10-27 nothing filed those: about 75 sat in Downloads for ten days, unseen, while their
articles' cards said "not in the archived PDF".  This script runs first in update.bat, so a supplement PDF is
filed here as a supplement and never reaches file_downloads.py, which would hold it back as one.

A file is a supplement when its NAME has a publisher's supplement pattern, or a generic one ("Data Sheet 1",
"Appendix", "Supplementary ..."), or when its sha256 is in supplement_map.json.  Its article is found from, in order:
  1. supplement_map.json, by sha256: files whose names say nothing ("Data Sheet 1.docx"), identified by hand;
  2. the name, which most publishers derive from the article's DOI:
       Wiley        bcp15939-sup-0001-...   -> 10.1111/bcp.15939      (any "<journal><article>-sup-NNNN")
       Springer     40262_2015_307_MOESM1_ESM -> 10.1007/s40262-015-0307-*
       Nature       41598_2025_3829_MOESM1_ESM -> 10.1038/s41598-025-03829-*
       Wiley (old)  BCP-83-823-s001         -> volume 83, first page 823
       Wiley link   downloadSupplement_doi=10.1002_jcph.899&file=... -> 10.1002/jcph.899
  3. its own text: a reference's DOI, or its title together with its first author's surname, near the top
     (Novo Nordisk's and AstraZeneca's supplements print the article's title and authors).
A name match is the DOI itself and is taken as it is; a text match follows the same rules as file_downloads.py.
It is filed as PMID_<pmid>_<label>.<ext>, keeping the publisher's own number: sup0001 (Wiley), ESM1 (Springer and
Nature), mmc1 (Elsevier), else the next free supNNNN.  The original is copied, checked byte-identical, and sent to
the Recycle Bin; a Downloads copy identical to a file already archived is recycled too.  Anything it cannot place
is left alone and listed, with the first line of its text, in supplements_waiting.json, which digest_prep.py
prints in digests\\STATUS.md.  To place one, add {"sha256", "size", "parent", "note"} to supplement_map.json
(optionally "label", e.g. "sup0001"); to silence a file that is not a supplement, {"sha256", "size", "ignore": true}.

Usage: python file_supplements.py [--dry-run]
"""
import datetime, hashlib, json, os, re, shutil, subprocess, sys, zipfile
from ncbi import norm
from recycle import recycle

HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
DOWNLOADS = os.path.join(os.path.expanduser("~"), "Downloads")
MAP_PATH = os.path.join(HERE, "supplement_map.json")
WAITING_PATH = os.path.join(HERE, "supplements_waiting.json")
LOG_PATH = os.path.join(HERE, "filing_log.json")
RECYCLED_LOG = os.path.join(HERE, "recycled_log.json")
dry = "--dry-run" in sys.argv[1:]

# ------------------------------------------------------------------ the references, as in file_downloads.py
refs = []
for w in json.load(open(os.path.join(HERE, "works.json"), encoding="utf-8")):
    pm, cr = w.get("pm") or {}, w.get("cr") or {}
    src = pm or cr
    pmid = pm.get("pmid", "")
    doi = (pm.get("doi") or cr.get("doi") or w.get("doi") or "").lower()
    if not (pmid or doi) or not src.get("title"):
        continue
    first = src.get("first_author") or (src.get("authors") or [""])[0]
    parts = (first or "").split()
    while len(parts) > 1 and re.fullmatch(r"[A-Z]{1,4}|\d+(?:st|nd|rd|th)|Jr|Sr", parts[-1]):
        parts = parts[:-1]
    refs.append({"pmid": pmid, "doi": doi, "title": src["title"], "surname": norm("".join(parts)),
                 "volume": str(src.get("volume") or "").strip(),
                 "fpage": (re.match(r"\s*([A-Za-z]?\d+)", str(src.get("pages") or "")) or [None, ""])[1] or "",
                 "label": "%s %s" % (src.get("first_author", ""), src.get("year", "")),
                 "stem": ("PMID_%s" % pmid) if pmid else "DOI_" + re.sub(r"[^A-Za-z0-9.]+", "_", doi)})
by_doi = {r["doi"]: r for r in refs if r["doi"]}
by_pmid = {r["pmid"]: r for r in refs if r["pmid"]}
# the DOI suffix with its punctuation removed: 10.1111/bcp.15939 -> bcp15939, 10.1002/psp4.12347 -> psp412347
by_squashed = {}
for r in refs:
    if r["doi"] and "/" in r["doi"]:
        by_squashed.setdefault(re.sub(r"[^a-z0-9]", "", r["doi"].split("/", 1)[1]), []).append(r)

# ------------------------------------------------------------------ which files are supplements
WILEY = re.compile(r"^([a-z]+\d+)[-_]sup[-_](\d{4})", re.I)
SPRINGER = re.compile(r"^(\d{3,5})_(\d{4})_(\d+)_MOESM(\d+)_ESM", re.I)
WILEY_OLD = re.compile(r"^([A-Z]{2,6})-(\d+)-(\d+)-s(\d+)", re.I)
WILEY_LINK = re.compile(r"^downloadSupplement_doi=(10\.\d{4,9})_([^&]+)&file=", re.I)
ELSEVIER = re.compile(r"^(?:\d-s2\.0-\S+-)?mmc(\d+)\.", re.I)
# Generic names only when the whole name says supplement: an article titled "Supplemental oxygen ..." or a journal
# supplement issue ("9-suppl_A-33.pdf", an Eur Heart J article) is not supplementary material.
GENERIC = re.compile(r"^(data ?sheet \d+|appendix( [a-z0-9]+)?|supplementary (data|materials?|information|files?|"
                     r"tables?|figures?|appendix)( \d+)?|supporting information( \d+)?|online resource \d+|"
                     r"table s\d+|figure s\d+|esm\d*)\.\w+$|^[a-z]{2,5}\d{5,6}supp\d", re.I)
EXTS = {".pdf", ".doc", ".docx", ".csv", ".txt", ".xls", ".xlsx", ".zip", ".tif", ".tiff", ".png", ".jpg",
        ".pptx", ".ppt", ".rtf", ".xml", ".r", ".mod", ".ctl"}


def sha(p):
    h = hashlib.sha256()
    with open(p, "rb") as f:
        for b in iter(lambda: f.read(1 << 20), b""):
            h.update(b)
    return h.hexdigest()


def base_name(name):
    """'bcp15939-sup-0001-supplementary information (1).docx' -> name without a browser's ' (1)' repeat mark."""
    stem, ext = os.path.splitext(name)
    return re.sub(r" \(\d+\)$", "", stem) + ext


def from_name(name):
    """(reference, label, how) from a publisher's file name, or (None, label, None)."""
    n = base_name(name)
    m = WILEY_LINK.match(n)
    if m:
        doi = (m.group(1) + "/" + m.group(2)).lower()
        sub = re.search(r"sup[-_](\d{4})", n, re.I)
        return by_doi.get(doi), "sup" + (sub.group(1) if sub else "0001"), "DOI %s in the download link" % doi
    m = WILEY.match(n)
    if m:
        hits = by_squashed.get(m.group(1).lower(), [])
        return (hits[0] if len(hits) == 1 else None), "sup" + m.group(2), "Wiley name -> DOI suffix %s" % m.group(1)
    m = SPRINGER.match(n)
    if m:
        jid, year, art, k = m.groups()
        pat = re.compile(r"/s0*%s-0*%s-0*%s-[0-9a-z]$" % (int(jid), int(year) % 1000, int(art)))
        hits = [r for r in refs if r["doi"] and pat.search(r["doi"])]
        return (hits[0] if len(hits) == 1 else None), "ESM" + k, "Springer name -> DOI s%s-%03d-%s" % (jid, int(year) % 1000, art)
    m = WILEY_OLD.match(n)
    if m:
        _, vol, page, k = m.groups()
        hits = [r for r in refs if r["volume"] == vol and r["fpage"] == page]
        return (hits[0] if len(hits) == 1 else None), "sup%04d" % int(k), "volume %s, first page %s" % (vol, page)
    m = ELSEVIER.match(n)
    if m:
        return None, "mmc" + m.group(1), None
    return None, None, None


def is_candidate(name, h, mapped):
    if h in mapped:
        return True
    n = base_name(name)
    if os.path.splitext(n)[1].lower() not in EXTS:
        return False
    return any(p.match(n) for p in (WILEY, SPRINGER, WILEY_OLD, WILEY_LINK, ELSEVIER)) or bool(GENERIC.search(n))


# ------------------------------------------------------------------ text, for files whose names say nothing
def text_of(path):
    """The first ~20 kB of a file's text.  Downloads are untrusted: they are only read, never run."""
    ext = os.path.splitext(path)[1].lower()
    try:
        if ext in (".docx", ".pptx", ".xlsx"):
            z = zipfile.ZipFile(path)
            names = {".docx": ["word/document.xml"], ".pptx": sorted(x for x in z.namelist() if x.startswith("ppt/slides/slide")),
                     ".xlsx": ["xl/sharedStrings.xml"]}[ext]
            xml = "".join(z.read(x).decode("utf-8", "replace") for x in names if x in z.namelist())
            xml = re.sub(r"</w:p>|</a:p>|</si>", "\n", xml)
            return re.sub(r"<[^>]+>", "", xml)[:20000]
        if ext == ".pdf":
            out = subprocess.run(["pdftotext", "-l", "2", "-q", path, "-"], capture_output=True, timeout=60).stdout
            return out.decode("utf-8", "replace")[:20000]
        if ext in (".doc", ".rtf"):
            raw = open(path, "rb").read(4 << 20)
            runs = re.findall(rb"(?:[\x20-\x7e]\x00){6,}", raw)  # Word 97 stores text as UTF-16LE
            txt = "\n".join(r.decode("utf-16le", "replace") for r in runs)
            if len(txt) < 200:
                txt = "\n".join(r.decode("latin-1") for r in re.findall(rb"[\x20-\x7e\r\n\t]{6,}", raw))
            return txt[:20000]
        if ext in (".csv", ".txt", ".xml", ".r", ".mod", ".ctl"):
            return open(path, encoding="utf-8", errors="replace").read(20000)
    except Exception:
        return ""
    return ""


def from_text(txt):
    top = txt[:4000]
    flat_top, flat = norm(top), norm(txt)
    dois = [d.lower().rstrip(".,;)") for d in re.findall(r"10\.\d{4,9}/[^\s\"<>]+", top)]
    for d in dois:
        r = by_doi.get(d)
        if r and (norm(r["title"])[:50] in flat or (r["surname"] and r["surname"] in flat)):
            return r, "its text prints DOI %s" % d
    hits = [r for r in refs if len(norm(r["title"])) >= 25 and norm(r["title"])[:50] in flat_top
            and r["surname"] and r["surname"] in flat]
    if len(hits) == 1:
        return hits[0], "its text prints the article's title and first author"
    return None, None


def first_line(txt):
    for line in txt.splitlines():
        line = re.sub(r"\s+", " ", line).strip()
        if len(line) >= 12 and not line.upper().startswith(("TOC ", "PAGEREF", "HYPERLINK")):
            return line[:160]
    return ""


# ------------------------------------------------------------------ filing
mapped = {e["sha256"]: e for e in (json.load(open(MAP_PATH, encoding="utf-8")) if os.path.exists(MAP_PATH) else [])}
mapped_sizes = {e.get("size") for e in mapped.values()}
filed_re = re.compile(r"(PMID_\d+|DOI_.+?)_([A-Za-z]+\d+)\.[A-Za-z0-9]+$")
arch_sha = {}
for f in os.listdir(ARCHIVE):
    p = os.path.join(ARCHIVE, f)
    if os.path.isfile(p) and re.match(r"(PMID_|DOI_|NJ_)", f):
        arch_sha[sha(p)] = f
log = json.load(open(LOG_PATH, encoding="utf-8")) if os.path.exists(LOG_PATH) else []
recycled, filed, waiting = [], [], []


def recycle_original(path, archive_file):
    if not dry and sha(path) == sha(os.path.join(ARCHIVE, archive_file)) and recycle(path):
        recycled.append({"recycled": path, "archive_copy": archive_file,
                         "at": datetime.datetime.now().isoformat(timespec="seconds")})


def free_label(stem, ext):
    k = 1
    while any(re.fullmatch(re.escape(stem) + r"_sup%04d\.[A-Za-z0-9]+" % k, f) for f in os.listdir(ARCHIVE)):
        k += 1
    return "sup%04d" % k


for root, dirs, files in os.walk(DOWNLOADS):
    for name in sorted(files):
        path = os.path.join(root, name)
        if not os.path.isfile(path):
            continue
        n = base_name(name)
        if os.path.splitext(n)[1].lower() not in EXTS:
            continue
        by_pattern = is_candidate(name, None, {})
        if not by_pattern and os.path.getsize(path) not in mapped_sizes:
            continue  # hash only what could be a supplement: Downloads can be large
        h = sha(path)
        if not by_pattern and h not in mapped:
            continue
        if mapped.get(h, {}).get("ignore"):
            continue  # Shafer: not a supplement (e.g. a May 2026 Appendix.docx)
        if h in arch_sha:  # already archived, byte for byte (a repeat download, or filed by hand)
            recycle_original(path, arch_sha[h])
            continue
        ext = os.path.splitext(n)[1].lower()
        r, label, how = from_name(name)
        name_how = how
        if h in mapped:
            e = mapped[h]
            r = by_pmid.get(e["parent"].replace("PMID_", "")) or {"stem": e["parent"], "label": e.get("note", ""), "pmid": "", "doi": ""}
            how = "supplement_map.json: %s" % e.get("note", "")
            label = e.get("label") or label
        txt = None
        if r is None:
            txt = text_of(path)
            r, how = from_text(txt)
        if r is None:
            waiting.append({"file": path, "size": os.path.getsize(path), "sha256": h,
                            "first_line": first_line(txt if txt is not None else text_of(path)),
                            "why": ("article not in the reference list (%s)" % name_how) if name_how else "no article identified"})
            continue
        label = label or free_label(r["stem"], ext)
        dst_name = "%s_%s%s" % (r["stem"], label, ext)
        dst = os.path.join(ARCHIVE, dst_name)
        if os.path.exists(dst):  # a different file already has this name: leave it for a look
            waiting.append({"file": path, "size": os.path.getsize(path), "sha256": h, "first_line": "",
                            "why": "%s already exists with different content" % dst_name})
            continue
        if not dry:
            shutil.copy2(path, dst)
            arch_sha[sha(dst)] = dst_name
            recycle_original(path, dst_name)
            log.append({"from": path, "to": dst_name, "pmid": r.get("pmid"), "doi": r.get("doi"), "how": how})
        filed.append((name, dst_name, r.get("label", ""), how,
                      os.path.exists(os.path.join(ARCHIVE, r["stem"] + ".pdf"))))

if not dry:
    json.dump(log, open(LOG_PATH, "w", encoding="utf-8"), indent=1)
    json.dump(waiting, open(WAITING_PATH, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    if recycled:
        old = json.load(open(RECYCLED_LOG, encoding="utf-8")) if os.path.exists(RECYCLED_LOG) else []
        json.dump(old + recycled, open(RECYCLED_LOG, "w", encoding="utf-8"), indent=1)  # escaped, as file_downloads.py writes it
print("%s %d supplement(s):" % ("Would file" if dry else "Filed", len(filed)))
for name, f, label, how, parent in filed:
    print("  %s -> %s  [%s; %s]%s" % (name, f, label, how, "" if parent else "  (article not archived)"))
if recycled:
    print("Moved %d supplement original(s) to the Recycle Bin (identical copies are in the archive)." % len(recycled))
if waiting:
    print("Supplements waiting (%d, listed in digests\\STATUS.md):" % len(waiting))
    for w in waiting:
        print("  %s: %s | %s" % (os.path.basename(w["file"]), w["why"], w["first_line"][:90]))
