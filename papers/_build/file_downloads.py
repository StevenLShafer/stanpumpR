"""File stanpumpR reference PDFs from Downloads into the archive as PMID_<pmid>.pdf.

Each PDF is identified from its own text, not its file name:
  - a DOI printed on its first two pages that belongs to a reference, confirmed by the title or first author; or
  - the reference's title printed verbatim (punctuation and line breaks ignored) and its first author's surname.
Word-overlap alone is never enough: on 2026-10-07 it filed an NEJM renal-artery paper as Cockcroft-Gault 1976
(80% of the title's words, the surname in a citation, a stray "31").
Anything weaker is reported for review and left alone.  Files are COPIED; Downloads is not changed.

Usage: python file_downloads.py [--dir <folder>] [--archive <folder>] [--dry-run]
"""
import json, logging, os, re, shutil, subprocess, sys, time
from ncbi import norm
from inreview import in_review_reason

sys.stdout.reconfigure(encoding="utf-8")  # Downloads file names carry Unicode (U+2010 hyphens); update.bat's PYTHONIOENCODING does not reach `python -E`

logging.getLogger("pypdf").setLevel(logging.ERROR)  # malformed-PDF chatter ("Ignoring wrong pointing object ...")

HERE = os.path.dirname(os.path.abspath(__file__))
args = sys.argv[1:]
dry = "--dry-run" in args
ARCHIVE = args[args.index("--archive") + 1] if "--archive" in args else os.path.dirname(HERE)
CACHE_PATH = os.path.join(HERE, "filing_cache.json")
LOG_PATH = os.path.join(HERE, "filing_log.json")
scan_dirs = [args[args.index("--dir") + 1]] if "--dir" in args else [os.path.join(os.path.expanduser("~"), "Downloads"), ARCHIVE]


def words(s):
    return set(w for w in re.findall(r"[a-z0-9]+", (s or "").lower()) if len(w) > 3)


SUFFIX = r"[A-Z]{1,4}|\d+(?:st|nd|rd|th)|Jr|Sr"  # initials, and generational suffixes ("Butterworth JF 4th")


def surname(name):
    parts = (name or "").split()
    while len(parts) > 1 and re.fullmatch(SUFFIX, parts[-1]):
        parts = parts[:-1]
    return norm("".join(parts))


def first_page(p):
    m = re.match(r"\s*([A-Za-z]?\d+)", str(p or ""))
    return m.group(1) if m else ""


def archive_name(pmid, doi):
    return ("PMID_%s.pdf" % pmid) if pmid else ("DOI_" + re.sub(r"[^A-Za-z0-9.]+", "_", doi) + ".pdf")


def page_count(pages):
    """Published length from a PubMed page range: '1043-50' -> 8, '1418-29' -> 12, '59-68; discussion 27A' -> 10."""
    m = re.match(r"\s*(\d+)\s*-\s*(\d+)\s*(?:$|[;,])", str(pages or ""))
    if not m:
        return None
    a, b = m.groups()
    if len(b) < len(a):
        b = a[: len(a) - len(b)] + b
    n = int(b) - int(a) + 1
    return n if 0 < n < 200 else None


# ------------------------------------------------------------------ the references to look for
refs = []
for w in json.load(open(os.path.join(HERE, "works.json"), encoding="utf-8")):
    pm, cr = w.get("pm") or {}, w.get("cr") or {}
    src = pm or cr
    pmid = pm.get("pmid", "")
    doi = (pm.get("doi") or cr.get("doi") or w.get("doi") or "").lower()
    if not (pmid or doi) or not src.get("title"):
        continue
    refs.append({"pmid": pmid, "doi": doi, "title": src["title"], "tw": words(src["title"]),
                 "journal_n": norm(src.get("journal_full") or "") + "|" + norm(src.get("journal") or ""),
                 "surname": surname(src.get("first_author") or (src.get("authors") or [""])[0]),
                 "fpage": first_page(src.get("pages")), "year": src.get("year"),
                 "volume": str(src.get("volume") or "").strip(), "npages": page_count(src.get("pages")),
                 "label": "%s %s" % (src.get("first_author", ""), src.get("year", "")), "file": archive_name(pmid, doi)})
by_doi = {r["doi"]: r for r in refs if r["doi"]}
by_pmid = {r["pmid"]: r for r in refs if r["pmid"]}


def surname_pattern(name):
    """Regex for a surname whose accented letters a scan's text layer may have mangled
    (Müller -> 'Mtiller', Vainionpää -> 'VainionpG'): each non-ASCII letter matches 0-2 characters."""
    parts = (name or "").split()
    while len(parts) > 1 and re.fullmatch(SUFFIX, parts[-1]):
        parts = parts[:-1]
    s = "".join(parts).lower()
    if all(ord(c) < 128 for c in s):
        return None
    pat = "".join(c if ("a" <= c <= "z" or c.isdigit()) else (".{0,2}" if c.isalpha() else "") for c in s)
    return pat if len(re.sub(r"\.\{0,2\}", "", pat)) >= 4 else None


for r in refs:
    r["sur_re"] = surname_pattern(r["label"].rsplit(" ", 1)[0])


def pdf_text(path, last=2, strict=False):
    # pdftotext receives its arguments in the Windows ANSI code page, so it cannot open a path with characters
    # outside it, e.g. Wiley's U+2010 hyphen ("long‐term"), and returned nothing: those PDFs were reported as
    # scans (2026-10-10: Pollak 2000, Guay 1988, Stoeckel 1981 ...).  Read such files through an ASCII-named copy.
    # `last` is the last page read (identification needs only the first two); None reads the whole document.
    tmp = None
    try:
        if not path.isascii():
            import tempfile
            fd, tmp = tempfile.mkstemp(suffix=".pdf")
            os.close(fd)
            shutil.copyfile(path, tmp)
        pages = ["-l", str(last)] if last else []
        r = subprocess.run(["pdftotext"] + pages + ["-q", tmp or path, "-"], capture_output=True, timeout=120)
        if strict and r.returncode != 0:
            raise RuntimeError("pdftotext returned %d" % r.returncode)
        return r.stdout.decode("utf-8", "replace")
    except Exception:
        if strict:                               # the caller must tell "no text" from "could not read"
            raise
        return ""
    finally:
        if tmp and os.path.exists(tmp):
            os.remove(tmp)


ENVELOPE_BACKUP = ".pre-unwrap"          # not ".pdf", so the filing loop never offers it as a candidate


def unwrap_envelope(path):
    """Strip a multipart/form-data envelope a download left wrapped around the PDF.

    Clio, Stanford's interlibrary loan, delivers through a form POST, and saving the response
    body verbatim writes the whole envelope: a boundary line, the part's headers, the PDF, then
    the closing boundary.  Poppler, pypdf and pdfium all skip the preamble, so such a file reads,
    renders and extracts normally -- the twelve already in the archive were digested correctly --
    but it is not a PDF by its magic bytes, and a stricter reader will refuse it.  Found
    2026-11-18 on nineteen files, seven in Downloads and twelve already filed.

    Returns True if the file was rewritten.  Nothing is replaced unless the body starts with
    %PDF, carries %%EOF near its end, and opens with the SAME page count as the envelope did;
    the original is recycled, never deleted, as everything in this archive is."""
    try:
        with open(path, "rb") as f:
            head = f.read(5)
    except OSError:
        return False
    if head.startswith(b"%PDF") or not head.startswith(b"--"):
        return False                             # a PDF, or something this does not understand
    try:
        data = open(path, "rb").read()
    except OSError:
        return False
    start = data.find(b"%PDF")
    nl = data.find(b"\r\n")
    boundary = data[:nl] if nl > 0 else b""
    if start < 0 or not boundary:
        return False
    end = data.rfind(boundary)
    body = (data[start:end] if end > start else data[start:]).rstrip(b"\r\n")
    if not body.startswith(b"%PDF") or b"%%EOF" not in body[-4096:]:
        return False
    if dry:
        print("  %s: wrapped in a %d-byte form envelope; --dry-run, so left as it is"
              % (os.path.basename(path), len(data) - len(body)), flush=True)
        return False
    import pypdf
    try:
        before = len(pypdf.PdfReader(path).pages)
    except Exception:
        before = None                            # unreadable as it stands: the page check cannot run
    tmp, keep = path + ".unwrapped", path + ENVELOPE_BACKUP
    try:
        with open(tmp, "wb") as f:
            f.write(body)
        after = len(pypdf.PdfReader(tmp).pages)
        if before is not None and after != before:
            raise RuntimeError("%d pages before, %d after" % (before, after))
        shutil.copyfile(path, keep)              # the original, on its way to the Recycle Bin
        os.replace(tmp, path)
    except Exception as e:
        for junk in (tmp, keep):
            if os.path.exists(junk):
                os.remove(junk)
        print("  %s: looks wrapped in a form envelope but was left alone (%s)"
              % (os.path.basename(path), e), flush=True)
        return False
    try:
        gone = recycle(keep)
    except Exception:
        gone = False
    if gone:
        unwrapped.append({"recycled": keep, "archive_copy": os.path.basename(path),
                          "why": "the form envelope was stripped; this is the original, envelope and all",
                          "at": datetime.datetime.now().isoformat(timespec="seconds")})
    else:                                        # the repair stands, but say where the spare copy is
        review.append((os.path.basename(path), "the form envelope was stripped, but the Recycle Bin refused the "
                       "original: %s is still there, remove it by hand" % os.path.basename(keep)))
    print("  %s: stripped a %d-byte form envelope (%d pages)"
          % (os.path.basename(path), len(data) - len(body), after), flush=True)
    return True


def identify(path):
    r, why, problem = identify_any(path)
    # An erratum, an abstract, a first-page preview or a journal's supplementary file repeats the article's DOI or
    # title, so it can pass the text rules for the article itself (2026-10-17: the one-page Xue 2017 erratum was filed
    # as the 13-page paper; 2026-10-19: a 4-page supplement "40262_..._MOESM1_ESM.pdf" as the 12-page Li 2024).  Such
    # a file is left for a look: one named like a supplement, or one with under half the article's published pages.
    # A print-to-PDF's page count means nothing, and a file already named PMID_ is taken as named.
    if r and not why.startswith("named"):
        if re.search(r"MOESM|_ESM\b|_ESM\.|suppl", os.path.basename(path), re.I):
            return None, None, "named like supplementary material for %s (%s): save it as %s by hand" % (
                r["file"], r["label"], r["file"].replace(".pdf", "_ESM1.pdf"))
        if r.get("npages") and r["npages"] >= 4 and "print" not in why.lower():
            try:
                import pypdf
                n = len(pypdf.PdfReader(path).pages)
            except Exception:
                n = None
            if n and n < r["npages"] / 2:
                return None, None, "%d pages, but it matches %s (%s, %d pp): an erratum, abstract or supplement? check by hand" % (
                    n, r["file"], r["label"], r["npages"])
    return r, why, problem


_requested = {}


def requested_ref(pmid):
    """A ref for a paper the DOWNLOAD QUEUE asks for but no citation produced.

    `refs` above comes from works.json, the works the stanpumpR SOURCE cites, so a paper a
    registry's `to_obtain` wants is not in it -- and until 2026-11-18 such a paper, correctly
    named PMID_<n>.pdf by hand, was refused as "not a stanpumpR reference" and left in
    Downloads.  Nine of the ten papers blocking a registry row arrived that way on that day.
    A model can want a paper long before anything in the code cites it; that is the ordinary
    case for a row whose readiness is `needs_full_text`.

    The queue is the gate, not the file name: arrivals.py puts every wanted PMID there, so
    this accepts exactly what somebody asked for and nothing else.  The details come from
    PubMed, as build.py's do, so the row filed is the same shape as any other."""
    if "rows" not in _requested:                 # not `if not _requested`: by_doi may have filled it
        _requested["rows"] = set()
        try:
            import csv
            with open(os.path.join(ARCHIVE, "stanpumpr-queue.csv"), encoding="utf-8") as f:
                _requested["rows"] = {(r.get("pmid") or "").strip() for r in csv.DictReader(f)}
        except Exception:
            pass                                  # no queue: behave exactly as before
    if pmid not in _requested["rows"]:
        return None
    try:
        import ncbi
        src = ncbi.esummary([pmid]).get(pmid)
    except Exception:
        _requested["failed"] = True              # retryable: the caller must not cache a miss
        return None
    if not src or not src.get("title"):
        # esummary() drops a PMID whose result carries an `error` entry (ncbi.py), so "no
        # summary" is a transient NCBI failure as often as a bad PMID.  The row is in the
        # queue, which means somebody asked for this PMID, so treat it as retryable rather
        # than caching a permanent miss (CodeRabbit, PR #784).
        _requested["failed"] = True
        return None
    return {"pmid": pmid, "doi": (src.get("doi") or "").lower(), "title": src["title"],
            "tw": words(src["title"]),
            "journal_n": norm(src.get("journal_full") or "") + "|" + norm(src.get("journal") or ""),
            "surname": surname(src.get("first_author") or (src.get("authors") or [""])[0]),
            "fpage": first_page(src.get("pages")), "year": src.get("year"),
            "volume": str(src.get("volume") or "").strip(),
            "npages": page_count(src.get("pages")),
            "label": "%s %s" % (src.get("first_author", ""), src.get("year", "")),
            "file": archive_name(pmid, (src.get("doi") or "").lower())}


def requested_ref_by_doi(doi):
    """The queue's row for a DOI, as a ref.  Same gate as requested_ref: only what was asked for."""
    if not _requested.get("by_doi"):
        _requested.setdefault("by_doi", {})
        try:
            import csv
            with open(os.path.join(ARCHIVE, "stanpumpr-queue.csv"), encoding="utf-8") as f:
                for r in csv.DictReader(f):
                    d, p = (r.get("doi") or "").strip().lower(), (r.get("pmid") or "").strip()
                    if d and p:
                        _requested["by_doi"][d] = p
        except Exception:
            pass
    pmid = _requested.get("by_doi", {}).get((doi or "").lower())
    return requested_ref(pmid) if pmid else None


def identify_any(path):
    name = os.path.basename(path)
    m = re.fullmatch(r"PMID_(\d+)(?: \(\d+\))?\.pdf", name, re.I)
    if m:
        r = by_pmid.get(m.group(1)) or requested_ref(m.group(1))
        if r:
            return r, "named PMID_%s" % m.group(1), None
    # A web page printed to PDF (Shafer does this when a journal offers no PDF): the print driver puts the page's
    # title in the metadata, often with the middle elided ("Effect of Rifampin on the Plasma Concen...Journal of
    # Clinical Psychopharmacology") and the site appended (" - CNKI", " _ Therapeutic Drug Monitoring").  Microsoft
    # Print to PDF may leave no text layer at all, and the page count bears no relation to the journal's pages.
    try:
        import pypdf
        meta = pypdf.PdfReader(path).metadata or {}
    except Exception:
        meta = {}
    producer = (str(meta.get("/Producer", "")) + " " + str(meta.get("/Creator", ""))).lower()
    if any(k in producer for k in ("print to pdf", "skia", "chrome", "microsoft edge", "mozilla")):
        title = str(meta.get("/Title") or "")
        parts = re.split(r"\.\.\.|…", title, maxsplit=1)
        head = norm(parts[0])
        tail = norm(parts[1]) if len(parts) > 1 else ""
        for sep in (" - ", " _ ", " | "):  # a whole title with the site appended
            if len(parts) == 1 and sep in title:
                head = norm(title.rsplit(sep, 1)[0])
        if len(head) >= 25:
            cands = [r for r in refs if norm(r["title"]).startswith(head) or (not tail and head.startswith(norm(r["title"])[:60]) and len(norm(r["title"])) >= 25)]
            if tail and len(cands) > 1:
                cands = [r for r in cands if norm(r["title"]).endswith(tail) or (tail and tail in r["journal_n"])]
            if len(cands) == 1:
                return cands[0], "printed web page; its title '%s' matches" % title[:60], None
    txt = pdf_text(path)
    # Publisher and library stamps ("Authorized licensed use limited to: Stanford ... Restrictions apply.",
    # "Downloaded from ...") give a scan a thin text layer; do not let them hide that it is a scan.
    STAMP = re.compile(r"authorized licensed use|restrictions apply|downloaded (from|on|by)|for personal use only|"
                       r"terms and conditions|copyright", re.I)
    real = "\n".join(l for l in txt.splitlines() if not STAMP.search(l))

    def by_name_and_pages():
        # The file name carries the start of exactly one reference's title AND the page count equals that
        # reference's published page range (the check used by hand for the Fujii/Boldt scans).
        # "... (1).pdf" is a repeat download; "..._pumps_by.13.pdf" a journal's serial number after a truncated title
        fname = norm(re.sub(r"(\s*\(\d+\)|\.\d+)$", "", os.path.splitext(name)[0]))

        def title_in_name(title):
            # The title must agree with the file name for as far as both run (publishers truncate file names), not
            # just share its first 40 letters: "population-pharmacokinetics-pharmacodynamics-of-docetaxel-..." (Bruno
            # 1998) matched Byon 2017's "Population Pharmacokinetics, Pharmacodynamics, and Exploratory ..." and
            # its 10 pages, and would have been filed under apixaban's PMID had that slot been free (2026-11-09).
            t = norm(title)
            if len(t) < 25:
                return False
            i = fname.find(t[:40])
            if i < 0:
                return False
            k = min(len(t), len(fname) - i)
            return fname[i:i + k] == t[:k]
        try:
            import pypdf
            n = len(pypdf.PdfReader(path).pages)
        except Exception:
            n = None
        cands = [r for r in refs if title_in_name(r["title"])]
        # publisher file names that encode the article: Springer DOIs (A_1015822715190 -> 10.1023/a:1015822715190,
        # BF00544571 -> 10.1007/bf00544571, s40262-015-0276-0 -> 10.1007/s40262-015-0276-0) and J-STAGE
        # volume_firstpage (19_444); still confirmed by the page count
        stem = os.path.splitext(name)[0].split(" (")[0]
        dois = []
        m = re.fullmatch(r"A_(\d{9,})", stem)
        if m:
            dois.append("10.1023/a:" + m.group(1))
        if re.fullmatch(r"BF\d{8}", stem, re.I):
            dois.append("10.1007/" + stem.lower())
        if re.fullmatch(r"s\d{5}-\d{3}-\d{4,5}-[\dxX]", stem):
            dois.append("10.1007/" + stem.lower())
        cands += [by_doi[d] for d in dois if d in by_doi and by_doi[d] not in cands]
        m = re.fullmatch(r"(\d{1,3})_(\d{1,5})", stem)
        if m:
            cands += [r for r in refs if r["volume"] == m.group(1) and r["fpage"] == m.group(2) and r not in cands]
        exact = [r for r in cands if n and r["npages"] == n]
        return (exact[0] if len(exact) == 1 else None), cands, n

    if len(real.strip()) < 200:
        hit, cands, n = by_name_and_pages()
        if hit:
            return hit, "scan: file name carries the title, %d pages = published range" % n, None
        if cands:
            return None, None, "scan; file name suggests %s but page count %s does not settle it: check and rename by hand" % (
                " or ".join("%s (%s, %s pp)" % (r["file"], r["label"], r["npages"]) for r in cands[:3]), n)
        return None, None, "no text layer (scan): rename it PMID_<pmid>.pdf by hand"
    page1 = txt.split("\f")[0]
    flat = norm(txt)
    top = norm(page1)[:3000]  # where an article prints its own title (a citing paper's reference list comes later)

    def on_top(r):
        return norm(r["title"])[:50] in top

    def has_author(r):
        if r["surname"] and r["surname"] in flat:
            return True
        return bool(r.get("sur_re")) and re.search(r["sur_re"], flat) is not None

    # 1. the article's own DOI (the first DOI printed on page 1), confirmed by its title or first author
    dois = [x.lower().rstrip(".,;)") for x in re.findall(r"10\.\d{4,9}/[^\s\"<>]+", page1)]
    if dois and dois[0] in by_doi:
        r = by_doi[dois[0]]
        if on_top(r) or has_author(r):
            return r, "own DOI %s" % dois[0], None
    # The same for a paper the DOWNLOAD QUEUE asks for but no citation produced: `by_doi` is
    # built from works.json, the works the SOURCE cites, so a wanted paper is not in it and a
    # publisher-named download of one (Wiley's "Br J Clin Pharmacol - 2017 - Moffett - ...")
    # could not be identified at all -- 30 of them sat unfiled on 2026-11-18.  Same proof as
    # a cited work: the title on page 1, or the first author (requested_ref, 2026-11-18).
    # ONLY the first DOI on page 1, as the branch above does: that is the article's own.  A
    # later DOI is one the paper CITES, and has_author() accepts a surname anywhere in the
    # text, so scanning further could file a PDF under a paper it merely cites (CodeRabbit,
    # PR #784).  Misfiling is worse than not filing: an unfiled paper is reported, a misfiled
    # one is silently wrong, and the digest written from it would describe the wrong work.
    if dois:
        r = requested_ref_by_doi(dois[0])
        # Stricter than the branch above, deliberately.  has_author() searches `flat`, which
        # is pages 1 AND 2 (pdf_text's default last=2), so a commentary that prints the
        # queued article's DOI on page 1 and names its author in a reference on page 2 would
        # pass (CodeRabbit, PR #784 -- it said "the full text", which is not quite right, but
        # page 2 is enough to be wrong).  A queue row is a bare PubMed record, with none of
        # the resolution a works.json entry has been through, so the evidence must be on
        # page 1: the title where an article prints its title, or the author in that region.
        if r and (on_top(r) or (r["surname"] and r["surname"] in top)):
            return r, "own DOI %s (wanted by the download queue)" % dois[0], None
    # 2. the reference's title at the top of page 1 (punctuation and line breaks ignored), with its first author
    hits = [r for r in refs if on_top(r)]
    for r in hits:
        if has_author(r):
            return r, "title at top of page 1, first author", None
    for r in hits:
        if not os.path.exists(os.path.join(ARCHIVE, r["file"])):
            return None, None, "possible match to %s (%s; title at top of page 1 but first author not found): check and rename by hand" % (
                r["file"], r["label"])
    # 3. an OCR-garbled title: the journal's own citation line at the top of page 1 ("J Clin Pharmacol. 1984; 24:58-64"),
    #    i.e. volume and first page together, with the year and the first author also in that top block
    head = page1[:1500]
    head_flat = norm(head)
    found = []
    for r in refs:
        if not (r["volume"].isdigit() and r["fpage"].isdigit() and r["year"]):
            continue
        if re.search(r"(?<!\d)%s\s*(\(\s*\d+\s*\))?\s*[:,]\s*%s(?!\d)" % (r["volume"], r["fpage"]), head) and str(r["year"]) in head:
            if (r["surname"] and r["surname"] in head_flat) or (r.get("sur_re") and re.search(r["sur_re"], head_flat)):
                found.append(r)
    if len(found) == 1:
        return found[0], "citation line %s:%s, year and first author at top of page 1" % (found[0]["volume"], found[0]["fpage"]), None
    # 4. OCR text too garbled for the rules above: fall back to file name + page count, as for a scan
    hit, cands, n = by_name_and_pages()
    if hit:
        return hit, "garbled text; file name carries the title, %d pages = published range" % n, None
    return None, None, None  # not a stanpumpR reference


# A Downloads PDF that identify() could not place at all was passed over without a word, and its cached verdict
# ({"file": null, "review": null}) kept it silent on every later run, so it was on no list.  On 2026-11-10 that hid four
# Lane downloads of papers ON the list: Tsuei 1980 (PubMed's own title reads "Disporition"), Göke 1995 (the PDF prints
# "a-Glucosidase" for the record's "alpha", and a four-letter accented surname gets no sur_re), and the OCR'd scans
# Pond 1985 and Cook 1992 (scrambled title lines).  Weaker evidence must still never FILE a paper (see the docstring),
# so such a PDF is now REPORTED, naming the likeliest references when page 1 shows their first author or their
# citation line and most of their title's words.  Only recent downloads are reported, so an unrelated PDF left in
# Downloads is listed for REPORT_DAYS and then left alone.
REPORT_DAYS = 14


def loose_surname_re(r):
    """The first author's surname with each accented letter matching 0-2 characters, for surnames too short for
    surname_pattern() (Göke -> 'g.{0,2}ke'); None for an all-ASCII surname, which is matched by r['surname']."""
    parts = r["label"].rsplit(" ", 1)[0].split()
    while len(parts) > 1 and re.fullmatch(SUFFIX, parts[-1]):
        parts = parts[:-1]
    s = "".join(parts).lower()
    if all(ord(c) < 128 for c in s):
        return None
    pat = "".join(c if ("a" <= c <= "z" or c.isdigit()) else (".{0,2}" if c.isalpha() else "") for c in s)
    return pat if len(re.sub(r"\.\{0,2\}", "", pat)) >= 3 else None


def unidentified_note(path):
    import unicodedata
    txt = pdf_text(path)
    page1 = txt.split("\f")[0]
    top = norm(page1)[:3000]
    head = page1[:1500]
    # the author block, folded to ASCII lower case with its word breaks kept: a surname must be a whole word here,
    # or "may", "long" and a cited "Smith" pass for authors (Iisalo 1977's page 1 suggested May 1985 and Smith 1984).
    # 1500 characters reach the byline whether it precedes the abstract or follows it (Tsuei 1980, CPT's old layout).
    byline = unicodedata.normalize("NFKD", page1[:1500]).encode("ascii", "ignore").decode().lower()
    page_words = words(page1[:4000])
    found = []
    for r in refs:
        if not r["tw"]:
            continue
        overlap = len(r["tw"] & page_words) / len(r["tw"])
        if overlap < 0.3:
            continue
        title = len(norm(r["title"])) >= 25 and norm(r["title"])[:50] in top
        pats = [re.escape(r["surname"])] if r["surname"] and len(r["surname"]) >= 3 else []
        pats += [p for p in (r.get("sur_re"), loose_surname_re(r)) if p]
        author = any(re.search(r"(?<![a-z])%s(?![a-z])" % p, byline) for p in pats)
        cite = bool(r["volume"].isdigit() and r["fpage"].isdigit() and r["year"] and str(r["year"]) in head
                    and re.search(r"(?<!\d)%s\s*(\(\s*\d+\s*\))?\s*[:,]\s*%s(?!\d)" % (r["volume"], r["fpage"]), head))
        # a surname alone is weak (coauthors, common names) and so is half a title ("Population pharmacokinetics
        # of ..." shares half its words with dozens of references): the first author needs three quarters of the
        # title with it, the citation line the author or 60% of the title.  The four of 2026-11-10 pass at 83-100%.
        if title or (author and overlap >= 0.75) or (cite and (author or overlap >= 0.6)):
            found.append(((title + author + cite, overlap), (title, author, cite, overlap), r))
    found.sort(key=lambda x: x[0], reverse=True)
    begins = re.sub(r"\s+", " ", page1).strip()[:80]
    if found:
        hints = []
        for _, (title, author, cite, overlap), r in found[:2]:
            why = ", ".join(x for x in ("title on page 1" if title else "", "first author" if author else "",
                                        "citation line" if cite else "", "%.0f%% of title words" % (100 * overlap)) if x)
            done = " (already archived)" if os.path.exists(os.path.join(ARCHIVE, r["file"])) else ""
            hints.append("%s (%s%s; %s)" % (r["file"], r["label"], done, why))
        return "not identified; likeliest %s: check and rename it by hand" % " or ".join(hints)
    return NOT_A_REFERENCE + " (page 1 begins '%s')" % begins


# A PDF with no likely reference at all is usually something else (a paper for another project, a report): listed
# one per line it buried the few that matter (105 of 156 lines on 2026-11-10), so those get one line in the report
# and are written out in full to UNPLACED_PATH.
NOT_A_REFERENCE = "not a stanpumpR reference, as far as the filer can tell"
PAGE1_MIN_LETTERS = 20          # below this, page 1 carries no text that can be screened
UNPLACED_PATH = os.path.join(HERE, "unplaced_downloads.txt")


cache = json.load(open(CACHE_PATH, encoding="utf-8")) if os.path.exists(CACHE_PATH) else {}
# A cached "not a reference" verdict is only good for the reference list it was made against: when the list
# grows (new drugs, new citations), re-check those PDFs.  (2026-10-09: Absalom 2016 and Schnider 2016 sat in
# Downloads unfiled because they were first seen before references.md cited them.)
import hashlib
# Bump RULES whenever identify() changes, so files it could not place are looked at again.
# 2026-10-10 "3": scans filed by file name + page count; citation-line rule for OCR-garbled titles; stamps ignored.
# 2026-10-17 "6": a 1-page PDF matched to an article of 4+ pages is held back (errata); page ranges with "; discussion".
# 2026-10-19 "7": supplements (MOESM/ESM/suppl names) and PDFs under half the published pages are held back.
# 2026-10-19 "8": generational suffixes stripped from surnames (Butterworth JF 4th).
# 2026-10-27 "9": positive verdicts expire with the list too (see below).
# 2026-11-09 "10": a title read from a file name must agree to the end of the shorter, not just its first 40 letters.
# 2026-11-10 "11": an unidentified recent download is reported (unidentified_note), not passed over in silence.
RULES = "11"
# The download queue is part of the signature, not only the cited works.  A cached "not a
# reference" verdict is replayed without calling identify() at all, so queueing a paper did
# nothing for a PDF already in Downloads: it stayed unfiled until its size or mtime changed.
# That cost two manual cache clears on 2026-11-18, once before the rules were even found to
# be the problem (CodeRabbit, PR #784).  Hashing the queue's bytes means an edit to it
# invalidates the cache, which is exactly when a verdict may have changed.
try:
    with open(os.path.join(ARCHIVE, "stanpumpr-queue.csv"), "rb") as _qf:
        _queue_sig = hashlib.sha1(_qf.read()).hexdigest()
except OSError:
    _queue_sig = "no-queue"
refs_sig = hashlib.sha1((RULES + "|" + _queue_sig + "|"
                         + "|".join(sorted(r["file"] for r in refs))).encode()).hexdigest()
if cache.get("__refs__") != refs_sig:
    # Every verdict is only as good as the list it was made against, the positive ones too: on 2026-10-17 de Winter
    # 2017 was matched to PMID_17385022 while the list held a wrong PMID for it, that file already existed (Hong 2007),
    # so the copy was left as a "non-identical copy", and the cached match kept it unfiled after the list was put
    # right.  Files still in Downloads with a positive verdict are only such copies, so re-checking them costs little.
    cache = {"__refs__": refs_sig}
log = json.load(open(LOG_PATH, encoding="utf-8")) if os.path.exists(LOG_PATH) else []
filed, review, unplaced = [], [], []
quarantined = []                                 # manuscripts under peer review: never filed, never digested
unscreened = []                                  # filed on their name, contents never read (no text layer)
unwrapped = []                                   # envelope originals, logged beside the recycled downloads
def candidates():
    """Downloads and its subfolders (people save into subfolders); the archive folder itself, top level only."""
    for d in scan_dirs:
        if not os.path.isdir(d):
            continue
        if os.path.abspath(d) == os.path.abspath(ARCHIVE):
            for name in sorted(os.listdir(d)):
                yield d, name
        else:
            for root, dirs, files in os.walk(d):
                dirs[:] = [x for x in dirs if os.path.abspath(os.path.join(root, x)) != os.path.abspath(ARCHIVE)]
                for name in sorted(files):
                    yield root, name


# Shafer wants ONE archive: a filed PDF is MOVED, i.e. copied, checked byte-identical, and the Downloads original
# sent to the Recycle Bin (restorable; never a permanent delete).  A Downloads PDF identical to one already in the
# archive (a repeat download, or a scan filed by hand) is recycled too.
import hashlib, datetime
from recycle import recycle
moving = not dry and "--archive" not in args
RECYCLED_LOG = os.path.join(HERE, "recycled_log.json")
recycled, would_recycle = [], []


def sha(p):
    return hashlib.sha256(open(p, "rb").read()).hexdigest()


# Every archived PDF, the _aam/_erratum/_reply/supplement variants included: on 2026-11-09 two authors' manuscripts
# still in Downloads (Acrobat held them) were not recognised as the archived PMID_n_aam.pdf and were filed again as
# PMID_n.pdf, the published version.
arch_sha = {sha(os.path.join(ARCHIVE, f)): f for f in os.listdir(ARCHIVE)
            if re.fullmatch(r"(PMID_\d+(_[A-Za-z0-9]+)?|DOI_.+)\.pdf", f)}


# A Downloads PDF identified as a paper the archive already has, but not byte-identical to it (a re-download the
# publisher stamped differently).  Until 2026-11-01 it was left alone without a word, and its cached verdict kept it
# silent on every later run, so it sat in Downloads on no list at all: 52 of them had built up, a second Singhvi 1988
# (PMID_2967089) among them.  Shafer, 2026-11-01: recycle the clear duplicates, list the rest.  So a second copy that
# copy_equivalent() finds redundant is recycled like an identical one, and any other is reported on every run.
DIFFERENT_COPY = "a different copy of %s is already archived (same paper, but it differs: page count or text): compare them, keep one, remove the other"


# Lines a publisher stamps on each downloaded page, which differ between two downloads of one paper (Wiley's footer
# added about 7% to the words of Xue 2016 and Ku 2018, which was enough to call them different papers).
DOWNLOAD_STAMP = re.compile(r"downloaded (?:from|for|by)|wiley online library|onlinelibrary\.wiley\.com|terms and conditions|"
                            r"creative commons licen[cs]e|for rules of use|clinicalkey|sciencedirect|for personal use only|"
                            r"all rights reserved|copyright ©", re.I)


def copy_words(p, strict=False):
    import collections
    text = "\n".join(l for l in pdf_text(p, None, strict=strict).splitlines() if not DOWNLOAD_STAMP.search(l))
    return collections.Counter(re.findall(r"[a-z0-9]{3,}", text.lower()))


def copy_equivalent(path, archived):
    """Whether a Downloads PDF is a redundant second copy of an archived one, i.e. the archived copy lacks nothing.

    True when both have the same page count and the Downloads copy either has the same words (at least 97% of the
    combined word counts in common: download stamps and watermarks differ, the paper does not) or has no text layer
    while the archived copy has one (ocr_scans.py's OCR of that scan, or a publisher copy with text).  Anything else,
    a cover page, an erratum or a different text layer, is a person's call."""
    import pypdf
    try:
        if len(pypdf.PdfReader(path).pages) != len(pypdf.PdfReader(archived).pages):
            return False
    except Exception:
        return False
    try:
        wa, wb = copy_words(path, strict=True), copy_words(archived, strict=True)
    except Exception:                            # noqa: BLE001
        # Extraction FAILED, which is not "this copy has no text layer".  A pdftotext timeout
        # gave empty text, the `na < 50` branch below read that as an image-only scan, and a
        # download was recycled as redundant against an archived copy that may differ -- a
        # different edition, or figures this one lacks (CodeRabbit on PR #780, 2026-11-18).
        # Equivalence cannot be established, so it is not redundant and a person decides.
        return False
    na, nb = sum(wa.values()), sum(wb.values())
    if na < 50:
        return nb >= 200
    return sum((wa & wb).values()) >= 0.97 * sum((wa | wb).values())


def recycle_copy(path, archive_file):
    """Recycle a redundant second copy (copy_equivalent() said so), or report it when it is not redundant."""
    if not copy_equivalent(path, os.path.join(ARCHIVE, archive_file)):
        review.append((os.path.basename(path), DIFFERENT_COPY % archive_file))
    elif not moving:
        would_recycle.append((os.path.basename(path), archive_file))
    elif recycle(path):
        recycled.append({"recycled": path, "archive_copy": archive_file, "why": "same paper, not byte-identical",
                         "at": datetime.datetime.now().isoformat(timespec="seconds")})
    else:  # the Recycle Bin refused it (open in another program, usually): say so rather than drop it silently
        review.append((os.path.basename(path), "a redundant second copy of %s, but the Recycle Bin refused it "
                       "(is it open in another program?): close it and run update.bat again" % archive_file))


def recycle_original(path, archive_file):
    if not (moving and sha(path) == sha(os.path.join(ARCHIVE, archive_file))):
        return
    if recycle(path):
        recycled.append({"recycled": path, "archive_copy": archive_file,
                         "at": datetime.datetime.now().isoformat(timespec="seconds")})
    else:  # 2026-11-09: 30 fresh Chrome downloads were filed but stayed locked, and this said nothing
        review.append((os.path.basename(path), "filed as %s (identical), but the Recycle Bin refused the original "
                       "(still open in a browser or viewer?): run update.bat again later" % archive_file))


for d, name in candidates():
        path = os.path.join(d, name)
        if not name.lower().endswith(".pdf") or not os.path.isfile(path):
            continue
        # Before unwrap_envelope, the sha and the cache: a manuscript under peer review is not
        # filed, not unwrapped, not touched.  Name and folder are free to judge, so they are
        # judged here, before anything reads or alters the file.  Page 1 is read further down,
        # once, for every path that could reach the archive.
        q = in_review_reason(path, name=name)
        if q:
            quarantined.append((path, q + " (name and folder alone)"))
            continue
        unwrap_envelope(path)                    # before the sha, the cache key and the archive skip
        in_archive_dir = os.path.abspath(d) == os.path.abspath(ARCHIVE)
        if in_archive_dir and re.fullmatch(r"(PMID_\d+(_[A-Za-z0-9]+)?|DOI_.+|NJ_.+)\.pdf", name):  # PMID_n_ESM1 = a supplement
            continue  # already filed (NJ_ = non-journal documents, see nonjournal_manifest.json)
        st = os.stat(path)
        key = "%s|%d|%d" % (path, st.st_size, int(st.st_mtime))
        if not in_archive_dir:
            h = cache.get(key, {}).get("sha") or sha(path)
            if key in cache:
                cache[key]["sha"] = h
            if h in arch_sha:  # already archived, byte for byte
                recycle_original(path, arch_sha[h])
                continue
        # THE screen, and the only one that reads the document.  It sits above every path that
        # follows -- the cached verdicts, identify()'s "check and rename by hand" problem, the
        # unplaced note, and the filing itself -- because adding it per-branch left two of them
        # uncovered, and both were the hand-rename route (CodeRabbit, PR #846).  On 2026-11-21
        # five manuscripts under review were in Downloads and one WAS reported as "rename it
        # PMID_<pmid>.pdf by hand".  A card can become public, so this has to refuse (Shafer).
        try:
            page1, unreadable = pdf_text(path, last=1, strict=True), False
        except Exception:
            page1, unreadable = "", True     # pdftotext failed: not "no text", but "not read"
        # LETTERS, not emptiness.  pdftotext returns a lone form feed for a page with no text
        # layer, and that is a NON-EMPTY string, so a truthiness test calls a blank page "text I
        # have read" and the fail-closed branches below never fire.  Measured on an image-only
        # PDF: 0 letters, and the first version of this check passed it straight through -- my own
        # fix for a CodeRabbit finding, caught by testing end to end rather than reasoning.
        #
        # The threshold governs the NOTICE, never the screening.  in_review_reason() is given the
        # text whatever its length: a scanned manuscript whose only selectable text is a "reviewer
        # copy" stamp carries 12 letters, and that stamp is the strongest evidence there is
        # (CodeRabbit, PR #846).  `screenable` says only whether a person should be told that
        # nothing meaningful was read, which is a different question from what the screen sees.
        letters = len(re.findall(r"[A-Za-z]", page1))
        screenable = letters >= PAGE1_MIN_LETTERS
        q = in_review_reason(path, name=name, text=page1 or None)
        if q:
            # The suffix is about what was READ, not about the threshold.  A page carrying a
            # 12-letter "Reviewer Copy" stamp is below PAGE1_MIN_LETTERS and yet supplied the
            # reason, so saying "name and folder alone" there would be false (CodeRabbit, PR
            # #846).  Any letters at all means the classifier read the page.
            quarantined.append((path, q if letters else q + " (name and folder alone)"))
            cache.pop(key, None)             # never cache a refusal: judge it again next run
            continue
        if unreadable:
            # FAIL CLOSED.  An empty extraction is a scan, which identify() already refuses and
            # reports; a FAILED extraction is a document nothing has inspected, and filing one on
            # the strength of its name is how a confidential copy named for a known PMID would
            # get in (CodeRabbit, PR #846).
            review.append((name, "its first page could not be read, so it could not be screened for "
                                 "a manuscript under peer review: left where it is"))
            cache.pop(key, None)
            continue

        if key in cache and (cache[key].get("review") or "").startswith("no text layer"):
            continue  # scans are reported once, on first sight
        recent = not in_archive_dir and time.time() - st.st_mtime < REPORT_DAYS * 86400
        if key in cache and not cache[key].get("review"):
            hit = cache[key]
            if not hit.get("file"):
                if hit.get("unplaced") and recent:
                    unplaced.append((path, hit["unplaced"]))
                continue
            if os.path.exists(os.path.join(ARCHIVE, hit["file"])):
                if not in_archive_dir:  # not identical to anything archived (checked above): a second copy
                    recycle_copy(path, hit["file"])
                continue
        _requested.pop("failed", None)
        r, why, problem = identify(path)
        if r is None and _requested.get("failed"):
            # PubMed could not be reached to check whether the download queue wants this
            # paper.  "Could not look it up" is not "it is not a reference", and caching the
            # second would make every later run replay the verdict without trying again --
            # which is exactly how nine papers stayed unfiled after the rules changed
            # (CodeRabbit, PR #784).  Nothing is cached, so the next run looks again.
            review.append((name, "could not reach PubMed to check it against the download queue: "
                                 "run update.bat again later"))
            continue
        cache[key] = {"file": r["file"] if r else None, "review": problem, "sha": None if in_archive_dir else h}
        if problem:
            review.append((name, problem))
            continue
        if not r:
            # Reported while recent.  A likely match is a review note, which also makes later runs look at the file
            # again (a cached verdict with no review is skipped above); one with no likely reference is cached as
            # "unplaced" and counted from the cache on later runs, until it is filed, moved, or REPORT_DAYS old.
            if recent:
                note = unidentified_note(path)
                if note.startswith(NOT_A_REFERENCE):
                    cache[key]["unplaced"] = note
                    unplaced.append((path, note))
                else:
                    cache[key]["review"] = note
                    review.append((name, note))
            continue
        dst = os.path.join(ARCHIVE, r["file"])
        if os.path.exists(dst):
            if not in_archive_dir and sha(path) == sha(dst):
                recycle_original(path, r["file"])  # an identical copy filed earlier in this same run
            elif not in_archive_dir:
                recycle_copy(path, r["file"])
            continue  # a different copy is already archived: never replace it, and which to keep is a person's call
        if not dry:
            if in_archive_dir:
                os.replace(path, dst)  # misnamed file already in the archive: just rename it
            else:
                shutil.copy2(path, dst)
                arch_sha[sha(dst)] = r["file"]  # later identical copies in this run are recycled too
                recycle_original(path, r["file"])
            log.append({"from": path, "to": r["file"], "pmid": r["pmid"], "doi": r["doi"], "how": why})
        filed.append((name, r["file"], r["label"], why))
        if not screenable:
            # Filed with its contents never read: identify_any() took it on its name (a scan
            # renamed by hand), so the peer-review screen above judged only the name and folder.
            # ocr_scans.py gives it a text layer and screen_inreview.py judges it, both later in
            # update.bat; digest_run.py refuses to card a paper with no readable text, so no card
            # can come of it meanwhile.  Say so, because running this script alone skips both
            # (CodeRabbit, PR #846).
            unscreened.append((r["file"], why))

if quarantined:
    print()
    print("QUARANTINED %d PDF(s) that look like manuscripts under peer review. NOT filed, NOT digested:" % len(quarantined))
    for path, why in sorted(quarantined):
        print("  %s" % os.path.basename(path))
        print("      %s" % why)
        print("      %s" % path)
    print("  They stay where they are. A paper in review must not reach the repository, because its")
    print("  card could become public. If one of these is in fact published, rename it and run again.")
    print()

if unscreened:
    print()
    print("FILED WITHOUT READING THE DOCUMENT: %d PDF(s) had no text layer, so only the name and"
          % len(unscreened))
    print("folder were screened for a manuscript under peer review:")
    for f, why in sorted(unscreened):
        print("  %-34s (%s)" % (f, why))
    print("  ocr_scans.py then screen_inreview.py judge them later in update.bat, and digest_run.py")
    print("  will not card a paper whose text is unreadable. Running THIS script alone skips both:")
    print("  run update.bat, or run screen_inreview.py yourself.")
    print()

if recycled or unwrapped:
    old = json.load(open(RECYCLED_LOG, encoding="utf-8")) if os.path.exists(RECYCLED_LOG) else []
    json.dump(old + recycled + unwrapped, open(RECYCLED_LOG, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
if unwrapped:
    print("Stripped a form envelope from %d PDF(s); the originals went to the Recycle Bin." % len(unwrapped))
# Its OWN condition.  Adding the envelope summary above left these two lines indented under
# IT, so the count of Downloads originals printed only when an envelope had been stripped --
# and then said "Moved 0 original(s)" when none had been.  The test run that passed had both
# lists non-empty, which is exactly the case that hides it (CodeRabbit, PR #797).
if recycled:
    same = sum(1 for e in recycled if e.get("why"))
    print("Moved %d original(s) from Downloads to the Recycle Bin (%d identical to an archived copy, %d the same paper "
          "differently stamped)." % (len(recycled), len(recycled) - same, same))
if would_recycle:
    print("Would recycle %d redundant second copies (same paper as the archived one):" % len(would_recycle))
    for name, f in would_recycle:
        print("  %s  (archived as %s)" % (name, f))

if not dry and "--archive" not in args:
    json.dump(cache, open(CACHE_PATH, "w", encoding="utf-8"), indent=1)
    json.dump(log, open(LOG_PATH, "w", encoding="utf-8"), indent=1)
print("%s %d PDF(s):" % ("Would file" if dry else "Filed", len(filed)))
for name, f, label, why in filed:
    print("  %s -> %s  [%s; %s]" % (name, f, label, why))
if review:
    print("Needs a look (%d):" % len(review))
    for name, p in review:
        print("  %s: %s" % (name, p))
if unplaced:
    if not dry:
        with open(UNPLACED_PATH, "w", encoding="utf-8") as f:
            f.write("Recent PDFs in Downloads that file_downloads.py could not match to any stanpumpR reference (%s).\n"
                    "File one that IS a reference by hand as PMID_<pmid>.pdf; move the rest out of Downloads.\n\n"
                    % datetime.datetime.now().isoformat(timespec="minutes"))
            for path, note in sorted(unplaced):
                f.write("%s\n    %s\n" % (path, note))
    print("Not stanpumpR references, as far as the filer can tell: %d recent PDF(s) in Downloads, listed in %s"
          % (len(unplaced), UNPLACED_PATH))
