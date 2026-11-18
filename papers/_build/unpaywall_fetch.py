"""Open-access copies through Unpaywall (api.unpaywall.org, the service the R package roadoi wraps), for the works the
archive still lacks: every "To download" or "free at PMC, by hand" row of the workbook that has a DOI.

Added 2026-10-28.  For each DOI the Unpaywall record lists its open locations (publisher, Europe PMC, institutional
repositories) with the version each one holds.  The script tries each location's PDF in turn and keeps the first that
is a real PDF whose first two pages carry the reference's title and first author and whose page count is at least half
the published range (so an abstract, a cover page or a supplement is never filed as the article):
  publishedVersion -> ..\\PMID_<pmid>.pdf (or DOI_<doi>.pdf), "In archive" from then on;
  acceptedVersion  -> ..\\PMID_<pmid>_aam.pdf, kept, but the work stays "To download" until the publisher's PDF is
                      obtained (Shafer's choice, 2026-10-28: an accepted manuscript can differ from the published text);
  submittedVersion -> never taken (a preprint is not the paper).
A PDF that fails the checks is held in unpaywall_tmp\\ and reported.  Unpaywall answers are cached in
%LOCALAPPDATA%\\stanpumpRPapers\\http_cache\\unpaywall\\, and every outcome is written to unpaywall_fetch_log.json (newest result per DOI).
Usage: python unpaywall_fetch.py [--limit N] [--dry]   (--dry looks up and reports, downloads nothing)
"""
import hashlib, json, os, re, subprocess, sys, time, urllib.parse, urllib.request
from collections import Counter
from openpyxl import load_workbook
from ncbi import norm

sys.stdout.reconfigure(encoding="utf-8")
EMAIL = "steveshafer@gmail.com"  # Unpaywall requires a real address on every request (Shafer, 2026-10-28)
HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
from machine import LOCAL  # the cache is per-machine, kept out of the Drive-synced archive (2026-10-30)
CACHE = os.path.join(LOCAL, "http_cache", "unpaywall")
TMP = os.path.join(HERE, "unpaywall_tmp")
LOG = os.path.join(HERE, "unpaywall_fetch_log.json")
PDFTOTEXT = r"C:\Program Files\Git\mingw64\bin\pdftotext.exe"
UA = {"User-Agent": "Mozilla/5.0 (stanpumpR reference archive; mailto:%s)" % EMAIL, "Accept": "application/pdf,*/*"}
os.makedirs(CACHE, exist_ok=True)
os.makedirs(TMP, exist_ok=True)
args = sys.argv[1:]
dry = "--dry" in args
limit = int(args[args.index("--limit") + 1]) if "--limit" in args else None


def unpaywall(doi):
    path = os.path.join(CACHE, hashlib.sha1(doi.lower().encode()).hexdigest() + ".json")
    if os.path.exists(path):
        return json.load(open(path, encoding="utf-8"))
    url = "https://api.unpaywall.org/v2/%s?email=%s" % (urllib.parse.quote(doi, safe="/()._-;:"), urllib.parse.quote(EMAIL))
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": UA["User-Agent"]}), timeout=60) as r:
            rec = json.loads(r.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        rec = {"error": e.code}
    time.sleep(0.2)
    json.dump(rec, open(path, "w", encoding="utf-8"))
    return rec


def fetch(url):
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=90) as r:
            return r.read(), r.geturl()
    except Exception as e:
        return None, str(e)[:120]


def pdf_text(path):
    try:
        return subprocess.run([PDFTOTEXT, "-l", "2", "-q", path, "-"], capture_output=True, timeout=60).stdout.decode("utf-8", "replace")
    except Exception:
        return ""


def pdf_pages(path):
    try:
        from pypdf import PdfReader
        return len(PdfReader(path).pages)
    except Exception:
        return None


def published_pages(pages):
    m = re.match(r"\s*[A-Za-z]?(\d+)\s*-\s*[A-Za-z]?(\d+)\s*(?:$|[;,])", str(pages or ""))
    if not m:
        return None
    a, b = m.groups()
    if len(b) < len(a):  # "117-22" -> 122
        b = a[:len(a) - len(b)] + b
    n = int(b) - int(a) + 1
    return n if 0 < n < 400 else None


def words(s):
    return set(w for w in re.findall(r"[a-z0-9]+", (s or "").lower()) if len(w) > 3)


ws = load_workbook(os.path.join(ARCHIVE, "stanpumpR-references.xlsx"), read_only=True)["References"]
rows = list(ws.iter_rows(values_only=True))
I = {k: i for i, k in enumerate(rows[0])}
todo = [{k: r[I[k]] for k in ("Ref", "Archive status", "PMID", "DOI", "First author", "Year", "Title", "Pages")} for r in rows[1:]
        if r[I["Archive status"]] in ("To download", "To download (free at PMC, by hand)") and r[I["DOI"]]]
if limit:
    todo = todo[:limit]
print("works to look up:", len(todo))

ORDER = {"publishedVersion": 0, "acceptedVersion": 1}
log, held = [], []
for t in todo:
    doi, pmid = t["DOI"].strip(), str(t["PMID"] or "").strip()
    base = ("PMID_%s" % pmid) if pmid else ("DOI_" + re.sub(r"[^A-Za-z0-9.]+", "_", doi))
    entry = {"ref": t["Ref"], "pmid": pmid, "doi": doi, "first_author": t["First author"], "year": t["Year"]}
    if os.path.exists(os.path.join(ARCHIVE, base + ".pdf")):
        continue
    rec = unpaywall(doi)
    if rec.get("error"):
        entry["result"] = "Unpaywall error %s" % rec["error"]
        log.append(entry)
        continue
    entry["oa_status"] = rec.get("oa_status")
    locs = [l for l in (rec.get("oa_locations") or []) if l.get("version") in ORDER]
    locs.sort(key=lambda l: (ORDER[l["version"]], l.get("host_type") != "publisher"))
    entry["locations"] = [{"version": l.get("version"), "host": l.get("host_type"), "pdf": l.get("url_for_pdf"),
                           "landing": l.get("url_for_landing_page"), "license": l.get("license")} for l in locs]
    if not locs:
        entry["result"] = "no open copy (Unpaywall: %s)" % (rec.get("oa_status") or "closed")
        log.append(entry)
        continue
    if dry:
        entry["result"] = "dry run: %d open location(s), best %s" % (len(locs), locs[0]["version"])
        log.append(entry)
        continue
    have_aam = os.path.exists(os.path.join(ARCHIVE, base + "_aam.pdf"))
    entry["result"], tried = "open copy listed, no PDF retrievable by script (landing pages in the log)", []
    for l in locs:
        if l["version"] == "acceptedVersion" and have_aam:
            entry["result"] = "accepted manuscript already archived as %s_aam.pdf" % base
            break
        url = l.get("url_for_pdf")
        if not url:
            continue
        b, final = fetch(url)
        time.sleep(1.0)
        if not b or not b.startswith(b"%PDF") or len(b) < 20000:
            tried.append({"url": url, "why": final if not b else "not a PDF (%d bytes)" % len(b)})
            continue
        tmp = os.path.join(TMP, base + ".pdf")
        open(tmp, "wb").write(b)
        txt, n = pdf_text(tmp), pdf_pages(tmp)
        overlap = len(words(t["Title"]) & words(txt)) / max(1, len(words(t["Title"])))
        au = norm((t["First author"] or "").split()[0]) if t["First author"] else ""
        au_ok = bool(au) and au in norm(txt)
        need = published_pages(t["Pages"])
        pages_ok = n is not None and (need is None or n * 2 >= need)
        check = {"url": url, "version": l["version"], "host": l.get("host_type"), "license": l.get("license"),
                 "title_overlap": round(overlap, 2), "author_on_page": au_ok, "pages": n, "published_pages": need}
        if overlap >= 0.6 and au_ok and pages_ok:
            name = base + (".pdf" if l["version"] == "publishedVersion" else "_aam.pdf")
            os.replace(tmp, os.path.join(ARCHIVE, name))
            entry.update(check, result="downloaded (%s) -> %s" % (l["version"], name))
            break
        check["why"] = "first pages do not match title/author" if not (overlap >= 0.6 and au_ok) else "too few pages"
        tried.append(check)
        held.append((base, check))
        entry["result"] = "HELD in unpaywall_tmp: " + check["why"]
    entry["tried"] = tried
    log.append(entry)
    print("%-15s %-22s %s" % (base, ("%s %s" % (t["First author"] or "", t["Year"] or ""))[:22], entry["result"]))

old = {e["doi"].lower(): e for e in (json.load(open(LOG, encoding="utf-8")) if os.path.exists(LOG) else [])}
old.update({e["doi"].lower(): e for e in log})
json.dump(list(old.values()), open(LOG, "w", encoding="utf-8"), indent=1)
print("this run:", Counter(re.sub(r" ->.*| \(Unpaywall.*|: .*", "", e["result"]) for e in log))
