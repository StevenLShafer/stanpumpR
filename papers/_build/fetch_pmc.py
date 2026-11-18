"""Download free PMC copies from the PMC Article Datasets bucket (s3://pmc-oa-opendata), verify, file as PMID_<pmid>.pdf."""
import json, os, re, subprocess, sys, time, urllib.request
from openpyxl import load_workbook
from ncbi import norm

# A first author called Jarčuška or Škrlín crashed the run on a cp1252 console, AFTER the PDF
# had been filed but BEFORE the fetch log was written, so the run lost its record of
# everything it had just done (2026-11-18).
sys.stdout.reconfigure(encoding="utf-8")

ARCHIVE = r"C:\dev\stanpumpRPapers"
HERE = os.path.dirname(os.path.abspath(__file__))
BUCKET = "https://pmc-oa-opendata.s3.amazonaws.com/"
TMP = os.path.join(HERE, "pmc_tmp")
os.makedirs(TMP, exist_ok=True)


def get(url, binary=False):
    for i in range(4):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "stanpumpR-archive/1.0"})
            with urllib.request.urlopen(req, timeout=120) as r:
                b = r.read()
            return b if binary else b.decode("utf-8", "replace")
        except Exception:
            if i == 3:
                raise
            time.sleep(3 * (i + 1))


def first_text(path, pages=2):
    try:
        return subprocess.run(["pdftotext", "-l", str(pages), "-q", path, "-"], capture_output=True, timeout=60).stdout.decode("utf-8", "replace")
    except Exception:
        return ""


def words(s):
    return set(w for w in re.findall(r"[a-z0-9]+", (s or "").lower()) if len(w) > 3)


ws = load_workbook(os.path.join(ARCHIVE, "stanpumpR-references.xlsx"))["References"]
h = [c.value for c in ws[1]]
I = {k: i for i, k in enumerate(h)}
todo = [{k: r[I[k]] for k in ("Ref", "PMID", "PMCID", "First author", "Year", "Title", "Journal / source", "Pages")}
        for r in ws.iter_rows(min_row=2, values_only=True) if r[I["Archive status"]] == "To download (free at PMC)"]

# The spreadsheet lists the works the stanpumpR SOURCE cites.  A paper can be wanted long
# before anything cites it -- a registry's `to_obtain`, or a reading list added by
# queue_add.py -- and those live only in the download queue, so without this they would never
# be fetched even when PMC has them free (2026-11-18).  The verification below is unchanged:
# a file whose first page does not carry the title and first author is still HELD.
if True:
    import csv as _csv
    known = {str(t["PMID"]) for t in todo if t["PMID"]}
    qwant = []
    with open(os.path.join(ARCHIVE, "stanpumpr-queue.csv"), encoding="utf-8") as _f:
        for _r in _csv.DictReader(_f):
            _p = (_r.get("pmid") or "").strip()
            _n = _r.get("notes") or ""
            if (_p and _p not in known and _n.startswith("wanted by ") and "free at PMC" in _n
                    and (_r.get("status") or "").strip().lower() == "pending"
                    and not os.path.exists(os.path.join(ARCHIVE, "PMID_%s.pdf" % _p))):
                qwant.append(_p)
                known.add(_p)
    if qwant:
        from ncbi import esummary
        _meta = esummary(qwant)
        for _p in qwant:
            _m = _meta.get(_p)
            if not _m or not _m.get("pmcid"):
                continue
            todo.append({"Ref": "download queue", "PMID": _p, "PMCID": _m["pmcid"],
                         "First author": _m.get("first_author", ""), "Year": _m.get("year"),
                         "Title": _m.get("title", ""), "Journal / source": _m.get("journal", ""),
                         "Pages": _m.get("pages", "")})
        print("from the download queue: %d paper(s) free at PMC and not yet here" % len(qwant))

log = []
for t in todo:
    pmc, pmid = t["PMCID"], t["PMID"]
    dst = os.path.join(ARCHIVE, "PMID_%s.pdf" % pmid)
    entry = {"ref": t["Ref"], "pmid": pmid, "pmcid": pmc, "first_author": t["First author"], "year": t["Year"]}
    if os.path.exists(dst):
        entry["result"] = "already in archive"
        log.append(entry)
        continue
    lst = get(BUCKET + "?list-type=2&prefix=%s.&max-keys=100" % pmc)
    keys = re.findall(r"<Key>([^<]+)</Key>", lst)
    vers = sorted({int(m.group(1)) for k in keys for m in [re.match(r"%s\.(\d+)/" % pmc, k)] if m})
    if not vers:
        entry["result"] = "not in PMC Article Datasets (free to read only)"
        log.append(entry)
        print(pmc, pmid, "-- not in bucket")
        continue
    v = vers[-1]
    base = "%s.%d/%s.%d" % (pmc, v, pmc, v)
    meta = {}
    if base + ".json" in keys:
        try:
            meta = json.loads(get(BUCKET + base + ".json"))
        except Exception:
            pass
    entry["license"] = meta.get("license_code") or meta.get("license") or ""
    entry["dataset"] = meta.get("is_manuscript") and "author_manuscript" or meta.get("article_type", "")
    if base + ".pdf" not in keys:
        entry["result"] = "in bucket but no PDF (XML/text only)"
        log.append(entry)
        print(pmc, pmid, "-- no pdf; keys:", [k.split("/")[-1] for k in keys])
        continue
    tmp = os.path.join(TMP, "%s.pdf" % pmc)
    with open(tmp, "wb") as f:
        f.write(get(BUCKET + base + ".pdf", binary=True))
    txt = first_text(tmp)
    tw, pw = words(t["Title"]), words(txt)
    overlap = len(tw & pw) / max(1, len(tw))
    au = norm((t["First author"] or "").split()[0])
    au_ok = au and au in norm(txt)
    entry.update(bytes=os.path.getsize(tmp), title_overlap=round(overlap, 2), author_on_page=bool(au_ok), version=v)
    if overlap >= 0.6 and au_ok:
        os.replace(tmp, dst)
        entry["result"] = "downloaded"
    elif not txt.strip():
        os.replace(tmp, dst)
        entry["result"] = "downloaded (no text layer - check by eye)"
    else:
        entry["result"] = "HELD: first page does not match title/author"
    log.append(entry)
    print(pmc, pmid, t["First author"], t["Year"], "->", entry["result"], "overlap=%.2f author=%s %s" % (overlap, bool(au_ok), entry["license"]))

# merge into the log of earlier runs (build.py reads it to mark the by-hand ones), newest result per PMID
log_path = os.path.join(HERE, "pmc_fetch_log.json")
merged = {e["pmid"]: e for e in (json.load(open(log_path)) if os.path.exists(log_path) else [])}
merged.update({e["pmid"]: e for e in log})
json.dump(list(merged.values()), open(log_path, "w"), indent=1)
from collections import Counter
print("this run:", Counter(e["result"] for e in log))
