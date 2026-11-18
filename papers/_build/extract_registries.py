"""Citations in the package's registry tables (inst/extdata/*.csv): every source, source_url and to_obtain field.

The help pages render these tables (reading lists, source columns), so their papers are stanpumpR references too.
Writes extract/Q_registries.jsonl in the extraction schema; resolve.py and merge.py treat it like the agents' files.
Usage: python extract_registries.py <repo checkout> [output path]
"""
import csv, glob, json, os, re, sys

sys.stdout.reconfigure(encoding="utf-8")
REPO = sys.argv[1]
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, "extract", "Q_registries.jsonl")  # a later round writes elsewhere and keeps only the new records

PMID = re.compile(r"(?:PMID[:\s]*|pubmed\.ncbi\.nlm\.nih\.gov/)(\d{5,9})", re.I)
DOI = re.compile(r"\b(10\.\d{4,9}/[^\s;,\"<>]+)")  # Elsevier DOIs carry parentheses: 10.1016/0009-9236(95)90020-9


def clean_doi(d):
    """Drop trailing punctuation, and a closing parenthesis that the DOI did not open."""
    d = d.rstrip(".,;:")
    while d.endswith(")") and d.count(")") > d.count("("):
        d = d[:-1].rstrip(".,;:")
    return d
PMC = re.compile(r"\b(PMC\d{5,9})\b")
URL = re.compile(r"https?://[^\s;\"<>]+")
CIT = re.compile(r"^\s*(?P<au>[A-Z][A-Za-z'\-]+(?: [A-Za-z'\-]+)?)\s+[A-Z]{1,4}\b.*?\.\s+(?P<title>[^.]{8,}?)\.\s+"
                 r"(?P<jn>[A-Z][A-Za-z .&]+?)\s+(?P<yr>(?:19|20)\d\d)\s*;\s*(?P<vol>\d+)(?:\(\d+\))?\s*:\s*(?P<pg>[A-Za-z]?\d+(?:-[A-Za-z]?\d+)?)")
SKIP_URL = re.compile(r"dailymed\.nlm\.nih\.gov/dailymed/search\.cfm|accessdata\.fda\.gov/scripts/cder/daf/?$", re.I)


def records_from(text, loc, drug):
    """Split a field into its citations; one record per citation or identifier."""
    out = []
    # inside one citation a ";" is followed by a volume ("1990;64:148"); between citations by a name or a link
    parts = [p for p in re.split(r"\s*;\s*(?=[A-Z]|https?://)", text) if p.strip()]
    for part in parts:
        pmids = PMID.findall(part)
        dois = [clean_doi(d) for d in DOI.findall(part)]
        pmcs = PMC.findall(part)
        urls = [u for u in URL.findall(part) if not re.search(r"doi\.org|pubmed\.ncbi|ncbi\.nlm\.nih\.gov/pmc", u)]
        m = CIT.match(part)
        rec = {"first_author": m.group("au") if m else None, "authors": None, "year": int(m.group("yr")) if m else None,
               "title": m.group("title") if m else None, "journal": m.group("jn").strip() if m else None,
               "volume": m.group("vol") if m else None, "issue": None, "pages": m.group("pg") if m else None,
               "pmid": pmids[0] if pmids else None, "doi": dois[0] if dois else None, "pmcid": pmcs[0] if pmcs else None,
               "url": None, "kind": "journal-article", "citation_text": part.strip()[:400], "locations": [loc],
               "drugs": [drug] if drug else [], "used_for": "registry source (%s)" % loc.split(":")[0].split("/")[-1],
               "search_hint": None, "notes": "extracted by extract_registries.py"}
        if not (pmids or dois or pmcs or m):
            if urls:
                if SKIP_URL.search(urls[0]):
                    continue
                rec.update(url=urls[0], kind="label" if re.search(r"dailymed|accessdata\.fda|fda\.gov|ema\.europa|medicines\.org", urls[0], re.I) else "website")
            elif len(part.strip()) < 25:
                continue  # a fragment, not a citation
        out.append(rec)
    return out


recs = {}
for f in sorted(glob.glob(os.path.join(REPO, "inst", "extdata", "*.csv"))):
    rel = "inst/extdata/" + os.path.basename(f)
    with open(f, encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    if not rows:
        continue
    cols = [c for c in rows[0] if re.search(r"^source$|source_url|to_obtain", c)]
    drugcol = next((c for c in ("drug", "ingredient", "molecule", "Drug") if c in rows[0]), None)
    for i, r in enumerate(rows, start=2):  # line 1 is the header
        for c in cols:
            v = (r.get(c) or "").strip()
            if not v or v.upper() == "NA":
                continue
            for rec in records_from(v, "%s:%d" % (rel, i), (r.get(drugcol) or "").strip() if drugcol else ""):
                key = rec["pmid"] or (rec["doi"] or "").lower() or rec["pmcid"] or rec["url"] or rec["citation_text"][:80]
                if key in recs:
                    recs[key]["locations"] = sorted(set(recs[key]["locations"] + rec["locations"]))
                    recs[key]["drugs"] = sorted(set(recs[key]["drugs"] + rec["drugs"]))
                    for k in ("pmid", "doi", "pmcid", "first_author", "year", "title", "journal", "volume", "pages"):
                        recs[key][k] = recs[key][k] or rec[k]
                else:
                    recs[key] = rec
with open(OUT, "w", encoding="utf-8") as fh:
    for rec in recs.values():
        rec["locations"] = rec["locations"][:12]
        fh.write(json.dumps(rec, ensure_ascii=False) + "\n")
from collections import Counter
print(len(recs), "works ->", OUT)
print(Counter("pmid" if r["pmid"] else "doi" if r["doi"] else "pmc" if r["pmcid"] else "url" if r["url"] else "parsed" if r["first_author"] else "text" for r in recs.values()))
