"""Index every local copy we could draw on, keyed by PMID and by lower-cased DOI."""
import csv, glob, json, os, re

SP = os.path.dirname(os.path.abspath(__file__))
SHAFER = r"C:\dev\ShaferPapers"
REPRINTS = r"C:\Users\steve\OneDrive\Documents\Google Drive\NONMEMCourse\Reprints"
CORPUS = r"C:\dev\Corpus"


def build():
    by_pmid, by_doi = {}, {}

    def add(pmid, doi, entry):
        if pmid:
            by_pmid.setdefault(str(pmid), []).append(entry)
        if doi:
            by_doi.setdefault(doi.lower(), []).append(entry)

    # ShaferPapers: PDFs named PMID_<pmid>.pdf
    for p in glob.glob(os.path.join(SHAFER, "PMID_*.pdf")):
        pmid = re.search(r"PMID_(\d+)", p).group(1)
        add(pmid, None, {"source": "ShaferPapers", "path": p, "bytes": os.path.getsize(p)})
    # Shafer queue: his own papers, some not yet downloaded
    queue = {}
    with open(os.path.join(SHAFER, "shafer-queue.csv"), encoding="utf-8") as f:
        for row in csv.DictReader(f):
            queue[row["pmid"]] = row
            if row.get("doi"):
                queue["doi:" + row["doi"].lower()] = row
    # Reprints: identified by hand from first-page text, PMIDs resolved
    for r in json.load(open(os.path.join(SP, "reprints_resolved.json"), encoding="utf-8")):
        if r.get("pmid"):
            p = os.path.join(REPRINTS, r["file"])
            add(r["pmid"], (r.get("pm") or {}).get("doi"), {"source": "Reprints", "path": p, "bytes": os.path.getsize(p),
                                                             "note": r.get("note", "")})
    # Master corpus: identity.csv (PMID/DOI -> accession), master/pdf/<acc>.pdf
    pdfdir = os.path.join(CORPUS, "master", "pdf")
    with open(os.path.join(CORPUS, "index", "identity.csv"), encoding="utf-8") as f:
        for row in csv.DictReader(f):
            acc = row["ACCESSION"]
            p = os.path.join(pdfdir, acc + ".pdf")
            if os.path.exists(p):
                pm = row["PMID"] if row["PMID"] not in ("", "NA") else None
                doi = row["DOI"] if row["DOI"] not in ("", "NA") else None
                add(pm, doi, {"source": "Corpus", "path": p, "bytes": os.path.getsize(p), "accession": acc})
    return by_pmid, by_doi, queue


if __name__ == "__main__":
    a, b, q = build()
    from collections import Counter
    print("pmids indexed:", len(a), "dois:", len(b), "queue rows:", sum(1 for k in q if not k.startswith("doi:")))
    print(Counter(e["source"] for v in a.values() for e in v))
