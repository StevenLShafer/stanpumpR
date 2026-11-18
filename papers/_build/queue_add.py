"""Add a list of PMIDs to the download queue, with a note saying who asked for them.

`arrivals.py` queues what a registry's `to_obtain` or papers/pending-work.csv asks for.  This
is the other way in: a batch of PMIDs somebody has decided are worth having -- a reading list
from another session, a reviewer's suggestions, the output of a literature search -- that no
registry row mentions yet.

    python queue_add.py <file of PMIDs> --label "future work: <where they came from>"
    python queue_add.py <file> --label "..." --dry-run

A PMID already in the archive or already in the queue is skipped and counted, so the script is
safe to run twice on the same list.

The note ALWAYS begins "wanted by ", because build.py rebuilds this queue from the works the
stanpumpR source cites and keeps only the rows carrying that prefix.  Without it a row added
here would survive exactly until the next filing run and then vanish, which is the trap the
whole arrangement exists to avoid (papers/_build/README.md).
"""
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
QUEUE = os.path.join(ARCHIVE, "stanpumpr-queue.csv")
REQUEST_NOTE = "wanted by "                      # keep in step with arrivals.py and build.py

sys.stdout.reconfigure(encoding="utf-8")


def archived():
    out = set()
    for f in os.listdir(ARCHIVE):
        m = re.fullmatch(r"PMID_(\d+)(?:_[A-Za-z0-9]+)?\.pdf", f, re.I)
        if m:
            out.add(m.group(1))
    return out


def main():
    import csv
    args = sys.argv[1:]
    paths = [a for a in args if not a.startswith("--")]
    if not paths:
        print(__doc__)
        return 2
    label = args[args.index("--label") + 1] if "--label" in args else "a reading list"
    dry = "--dry-run" in args

    wanted, seen = [], set()
    with open(paths[0], encoding="utf-8") as f:
        for tok in re.split(r"[^0-9]+", f.read()):
            if tok and tok not in seen:
                seen.add(tok)
                wanted.append(tok)
    if not wanted:
        print("queue_add: no PMIDs in %s" % paths[0])
        return 1

    have = archived()
    with open(QUEUE, encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows, fields = list(reader), reader.fieldnames
    held = {(r.get("pmid") or "").strip() for r in rows}

    new = [p for p in wanted if p not in have and p not in held]
    print("queue_add: %d PMID(s); %d already archived, %d already queued, %d to add"
          % (len(wanted), sum(p in have for p in wanted), sum(p in held for p in wanted), len(new)))
    if not new:
        return 0

    if HERE not in sys.path:
        sys.path.insert(0, HERE)                 # so `python -I queue_add.py` finds ncbi too
    import ncbi
    meta = ncbi.esummary(new)
    unresolved = [p for p in new if p not in meta]
    if unresolved:
        # A PMID that PubMed does not know is a typo or a transcription slip, and queueing it
        # would send somebody hunting for a paper that does not exist.  Named, never added.
        print("  %d PMID(s) PubMed does not recognise, NOT added: %s"
              % (len(unresolved), ", ".join(unresolved)))
        new = [p for p in new if p in meta]

    added = []
    for p in new:
        m = meta[p]
        doi = m.get("doi", "")
        note = REQUEST_NOTE + label
        if m.get("pmcid"):
            note += " (free at PMC %s)" % m["pmcid"]
        added.append({
            "pmid": p, "doi": doi,
            "journal": m.get("journal_full") or m.get("journal", ""),
            "year": m.get("year") or "", "volume": m.get("volume", ""),
            "pages": m.get("pages", ""),
            "download_link": ("https://doi-org.laneproxy.stanford.edu/%s" % doi) if doi
                             else "https://pubmed.ncbi.nlm.nih.gov/%s/" % p,
            "status": "pending", "downloaded_at": "", "notes": note,
        })

    free = sum(1 for a in added if "free at PMC" in a["notes"])
    print("  %d to add, %d of them free at PMC" % (len(added), free))
    if dry:
        for a in added[:10]:
            print("    PMID_%-9s %s %s  %s" % (a["pmid"], a["year"], a["journal"][:46], a["doi"]))
        if len(added) > 10:
            print("    ... and %d more (--dry-run: nothing written)" % (len(added) - 10))
        return 0

    tmp = QUEUE + ".new"
    try:
        with open(tmp, "w", newline="", encoding="utf-8") as f:
            wr = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
            wr.writeheader()
            wr.writerows(rows + added)
        os.replace(tmp, QUEUE)                   # atomic: the queue is on a synced drive
    except PermissionError:
        if os.path.exists(tmp):
            os.remove(tmp)
        print("  stanpumpr-queue.csv is open in another program -- nothing added")
        return 1
    print("  added %d row(s) to stanpumpr-queue.csv, noted \"%s%s\"" % (len(added), REQUEST_NOTE, label))
    return 0


if __name__ == "__main__":
    sys.exit(main())
