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

KNOWN LIMITATION, not yet fixed (CodeRabbit, PR #784): the queue is read, added to, and
replaced without a lock, and build.py rewrites it the same way.  Two writers at once -- two
Claude sessions on this machine, or this script while update.bat is running -- can each read
the queue before either writes, and the later write silently drops the earlier one's rows.
Each writer now stages on a temporary file of its own, so they cannot publish each other's
half-written data, but that does not make the read-modify-write safe.  The fix is a lock file
beside the queue that every writer takes across its whole read-and-replace; it is not here
because it has to be added to build.py in the same change and wants a test.  Until then, do
not run this while update.bat is running.
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
    """PMIDs whose ARTICLE is here.  Only the unsuffixed name counts: PMID_33205593_sup0002.pdf
    is a supplement, and treating it as the article meant the article itself was reported as
    archived and never queued (CodeRabbit, PR #784)."""
    out = set()
    for f in os.listdir(ARCHIVE):
        m = re.fullmatch(r"PMID_(\d+)\.pdf", f, re.I)
        if m:
            out.add(m.group(1))
    return out


def main():
    import csv
    import argparse
    # argparse rather than scanning sys.argv: `--label` with no value raised IndexError, and a
    # leading `--label x file.txt` took the label's value as the filename (CodeRabbit, PR #784).
    ap = argparse.ArgumentParser(description="Add PMIDs to the archive's download queue.")
    ap.add_argument("pmids", metavar="FILE", help="a file of PMIDs, any separator")
    ap.add_argument("--label", default="a reading list",
                    help="what to record as having asked for them")
    ap.add_argument("--dry-run", action="store_true", help="report without writing")
    ns = ap.parse_args()
    paths, label, dry = [ns.pmids], ns.label, ns.dry_run

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

    # A staging name of its own.  build.py stages on "<queue>.new" too, and two writers
    # sharing it meant one could publish the other's half-written file (CodeRabbit, PR #784).
    # This removes the COLLISION; it does not make concurrent writers safe -- see the module
    # docstring.
    import tempfile
    fd, tmp = tempfile.mkstemp(prefix="queue-add-", suffix=".csv", dir=os.path.dirname(QUEUE))
    os.close(fd)
    try:
        with open(tmp, "w", newline="", encoding="utf-8") as f:
            wr = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
            wr.writeheader()
            wr.writerows(rows + added)
        os.replace(tmp, QUEUE)                   # atomic: the queue is on a synced drive
    except PermissionError:
        print("  stanpumpr-queue.csv is open in another program -- nothing added")
        return 1
    finally:
        # Any failure, not only PermissionError: a full disk during writerows() or on close
        # bypassed the handler above and left another partial CSV beside the queue on every
        # retry (CodeRabbit, PR #784).  os.replace consumes `tmp`, so if it is still here the
        # write did not finish.  A cleanup failure must not mask the original error.
        if os.path.exists(tmp):
            try:
                os.remove(tmp)
            except OSError:
                pass
    print("  added %d row(s) to stanpumpr-queue.csv, noted \"%s%s\"" % (len(added), REQUEST_NOTE, label))
    return 0


if __name__ == "__main__":
    sys.exit(main())
