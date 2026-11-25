"""Close the loop: say which papers the models are WAITING FOR have arrived.

The archive's pull side works -- a registry row names the paper it wants by PMID, often with
the question it should answer, and `papers/digests/` puts the card beside the code.  What has
never existed is the PUSH: nothing notices that a paper somebody asked for weeks ago is now
sitting in the archive.  On 2026-11-16 a hand check found 41 of the 60 PMIDs that registries
list under `to_obtain` already filed, 14 of them with no card, and nobody had been told.
Shafer that day: the fear is "pulling papers, having them land in the archive, and not closing
the loop with the model".  This is that check, run by update.bat after every filing.

It reports four things, and only things that need an action:

  ARRIVED, NOT DIGESTED  a registry's `to_obtain` PMID is in the archive with no card.
                         -> it is in the digest queue; nothing to do but let the batch reach it.
  DIGESTED, ROW STALE    EVERY PMID the row asks for has arrived and has a card, and the row
                         still says it is waiting.
                         -> read the cards and update `evidence_read` / `readiness` / `to_obtain`.
  PARTLY SATISFIED       some of the row's PMIDs have arrived and others have not.
                         -> narrow `to_obtain` to what is still missing; do NOT clear it.
                         A row often asks for several papers (nicardipine's names Wagner 1987,
                         Graham 1985 and Modi 1993), and the first version of this script called
                         such a row stale as soon as ONE of them had a card, which would have
                         thrown away two live requests.
  PUBLISHED SUPERSEDES   a card was written from an accepted manuscript (`edition:` says so)
                         and the published PDF has since been filed.
                         -> re-digest, or read what the AAM's card says it could not.
  PENDING WORK           a row of papers/pending-work.csv whose paper is now here: work that was
                         ready except for it, recorded when its pull request was merged and
                         closed (papers/PENDING-WORK.md).
                         -> do it in a NEW pull request against current master.  Nothing is
                         parked and nothing is reopened: the row IS the handover.
  STILL MISSING          counted, and ADDED to stanpumpr-queue.csv as a pending row when the
                         queue did not already carry them, with what wants the paper in
                         `notes` (`--no-enqueue` reports without writing).
                         -> the queue is what drives the downloading, so a wanted paper that
                         never reaches it is a request nobody can see.  On 2026-11-18 nine of
                         the ten PMIDs that registry rows were blocked on had never reached
                         it.  build.py rebuilds the queue from the works the SOURCE cites and
                         keeps these rows by their `notes` prefix.

The registries are read from `origin/master`, not from a working tree: several checkouts of
this repository exist on this machine and any of them may be on a branch.  `--repo` overrides
the path, `--no-fetch` skips the fetch (for a machine that is offline).

RUNNING WHERE THE ARCHIVE IS NOT (`--no-archive`), and why that is sound
-----------------------------------------------------------------------
Everything above needs the archive, which is on the Drive on one computer, so this check runs
only where the papers are -- and so the two categories that are READY WORK (a row whose papers
have all arrived AND been carded; a pending-work row whose paper is here) are seen only by
whoever happens to run update.bat.  On 2026-11-25 that was 27 stale registry rows and 20 ready
pending-work rows, every one of them with the paper already in hand: by then the rate-limiting
step was no longer GETTING the papers but noticing they had come.

`--no-archive` makes the arrival set the CARDS ON MASTER instead of the PDFs on disk.  That is
not an approximation of convenience: a card cannot be written for a paper nobody has, so
`cards` is a subset of `have`.  For a STALE REGISTRY ROW the two tests agree exactly, because
that category already requires every one of the row's PMIDs to be carded -- measured on
2026-11-25, both ways gave the same 27 rows, line for line.

What the mode gives up is only what the cards cannot know, and in one direction only: a paper
that has ARRIVED BUT IS NOT CARDED YET reads as still missing.  So a pending-work row is
reported only once its paper is carded (on 2026-11-25, 20 of 22 -- the two left out were
esketamine papers filed and not yet digested), the ARRIVED-NOT-DIGESTED category is empty by
construction, a row whose papers are all here but only some carded shows as partly satisfied
instead, and a published edition superseding an accepted manuscript is not reported at all,
since the suffixes live in the file names.  The mode therefore UNDER-reports and never
over-reports, which is the safe direction for a watch; it says so in its own output, so that a
short list is not read as good news.

It needs nothing but a clone, so it can run in a scheduled cloud session, which is the point:
the detection was never the gap, the gap was that its output went to a terminal.  It writes
nothing, so `--no-enqueue` is forced on (the queue lives in the archive).

`--tasks` prints one TAB-separated line per item instead of the report -- kind, where, drug,
PMIDs, cards, and what the row asks for -- so that something else can turn each into a message,
an issue or a session.  Nothing in this script decides to start work: it reports, as it always
has.

Usage: python arrivals.py [--repo <path>] [--ref <ref>] [--no-fetch] [--all] [--no-enqueue]
                          [--no-archive] [--tasks]
"""
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)

# Parsed only when this is the script being run, so that another script can IMPORT this one
# (digest_run.py orders its queue by what the models are blocked on, 2026-11-19) without its own
# flags being read as ours.  An importer gets the defaults below, origin/master among them.
args = sys.argv[1:] if __name__ == "__main__" else []
REPO = args[args.index("--repo") + 1] if "--repo" in args else r"C:\dev\stanpumpR"
FETCH = "--no-fetch" not in args
SHOW_MISSING = "--all" in args
REF = args[args.index("--ref") + 1] if "--ref" in args else "origin/master"
# `--ref` is for testing a change before it is on master (point it at your own branch).
# The DEFAULT must stay origin/master: a row that is only on a branch is invisible to every
# other machine, which is the one way the pending-work register can fail.

NO_ARCHIVE = "--no-archive" in args
# Take the cards on `REF` as the arrival set, so the check runs in a clone with no papers
# beside it (the docstring says why that is exact for the categories it reports).
TASKS = "--tasks" in args
# One TAB-separated line per item instead of the report, for a caller that acts on them.

ENQUEUE = "--no-enqueue" not in args and not NO_ARCHIVE
# A wanted paper that is in NEITHER the archive nor the download queue is invisible to the
# person doing the downloading: on 2026-11-18 nine of the ten PMIDs that registry rows were
# blocked on had never reached stanpumpr-queue.csv, so nobody was looking for them and the
# filer, which identifies a PDF against the reference list, called them "not a stanpumpR
# reference" when they finally arrived by hand.  So the report also WRITES: it adds a pending
# queue row for each one, with what wants it and why in `notes`.

QUEUE = os.path.join(ARCHIVE, "stanpumpr-queue.csv")
REQUEST_NOTE = "wanted by "
# build.py rebuilds this queue from the works the stanpumpR SOURCE cites and keeps any row
# whose notes begin with REQUEST_NOTE; without that these rows would be dropped on the next
# build, because a paper can be wanted by a model long before the code cites it.  The literal
# is duplicated there deliberately: importing this module would run its argument parsing.

sys.stdout.reconfigure(encoding="utf-8")


def git(*a):
    """Run git in REPO and return stdout ('' on failure, so a missing file is simply empty)."""
    r = subprocess.run(["git", "-C", REPO] + list(a), capture_output=True, text=True,
                       encoding="utf-8", errors="replace")
    return r.stdout if r.returncode == 0 else ""


def archived_pmids():
    """{pmid: set(of suffixes)} for every PMID_<n>[_<suffix>].pdf in the archive.

    The suffix matters: PMID_32238008_aam.pdf is the accepted manuscript and
    PMID_32238008.pdf the published version, and a card written from the first is
    superseded by the second."""
    if NO_ARCHIVE:
        # No papers beside us: a card is proof its paper arrived, so the cards ARE the
        # arrival set.  Every suffix reads as "" (published), which is why the superseded
        # check is skipped in this mode rather than answered wrongly.
        return {p: {""} for p in cards_on_master()}
    out = {}
    for f in os.listdir(ARCHIVE):
        m = re.fullmatch(r"PMID_(\d+)(?:_([A-Za-z0-9]+))?\.pdf", f, re.I)
        if m:
            out.setdefault(m.group(1), set()).add((m.group(2) or "").lower())
    return out


def cards_on_master():
    """{pmid: card path} from origin/master, so a card merged by another machine counts."""
    out = {}
    for line in git("ls-tree", "--name-only", REF, "papers/digests/").splitlines():
        m = re.fullmatch(r"papers/digests/PMID_(\d+)(?:_[A-Za-z0-9]+)?\.md", line.strip(), re.I)
        if m:
            out.setdefault(m.group(1), line.strip())
    return out


def card_edition(path):
    """The card's `edition:` line, '' if it has none (most cards are of a published paper)."""
    for line in git("show", "%s:%s" % (REF, path)).splitlines()[:40]:
        m = re.match(r"\s*edition:\s*(.+?)\s*$", line)
        if m:
            return m.group(1).strip().strip('"')
    return ""


def wanted():
    """Every (registry, drug, [pmids], reason) a registry row asks for under `to_obtain`.

    The PMIDs are kept together PER ROW, because a row asking for three papers is satisfied
    only when all three have arrived; see PARTLY SATISFIED above.

    Only `to_obtain` is read, deliberately: a PMID in `source` is the paper a row was BUILT
    from and is not a request, and scanning free prose would report every citation."""
    import csv
    import io
    out = []
    names = [n for n in git("ls-tree", "--name-only", REF, "inst/extdata/").splitlines()
             if n.lower().endswith("registry.csv")]
    for name in names:
        text = git("show", "%s:%s" % (REF, name))
        if not text:
            continue
        rows = list(csv.DictReader(io.StringIO(text)))
        if not rows or "to_obtain" not in rows[0]:
            continue
        for r in rows:
            t = (r.get("to_obtain") or "").strip()
            if not t or t.upper() == "NA":
                continue
            drug = (r.get("drug") or r.get("stanpumpr_drug") or r.get("model_id") or "").strip()
            pmids = list(dict.fromkeys(re.findall(r"PMID[:\s]*(\d{4,9})", t)))
            if pmids:
                out.append((os.path.basename(name), drug, pmids, t,
                            (r.get("model_id") or "").strip()))
    return out


def wanted_pmids():
    """{pmid: [what wants it]} for every paper a model is BLOCKED on, arrived or not.

    The same two registers main() reports from -- a registry row's `to_obtain` and the pending rows
    of papers/pending-work.csv -- but keyed by PMID and without asking whether the paper is here.
    main() wants the ones that have NOT arrived (to enqueue them); digest_run.py wants the opposite,
    the ones that are here and have no card, so that its queue digests what somebody is waiting on
    first.  Read from origin/master like everything else in this module.
    """
    out = {}

    def add(pmid, label):
        if label not in out.setdefault(pmid, []):
            out[pmid].append(label)

    for reg, drug, pmids, reason, model_id in wanted():
        label = re.sub(r"Registry\.csv$", "", reg) or reg
        for p in pmids:
            add(p, "%s: %s" % (label, drug or "-"))
    for pmid, drug, what, where, raised in pending_work():
        add(pmid, "pending-work: %s" % (drug or "-"))
    return out


def pending_work():
    """The `pending` rows of papers/pending-work.csv, read from origin/master.

    A row records work that was ready except for one paper, kept when its pull request was merged
    and closed so that closing cost nothing.  Read from master deliberately: a row still sitting on
    a closed branch is invisible, which is the one way this register can fail."""
    import csv
    import io
    text = git("show", "%s:papers/pending-work.csv" % REF)
    if not text.strip():
        return []
    out = []
    for r in csv.DictReader(io.StringIO(text)):
        if (r.get("status") or "").strip().lower() != "pending":
            continue
        pmid = re.sub(r"\D", "", r.get("pmid") or "")
        if pmid:
            out.append((pmid, (r.get("drug") or "-").strip(),
                        (r.get("what_the_paper_would_let_us_do") or "").strip(),
                        (r.get("where") or "").strip(), (r.get("raised_in") or "").strip()))
    return out


def uncited(reason, pmids):
    """A warning when `to_obtain` seems to name more papers than it gives PMIDs for.

    Not every citation in a `to_obtain` carries a PMID: nicardipine's names Wagner 1987 and
    Graham 1985 in prose and only Modi 1993 by number, and digoxin's names Koup 1975 and
    Kramer 1974 the same way.  Counting PMIDs alone calls such a row satisfied as soon as the
    one numbered paper arrives, and clearing it would throw the others away.  Publication
    years are the cheapest proxy for "how many works are cited here": more years than PMIDs
    means READ THE PROSE before touching the row.  A heuristic, and deliberately noisy in the
    safe direction -- it can only ask for a second look, never clear anything itself."""
    years = set(re.findall(r"\b(?:19|20)\d\d\b", reason))
    extra = len(years) - len(pmids)
    return ("   <-- names %d publication year(s) but only %d PMID(s): READ the `to_obtain` text,"
            " other papers may still be wanted" % (len(years), len(pmids))) if extra > 0 else ""


def enqueue(wanted_by):
    """Add a pending queue row for each wanted PMID that is not in the archive OR the queue.

    `wanted_by` is {pmid: [label, ...]}, the labels saying what wants it ("parkinsons:
    apomorphine", "pending-work: diltiazem").  Returns (added, already, problem): the rows
    written, the PMIDs the queue already carried, and a one-line reason if nothing could be
    written.  Nothing is ever removed or rewritten here -- a row the queue already has is
    left exactly as it stands, since its status may be the record of a download in progress.
    """
    import csv                                   # as wanted() and pending_work() do
    if not wanted_by:
        return [], [], ""
    try:
        with open(QUEUE, encoding="utf-8") as f:
            reader = csv.DictReader(f)
            rows, fields = list(reader), reader.fieldnames
    except FileNotFoundError:
        return [], [], "stanpumpr-queue.csv is not in the archive -- nothing enqueued"
    if not fields:
        return [], [], "stanpumpr-queue.csv has no header -- nothing enqueued"
    held = {(r.get("pmid") or "").strip() for r in rows}
    new = [p for p in sorted(wanted_by, key=int) if p not in held]
    already = sorted((p for p in wanted_by if p in held), key=int)
    if not new:
        return [], already, ""
    try:
        import ncbi
        meta = ncbi.esummary(new)
    except Exception as e:                       # offline, or NCBI refusing: say so, add nothing
        return [], already, "could not reach PubMed (%s); %d row(s) not added" % (e, len(new))
    added = []
    for p in new:
        m = meta.get(p, {})
        doi = m.get("doi", "")
        note = REQUEST_NOTE + "; ".join(dict.fromkeys(wanted_by[p]))
        if m.get("pmcid"):
            note += " (free at PMC %s)" % m["pmcid"]
        added.append({
            "pmid": p, "doi": doi,
            "journal": m.get("journal_full") or m.get("journal", ""),
            "year": m.get("year") or "", "volume": m.get("volume", ""),
            "pages": m.get("pages", ""),
            # The same link build.py writes: the Lane proxy where there is a DOI to proxy.
            "download_link": ("https://doi-org.laneproxy.stanford.edu/%s" % doi) if doi
                             else "https://pubmed.ncbi.nlm.nih.gov/%s/" % p,
            "status": "pending", "downloaded_at": "", "notes": note,
        })
    tmp = QUEUE + ".new"
    try:
        with open(tmp, "w", newline="", encoding="utf-8") as f:
            wr = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
            wr.writeheader()
            wr.writerows(rows + added)
        os.replace(tmp, QUEUE)                   # atomic: the queue is on a synced drive
    except PermissionError:
        for leftover in (tmp,):
            if os.path.exists(leftover):
                os.remove(leftover)
        return [], already, "stanpumpr-queue.csv is open in another program -- %d row(s) not added" % len(new)
    return added, already, ""



def diag(msg):
    """A human-readable line.  In --tasks mode it goes to STDERR, so that stdout carries
    nothing but task records: a consumer reading six tab-separated fields was handed the
    early-exit messages ("cannot read origin/master", "no registry lists a `to_obtain` PMID")
    and could not parse them (CodeRabbit, PR #926)."""
    print(msg, file=sys.stderr if TASKS else sys.stdout)


def no_archive_caveat():
    """Printed on EVERY exit in --no-archive mode, the early ones included.

    Without it a short list reads as good news: the arrival set is the cards, so a paper that
    is filed and not yet digested counts as still to obtain.  On 2026-11-25 that was 11 'still
    to obtain' in this mode against 5 beside the archive.  It was missing from the two early
    returns until CodeRabbit pointed it out on PR #926, which made the docstring's "every
    exit" an overclaim.

    In --tasks mode it is one `#`-prefixed line on STDOUT, not stderr: a consumer that pipes
    stdout must still see it, and skipping `#` lines is the ordinary contract for a
    tab-separated stream.  That is the one line of stdout that is not a record."""
    if not NO_ARCHIVE:
        return
    if TASKS:
        print("#\tno-archive mode: arrival = a card on %s, so a paper that arrived with no"
              " card reads as still missing, and no superseded edition is reported" % REF)
    else:
        diag("\nNOTE: --no-archive. Arrival means A CARD ON %s, not a PDF on disk, so"
             " a paper that is\n      filed but not yet digested counts as STILL TO OBTAIN,"
             " nothing is reported as\n      arrived-not-digested, and no superseded edition"
             " is reported. The ready rows above\n      are complete for the registries and may"
             " be short for pending-work. Run it beside\n      the archive for the full picture."
             % REF)


def main():
    if FETCH:
        subprocess.run(["git", "-C", REPO, "fetch", "origin", "master", "-q"],
                       capture_output=True, text=True)
    if not git("rev-parse", REF).strip():
        diag("arrivals: cannot read %s in %s -- skipped" % (REF, REPO))
        no_archive_caveat()
        return 0

    have, cards, rows = archived_pmids(), cards_on_master(), wanted()
    if not rows:
        diag("arrivals: no registry lists a `to_obtain` PMID -- nothing to check")
        no_archive_caveat()
        return 0

    undigested, stale, partly, missing = [], [], [], []
    seen, allpmids, wanted_by = set(), set(), {}
    for reg, drug, pmids, reason, model_id in rows:
        key = (reg, model_id or drug, tuple(pmids))
        if key in seen:                         # the same row read twice
            continue
        seen.add(key)
        allpmids.update(pmids)
        here = [p for p in pmids if p in have]
        gone = [p for p in pmids if p not in have]
        label = re.sub(r"Registry\.csv$", "", reg) or reg
        for p in gone:                          # not here, whatever the rest of the row says
            wanted_by.setdefault(p, []).append("%s: %s" % (label, drug or "-"))
        if not here:                            # none of them has arrived
            missing.extend((reg, drug, p) for p in gone)
            continue
        if gone:                                # some here, some not: narrow, never clear
            partly.append((reg, drug, here, gone, reason))
            missing.extend((reg, drug, p) for p in gone)
            continue
        if all(p in cards for p in here):       # every one arrived AND digested
            stale.append((reg, drug, here, reason))
        else:
            undigested.append((reg, drug, [p for p in here if p not in cards], reason))

    # A card written from an accepted manuscript whose published PDF has since been filed.
    superseded = []
    for pmid, suffixes in (() if NO_ARCHIVE else sorted(have.items())):
        if "" in suffixes and len(suffixes) > 1 and pmid in cards:
            ed = card_edition(cards[pmid])
            if ed and ed != "published":
                superseded.append((pmid, cards[pmid], ed, sorted(s for s in suffixes if s)))

    waiting = pending_work()
    pending = [p for p in waiting if p[0] in have]
    for pmid, drug, what, where, raised in waiting:
        if pmid not in have:                    # pending work whose paper has still not come
            wanted_by.setdefault(pmid, []).append("pending-work: %s" % (drug or "-"))

    added, already_queued, enqueue_problem = ([], [], "") if not ENQUEUE else enqueue(wanted_by)

    if TASKS:
        # kind <TAB> where <TAB> drug <TAB> pmids <TAB> cards <TAB> what-the-row-asks-for
        # Stable field order; a field's text is flattened, never truncated, so a consumer
        # has the whole request.  `ready` is the only kind that is work somebody can start
        # now; the rest are named so that a filter cannot silently drop a category it has
        # not heard of.
        def flat(x):
            """One field, with tabs and line breaks squeezed out.

            EVERY field is flattened, not just `what`: `where` and `drug` come from CSV
            columns, which may hold either character, and a newline in one of them would
            write a single task across several lines while a tab would add a column
            (CodeRabbit, PR #926)."""
            return re.sub(r"\s+", " ", str(x or "")).strip()

        def emit(kind, where, drug, pmids, what):
            print("\t".join(flat(f) for f in (
                kind, where, drug or "-", ",".join(pmids),
                ",".join(cards.get(x, "") for x in pmids), what)))
        for reg, drug, pmids, reason in stale:
            emit("ready-registry-row", reg, drug, pmids, reason)
        for pmid, drug, what, where, raised in pending:
            emit("ready-pending-work", where or "papers/pending-work.csv", drug, [pmid], what)
        for pmid, path, ed, sfx in superseded:
            emit("superseded-edition", path, "-", [pmid],
                 "card written from %s; published PDF now filed" % ed)
        for reg, drug, pmids, reason in undigested:
            emit("arrived-not-digested", reg, drug, pmids, reason)
        for reg, drug, here, gone, reason in partly:
            emit("partly-satisfied", reg, drug, here + gone, reason)
        no_archive_caveat()
        return 0

    if not (undigested or stale or partly or superseded or pending or added or enqueue_problem):
        print("arrivals: nothing waiting has arrived (%d `to_obtain` PMIDs, %d still to obtain)"
              % (len(allpmids), len(set(p for _, _, p in missing))))
        no_archive_caveat()
        return 0

    def plist(ps):
        return ", ".join("PMID_" + p for p in ps)

    print("\n=== Papers a model is waiting for that are now IN THE ARCHIVE ===")
    if undigested:
        print("\nArrived, not yet digested (the digest batch will reach them):")
        for reg, drug, ps, reason in sorted(undigested):
            print("  %-26s %-22s %s" % (reg, drug[:21], plist(ps)))
            print("      wanted: %s" % reason[:150])
    if stale:
        print("\nEvery PMID the row names has arrived AND been digested, and the row still"
              " says it is waiting -- read the cards and update the row:")
        for reg, drug, ps, reason in sorted(stale):
            print("  %-26s %-22s %s%s" % (reg, drug[:21], plist(ps), uncited(reason, ps)))
            print("      wanted: %s" % reason[:150])
    if partly:
        print("\nPartly satisfied -- NARROW `to_obtain` to what is still missing, do not clear it:")
        for reg, drug, here, gone, reason in sorted(partly):
            print("  %-26s %-22s here: %s | still wanted: %s"
                  % (reg, drug[:21], plist(here), plist(gone)))
            print("      wanted: %s" % reason[:150])
    if superseded:
        print("\nA card was written from a non-published edition and the published PDF is now here:")
        for pmid, path, ed, suffixes in superseded:
            print("  PMID_%s  card edition: %s  (also here: %s)"
                  % (pmid, ed, ", ".join("_" + s for s in suffixes)))
            print("      %s" % path)
    if pending:
        print("\nPending work whose paper is now here -- do it in a NEW pull request against current"
              " master (papers/PENDING-WORK.md):")
        for pmid, drug, what, where, raised in sorted(pending, key=lambda x: (x[1], x[0])):
            print("  PMID_%-9s %-20s %s" % (pmid, drug[:19], "(raised in %s)" % raised if raised else ""))
            print("      would let us: %s" % what[:300])
            if where:
                print("      where: %s" % where[:200])
            if pmid not in cards:
                print("      (not digested yet -- the batch will reach it)")
    if added:
        print("\nAdded to the download queue (stanpumpr-queue.csv) -- wanted by a model, not"
              " here, and not previously queued:")
        for a in added:
            print("  PMID_%-9s %s" % (a["pmid"], a["notes"]))
            print("      %s %s %s  %s" % (a["journal"][:40], a["year"], a["volume"],
                                          a["download_link"][:90]))
    if enqueue_problem:
        print("\nNothing was added to the download queue: %s" % enqueue_problem)

    still = sorted(set(p for _, _, p in missing))
    tail = ("" if not ENQUEUE else                # with --no-enqueue the queue was never read,
            ", of which %d were already in the download queue and %d were just added"
            % (len(already_queued), len(added)))  # so say nothing about it rather than "0"
    print("\n(%d rows ask for a paper by PMID; %d PMIDs in all; %d still to obtain%s.)"
          % (len(seen), len(allpmids), len(still), tail))
    if SHOW_MISSING and missing:
        print("Still to obtain:")
        for reg, drug, pmid in sorted(set(missing)):
            print("  %-26s %-24s PMID_%s" % (reg, drug[:23], pmid))
    no_archive_caveat()
    return 0


if __name__ == "__main__":
    sys.exit(main())
