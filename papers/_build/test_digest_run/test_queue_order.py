"""Test the queue ORDER out of _build\\digest_run.py (step 3): what the run digests first, and that nothing
can be starved.

Why it exists: until 2026-11-19 the third sort key was the STEM, a string like PMID_9559795, so within a tier
the queue ran lexicographically by PMID (PMID_10606836 < PMID_2054269 < PMID_9559795).  Of the 359 papers then
waiting for a card 12 began with 1, 68 with 2 and 269 with 3-9, so at a cap of 20 a stem beginning with 9 sat
about seventeen runs from the front while newly filed papers kept arriving ahead of it: a permanent starvation,
not a queue.  PMID_9559795 (Tod 1998, the source of R/drugs_amikacin.R) was one of nine papers a pending-work
row named, missed the next run's queue for that reason alone, and had to be digested by hand (PR #820).

Both of the shipped slices are exec'd verbatim out of the file (it is a script, so it cannot be imported), as
test_loop.py does: the helpers (arrived / ident / wanted_by) and the two-lane selection.  So this tests the file
as it stands, not a copy -- but it finds them by TEXT, so if someone moves the "PMID_STEM = re.compile" /
"ocr_log = os.path.join" / "AGE_SLOT = 5" / "queued_at = time.time()" lines the slices are taken from, this
fails on an index error and the slice bounds need updating.

Nothing here reads the archive or the repository: `blocked` and pdf_hashes.json are synthetic, in a temp
directory.  No tokens, under a second.
"""
import io, json, math, os, re, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(os.path.dirname(HERE), "digest_run.py")   # the shipped script, beside us
WORK = tempfile.mkdtemp(prefix="digest_order_test_")         # never write into the archive
sys.stdout.reconfigure(encoding="utf-8")

text = io.open(SRC, encoding="utf-8").read()
helpers = "if True:\n" + text[text.index('    PMID_STEM = re.compile('):
                               text.index('    ocr_log = os.path.join(HERE, "ocr_log.json")')]
order = "if True:\n" + text[text.index("    AGE_SLOT = 5"):text.index("    queued_at = time.time()")]
print("exec'ing %d lines of the real helpers and %d of the real selection"
      % (helpers.count("\n"), order.count("\n")))

# What the helpers read: a synthetic pdf_hashes.json, and a synthetic `blocked` register.
BLOCKED = {"9559795": ["pending-work: amikacin"], "7324829": ["vasoactive: atropine"],
           "42479139": ["parkinsons: levodopa"], "8995498": ["chemotherapy: cytarabine"]}
HASHED = {"PMID_9559795": {"changed": 300.0, "content_changed": 100.0},   # content_changed wins
          "PMID_7324829": {"changed": 200.0},                            # no content_changed: changed
          "PMID_42479139": {"changed": 0, "content_changed": 0},         # both empty: unknown
          "PMID_8995498_aam": {"content_changed": 50.0}}
json.dump(HASHED, io.open(os.path.join(WORK, "pdf_hashes.json"), "w", encoding="utf-8"))

ns = {"os": os, "json": json, "re": re, "HERE": WORK, "blocked": BLOCKED}
exec(helpers, ns)
arrived, ident, wanted_by = ns["arrived"], ns["ident"], ns["wanted_by"]

fails = []


def ok(what, cond):
    print("   %-72s %s" % (what, "ok" if cond else "FAILED"))
    if not cond:
        fails.append(what)


print("\n1. the helpers")
ok("arrived() prefers content_changed to changed", arrived("PMID_9559795") == 100.0)
ok("arrived() falls back to changed", arrived("PMID_7324829") == 200.0)
ok("arrived() is 0 for a stem with no record, which sorts FIRST", arrived("PMID_1") == 0)
ok("arrived() reads a _aam stem by its own name", arrived("PMID_8995498_aam") == 50.0)
# The defect itself: as strings, "PMID_10606836" < "PMID_2054269" < "PMID_9559795".
stems = ["PMID_9559795", "PMID_2054269", "PMID_10606836"]
ok("ident() sorts PMIDs as integers, not strings",
   sorted(stems, key=ident) == ["PMID_2054269", "PMID_9559795", "PMID_10606836"])
ok("ident() puts a stem that is not a PMID after every PMID",
   sorted(["NJ_label-lantus_021081", "PMID_42479139"], key=ident) == ["PMID_42479139", "NJ_label-lantus_021081"])
ok("wanted_by() finds what a model is waiting for", wanted_by("PMID_9559795") == ["pending-work: amikacin"])
ok("wanted_by() reads a _aam stem's PMID", wanted_by("PMID_9559795_aam") == ["pending-work: amikacin"])
ok("wanted_by() is empty for a paper nobody asked for", wanted_by("PMID_11111111") == [])
ok("wanted_by() is empty for a stem that is not a PMID", wanted_by("NJ_label-lantus_021081") == [])


def pick(pool, cap=20):
    """One run's queue, from the shipped selection.  Returns (queue, lane of each stem)."""
    space = {"cand": [dict(c) for c in pool], "cap": cap, "ident": ident}
    exec(order, space)
    return space["queue"], space["lane_of"]


def paper(n, tier=4, blocked=False, stale=False, arr=None):
    return {"stem": "PMID_%d" % n, "tier": tier, "stale": stale, "blocked": blocked,
            "arr": n if arr is None else arr, "arrived": n if arr is None else arr}


# digest_run's own candidate dicts carry "arrived"; the lane keys read that name.  `arr` is kept above
# only so a reader of this file can see the two are the same number.
print("\n2. the shape of the defect: the papers a model wants all have HIGH pmids")
backlog = ([paper(9000000 + i, blocked=True) for i in range(8)]        # wanted, and last alphabetically
           + [paper(1000000 + i) for i in range(40)])                 # not wanted, and first
queue, lanes = pick(backlog)
wanted_first = [s for s in queue if any(c["stem"] == s and c["blocked"] for c in backlog)]
ok("every paper a model is waiting for is in the FIRST run of 20", len(wanted_first) == 8)
# They take every MERIT place until they run out.  Not places 1-8: place 5 is the age lane's, and
# here the wanted papers are the NEWEST arrivals, so the age lane spends it on an old unwanted one.
ok("they take the front of the merit lane, before the papers nobody asked for",
   [s for s in queue if lanes[s] == "merit"][:8]
   == sorted((c["stem"] for c in backlog if c["blocked"]), key=ident))
ok("so all eight are within the first nine places, place 5 being the age lane's",
   set(wanted_first) == set(queue[:9]) - {queue[4]} and lanes[queue[4]] == "age")
old = [x[2] for x in sorted((c["tier"], not c["stale"], c["stem"]) for c in backlog)][:20]
ok("the OLD key would have taken none of them (this is the regression guard)",
   not any(c["blocked"] for c in backlog if c["stem"] in old))

print("\n3. the tier still leads, among papers nobody is waiting for")
mixed = [paper(5000000, tier=5), paper(5000001, tier=4), paper(5000002, tier=1), paper(5000003, tier=2),
         paper(5000004, tier=3)]
queue, lanes = pick(mixed, cap=1)
ok("a paper a drug model cites goes before one only the docs cite", queue == ["PMID_5000002"])
staleness = [paper(6000000, arr=10), paper(6000001, stale=True, arr=20)]
queue, lanes = pick(staleness, cap=1)
ok("a stale card is rewritten before a missing one is written", queue == ["PMID_6000001"])

print("\n4. oldest arrived first, so a paper filed today joins the BACK")
fifo = [paper(7000000 + i, arr=1000 - i) for i in range(30)]   # pmid rises as arrival falls
queue, lanes = pick(fifo)
ok("the queue is in arrival order, not pmid order",
   queue[:3] == ["PMID_7000029", "PMID_7000028", "PMID_7000027"])

print("\n5. the age lane: every 5th place, whatever the tier")
tiered = ([paper(8000000 + i, tier=1, blocked=True, arr=5000 + i) for i in range(30)]   # new, top priority
          + [paper(8100000 + i, tier=5, arr=i) for i in range(10)])                     # old, bottom tier
queue, lanes = pick(tiered)
ok("4 of a run of 20 come from the age lane", sum(1 for s in queue if lanes[s] == "age") == 4)
ok("they are places 5, 10, 15 and 20", [n for n, s in enumerate(queue, 1) if lanes[s] == "age"] == [5, 10, 15, 20])
ok("each is the oldest paper still waiting, bottom tier though it is",
   [s for s in queue if lanes[s] == "age"] == ["PMID_8100000", "PMID_8100001", "PMID_8100002", "PMID_8100003"])
ok("no stem is taken twice", len(set(queue)) == len(queue))
queue, lanes = pick(tiered, cap=4)
ok("a run shorter than one age slot still gives the age lane its LAST place",
   [n for n, s in enumerate(queue, 1) if lanes[s] == "age"] == [4])
queue, lanes = pick(tiered, cap=2)
ok("...and at a cap of 2 that is one merit, one age", [lanes[s] for s in queue] == ["merit", "age"])
queue, lanes = pick(tiered, cap=1)
ok("but a run of ONE cannot split, so its single place goes to merit", lanes[queue[0]] == "merit")


def drain(pool, runs, cap=20, arriving=0):
    """Run `runs` consecutive runs; return {stem: the run that digested it}, and what is left."""
    pool, when, t = [dict(c) for c in pool], {}, max(c["arrived"] for c in pool) + 1
    for r in range(1, runs + 1):
        for s in pick(pool, cap)[0]:
            when[s] = r
        pool = [c for c in pool if c["stem"] not in when]
        for k in range(arriving):        # the worst case: top priority AND the newest arrival
            t += 1
            pool.append(paper(9500000 + r * 100 + k, tier=1, blocked=True, arr=t))
    return when, pool


print("\n6. fairness: ten consecutive runs of 20 against a backlog of 280")
real = [paper(2000000 + i, tier=4, arr=i) for i in range(270)] + [paper(2900000 + i, tier=5, arr=i) for i in range(10)]
when, left = drain(real, 10)
ok("ten runs digest 200 of the 280", len(when) == 200 and len(left) == 80)
when, left = drain(real, math.ceil(280 / 20))
ok("the whole backlog drains in ceil(280/20) = 14 runs, the floor", len(when) == 280 and max(when.values()) == 14)

print("\n7. fairness under a hostile stream: 25 top-priority papers arriving EVERY run")
when, left = drain(real, 80, arriving=25)
rank = {c["stem"]: k for k, c in enumerate(sorted(real, key=lambda c: (c["arrived"], ident(c["stem"]))), 1)}
reached = {s: r for s, r in when.items() if s in rank}
ok("not one of the 280 is starved, though every run is swamped", len(reached) == 280)
ok("each waits at most ceil(its arrival rank / 4) runs, the age lane's bound",
   all(r <= math.ceil(rank[s] / 4) for s, r in reached.items()))
ok("the bottom tier is reached too (the old key starved it for ever)",
   all(c["stem"] in when for c in real if c["tier"] == 5))   # `in`, not when[...]: a starved stem
                                                             # must be REPORTED, not a KeyError
old_when, _ = drain(real, 80, arriving=25)      # same stream, the old key
old_when = {}
for r in range(1, 81):
    pool = [c for c in real if c["stem"] not in old_when] + [
        paper(9500000 + rr * 100 + k, tier=1, blocked=True) for rr in range(1, r + 1) for k in range(25)]
    for s in [x[2] for x in sorted((c["tier"], not c["stale"], c["stem"]) for c in pool)][:20]:
        old_when.setdefault(s, r)
ok("the OLD key starves most of the same backlog for ever (what this replaced)",
   sum(1 for c in real if c["stem"] not in old_when) > 200)

print("\n8. the ordering is deterministic, so --dry shows the order a real run will take")
ok("the same backlog gives the same queue twice", pick(real)[0] == pick(real)[0])
ok("the order does not depend on the manifest's own order",
   pick(real)[0] == pick(list(reversed(real)))[0])

print("\n%d check(s) failed" % len(fails) if fails else "\nall checks passed")
for f in fails:
    print("  FAILED: %s" % f)
sys.exit(1 if fails else 0)
