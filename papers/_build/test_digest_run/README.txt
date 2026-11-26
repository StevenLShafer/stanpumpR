Tests for digest_run.py's batch deadline, per-card logging and queue ORDER (2026-11-19).

    C:\Python314\python.exe -E test_run_claude.py     # the deadline, and the tree it kills
    C:\Python314\python.exe -E test_loop.py           # the batch loop: timeout logged, next batch still runs
    C:\Python314\python.exe -E test_queue_order.py    # step 3: what is digested first, and that nothing starves

All three exec the SHIPPED code straight out of ..\digest_run.py (it is a script, so it cannot be imported):
test_run_claude.py execs class Job / read_text / run_claude and drives them with fake_claude.py, and test_loop.py
execs the real batch loop with run_claude stubbed, and the real save_log() beside it, which the loop calls.
So they test the file as it stands, not a copy -- but they find it by TEXT, so if someone renames those
helpers or the "# 0. hold:", "# 5. refresh STATUS.md", "def save_log(entries):" or "# THIS interpreter"
markers the slices are taken from, the tests fail on an index error and the slice bounds need updating.
A name the loop STARTS calling is the other way this goes stale, and it is quieter: on 2026-11-21 the loop
moved from json.dump to the atomic save_log() (CodeRabbit, PR #844) and test_loop.py died with a NameError
attributed to an unrelated line of digest_run.py, because compile() was given that file's name.
Nothing here touches the archive, the repository or the real claude CLI: no tokens, about 40 seconds.

test_queue_order.py does the same for step 3, slicing the helpers (arrived / ident / wanted_by) and the two-lane
selection on the "PMID_STEM = re.compile", "ocr_log = os.path.join", "AGE_SLOT = 5" and "queued_at = time.time()"
lines; its `blocked` register and pdf_hashes.json are synthetic, in a temp directory, so it reads neither the
archive nor the repository and takes under a second.  Why it exists: until 2026-11-19 the third sort key was the
STEM, a string, so within a tier the queue ran lexicographically by PMID (PMID_10606836 < PMID_2054269 <
PMID_9559795) and a stem beginning with 9 sat about seventeen runs from the front while newly filed papers kept
arriving ahead of it.  PMID_9559795 (Tod 1998, amikacin's source) was lost that way and digested by hand (PR
#820).  The test checks both halves of the replacement, because they fail independently: revert the merit key to
the stem and 4 checks fail; remove the age lane and 6 do, the starvation ones among them (both mutations were
run).

Why they exist: run 20261119-1542 froze for 78 minutes, past its own BATCH_TIMEOUT, with both of its batch's
cards written, no CPU and no child process left, and lost 8 of its 10 batches.  subprocess.run(capture_output=
True) blocks in communicate() until EVERY holder of the inherited stdout/stderr write handles closes it, and
claude -p leaves helpers (MCP servers) that outlive it; CPython's timeout path then kills only the direct child
and, on Windows, calls communicate() AGAIN with no timeout.  Measured here: a 5-second timeout raised only after
40.1 s, when a grandchild exited.  run_claude() therefore uses files rather than pipes, and a job object so the
deadline ends the whole tree even when the intermediate process is already dead (taskkill /T cannot find an
orphan, whose parent pid is a dead pid).

One artifact worth knowing before changing a test: a card written within about 1.4 ms of t0 can have a timestamp
that still reads as OLDER than t0 on this machine, so the run's `mtime >= t0` test calls it unwritten.  No real
batch can do that (claude takes minutes), which is why fake_claude.py waits before it writes; do not "fix" the
run for it.

A second artifact, which no test covers because it needs an open digest branch: a card waiting in a PR is skipped
by one of TWO paths, and a run report shows whichever applies.  A card on a pushed branch that the cards worktree
does not hold reads as `missing` in digest_manifest.json, and `held`/cards.unmerged_cards() is the only thing
keeping it out of the queue -- "waiting in origin/claude/digests-<stamp>" in the report, which is how run
20261119-2336 correctly left PMID_9559795, PMID_10606836 and PMID_12630975 alone.  A card a run wrote ITSELF is
`done` in the manifest AND listed by unmerged_cards() once its branch is pushed (PMID_8995498 after that run), and
there the `digest == "done"` test short-circuits first, so `held` never sees it.  Neither is a defect, and cards
committed on digests/working are a third case again: `done`, in the worktree, and NOT listed by unmerged_cards().
Check which case you have before reading a "waiting in" line as evidence of either of the others.
