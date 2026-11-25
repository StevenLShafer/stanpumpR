"""Test the real batch loop out of _build\\digest_run.py: a batch that times out must be logged, counted and
followed by the next batch, and a card must be logged the moment it appears rather than when the batch returns.

The loop's text is exec'd verbatim from the file; only run_claude is stubbed (it has its own tests), so this is
the shipped loop, not a copy.  The stub stands in for: batch 1, a claude that writes nothing and has to be
killed at the deadline; batch 2, a claude that writes both cards while it runs and exits 0.
"""
import io, json, os, re, subprocess, sys, tempfile, time, types

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(os.path.dirname(HERE), "digest_run.py")   # the shipped script, beside us
WORK = tempfile.mkdtemp(prefix="digest_run_test_")   # never write into the archive
CARDS = os.path.join(WORK, "loopcards")
os.makedirs(CARDS, exist_ok=True)
for f in os.listdir(CARDS):
    os.remove(os.path.join(CARDS, f))

text = io.open(SRC, encoding="utf-8").read()
start = text.index("    for i, planned in enumerate(batches):")
end = text.index("    # 5. refresh STATUS.md")
loop = "if True:\n" + text[start:end]
# The loop calls save_log() (atomic since 2026-11-21, CodeRabbit PR #844), so the namespace needs
# it.  Taken from the file rather than stubbed, so the log writing under test is the shipped one.
saver = text[text.index("def save_log(entries):"):text.index("\n# THIS interpreter")]
print("exec'ing %d lines of the real batch loop, plus save_log()" % loop.count("\n"))

said, written_log = [], os.path.join(WORK, "loop_log.json")


def say(msg):
    said.append(msg)
    print("   report| " + msg.replace("\n", "\n   report| ")[:300])


calls = []


def stub_run_claude(cmd, prompt, scratch, timeout, tick=None, poll=5.0):
    """batch 1: nothing written, killed at the deadline.  batch 2: cards appear while it runs."""
    n = len(calls) + 1
    stems = re.findall(r"T_LOOP_\w+", prompt)   # the loop puts the batch in the prompt
    calls.append({"n": n, "stems": stems, "timeout": timeout})
    if n == 1:
        for _ in range(3):
            tick()                                  # no card appears, so no entry should be made
            time.sleep(0.2)
        return "timed out after 75 min; its process tree was ended and the run went on.\nnothing to show", "timeout"
    time.sleep(0.4)    # as a real batch does: a card cannot appear within the ~1.4 ms in which a
                       # file's timestamp can still read as older than t0 (measured on this machine)
    for s in stems:
        open(os.path.join(CARDS, s + ".md"), "w", encoding="utf-8").write("card\n")
        time.sleep(0.1)
        tick()                                      # each card is logged here, mid-batch
    return "fake: wrote the cards", 0


ns = {"os": os, "time": time, "json": json, "re": re, "subprocess": subprocess, "sys": sys,
      "say": say, "run_claude": stub_run_claude,
      "CARDS": CARDS, "LOG": written_log, "LOCAL": WORK, "SNAP": r"C:\snap", "ARCH": HERE,
      "CLAUDE": "claude.exe", "BATCH_TIMEOUT": 75 * 60, "stamp": "TEST-0000", "sha": "deadbeef",
      "today": "2026-11-19", "base": "PROMPT {CHECKOUT} {SHA} {CARDS}\n", "log": [],
      "queued_at": time.time(), "cards": types.SimpleNamespace(card_author=lambda p: (None, False)),
      "batches": [["T_LOOP_A", "T_LOOP_B"], ["T_LOOP_C", "T_LOOP_D"]]}
exec(compile(saver, SRC, "exec"), ns)     # def save_log, as shipped
exec(compile(loop, SRC, "exec"), ns)

log = ns["log"]
fails = []


def check(ok, msg):
    print(("  PASS  " if ok else "  FAIL  ") + msg)
    if not ok:
        fails.append(msg)


print("\nlog entries: %s" % json.dumps(log, indent=None))
check(len(calls) == 2, "both batches ran: the timed-out batch 1 was FOLLOWED BY batch 2 (%d calls)" % len(calls))
t = {e["stem"]: e for e in log}
check(sorted(t) == ["T_LOOP_A", "T_LOOP_B", "T_LOOP_C", "T_LOOP_D"], "every stem of both batches is logged")
check(all(t[s]["exit"] == "timeout" and not t[s]["card_written"] for s in ("T_LOOP_A", "T_LOOP_B")),
      "batch 1's stems are counted with exit 'timeout' and no card")
check(all(t[s]["exit"] == 0 and t[s]["card_written"] for s in ("T_LOOP_C", "T_LOOP_D")),
      "batch 2's stems are counted with exit 0 and a card")
check(not any(e["exit"] == "running" for e in log), "no entry is left at the provisional 'running'")
check(all(isinstance(e["minutes"], float) or isinstance(e["minutes"], int) for e in log), "every entry has minutes")
# the whole point of item 4: the card line is in the report BEFORE the batch's output block
lines = "\n".join(said)
i_card = lines.index("- T_LOOP_C: card written")
i_out = lines.index("fake: wrote the cards")
check(i_card < i_out, "the card was reported as it appeared, before the batch's output block")
check(json.load(open(written_log))[0]["stem"] == "T_LOOP_A", "digest_run_log.json was written during the run")
check("NO CARD" in lines and "- T_LOOP_A: NO CARD (see output)" in lines, "a batch with no card still says so")
check(lines.count("## Batch") == 2, "both batches appear in the report")

print("\n%s" % ("ALL PASS" if not fails else "FAILURES:\n  " + "\n  ".join(fails)))
sys.exit(1 if fails else 0)
