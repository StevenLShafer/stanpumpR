"""Test the real run_claude()/Job text out of _build\\digest_run.py, with a fake claude.

The helpers are exec'd straight from the file (digest_run.py is a script, so it cannot be imported), which
means these cases test the shipped code, not a copy of it.
"""
import io, os, re, subprocess, sys, tempfile, time

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(os.path.dirname(HERE), "digest_run.py")   # the shipped script, beside us
PY = sys.executable
FAKE = os.path.join(HERE, "fake_claude.py")
WORK = tempfile.mkdtemp(prefix="digest_run_test_")   # never write into the archive
CARDS = os.path.join(WORK, "cards")
os.makedirs(CARDS, exist_ok=True)

text = io.open(SRC, encoding="utf-8").read()
start = text.index("class Job:")
end = text.index("\n# 0. hold:")
ns = {"os": os, "subprocess": subprocess, "time": time, "sys": sys}
exec(compile(text[start:end], SRC, "exec"), ns)          # class Job, read_text, run_claude, verbatim
run_claude, Job = ns["run_claude"], ns["Job"]
print("exec'd %d lines of helpers from digest_run.py" % text[start:end].count("\n"))


def alive(pid):
    out = subprocess.run(["tasklist", "/FI", "PID eq %d" % pid, "/NH"], capture_output=True, text=True).stdout
    return str(pid) in out


def case(name, mode, grandchild_secs, timeout, stems):
    scratch = os.path.join(WORK, "scratch-" + name)
    os.makedirs(scratch, exist_ok=True)
    for s in stems:
        if os.path.exists(os.path.join(CARDS, s + ".md")):
            os.remove(os.path.join(CARDS, s + ".md"))
    gpid = os.path.join(WORK, "grandchild.pid")
    if os.path.exists(gpid):
        os.remove(gpid)
    cmd = [PY, "-E", FAKE, mode, str(grandchild_secs), CARDS] + stems
    ticks = []
    t0 = time.time()
    out, code = run_claude(cmd, "PROMPT LINE 1\n" + "x" * 5000, scratch, timeout,
                           tick=lambda: ticks.append(time.time() - t0), poll=1.0)
    secs = time.time() - t0
    g = int(open(gpid).read()) if os.path.exists(gpid) else None
    time.sleep(1.0)                                       # let a kill settle before asking
    return {"name": name, "secs": secs, "code": code, "out": out, "ticks": len(ticks),
            "grandchild": g, "grandchild_alive": alive(g) if g else None,
            "cards": [s for s in stems if os.path.exists(os.path.join(CARDS, s + ".md"))]}


fails = []


def check(ok, msg):
    print(("  PASS  " if ok else "  FAIL  ") + msg)
    if not ok:
        fails.append(msg)


# 1. the bug itself: the child exits, a helper keeps the inherited handles for 40 s.  The old code waited for
#    EOF, so it returned only after 40 s -- and with a helper that never exits, never.
print("\ncase 1: child exits at ~1.5 s, helper holds the handles 40 s, deadline 25 s")
r = case("orphan", "exit", 40, 25, ["T_ORPHAN_A", "T_ORPHAN_B"])
print("   -> %.1f s, exit %r, %d ticks, cards %s" % (r["secs"], r["code"], r["ticks"], r["cards"]))
check(r["secs"] < 10, "returned in %.1f s, not after the helper's 40 s (the old code: 40.1 s)" % r["secs"])
check(r["code"] == 0, "exit code 0, not 'timeout'")
check(r["cards"] == ["T_ORPHAN_A", "T_ORPHAN_B"], "both cards present")
check("fake claude: mode=exit" in r["out"], "stdout captured from the file")
check("prompt 5014 chars" in r["out"], "the whole prompt reached the child on stdin")

# 2. a child that never returns: the deadline must end it, and the tree with it
print("\ncase 2: child hangs for ever (helper too), deadline 12 s")
r = case("hang", "hang", 100000, 12, ["T_HANG_A"])
print("   -> %.1f s, exit %r, %d ticks, cards %s, helper pid %s alive after: %s"
      % (r["secs"], r["code"], r["ticks"], r["cards"], r["grandchild"], r["grandchild_alive"]))
check(10 <= r["secs"] < 25, "ended at the deadline (%.1f s), not for ever" % r["secs"])
check(r["code"] == "timeout", "exit recorded as 'timeout'")
check(r["grandchild_alive"] is False, "the helper was killed too: the whole tree is gone")
check(r["cards"] == ["T_HANG_A"], "a card written before the kill is kept")
check(r["ticks"] >= 5, "tick() ran during the batch (%d times), so a card is logged as it appears" % r["ticks"])
check("timed out after 0 min" in r["out"] and "fake claude: mode=hang" in r["out"],
      "the timeout output keeps what the child had printed")

# 3. a normal, prompt exit
print("\ncase 3: child exits at once, no helper left")
r = case("clean", "exit", 0.1, 60, ["T_CLEAN_A"])
print("   -> %.1f s, exit %r, cards %s" % (r["secs"], r["code"], r["cards"]))
check(r["secs"] < 10 and r["code"] == 0, "returned in %.1f s with exit 0" % r["secs"])

# 4. the job object is real on this machine (else the fallback is taskkill /T /F)
j = Job()
print("\ncase 4: job object available: %s" % bool(j.h))
check(bool(j.h), "a job object could be created, so a timeout ends orphans too")
j.close()

print("\n%s" % ("ALL PASS" if not fails else "FAILURES:\n  " + "\n  ".join(fails)))
sys.exit(1 if fails else 0)
