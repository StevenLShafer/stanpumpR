"""What release() must do when a backup fails. Run it: python test_bundle_repo.py

release() undoes a failed backup, and the ORDER it does it in was a real concurrency race
(CodeRabbit, PR #847): releasing the bundle reservation before the sidecar let another run claim
the freed name, write its own sidecar and publish -- whereupon this cleanup, still walking its
list, saw a zero-byte reservation at that name (the other run's) and deleted the other run's
sidecar, leaving it to publish a 276 MB bundle with no metadata. A bundle without its sidecar
cannot be checked by --verify and is counted by the age guard as that week's backup.

The cases are here, in the repository, because the fixes on PRs #846 and #847 were cited in four
commit messages while living only in a session's scratchpad, where they guaranteed nothing.
"""
import io
import os
import shutil
import sys
import tempfile

BUILD = os.path.dirname(os.path.abspath(__file__))
sys.argv = ["bundle_repo.py", "--list"]          # exits before any backup work is done

# exec into a namespace rather than import: bundle_repo.py is a script that calls sys.exit()
# for --list, and a module that raises during import is dropped from sys.modules, so the
# definitions would be lost. A namespace keeps everything defined before the exit. (The same
# obstacle is why the digest queue's own refusal had to move into inreview.py to be testable.)
_src = os.path.join(BUILD, "bundle_repo.py")
_ns = {"__name__": "bundle_repo_under_test", "__file__": _src}
try:
    exec(compile(io.open(_src, encoding="utf-8").read(), _src, "exec"), _ns)
except SystemExit:
    pass
release = _ns["release"]
prune = _ns["prune"]
take_lock = _ns["take_lock"]
drop_lock = _ns["drop_lock"]

bad = []


def touch(p, data=b""):
    io.open(p, "wb").write(data)


def case(label, setup, want_gone, want_kept, hook=None):
    """Run release() over a scratch directory and check what survived.

    `hook` runs after each removal, so a competing run can be interleaved at the exact moment
    this one releases a name -- which is the only way the race can be exercised.
    """
    with tempfile.TemporaryDirectory() as d:
        q = {k: os.path.join(d, v) for k, v in
             (("tmp", "b.bundle.1.part"), ("side_tmp", "b.json.1.part"),
              ("path", "b.bundle"), ("side", "b.json"))}
        setup(q)
        real, order = os.remove, []

        def watched(p):
            order.append(os.path.basename(p))
            real(p)
            if hook:
                hook(q, os.path.basename(p))

        os.remove = watched
        try:
            release(q["tmp"], q["side_tmp"], q["path"], q["side"])
        finally:
            os.remove = real

        problems = [("%s should be gone" % k) for k in want_gone if os.path.exists(q[k])]
        problems += [("%s should have been kept" % k) for k in want_kept
                     if not os.path.exists(q[k])]
        if problems:
            bad.append((label, problems))
        print("  %s %-52s removed: %s" % ("ok  " if not problems else "FAIL", label[:52],
                                          ", ".join(order) or "nothing"))
        return order


def ordinary(q):
    touch(q["tmp"], b"a partial bundle")
    touch(q["side_tmp"], b"{}")
    touch(q["path"])                             # the reservation: a ZERO-BYTE file
    touch(q["side"], b"{}")


order = case("owns the reservation: all four go", ordinary,
             ["tmp", "side_tmp", "path", "side"], [])
if order and "b.json" in order and "b.bundle" in order:
    if order.index("b.json") > order.index("b.bundle"):
        bad.append(("ordering", ["the reservation was released before the sidecar"]))
        print("  FAIL the reservation was released BEFORE the sidecar: the race is open")
    else:
        print("  ok   the sidecar was released before the reservation")


def claimed_mid_release(q, just_removed):
    if just_removed == "b.bundle":               # our reservation has just been released
        touch(q["path"])                         # another run claims the name at once
        touch(q["side"], b'{"master": "the other run\'s"}')


case("another run claims the name as we release it",
     lambda q: (touch(q["tmp"], b"partial"), touch(q["path"]), touch(q["side"], b"{}")),
     [], ["path", "side"], hook=claimed_mid_release)

case("a finished generation is never touched",
     lambda q: (touch(q["tmp"], b"partial"),
                touch(q["path"], b"a real bundle, not a reservation"),
                touch(q["side"], b"{}")),
     ["tmp"], ["path", "side"])

case("nothing to release", lambda q: None, [], [])

# ---------------------------------------------------------------------- pruning
# prune() removes the BUNDLE first. Before the lock existed the sidecar went first, because
# freeing the bundle name let another run claim it; under the lock no other run can be here, and
# the remaining risk is an INTERRUPTION between the two removals. An orphaned .json is ignored
# by existing(); an orphaned BUNDLE would be counted as a generation that --verify cannot check,
# since its commit lives only in the sidecar (CodeRabbit, PR #847).
def prune_cases():
    import datetime
    problems = []

    def generations(d, n):
        rows = []
        for i in range(n):
            stamp = "2026-01-%02d" % (i + 1)
            bundle = os.path.join(d, "stanpumpR-%s.bundle" % stamp)
            io.open(bundle, "wb").write(b"a bundle")
            io.open(os.path.join(d, "stanpumpR-%s.json" % stamp), "w",
                    encoding="utf-8").write("{}")
            rows.append((datetime.datetime.fromisoformat(stamp), bundle))
        return sorted(rows, reverse=True)       # newest first, as existing() returns

    with tempfile.TemporaryDirectory() as d:
        rows = generations(d, 4)
        removed = prune(rows, 2)
        left = sorted(f for f in os.listdir(d))
        ok = len(removed) == 2 and len(left) == 4
        if not ok:
            problems.append("keeping 2 of 4 left %s" % left)
        print("  %s prune: %-45s %s" % ("ok  " if ok else "FAIL",
                                        "keeps the newest 2 of 4, with their sidecars",
                                        ", ".join(removed)))

    # interrupted between the two removals: the bundle is gone, its sidecar is not
    with tempfile.TemporaryDirectory() as d:
        rows = generations(d, 2)
        real, calls = os.remove, []

        def once(p):
            calls.append(p)
            real(p)
            if p.endswith(".bundle"):
                raise KeyboardInterrupt("interrupted after the bundle was removed")

        os.remove = once
        try:
            prune(rows, 1)
        except KeyboardInterrupt:
            pass
        finally:
            os.remove = real
        left = sorted(os.listdir(d))
        orphan_bundle = [f for f in left if f.endswith(".bundle")
                         and f[: -len(".bundle")] + ".json" not in left]
        ok = not orphan_bundle
        if not ok:
            problems.append("an orphaned bundle survived: %s" % orphan_bundle)
        print("  %s prune: %-45s %s" % ("ok  " if ok else "FAIL",
                                        "an interruption leaves no bundle without a sidecar",
                                        ", ".join(left)))

    # a bundle that cannot be removed keeps its sidecar: the generation is still there
    with tempfile.TemporaryDirectory() as d:
        rows = generations(d, 2)
        real = os.remove

        def refuse(p):
            if p.endswith(".bundle"):
                raise PermissionError(13, "in use", p)
            real(p)

        os.remove = refuse
        try:
            removed = prune(rows, 1)
        finally:
            os.remove = real
        left = sorted(os.listdir(d))
        ok = not removed and len(left) == 4
        if not ok:
            problems.append("a bundle that could not be removed lost its sidecar: %s" % left)
        print("  %s prune: %-45s %s" % ("ok  " if ok else "FAIL",
                                        "a bundle it cannot remove keeps its sidecar",
                                        ", ".join(left)))
    return problems


bad += [("prune", p) for p in [prune_cases()] if p]

# ---------------------------------------------------------------------- the lock
# The lock is the KERNEL'S now, not a file whose contents are inspected: take_lock() holds an
# exclusive byte lock for the life of the process. So there is no stale lock and no recovery
# path, and the two races CodeRabbit found in the old scheme -- a live lock deleted after a
# pause, and two runs both judging a lock stale -- are not expressible (PR #847).
#
# That has to be tested with REAL PROCESSES. Sequential calls in one process prove nothing: the
# same process can re-lock its own file on every platform.
import json
import socket
import subprocess
import sys
import time

CHILD = """
import os, sys, time
# read BEFORE sys.argv is replaced for the exec
build, seconds, lock = sys.argv[1], float(sys.argv[2]), sys.argv[3]
sys.path.insert(0, build)
src = os.path.join(build, "bundle_repo.py")
sys.argv = ["bundle_repo.py", "--list"]
_ns = {"__name__": "child", "__file__": src}
# Hide the module's own --list output at the FILE DESCRIPTOR level: it calls
# sys.stdout.reconfigure(), which a StringIO does not have.
_null = os.open(os.devnull, os.O_WRONLY)
_saved = os.dup(1)
sys.stdout.flush()               # buffered text would otherwise reach the real stdout later
os.dup2(_null, 1)
try:
    exec(compile(open(src, encoding="utf-8").read(), src, "exec"), _ns)
except SystemExit:
    pass
finally:
    sys.stdout.flush()           # flush INTO devnull, then restore
    os.dup2(_saved, 1)
    os.close(_saved)
    os.close(_null)
# The TEMPORARY lock, never the archive's own: these tests overwrite the note, and the real
# one may carry a recent foreign-host note that the advisory check depends on (CodeRabbit,
# PR #847). Overriding the module's globals is enough -- take_lock() reads them from here.
_ns["LOCK"] = lock
_ns["OUT"] = os.path.dirname(lock)
busy = _ns["take_lock"]()
print("REFUSED" if busy else "ACQUIRED", flush=True)
if busy:
    sys.exit(2)
time.sleep(seconds)
sys.exit(0)
"""


def lock_cases():
    problems = []
    build = os.path.dirname(_ns["__file__"])

    # Never touch the archive's own lock note. These cases write foreign-host notes and
    # unparseable ones, and the real file may hold a recent note from another computer that the
    # advisory check depends on -- erasing it would defeat exactly the protection under test
    # (CodeRabbit, PR #847). The module's LOCK and OUT are globals, so overriding them here
    # redirects take_lock() and drop_lock() for the duration.
    sandbox = tempfile.mkdtemp()
    real_lock, real_out = _ns["LOCK"], _ns["OUT"]
    _ns["LOCK"] = os.path.join(sandbox, "bundle_repo.lock")
    _ns["OUT"] = sandbox
    try:
        return _lock_cases(build, _ns["LOCK"])
    finally:
        _ns["LOCK"], _ns["OUT"] = real_lock, real_out
        shutil.rmtree(sandbox, ignore_errors=True)


def _lock_cases(build, lock):
    problems = []
    # IN THE SANDBOX, beside the temporary lock. A fixed path was opened "w" and then
    # removed, so it could overwrite and delete a file of that name that was already there,
    # and two test runs would collide (CodeRabbit, PR #847). The sandbox's own finally
    # removes the whole directory, so this needs no cleanup of its own.
    child_py = os.path.join(os.path.dirname(lock), "lock_child.py")
    try:
        io.open(child_py, "w", encoding="utf-8").write(CHILD)
    except OSError:
        print("  skip lock: cannot write a child script beside the archive")
        return []

    def child(seconds, wait_for_line=True):
        p = subprocess.Popen([sys.executable, "-E", child_py, build, str(seconds), lock],
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        first = p.stdout.readline().strip() if wait_for_line else ""
        return p, first

    def report(label, ok, detail=""):
        if not ok:
            problems.append("%s: %s" % (label, detail or "failed"))
        print("  %s lock: %-46s %s" % ("ok  " if ok else "FAIL", label[:46], detail[:36]))

    # 1. a second process is refused while the first holds it
    holder, line = child(20)
    try:
        report("a child acquires it when free", line == "ACQUIRED", line)
        other, line2 = child(1)
        other.wait(timeout=30)
        report("a second process is REFUSED while held", line2 == "REFUSED", line2)
    finally:
        holder.kill()
        holder.wait()

    # 2. the kernel releases it when the holder DIES -- the case no grace period can get right
    time.sleep(0.3)
    after, line3 = child(1)
    after.wait(timeout=30)
    report("granted immediately after the holder is killed", line3 == "ACQUIRED", line3)

    # 3. this process can take and release it, and the FILE is never removed
    busy = take_lock()
    took = not busy
    report("this process takes it", took, busy or "took it")
    lock_path = lock
    drop_lock()
    report("the lock file is left in place after release", os.path.exists(lock_path),
           "present" if os.path.exists(lock_path) else "GONE")

    # 4. an unreadable note does not stop acquisition: the note is advisory, the lock is not
    io.open(lock_path, "w", encoding="utf-8").write("not json")
    busy = take_lock()
    report("an unreadable note does not block the lock", not busy, busy or "took it")
    drop_lock()

    # 5. another computer's recent note makes this run stand down (advisory, and labelled so)
    io.open(lock_path, "w", encoding="utf-8").write(
        json.dumps({"host": socket.gethostname() + "-other", "pid": 1, "started": "now"}))
    busy = take_lock()
    report("another computer's recent note stands us down", bool(busy), busy or "took it")
    drop_lock()

    # 6. ... and an old one does not
    old_t = time.time() - (_ns["LOCK_FOREIGN_HOURS"] + 1) * 3600
    os.utime(lock_path, (old_t, old_t))
    busy = take_lock()
    report("another computer's OLD note is ignored", not busy, busy or "took it")
    drop_lock()

    return problems


bad += [("lock", p) for p in [lock_cases()] if p]

print()
if bad:
    print("%d FAILED:" % len(bad))
    for label, problems in bad:
        print("  %-48s %s" % (label, "; ".join(problems)))
    raise SystemExit(1)
print("release() behaves in every case, including the interleaving.")
