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

print()
if bad:
    print("%d FAILED:" % len(bad))
    for label, problems in bad:
        print("  %-48s %s" % (label, "; ".join(problems)))
    raise SystemExit(1)
print("release() behaves in every case, including the interleaving.")
