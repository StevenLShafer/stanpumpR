"""Run digest_run.py's REAL queue over a fake archive. python test_digest_queue.py

CodeRabbit asked four times, on PR #846, for the queue itself to be executed against a stale
non-scan manifest -- not its call site inspected, not the decision function tested on its own --
and was right to keep asking: those cover different things, and a card can become public.

The obstacle was that importing digest_run.py starts an unattended digest run. The way round it
is a side-effect-controlled sandbox, which CodeRabbit suggested:

  * a temporary tree whose `_build` directory holds copies of digest_run.py and inreview.py,
    and STUBS for cards.py and machine.py -- the sandbox's own `_build` is first on sys.path,
    so `import cards` finds the stub rather than the real module, which drives git against the
    stanpumpR repository;
  * the master snapshot pre-seeded with the stub's sha, so the run skips its `git archive`;
  * `--dry`, which builds and prints the queue and exits before any model is called.

Every manifest entry below says `scan: False` -- the case under test. digest_prep.py's return
code is unchecked, so a failed preparation can leave a stale non-scan entry on a paper whose
text is missing, unreadable or blank, and the queue must refuse it anyway.

THE CHECK REFUSES TO PASS VACUOUSLY. The first version of this harness reported every paper as
correctly refused when the run had in fact crashed before printing a queue at all; only the one
paper that SHOULD have been queued exposed it. So the queue line must be found, or nothing below
it counts as evidence.
"""
import io
import json
import os
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))

PAPERS = [
    # stem,      extracted text ("DIRECTORY" = unreadable, None = no file),  must be refused
    ("PMID_101", None, True),
    ("PMID_102", "DIRECTORY", True),
    ("PMID_103", "   \n \t ", True),
    ("PMID_104", "\f\f", True),
    ("PMID_105", "", True),
    ("PMID_106", "Tracked Changes Manuscript", True),
    ("PMID_107", "Propofol pharmacokinetics in 24 volunteers.", False),
]

CARDS_STUB = '''"""A stub. The real cards.py drives git against the stanpumpR repository."""
import os
HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
CARDS = os.path.join(ARCH, "cards")
REPO = os.path.join(ARCH, "repo")
os.makedirs(CARDS, exist_ok=True)
os.makedirs(REPO, exist_ok=True)


def sync(say=None):
    return "0" * 40


def unmerged_cards():
    return {}


def unmerged_branches():
    return []


def master_cards():
    return set()


def card_author(path):
    return None, False


def commit_and_push(*a, **k):
    raise AssertionError("a --dry run must never push")
'''

# NOT STUBBED, and worth recording why: digest_run.py reads an OAuth token from the Windows
# registry inside a try/except OSError, and ModuleNotFoundError is not an OSError -- so off
# Windows the module could not be imported at all, and this test passed here and failed in CI
# (fda-labels, PR #846). A winreg.py placed in the sandbox does NOT reproduce that, because
# winreg is a BUILT-IN module on Windows and built-ins are resolved before sys.path, so the
# shadow is never used. The fix (catching ImportError) was verified locally with a sys.meta_path
# blocker, and this test's own evidence for it is the CI run on Linux. Leaving a stub here that
# does nothing on the machine the pipeline runs on would be worse than leaving it out.

MACHINE_STUB = '''import os
LOCAL = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "local")
os.makedirs(LOCAL, exist_ok=True)
'''


def build(tmp):
    build_dir = os.path.join(tmp, "_build")
    text_dir = os.path.join(build_dir, "text")
    ctx_dir = os.path.join(build_dir, "context")
    os.makedirs(text_dir)
    os.makedirs(ctx_dir)
    for f in ("digest_run.py", "inreview.py"):
        shutil.copy2(os.path.join(HERE, f), os.path.join(build_dir, f))
    io.open(os.path.join(build_dir, "cards.py"), "w", encoding="utf-8").write(CARDS_STUB)
    io.open(os.path.join(build_dir, "machine.py"), "w", encoding="utf-8").write(MACHINE_STUB)

    manifest = []
    for stem, body, _refuse in PAPERS:
        io.open(os.path.join(tmp, stem + ".pdf"), "wb").write(b"%PDF-1.4 a fake paper")
        if body == "DIRECTORY":
            os.makedirs(os.path.join(text_dir, stem + ".txt"))   # cannot be read
        elif body is not None:
            io.open(os.path.join(text_dir, stem + ".txt"), "w", encoding="utf-8").write(body)
        # tier() reads a context file; no `locations` means tier 4, the ordinary case
        io.open(os.path.join(ctx_dir, stem + ".json"), "w", encoding="utf-8").write(
            json.dumps({"locations": []}))
        # scan: False on EVERY entry -- the stale non-scan manifest under test
        manifest.append({"stem": stem, "digest": "missing", "scan": False, "pages": 8,
                         "edition": "published", "tier": 4})
    io.open(os.path.join(build_dir, "digest_manifest.json"), "w", encoding="utf-8").write(
        json.dumps(manifest, indent=1))

    snap = os.path.join(tmp, "local", "master_snapshot")
    os.makedirs(snap, exist_ok=True)
    io.open(os.path.join(snap, ".sha"), "w", encoding="utf-8").write("0" * 40)
    return build_dir


def main():
    with tempfile.TemporaryDirectory() as tmp:
        build_dir = build(tmp)
        r = subprocess.run([sys.executable, "-E", os.path.join(build_dir, "digest_run.py"),
                            "--dry", "--cap", "10"],
                           capture_output=True, text=True, cwd=tmp)
        out = r.stdout + r.stderr

        queued = next((l for l in out.splitlines() if l.startswith("queue (")), None)
        if queued is None:
            print("THE RUN NEVER PRINTED A QUEUE, so nothing here is evidence of anything.")
            print(out[-2000:])
            return 1

        print("  %s" % queued[:150])
        bad = []
        for stem, _body, refuse in PAPERS:
            in_queue = stem in queued
            ok = in_queue != refuse
            if not ok:
                bad.append("%s was %s" % (stem, "queued but must be refused" if in_queue
                                          else "refused but should have been queued"))
            print("  %s %-10s %s" % ("ok  " if ok else "FAIL", stem,
                                     "queued" if in_queue else "not queued"))

        # the refusals must say WHY, not merely drop the paper
        for stem, _body, refuse in PAPERS:
            if refuse and stem not in out:
                bad.append("%s was dropped without a reason in the report" % stem)

        print()
        if bad:
            print("FAILED: " + "; ".join(bad))
            print(out[-1500:])
            return 1
        print("The real queue refuses every paper whose text is missing, unreadable or blank,")
        print("and queues the one that is readable -- with every manifest entry saying scan: False.")
        return 0


if __name__ == "__main__":
    sys.exit(main())
