"""Say whether the archive's running scripts and the repository's copy of them still agree.

The scripts in this folder are the VERSION-CONTROLLED RECORD of the archive's pipeline; the
copies that actually run live beside the papers, in the archive's own `_build` folder, because
most of them find the archive as the parent of their own directory.  So there are two copies,
and a mirror nobody checks is worse than no mirror: it looks like history while quietly
describing something else.

This is the check.  `update.bat` runs it last, so every filing run says whether the two have
drifted apart, naming the files.  It only ever reports -- it changes nothing and always exits
0, so it can never break the chain it is appended to.

The repository is found at `--repo` or C:\\dev\\stanpumpR; a machine without that checkout
simply says so and stops.  Run it by hand from either copy:

    python check_in_step.py [--repo <path>] [--archive <path>]
"""
import os
import sys

args = sys.argv[1:]
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = args[args.index("--repo") + 1] if "--repo" in args else r"C:\dev\stanpumpR"
# The archive's _build is this folder when the archive copy runs it, and the archive proper
# when the repository's copy does; `--archive` settles it on any other machine.
ARCHIVE = (args[args.index("--archive") + 1] if "--archive" in args
           else os.path.dirname(HERE) if os.path.basename(os.path.dirname(HERE)) != "papers"
           else r"D:\My Drive\stanpumpRPapers")

MIRROR = os.path.join(REPO, "papers", "_build")
ROOT_FILES = ("update.bat", "digests.bat")      # these two live in the archive's root


def archive_path(name):
    return os.path.join(ARCHIVE, name) if name in ROOT_FILES else os.path.join(ARCHIVE, "_build", name)


def content(path):
    """The file's bytes with line endings normalised.

    .gitattributes marks this folder `-text` so a checkout matches the archive byte for byte,
    but a machine whose Git is configured otherwise would otherwise see all thirty files as
    drifted -- a check that cries wolf is one nobody reads.  What matters is whether the CODE
    differs, so compare the content and let the endings be."""
    with open(path, "rb") as f:
        return f.read().replace(b"\r\n", b"\n")


def main():
    if not os.path.isdir(MIRROR):
        print("in-step check: no repository copy at %s -- skipped" % MIRROR)
        return 0
    # This script is compared like any other: it lives in both copies, so excluding it from
    # one list and not the other made it report ITSELF as uncommitted on the first run.
    names = sorted(f for f in os.listdir(MIRROR) if f.endswith((".py", ".sh", ".bat")))
    missing, differs = [], []
    for n in names:
        src = archive_path(n)
        if not os.path.exists(src):
            missing.append(n)                   # in the repository, gone from the archive
        elif content(os.path.join(MIRROR, n)) != content(src):
            differs.append(n)
    # The other direction: a script the archive has gained that was never committed, which is
    # how a mirror silently stops being a record of what runs.
    adir = os.path.join(ARCHIVE, "_build")
    uncommitted = sorted(f for f in (os.listdir(adir) if os.path.isdir(adir) else [])
                         if f.endswith((".py", ".sh")) and f not in names)

    if not (missing or differs or uncommitted):
        print("in-step check: the archive's %d scripts match the repository's copy." % len(names))
        return 0
    print("\nin-step check: the archive and the repository have DRIFTED APART.")
    print("  The archive's copy is the one that runs; the repository's is the record.")
    for n in differs:
        print("  differs:      %s" % n)
    for n in uncommitted:
        print("  not in git:   %s   (the archive has it; the repository does not)" % n)
    for n in missing:
        print("  not in the archive: %s   (committed, but not where it runs)" % n)
    print("  To bring the record up to date, copy the archive's files into %s" % MIRROR)
    print("  and commit them, saying in the message what changed and why.")
    return 0                                     # never break update.bat's chain


if __name__ == "__main__":
    sys.exit(main())
