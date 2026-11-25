"""Say whether the archive's running scripts and the repository's RECORD of them still agree.

The scripts in this folder are the VERSION-CONTROLLED RECORD of the archive's pipeline; the
copies that actually run live beside the papers, in the archive's own `_build` folder, because
most of them find the archive as the parent of their own directory.  So there are two copies,
and a mirror nobody checks is worse than no mirror: it looks like history while quietly
describing something else.

This is the check.  `update.bat` runs it last, so every filing run says whether the two have
drifted apart, naming the files.  It only ever reports -- it changes nothing in either copy and
always exits 0, so it can never break the chain it is appended to.

WHAT IT COMPARES THE ARCHIVE WITH, and why that matters.  Until 2026-11-19 it read the WORKING
TREE of the checkout at `--repo`, whatever branch that checkout happened to be on.  Several
checkouts and worktrees live on this machine and any of them may be on a branch (AGENTS.md,
*Several sessions run at once*), so on 2026-11-19 it reported build.py, fetch_pmc.py,
file_downloads.py and update.bat as drifted when all four matched `origin/master` byte for
byte: `C:\\dev\\stanpumpR` was simply on a branch that predated the commit which changed them.
Worse than the false alarm was its advice -- copy the archive's files into that checkout and
commit them -- which would have committed onto another session's branch, the very collision
AGENTS.md rule 1 exists to prevent.

So the record it compares with is the COMMITTED one: it fetches `master` and reads the files
out of `origin/master` with `git cat-file`, resolving the ref to a commit ONCE so that a fetch
by another session cannot move the subject of the comparison halfway through.  The working tree
is read only when git cannot supply the record at all.  EVERY message names the ref or path it
compared against, because a check whose subject you cannot see is one you cannot trust -- and
when the fetch failed, or the working tree stood in, the message says so on a line of its own,
since either can turn real agreement into an apparent difference.

The repository is found at `--repo` or C:\\dev\\stanpumpR; a machine without that checkout
simply says so and stops.  Run it by hand from either copy:

    python check_in_step.py [--repo <path>] [--archive <path>]
"""
import collections
import os
import subprocess
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
IN_GIT = "papers/_build"                        # git spells its own paths with forward slashes
BRANCH = "master"                               # the only integration branch (AGENTS.md)
ROOT_FILES = ("update.bat", "digests.bat")      # these two live in the archive's root
SUFFIXES = (".py", ".sh", ".bat")
SUB_SUFFIXES = SUFFIXES + (".txt", ".md")
SUB_FOLDERS = ("test_digest_run",)
# A SUBFOLDER of `_build` is part of the record only if it is NAMED here (2026-11-25).
# `test_digest_run`, the tests for digest_run.py's deadline, logging and queue order, had never
# been committed or compared, because this check and the mirror both used a flat os.listdir --
# the gap papers/_build/README.md says the mirror exists to close, reopened one directory down.
#
# The first attempt at this took ANY subfolder whose files had a recorded suffix, and that is
# why the list is explicit instead.  `_build/text` holds `pdftotext` output for every paper in
# the archive and `_build/context` a JSON file each: the run of this check that found them
# listed 2,000-odd extractions as "not in git" and advised committing them, which would have
# put copyrighted paper text into a public repository -- the one thing papers/README.md says
# must never happen, and the reason the digests can be published at all.  A named folder cannot
# do that; a suffix rule was one new directory away from doing it again.
#
# A named subfolder takes `.txt` and `.md` too, since a test folder's README records what its
# tests slice out of the shipped code and is useless apart from them.  The TOP level does not:
# there `.txt` is generated output (unplaced_downloads.txt, branches-awaiting-pr.txt) and no
# part of any record.  Deeper than one level is not walked, and a stated limit beats a
# recursion whose reach nobody knows.
FETCH_SECONDS = 60                              # a stalled fetch must not hold up a filing run

# What the archive was compared with.  `label` names it in every message; `notes` are facts
# about it that every message carries (a fetch that failed, a working tree standing in), and
# `caveats` the consequence, said only where something was actually found to differ.
Record = collections.namedtuple("Record", "label notes caveats names read")


def in_record(name):
    """Whether a path (relative to `_build`, git-style slashes) belongs to the record."""
    if "/" in name:
        head, tail = name.split("/", 1)
        return head in SUB_FOLDERS and "/" not in tail and tail.endswith(SUB_SUFFIXES)
    return name.endswith(SUFFIXES)


def archive_path(name):
    """Where `name` lives in the archive.  Git spells its paths with '/', Windows with '\'."""
    parts = name.split("/")
    if name in ROOT_FILES:
        return os.path.join(ARCHIVE, *parts)
    return os.path.join(ARCHIVE, "_build", *parts)


def one_level(root):
    """Paths under `root`, git-style, the folder itself and one level of subfolder.

    The same enumeration the committed record gets from `ls-tree -r`, so the two directions of
    the comparison cannot disagree about what counts.  `desktop.ini` and the rest fall out
    through in_record(), which is also what keeps a Drive artifact from reading as a script."""
    if not os.path.isdir(root):
        return []
    out = []
    for entry in sorted(os.listdir(root)):
        full = os.path.join(root, entry)
        if os.path.isdir(full):
            if entry not in SUB_FOLDERS:        # never even listed: _build/text has thousands
                continue
            out += ["%s/%s" % (entry, f) for f in sorted(os.listdir(full))
                    if os.path.isfile(os.path.join(full, f))]
        else:
            out.append(entry)
    return [n for n in out if in_record(n)]


def normalised(data):
    """The bytes with line endings normalised.

    .gitattributes marks this folder `-text` so a checkout matches the archive byte for byte,
    but a machine whose Git is configured otherwise would otherwise see all thirty files as
    drifted -- a check that cries wolf is one nobody reads.  What matters is whether the CODE
    differs, so compare the content and let the endings be."""
    return data.replace(b"\r\n", b"\n")


def file_bytes(path):
    with open(path, "rb") as handle:
        return handle.read()


def git(*argv, **kw):
    """Run git in REPO and return (ok, output).  Never raises: a git that cannot be run, a
    directory that is not a repository and a ref that does not exist are all answers this
    check reports."""
    timeout = kw.get("timeout")
    try:
        done = subprocess.run(("git", "-C", REPO) + argv, capture_output=True, timeout=timeout)
    except OSError as exc:                      # git itself could not be run on this machine
        return False, "git could not be run: %s" % exc
    except subprocess.TimeoutExpired:
        # run() kills the child before re-raising; a transport helper that child spawned can
        # linger a moment, so this SHORTENS a stalled fetch rather than bounding it exactly.
        return False, "git %s took longer than %d seconds and was killed" % (argv[0], timeout)
    if done.returncode != 0:
        return False, (done.stderr.decode("utf-8", "replace").strip()
                       or "git %s exited %d" % (argv[0], done.returncode))
    return True, done.stdout


def text(out):
    """git's output as text.  Kept whole, newlines and all: `ls-tree`'s is a list of names."""
    return out.decode("utf-8", "replace").strip() if isinstance(out, bytes) else str(out).strip()


def oneline(out):
    """A reason, on ONE line.  git's errors run to several ("fatal: ...", then "Please make
    sure you have the correct access rights"), and a reason printed inside an indented report
    has to stay on the line it was indented onto.  Never use this where the output is a list."""
    return " ".join(text(out).split()).rstrip(".")


def committed_record():
    """The record as master has it, or (None, why it could not be had)."""
    ok, why = git("--version")
    if not ok:
        # Said as what it is.  A git that cannot be run establishes nothing about the checkout,
        # and calling that "not a repository" would be this pipeline's own favourite mistake:
        # reporting a failure to look as a fact about the thing left unlooked at.
        return None, oneline(why)
    ok, why = git("rev-parse", "--git-dir")
    if not ok:
        return None, "%s is not a git repository (%s)" % (REPO, oneline(why))

    fetched, fetch_why = git("fetch", "origin", BRANCH, timeout=FETCH_SECONDS)
    # Resolving the ref to a commit NOW, and reading every file out of that one commit, is what
    # stops a fetch by another session from changing what is being compared mid-run.
    ok, out = git("rev-parse", "origin/" + BRANCH)
    if not ok and fetched:
        ok, out = git("rev-parse", "FETCH_HEAD")   # a clone whose refspec has no origin/master
    if not ok:
        return None, ("cannot resolve origin/%s in %s (%s)%s"
                      % (BRANCH, REPO, oneline(out),
                         "" if fetched else "; the fetch failed too: " + oneline(fetch_why)))
    sha = text(out)

    label = "origin/%s (%s)" % (BRANCH, sha[:8])
    notes, caveats = [], []
    if not fetched:
        # Said on every run that cannot fetch, so a stale ref never passes for a current one.
        notes.append("origin/%s COULD NOT BE FETCHED, so it may be out of date -- %s."
                     % (BRANCH, oneline(fetch_why)))
        caveats.append("Because the fetch failed, a difference above may be a change that has"
                       " already been committed and merged.")
    # True of the committed record and only of it: in the fallback below a change committed on
    # a branch is exactly what the working tree WOULD show, so the sentence belongs here.
    caveats.append("A change already committed on a branch shows here until it is merged"
                   " into %s." % BRANCH)

    ok, out = git("ls-tree", "-r", "--name-only", "%s:%s" % (sha, IN_GIT))
    if not ok:
        return None, "%s has no %s (%s)" % (label, IN_GIT, oneline(out))
    names = sorted(n for n in (x.strip() for x in text(out).splitlines()) if in_record(n))

    def read(name):
        got, blob = git("cat-file", "blob", "%s:%s/%s" % (sha, IN_GIT, name))
        return blob if got else None

    return Record(label, notes, caveats, names, read), None


def working_tree_record(reason):
    """The fallback, used only when git cannot supply the committed record."""
    if not os.path.isdir(MIRROR):
        return None, "%s, and there is no working copy at %s either" % (reason, MIRROR)
    label = "the working tree at " + MIRROR
    ok, out = git("rev-parse", "--abbrev-ref", "HEAD")
    if ok:
        on = text(out)
        label += " (on %s)" % ("a detached HEAD" if on == "HEAD" else "branch " + on)
    notes = ["Read from the working tree, not from %s, because %s." % (BRANCH, reason)]
    caveats = ["A working tree can sit on a branch that predates %s, so a difference above may"
               " be that branch rather than real drift." % BRANCH]
    names = one_level(MIRROR)
    return Record(label, notes, caveats, names,
                  lambda n: file_bytes(os.path.join(MIRROR, *n.split("/")))), None


def main():
    if not os.path.isdir(REPO):
        print("in-step check: no repository at %s -- skipped" % REPO)
        return 0

    rec, why = committed_record()
    if rec is None:
        rec, why = working_tree_record(why)
    if rec is None:
        print("in-step check: nothing to compare -- %s." % why)
        return 0
    if not rec.names:
        # The record is there but holds no scripts: a commit that predates the mirror, or a
        # checkout of one.  Listing every archive script as "not in git" there is true but
        # useless, and it reads like a disaster rather than a wrong starting point.
        print("in-step check: %s carries no pipeline scripts. Nothing compared." % rec.label)
        for note in rec.notes:
            print("  %s" % note)
        return 0

    missing, differs, unreadable = [], [], []
    for name in rec.names:
        src = archive_path(name)
        recorded = rec.read(name)
        if recorded is None:
            unreadable.append(name)             # say we could not look, never that it differs
        elif not os.path.exists(src):
            missing.append(name)                # in the record, gone from the archive
        elif normalised(recorded) != normalised(file_bytes(src)):
            differs.append(name)
    # The other direction: a script the archive has gained that was never committed, which is
    # how a mirror silently stops being a record of what runs.
    adir = os.path.join(ARCHIVE, "_build")
    uncommitted = sorted(f for f in one_level(adir)
                         if not f.endswith(".bat") and f not in rec.names)
    # `.bat` is left out of THIS direction only: update.bat and digests.bat live in the
    # archive's root, not in `_build`, so looking for them here would report them missing for
    # ever.  It was the same before one_level(); only the walk is new.

    if not (missing or differs or uncommitted or unreadable):
        print("in-step check: the archive's %d scripts match the record in %s."
              % (len(rec.names), rec.label))
        for note in rec.notes:
            print("  %s" % note)
        return 0
    print("\nin-step check: the archive and the record in %s have DRIFTED APART." % rec.label)
    print("  The archive's copy is the one that runs; the repository's is the record.")
    for note in rec.notes:
        print("  %s" % note)
    for name in differs:
        print("  differs:      %s" % name)
    for name in uncommitted:
        print("  not in git:   %s   (the archive has it; the record does not)" % name)
    for name in missing:
        print("  not in the archive: %s   (recorded, but not where it runs)" % name)
    for name in unreadable:
        print("  could not read:     %s   (out of the record; nothing established about it)"
              % name)
    for caveat in rec.caveats:
        print("  %s" % caveat)
    print("  To bring the record up to date, copy the archive's files into papers/_build in a")
    print("  checkout that is on %s or on a branch of your own -- never into one sitting on"
          % BRANCH)
    print("  another session's branch -- and commit them, saying what changed and why.")
    return 0                                     # never break update.bat's chain


if __name__ == "__main__":
    sys.exit(main())
