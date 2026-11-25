"""Back the GitHub repository up to Google Drive as a dated git bundle. Read-only to the archive.

Steven L. Shafer, 2026-11-21, on where the work lives: GitHub is definitive for everything
public, Google Drive holds the copyrighted papers -- and he wants a copy of the repository on
Drive too, "simply for backup. For example, if Microsoft goes bankrupt (unlikely), or github
gets hacked (probably the juiciest target on the planet) then I still have a copy."

WHY A BUNDLE AND NOT A FOLDER COPY.  Three reasons, and the third is the real one:

  * a bundle is ONE file holding the complete history -- `git clone <file>` reconstitutes the
    repository -- so there is nothing to sync, diff or half-copy;
  * it cannot be mistaken for a working tree or edited in place, so it can never become a
    second source of truth, which a mirror eventually does;
  * the risk actually worth covering is not loss but TAMPERING.  GitHub's durability is
    excellent and bankruptcy would give months of notice; a force-pushed history or a quiet
    malicious commit is the credible threat, and against that you want a DATED, IMMUTABLE copy
    from a known-good day.  A continuously synced mirror would faithfully mirror the tampering.
    So these are deliberately stale, and several generations are kept.

WHAT IT BACKS UP, and what it cannot.  Every branch, tag and commit that GitHub holds.  Not the
pull-request discussions, review threads, issues or Actions logs, which live only in GitHub's
own database -- in this project most of the reasoning is in commit messages by deliberate
practice, so that loss is smaller than it sounds, but it is real.

It bundles from a MIRROR of origin, not from a working checkout: a checkout may be behind, may
be on a branch, and may carry local commits that were never pushed.  The mirror is updated
incrementally, so only the first run transfers the whole repository.

  python bundle_repo.py              make one if the newest is older than BUNDLE_EVERY_DAYS
  python bundle_repo.py --force      make one now regardless
  python bundle_repo.py --keep N     keep the newest N (default BUNDLE_KEEP)
  python bundle_repo.py --list       show what is on Drive and exit

Each bundle gets a sidecar .json naming the commit it was taken from, so a later comparison can
tell whether history was rewritten under you.
"""
import datetime
import glob
import io
import json
import os
import socket
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
OUT = os.path.join(ARCH, "_backup")            # inside the archive, so ONE Drive folder
MIRROR = os.path.join("C:" + os.sep, "dev", "stanpumpR-mirror.git")   # a cache, not a backup
REMOTE = "https://github.com/StevenLShafer/stanpumpR.git"

LOCK = os.path.join(OUT, "bundle_repo.lock")
HOST = socket.gethostname()      # the lock names its computer: Drive syncs _backup to the others
LOCK_FOREIGN_HOURS = 6           # another computer's note older than this is ignored
_LOCK_FD = None                  # the open descriptor IS the lock; see take_lock()

BUNDLE_EVERY_DAYS = 7
BUNDLE_KEEP = 8                                # about two months of weekly generations

sys.stdout.reconfigure(encoding="utf-8")
args = sys.argv[1:]
force = "--force" in args
keep = int(args[args.index("--keep") + 1]) if "--keep" in args else BUNDLE_KEEP
if keep < 1:
    # --keep 0 made rows[keep:] select EVERY bundle, so the prune deleted the generation just
    # written along with all the older ones. One flag away from destroying the whole backup
    # (CodeRabbit, PR #847).
    sys.exit("--keep must be at least 1; refusing to run, because --keep 0 would delete every "
             "bundle including the one this run would write.")


def git(*a, **kw):
    """Run git, raising with its own stderr rather than a bare exit code."""
    r = subprocess.run(("git",) + a, capture_output=True, text=True, **kw)
    if r.returncode != 0:
        raise RuntimeError("git %s failed (%d): %s" % (" ".join(a[:2]), r.returncode,
                                                       (r.stderr or r.stdout or "").strip()[:400]))
    return r.stdout.strip()


def _lock_exclusive(fd):
    """Take an exclusive, non-blocking lock on fd. Raises OSError if someone else holds it."""
    try:
        import fcntl
    except ImportError:
        import msvcrt
        msvcrt.locking(fd, msvcrt.LK_NBLCK, 1)
    else:
        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)


def _unlock(fd):
    try:
        import fcntl
    except ImportError:
        import msvcrt
        try:
            msvcrt.locking(fd, msvcrt.LK_UNLCK, 1)
        except OSError:
            pass
    else:
        fcntl.flock(fd, fcntl.LOCK_UN)


def take_lock():
    """Hold an OS-level exclusive lock for the life of this process, or say who has it.

    THERE IS NO STALE LOCK, which is the point.  The kernel owns the lock and drops it when the
    process dies, so a crashed run leaves nothing to recover from -- and the recovery logic that
    went with the old scheme is gone, along with both of the races in it (CodeRabbit, PR #847):

      * a run paused for longer than a grace period could have its live lock deleted;
      * two runs could both judge a lock stale, and the second's os.remove() would delete the
        first's NEW lock.

    Neither is expressible now: nothing removes a lock, ever.  A grace period cannot tell a
    crashed process from a paused one, and trying was the wrong shape of answer.

    Verified on this archive before the rewrite, because Drive is a virtual filesystem and byte
    locks are not guaranteed there: on the local disk and on the Drive archive alike, a second
    refused while the first holds the lock, granted after it releases, and granted immediately
    after the holder is KILLED.

    WITHIN one computer this is exact.  ACROSS computers it cannot be: byte locks do not travel
    through Google Drive's sync, so the owner note below is ADVISORY -- it lets a person see
    that another machine was running, and refuses while such a note is recent, but two machines
    could still overlap.  That is the limitation Drive imposes, and it is why the project's rule
    is that only one computer runs the pipeline at a time (AGENTS.md). The note is never
    enforcement; the kernel's lock is.

    The lock FILE is never deleted. Its existence means nothing -- the byte lock means
    everything -- and deleting it would reintroduce a race: between the close and the unlink,
    another run could acquire the file, and a third could create a new one, leaving two runs
    each holding a lock on a different file.
    """
    global _LOCK_FD
    os.makedirs(OUT, exist_ok=True)
    fd = os.open(LOCK, os.O_CREAT | os.O_RDWR)
    try:
        _lock_exclusive(fd)
    except OSError:
        # Someone else holds it. We hold no lock, so an ordinary read is right here -- though it
        # may still fail against the holder's mandatory lock, in which case the message simply
        # names nobody.
        held = {}
        try:
            held = json.loads(io.open(LOCK, encoding="utf-8").read() or "{}")
        except (ValueError, OSError):
            pass
        os.close(fd)
        if not held.get("host"):
            # Expected, not an error: the holder's lock on byte 0 is MANDATORY on Windows, so
            # its note cannot be read from here. The refusal is still exact -- the kernel
            # answered it -- we just cannot say whose it is.
            who = "its note cannot be read while it holds the lock"
        elif held.get("host") == HOST:
            who = "pid %s on this computer, started %s" % (held.get("pid"), held.get("started"))
        else:
            who = "on %s, started %s" % (held.get("host"), held.get("started"))
        return "another backup holds the lock (%s); nothing to do here" % who

    # We hold it. The note that follows is for a person reading _backup, and for the
    # cross-machine advisory check below.
    note = {"host": HOST, "pid": os.getpid(), "started": time.strftime("%Y-%m-%d %H:%M:%S")}
    # Read the previous note through the SAME descriptor. msvcrt.locking() takes a MANDATORY
    # lock on byte 0, so a second handle on the file -- io.open(LOCK) -- cannot read it, not
    # even in the process that holds the lock: the read raised, the exception was swallowed,
    # `previous` became {} and the advisory check below silently never fired. Caught by this
    # script's own foreign-note case, which is what those cases are for.
    try:
        os.lseek(fd, 0, os.SEEK_SET)
        previous = json.loads(os.read(fd, 65536).decode("utf-8", "replace") or "{}")
    except (ValueError, OSError):
        previous = {}
    if isinstance(previous, dict) and previous.get("host") not in (None, HOST):
        try:
            age_h = (time.time() - os.path.getmtime(LOCK)) / 3600
        except OSError:
            age_h = LOCK_FOREIGN_HOURS + 1
        if age_h < LOCK_FOREIGN_HOURS:
            # ADVISORY only: we DO hold the kernel's lock, and another computer's lock is
            # invisible to it. Standing down is the project's one-computer-at-a-time rule,
            # not a guarantee.
            _unlock(fd)
            os.close(fd)
            return ("a backup on %s (started %s) may still be running; nothing to do here. Its "
                    "note in %s is %.1f h old -- wait for it, or run again once you know that "
                    "run has finished" % (previous.get("host"), previous.get("started"),
                                          os.path.basename(LOCK), age_h))

    os.lseek(fd, 0, os.SEEK_SET)
    os.truncate(fd, 0)
    os.write(fd, json.dumps(note).encode("utf-8"))
    _LOCK_FD = fd                      # held open: closing it would release the lock
    return None


def drop_lock():
    """Release the lock. The file stays: its contents are a note, not the lock itself."""
    global _LOCK_FD
    if _LOCK_FD is None:
        return
    _unlock(_LOCK_FD)
    try:
        os.close(_LOCK_FD)
    except OSError:
        pass
    _LOCK_FD = None


def prune(rows, keep):
    """Remove all but the newest `keep` generations. Returns the names removed.

    THE BUNDLE GOES FIRST, and the reasoning flipped once: before the lock existed, freeing the
    bundle name let another run claim it and have its fresh sidecar deleted by this loop, so the
    sidecar went first.  Under the lock no other run can be here at all, and the remaining risk
    is an INTERRUPTION between the two removals -- where an orphaned .json is ignored by
    existing(), while an orphaned BUNDLE would be counted as a generation that --verify cannot
    check, because its commit is only recorded in the sidecar (CodeRabbit, PR #847).

    So: bundle first while serialised, sidecar first while not. The lock is what makes the
    first order the safe one, and this docstring is the record of why.
    """
    removed = []
    for _d, bundle in rows[keep:]:
        side = bundle[: -len(".bundle")] + ".json"
        try:
            os.remove(bundle)
        except OSError:
            continue                             # leave its sidecar: the generation is still there
        removed.append(os.path.basename(bundle))
        try:
            if os.path.exists(side):
                os.remove(side)
        except OSError:
            pass                                 # an orphaned sidecar is ignored by existing()
    return removed


def release(tmp, side_tmp, path, side):
    """Undo a failed backup: remove this run's temporaries, and its reservation if it still owns it.

    ORDER MATTERS, and getting it wrong was a real race (CodeRabbit, PR #847). Releasing the
    bundle name first let another run claim it, write ITS sidecar and publish -- and this
    cleanup, still walking its list, would then see a zero-byte reservation at `path` (the other
    run's) and delete the other run's sidecar, leaving it to publish a bundle with no metadata.

    So ownership is decided ONCE, while this run still holds the reservation -- no other run can
    claim the name while the file exists -- and the sidecar goes BEFORE the reservation. After
    the reservation is released nothing here touches anything.

    A reservation is a ZERO-BYTE file. If `path` holds a real bundle the generation is finished,
    and neither published name may be touched.
    """
    for scratch in (tmp, side_tmp):              # always this run's own
        try:
            if os.path.exists(scratch):
                os.remove(scratch)
        except OSError:
            pass
    try:
        ours = os.path.exists(path) and os.path.getsize(path) == 0
    except OSError:
        ours = False
    if not ours:
        return
    for name in (side, path):                    # the sidecar first, the reservation last
        try:
            if os.path.exists(name):
                os.remove(name)
        except OSError:
            pass


def stamp_of(d):
    """How a generation is shown: the date, plus the minute when there was more than one."""
    return d.date().isoformat() if (d.hour, d.minute) == (0, 0) else d.strftime("%Y-%m-%d %H:%M")


def existing():
    """The bundles on Drive, newest first, by the date in the name rather than by mtime."""
    found = []
    for p in glob.glob(os.path.join(OUT, "stanpumpR-*.bundle")):
        stem = os.path.basename(p)[len("stanpumpR-"):-len(".bundle")]
        # Either YYYY-MM-DD or YYYY-MM-DD-HHMM (a second run on one day).  Both are parsed, so
        # both are pruned in their turn; anything else is not ours and is never deleted.
        head, _, tail = stem.rpartition("-")
        try:
            if head and len(tail) == 4 and tail.isdigit():
                d = datetime.datetime.fromisoformat(head).replace(
                    hour=int(tail[:2]), minute=int(tail[2:]))
            else:
                d = datetime.datetime.fromisoformat(stem)
        except ValueError:
            continue           # not one of ours; never deleted
        try:
            empty = os.path.getsize(p) == 0
        except FileNotFoundError:
            # Another run removed its own failed reservation after glob() listed it. Nothing is
            # wrong; this name simply is not there. Any OTHER filesystem error propagates.
            continue
        if empty:
            # A zero-byte file is a name CLAIMED by a run that has not finished writing yet.
            # It is not a generation: listing it let --keep prune another run's reservation,
            # after which a third run could publish into that name and the first would
            # overwrite it (CodeRabbit, PR #847). A finished bundle is never zero bytes.
            continue
        found.append((d, p))
    return sorted(found, reverse=True)


if "--list" in args:
    rows = existing()
    if not rows:
        print("No bundles in %s" % OUT)
    for d, p in rows:
        side = p[: -len(".bundle")] + ".json"
        sha = ""
        if os.path.exists(side):
            try:
                sha = json.load(io.open(side, encoding="utf-8")).get("master", "")[:12]
            except ValueError:
                sha = "(unreadable sidecar)"
        when = stamp_of(d)
        print("  %-16s %7.1f MB  master %s" % (when, os.path.getsize(p) / 1e6, sha))
    sys.exit(0)

if "--verify" in args:
    # Is every saved generation still an ancestor of what GitHub serves NOW?  Fetched live and
    # checked, because the alternative -- telling a person to run `git fetch` and then
    # `merge-base ... FETCH_HEAD` as two commands -- silently compares against a previous
    # fetch's FETCH_HEAD when the fetch fails (CodeRabbit, PR #847).  And it must not be
    # compared against a clone's origin/master: in a clone made from a bundle `origin` IS the
    # bundle, so that test compares the saved commit with itself and can never fail.
    if not os.path.isdir(os.path.join(MIRROR, "objects")):
        sys.exit("No local mirror yet; run this script once without --verify first.")
    try:
        git("--git-dir", MIRROR, "fetch", REMOTE, "+refs/heads/master:refs/verify/master")
    except RuntimeError as exc:
        sys.exit("Could not reach GitHub, so NOTHING was checked: %s" % exc)
    live = git("--git-dir", MIRROR, "rev-parse", "refs/verify/master")
    print("GitHub's master is now %s" % live)
    rows = existing()
    if not rows:
        sys.exit("No bundles in %s to check." % OUT)
    # 0 every generation verified intact; 1 at least one could not be checked, so nothing is
    # proven wrong and nothing is confirmed either; 2 at least one is PROVEN not in live history.
    worst = 0
    for d, bp in rows:
        side = bp[: -len(".bundle")] + ".json"
        if not os.path.exists(side):
            print("  %-16s no sidecar, so its commit is unknown" % stamp_of(d))
            worst = max(worst, 1)
            continue
        try:
            saved = json.load(io.open(side, encoding="utf-8")).get("master", "")
        except ValueError:
            print("  %-16s its sidecar cannot be read" % stamp_of(d))
            worst = max(worst, 1)
            continue
        r = subprocess.run(("git", "--git-dir", MIRROR, "merge-base", "--is-ancestor",
                            saved, live), capture_output=True, text=True)
        # 0 is an ancestor, 1 is NOT an ancestor, and anything else is git failing to answer --
        # 128 when it does not have the commit object at all.  "I could not tell" is not
        # "rewritten", and reporting it as tampering would cry wolf; reporting it as intact
        # would be the vacuous check this script already had once (CodeRabbit, PR #847).
        if r.returncode == 0:
            print("  %-16s intact (%s is an ancestor of live master)" % (stamp_of(d),
                                                                         saved[:12]))
        elif r.returncode == 1:
            print("  %-16s *** %s IS NOT IN LIVE HISTORY: it was rewritten, or GitHub was "
                  "rolled back ***" % (stamp_of(d), saved[:12]))
            worst = max(worst, 2)
        else:
            print("  %-16s could not be checked: git could not place %s (%s)"
                  % (stamp_of(d), saved[:12],
                     (r.stderr or r.stdout or "exit %d" % r.returncode).strip()[:80]))
            worst = max(worst, 1)
    sys.exit(worst)

today = datetime.date.today()
have = existing()
if have and not force:
    # From NOW, not from today's midnight: a generation written at noon seven days ago
    # measured as six days old, so the weekly guard skipped it and waited another week.
    age = (datetime.datetime.now() - have[0][0]).days
    if age < BUNDLE_EVERY_DAYS:
        print("The newest bundle is %d day(s) old (%s); nothing to do. --force overrides."
              % (age, have[0][0]))
        sys.exit(0)

# 0. the lock: everything below mutates _backup, so no second run may be doing the same
busy = take_lock()
if busy:
    sys.exit("bundle_repo: %s." % busy)
import atexit
atexit.register(drop_lock)       # covers sys.exit, an exception, and the ordinary end

# 1. the mirror: a faithful copy of what GitHub holds, updated incrementally
if not os.path.isdir(os.path.join(MIRROR, "objects")):
    print("Cloning a mirror of origin into %s (first run only, about 270 MB) ..." % MIRROR)
    git("clone", "--mirror", REMOTE, MIRROR)
else:
    print("Updating the mirror ...")
    git("--git-dir", MIRROR, "remote", "update", "--prune")

master = git("--git-dir", MIRROR, "rev-parse", "refs/heads/master")
commits = git("--git-dir", MIRROR, "rev-list", "--all", "--count")
branches = len(git("--git-dir", MIRROR, "for-each-ref", "--format=%(refname)",
                   "refs/heads").splitlines())

# 2. the bundle
os.makedirs(OUT, exist_ok=True)
# A generation is IMMUTABLE: never write over one.  The name was date-only and os.replace()
# overwrote it, so a same-day --force after origin had moved replaced a saved generation with a
# different commit -- against this script's own stated requirement.  A second run on one day
# takes a -HHMM suffix, which existing() also parses, so it is pruned in its turn.
#
# The name is CLAIMED atomically, not checked and then written.  os.path.exists() followed by a
# write leaves a window in which two overlapping runs both pass and the later one overwrites the
# earlier bundle; and `assert` is stripped by python -O, so a guard written that way may not be
# there at all (CodeRabbit, PR #847).  O_CREAT|O_EXCL is a single operation that fails when the
# name is taken, so the reservation cannot race.
def claim(stamp):
    """Reserve a bundle name atomically, or None if something already holds it."""
    candidate = os.path.join(OUT, "stanpumpR-%s.bundle" % stamp)
    try:
        fd = os.open(candidate, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    except FileExistsError:
        return None
    os.close(fd)                     # an empty placeholder; only this run may replace it
    return candidate


path = claim(today.isoformat())
if path is None:
    path = claim("%s-%s" % (today.isoformat(), datetime.datetime.now().strftime("%H%M")))
if path is None:
    sys.exit("A bundle for this date and minute already exists; nothing was written. Wait a "
             "minute and run again, or remove the generation deliberately if that is what you "
             "mean to do -- this script will not write over a saved one.")
name = os.path.basename(path)
tmp = "%s.%d.part" % (path, os.getpid())       # this process's own; never a shared name
side = path[: -len(".bundle")] + ".json"
side_tmp = "%s.%d.part" % (side, os.getpid())
print("Writing %s ..." % name)
try:
    git("--git-dir", MIRROR, "bundle", "create", tmp, "--all")
    # Verify BEFORE it takes the name, and before anything old is pruned: an unverified backup
    # is not a backup, and a corrupt one that displaced a good generation is worse than none.
    git("--git-dir", MIRROR, "bundle", "verify", tmp)
    # The commit the BUNDLE contains, read from the bundle itself.  The `master` read from the
    # mirror earlier is not it: the mirror is shared, so another run updating it between that
    # read and `bundle create` would leave the sidecar naming a commit this bundle does not
    # hold -- and --verify would then check the wrong commit and could call a generation intact
    # when its real tip is no longer live (CodeRabbit, PR #847).
    heads = git("--git-dir", MIRROR, "bundle", "list-heads", tmp)
    bundled = next((l.split()[0] for l in heads.splitlines()
                    if l.split()[1:2] == ["refs/heads/master"]), None)
    if not bundled:
        raise RuntimeError("the bundle holds no refs/heads/master; refusing to record one")
    master = bundled
    size = os.path.getsize(tmp)

    # Prepare the sidecar before publishing anything: a bundle published without one is
    # treated as this week's backup by the age guard and cannot be checked by --verify.
    io.open(side_tmp, "w", encoding="utf-8", newline="\n").write(
        json.dumps({"date": today.isoformat(), "master": master, "commits": int(commits),
                    "branches": branches, "bytes": size, "remote": REMOTE,
                    "restore": 'git clone "%s" stanpumpR' % name,
                    "note": "Complete history of every branch and tag, and every refs/pull/* "
                            "ref GitHub held. Pull-request discussions, issues and Actions "
                            "logs are NOT here: they live only in GitHub."},
                   indent=1))
    # Now publish, sidecar first: an orphaned .json is ignored by existing(), which globs
    # .bundle files, whereas an orphaned bundle is the failure described above.
    os.replace(side_tmp, side)
    os.replace(tmp, path)                      # over our own empty placeholder, nothing else
except BaseException:
    # Everything above is protected, not just the create: a failure in verify or in the rename
    # left the zero-byte reservation as the newest "generation", after which the age guard said
    # "nothing to do" until that date was a week old, and left the .part file behind as well.
    # The reservation is removed only while it is still EMPTY, so a finished bundle is never
    # touched by this.
    release(tmp, side_tmp, path, side)
    raise

print("  %.1f MB, %s commits, %d branches, master %s" % (size / 1e6, commits, branches, master[:12]))

# 4. prune, oldest first, only after the new one is verified and in place
for pruned in prune(existing(), keep):
    print("  pruned %s" % pruned)

print()
print('Restore with:  git clone "%s" stanpumpR' % path)
print("Check against live GitHub with:  python bundle_repo.py --verify")
