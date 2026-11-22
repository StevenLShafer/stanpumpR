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
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
OUT = os.path.join(ARCH, "_backup")            # inside the archive, so ONE Drive folder
MIRROR = os.path.join("C:" + os.sep, "dev", "stanpumpR-mirror.git")   # a cache, not a backup
REMOTE = "https://github.com/StevenLShafer/stanpumpR.git"

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
        when = d.date().isoformat() if (d.hour, d.minute) == (0, 0) else d.strftime("%Y-%m-%d %H:%M")
        print("  %-16s %7.1f MB  master %s" % (when, os.path.getsize(p) / 1e6, sha))
    sys.exit(0)

today = datetime.date.today()
have = existing()
if have and not force:
    age = (datetime.datetime.combine(today, datetime.time()) - have[0][0]).days
    if age < BUNDLE_EVERY_DAYS:
        print("The newest bundle is %d day(s) old (%s); nothing to do. --force overrides."
              % (age, have[0][0]))
        sys.exit(0)

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
# different commit -- against this script's own stated requirement (CodeRabbit, PR #847).  A
# second run on the same day takes a -HHMM suffix, which existing() also parses, so it is
# pruned in its turn rather than living for ever.
name = "stanpumpR-%s.bundle" % today.isoformat()
if os.path.exists(os.path.join(OUT, name)):
    name = "stanpumpR-%s-%s.bundle" % (today.isoformat(),
                                       datetime.datetime.now().strftime("%H%M"))
path = os.path.join(OUT, name)
assert not os.path.exists(path), "refusing to overwrite the saved generation %s" % name
tmp = path + ".part"                           # never leave a half-written bundle named as one
print("Writing %s ..." % name)
git("--git-dir", MIRROR, "bundle", "create", tmp, "--all")

# 3. verify BEFORE it takes the name, and before anything old is pruned.  An unverified backup
#    is not a backup, and a corrupt one that displaced a good generation is worse than none.
git("--git-dir", MIRROR, "bundle", "verify", tmp)
os.replace(tmp, path)
size = os.path.getsize(path)

io.open(path[: -len(".bundle")] + ".json", "w", encoding="utf-8", newline="\n").write(
    json.dumps({"date": today.isoformat(), "master": master, "commits": int(commits),
                "branches": branches, "bytes": size, "remote": REMOTE,
                "restore": "git clone %s stanpumpR" % name,
                "note": "Complete history of every branch and tag. Pull-request discussions, "
                        "issues and Actions logs are NOT here: they live only in GitHub."},
               indent=1))
print("  %.1f MB, %s commits, %d branches, master %s" % (size / 1e6, commits, branches, master[:12]))

# 4. prune, oldest first, only after the new one is verified and in place
rows = existing()
for d, p in rows[keep:]:
    side = p[: -len(".bundle")] + ".json"
    os.remove(p)
    if os.path.exists(side):
        os.remove(side)
    print("  pruned %s" % os.path.basename(p))

print()
print("Restore with:  git clone %s stanpumpR" % path)
print()
print("To tell whether history has been rewritten since, compare this bundle's commit with what")
print("GitHub serves NOW.  It must be fetched live: in a clone made from a bundle `origin` is the")
print("bundle file itself, so comparing against origin/master compares the saved commit with")
print("itself and can never fail.")
print("  git fetch %s master" % REMOTE)
print("  git merge-base --is-ancestor %s FETCH_HEAD   && echo intact || echo HISTORY REWRITTEN"
      % master)
