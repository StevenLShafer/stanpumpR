"""Where the paper digests (evidence cards) live, and how a run's new cards reach the repository.

The cards moved out of this archive into the stanpumpR repository on 2026-10-29 (PR #622), under
`papers/digests/`, so that a card travels with the code its `checked_against:` line names.  The
PDFs stay here, because they are copyrighted; the cards are facts and paraphrase and are public.

For the first hours after the move the scripts still wrote to `stanpumpRPapers\\digests\\` while
master held its own copy, and the two diverged three times in one day (PRs #622 and #623).  So
the write target is now a git worktree of the repository, pinned to master, and this module is
the single place that says where that is:

    TREE   a worktree of REPO, kept on the local branch `digests/working`
    CARDS  TREE\\papers\\digests -- the cards, the only directory a run writes to
    DIG    this archive's `digests\\`, which now holds ONLY the generated STATUS.md and REVIEW.md
           (they describe the archive's PDFs and would churn in every commit)

`sync()` brings the worktree up to date with origin/master before a run, without dropping a card
master does not have yet; `commit_and_push()` puts the run's own cards on their own branch, cut
from origin/master, afterwards, so they arrive as a pull request instead of as loose files in a
checkout nobody is looking at.  Neither ever touches the shared checkout at REPO beyond fetching
it.  The digest session also commits hand-written cards here, on digests/working (2026-10-29), so
neither function may assume the worktree's commits or uncommitted files are the runner's.
"""
import os
import subprocess

REPO = r"C:\dev\stanpumpR"
TREE = r"C:\dev\stanpumpR-digests"
CARDS = os.path.join(TREE, "papers", "digests")
BRANCH = "digests/working"              # the worktree's own branch; never pushed under this name
PUSH_PREFIX = "claude/digests-"         # one pushed branch per run, so nothing is ever force-pushed

HERE = os.path.dirname(os.path.abspath(__file__))
ARCH = os.path.dirname(HERE)
DIG = os.path.join(ARCH, "digests")     # STATUS.md and REVIEW.md only


def git(*args, tree=None, check=False):
    """Run git in TREE (or `tree`) and return the completed process."""
    return subprocess.run(["git", "-C", tree or TREE] + list(args),
                          capture_output=True, text=True, check=check)


def _in_master_history(path, blob):
    """True if `blob` was ever origin/master's content of `path`, so a squash merge or a later
    revision on master still counts as master having had it."""
    return blob in git("log", "--format=", "--raw", "--no-abbrev", "origin/master", "--", path).stdout


def cards_not_in_master(ref):
    """The card paths `ref` itself added or changed (since its merge base with origin/master) whose
    content master has never held: what a reset to master, or a re-digest, would throw away."""
    names = git("diff", "--name-only", "--diff-filter=AM", "origin/master..." + ref,
                "--", "papers/digests").stdout.split()
    out = []
    for p in names:
        if os.path.basename(p) == "README.md":
            continue
        blob = git("rev-parse", "%s:%s" % (ref, p)).stdout.strip()
        if blob and not _in_master_history(p, blob):
            # A squash-merged PR puts only its branch's LAST version of a card into master's history, so a version
            # revised on the branch before the merge is never found above.  When master's own version of the card
            # was committed after this one, this one was superseded, not left unmerged (2026-11-09: #662's runner
            # version of PMID_3364955, reviewed on the branch and squash-merged, kept being "kept" and the merge of
            # master into digests/working conflicted on it).
            m_time = git("log", "-1", "--format=%ct", "origin/master", "--", p).stdout.strip()
            r_time = git("log", "-1", "--format=%ct", ref, "--", p).stdout.strip()
            if m_time and r_time and int(m_time) > int(r_time):
                continue
            out.append(p)
    return out


def stem_of(path):
    return os.path.splitext(os.path.basename(path))[0]


def sync(say=print):
    """Make sure the worktree exists and is up to date with origin/master.  Returns master's short sha.

    The worktree is not only the runner's: the digest session commits hand-written cards on
    digests/working here and pushes them as its own PRs.  So it is moved to origin/master only when
    that loses nothing (2026-10-29, after a reset would have dropped PMID_8357297's hand-written card,
    committed and pushed for #629 but not merged, and the next run would have digested it again):
      - uncommitted cards (a crashed run's, or a session's work in progress) are kept;
      - committed cards that master has never held are kept, and origin/master is MERGED in instead;
      - otherwise the branch is reset to origin/master, as before.
    """
    if not os.path.isdir(os.path.join(TREE, ".git")) and not os.path.exists(os.path.join(TREE, ".git")):
        r = subprocess.run(["git", "-C", REPO, "worktree", "add", "-B", BRANCH, TREE, "origin/master"],
                           capture_output=True, text=True)
        if r.returncode:
            raise SystemExit("cards.sync: cannot create the worktree at %s:\n%s" % (TREE, r.stderr.strip()))
        say("cards: created the digests worktree at %s" % TREE)

    # --prune, and all branches rather than just master: a merged run's branch is deleted on the
    # remote, and without pruning its stale remote-tracking ref makes unmerged_branches() warn about
    # it for ever (it did, on the first two runs after the move).
    if git("fetch", "origin", "--prune", "-q").returncode:
        raise SystemExit("cards.sync: cannot fetch origin in %s" % TREE)

    dirty = git("status", "--porcelain", "--", "papers/digests").stdout.strip()
    ahead = cards_not_in_master("HEAD")
    if dirty or ahead:
        if dirty:
            say("cards: %d card(s) in %s are uncommitted (a run that did not finish, or a session working "
                "here); keeping them" % (len(dirty.splitlines()), CARDS))
        if ahead:
            say("cards: %d card(s) committed on %s are not in master yet (%s); keeping them"
                % (len(ahead), BRANCH, ", ".join(stem_of(p) for p in ahead[:8]) + (", ..." if len(ahead) > 8 else "")))
        # bring master's cards in without dropping anything; git refuses to overwrite a modified file
        if git("merge-base", "--is-ancestor", "origin/master", "HEAD").returncode == 0:
            say("cards: %s already contains origin/master; nothing to merge" % BRANCH)
        else:
            r = git("merge", "-q", "--no-edit", "origin/master")
            if r.returncode:
                git("merge", "--abort")
                behind = git("rev-list", "--count", "HEAD..origin/master").stdout.strip()
                say("cards: could not merge origin/master into %s (%s); the worktree stays %s commit(s) behind "
                    "master, and the queue skips every card master has"
                    % (BRANCH, (r.stderr or r.stdout).strip()[:200], behind))
            else:
                say("cards: merged origin/master into %s instead of resetting it" % BRANCH)
    else:
        if git("checkout", "-q", "-B", BRANCH, "origin/master").returncode:
            raise SystemExit("cards.sync: cannot move %s to origin/master in %s" % (BRANCH, TREE))

    sha = git("rev-parse", "--short", "origin/master").stdout.strip()
    if not sha:
        raise SystemExit("cards.sync: cannot read origin/master in %s" % TREE)
    os.makedirs(CARDS, exist_ok=True)
    return sha


def unmerged_cards():
    """{stem: branch} for every card a remote digest branch carries that master has never held.

    NOT `git branch --no-merged`: a squash-merged branch's commits never become ancestors of master,
    so every past run would look unmerged for ever.  What matters is only whether a branch carries a
    card master lacks (cards_not_in_master() compares the content, from the branch's merge base).
    Both the runner's branches and the digest session's hand-made ones count: a card waiting in
    either is not to be digested again.
    """
    out = git("branch", "-r", "--list", "origin/" + PUSH_PREFIX + "*",
              "--format=%(refname:short)").stdout.split()
    held = {}
    for b in out:
        for p in cards_not_in_master(b):
            held.setdefault(stem_of(p), b)
    return held


def unmerged_branches():
    """Remote digest branches holding a card master does not have, as 'branch (n card(s))'."""
    counts = {}
    for b in unmerged_cards().values():
        counts[b] = counts.get(b, 0) + 1
    return ["%s (%d card(s))" % (b, n) for b, n in sorted(counts.items())]


def master_cards():
    """Stems with a card on origin/master."""
    names = git("ls-tree", "--name-only", "origin/master", "papers/digests/").stdout.split()
    return {stem_of(p) for p in names if p.endswith(".md") and os.path.basename(p) != "README.md"}


def card_author(path):
    """(digest_by, protected): a card is protected from unattended rewriting unless the unattended
    runner wrote it and nobody has revised or reviewed it since."""
    head = open(path, encoding="utf-8").read(4000).split("\n---", 1)[0]
    by = next((l.split(":", 1)[1].strip() for l in head.splitlines() if l.startswith("digest_by:")), "")
    touched = any(l.startswith(("revised:", "reviewed:")) for l in head.splitlines())
    return by, ("unattended" not in by) or touched


def _commit_on_master(paths, msg):
    """A commit whose parent is origin/master and whose tree is master's plus `paths` as HEAD has them.

    The run's commit is made on digests/working, which may also hold a session's unmerged commits (and
    merges of master); pushing that would put the session's cards in the run's PR.  So the pushed branch
    is this commit instead, built in a private index so neither the worktree nor its index is touched."""
    idx = os.path.join(HERE, "digest_runs", "push.index")
    env = dict(os.environ, GIT_INDEX_FILE=idx)

    def run(*a):
        return subprocess.run(["git", "-C", TREE] + list(a), capture_output=True, text=True, env=env)
    try:
        if run("read-tree", "origin/master").returncode:
            return None
        for p in paths:
            blob = git("rev-parse", "HEAD:" + p).stdout.strip()
            if not blob or run("update-index", "--add", "--cacheinfo", "100644,%s,%s" % (blob, p)).returncode:
                return None
        tree = run("write-tree").stdout.strip()
        if not tree:
            return None
        return git("commit-tree", tree, "-p", "origin/master", "-F", msg).stdout.strip() or None
    finally:
        if os.path.exists(idx):
            os.remove(idx)


def commit_and_push(stamp, stems, sha, say=print):
    """Commit this run's cards and push them on their own branch.  Returns the branch, or None.

    Only the cards this run wrote, plus uncommitted cards the unattended runner wrote in a run that
    did not finish, are committed.  Anything else uncommitted in the worktree is a session's work in
    progress and is left alone (2026-10-29: `git add papers/digests` would have swept two such cards
    into a run's PR)."""
    pending = [l[3:].strip().strip('"') for l in
               git("status", "--porcelain", "-uall", "--", "papers/digests").stdout.splitlines()]
    mine, left = [], []
    for p in pending:
        full = os.path.join(TREE, p)
        ours = stem_of(p) in stems or (os.path.exists(full) and not card_author(full)[1])
        (mine if ours and p.endswith(".md") and os.path.basename(p) != "README.md" else left).append(p)
    if left:
        say("cards: leaving %d uncommitted file(s) this run did not write: %s" % (len(left), ", ".join(left[:8])))
    if not mine:
        say("cards: no card changed, nothing to push")
        return None
    git("add", "--", *mine)
    named = ", ".join(stems[:12]) + (", ..." if len(stems) > 12 else "")
    body = ("Paper digests from the unattended run %s\n\n"
            "Written by _build/digest_run.py against stanpumpR master %s.\n"
            "Cards in this run: %s\n\n"
            "Every card states what its source says and then verifies the repository against it; "
            "anything the paper and the code could not settle is in the card's own "
            "`open_questions:` rather than guessed at.\n\n"
            "Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>\n" % (stamp, sha, named))
    msg = os.path.join(HERE, "digest_runs", stamp + "-commit.txt")
    with open(msg, "w", encoding="utf-8") as f:
        f.write(body)
    # `-- paths`: commit these cards only, even if a session has something else staged in this worktree
    if git("commit", "-q", "-F", msg, "--", *mine).returncode:
        say("cards: COMMIT FAILED -- the cards are in %s and are not pushed" % CARDS)
        return None
    branch = PUSH_PREFIX + stamp
    head = _commit_on_master(mine, msg)
    if not head:
        say("cards: COULD NOT BUILD the branch commit on origin/master -- the cards are committed on %s in %s "
            "and are not pushed" % (BRANCH, TREE))
        return None
    r = git("push", "-q", "origin", head + ":refs/heads/" + branch)
    if r.returncode:
        say("cards: PUSH FAILED (%s) -- the commit is on %s in %s" % (r.stderr.strip()[:200], BRANCH, TREE))
        return None
    say("cards: pushed %s" % branch)
    # Shafer, 2026-10-29: the unattended runs open their own pull request, one per run ("Yes, fully automatic").
    # A failure here must be loud and name the branch, and must NOT fail the run: the cards are already
    # committed and pushed, so a gh outage degrades to the compare link a person can open by hand.
    compare = "https://github.com/StevenLShafer/stanpumpR/compare/%s?expand=1" % branch
    pr_body = os.path.join(HERE, "digest_runs", stamp + "-pr.md")
    with open(pr_body, "w", encoding="utf-8") as f:
        f.write(body.split("Co-Authored-By:")[0].rstrip() + "\n\n"
                "🤖 Generated with [Claude Code](https://claude.com/claude-code)\n")
    try:
        r = subprocess.run(["gh", "pr", "create", "--repo", "StevenLShafer/stanpumpR", "--base", "master",
                            "--head", branch, "--title", "Paper digests: unattended run %s" % stamp,
                            "--body-file", pr_body], capture_output=True, text=True, timeout=180)
        failed = r.returncode != 0
        detail = (r.stderr or r.stdout).strip()[:300]
    except (OSError, subprocess.TimeoutExpired) as e:
        failed, detail, r = True, str(e)[:300], None
    if failed:
        say("cards: PULL REQUEST NOT OPENED (%s) -- the cards ARE committed and pushed on %s; open the PR by "
            "hand: %s" % (detail, branch, compare))
    else:
        # gh prints the PR url on stdout, but do not let an empty or unexpected stdout raise IndexError
        # here: the cards are pushed and the PR is open by this point, and a traceback would end the run
        # as a failure after it had in fact succeeded.
        url = next((l for l in (r.stdout or "").strip().splitlines()[::-1] if l.strip()), compare)
        say("cards: opened %s" % url)
    return branch
