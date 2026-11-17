# Work that is waiting on a paper

`pending-work.csv` is the register of work that is **ready to do except that a paper is not here
yet**. Its purpose is narrow and worth stating plainly: it lets a pull request whose work is
*mostly* done be merged and closed without losing the part that could not be finished.

It is not a way of reviving an old branch. Steven L. Shafer, 2026-11-17:

> The key is that if a PR is archived, but more work is pending additional papers, nothing is lost
> in the archiving step.

So when the paper arrives, the work is done in a **new** pull request, written against whatever
master looks like then. Nothing is parked, nothing is rebased, nothing rots. The only thing that has
to survive is the *description* of what to do — which is why every row must stand on its own.

## Why not just `to_obtain`?

The registries' `to_obtain` column is the same idea and came first, and where a registry row exists
it remains the right place — it sits beside the readiness and the blocking issue, and
`_build/arrivals.py` already reads it. But `to_obtain` only exists where there is a registry row.
Pending work often is not one: a help page that cannot be written, a covariate that cannot be
applied, a card whose question the paper would answer, a band with no source. Those have nowhere
else to live.

So: **a registry row's pending paper goes in that row's `to_obtain`; everything else goes here.**
Do not write the same paper in both — `arrivals.py` reports from both and would say it twice.

## The columns

| Column | What goes in it |
|---|---|
| `pmid` | The PubMed id. One per row; a piece of work needing two papers gets two rows, because they arrive separately. |
| `drug` | The drug, or `-` where it is not about one. |
| `what_the_paper_would_let_us_do` | **The row's whole value.** Written so somebody who has never seen the original pull request can act on it: what is missing now, what the paper is expected to supply, and what changes once it does. "Confirm the parameters" is not enough; "the apparent volume is derived from the abstract's Vss and would be read directly from Table 2" is. |
| `where` | The files or rows it would touch, so the future session can find its footing. |
| `raised_in` | The pull request it came from — provenance, not a thing to reopen. |
| `raised_on` | ISO date. |
| `status` | `pending` until the paper arrives; `done` with the new PR number once the work lands, or `dropped` with the reason. Rows are **not deleted**: a dropped row is the record that the question was asked and answered. |

## How a row gets acted on

`_build/arrivals.py` in the paper archive reads this file from `origin/master` along with every
registry's `to_obtain`, and reports a `pending` row whose PMID is now in the archive. That is the
only trigger; nothing watches a branch.

**Merge the row before archiving the pull request.** `arrivals.py` reads `origin/master`, so a row
that is still on the closed branch is invisible — which would lose exactly what this file exists to
keep.
