# Advance a single exponential state over the time line:
#
#     state[i] = state[i - 1] * l[i] + bolus[i] + infusion[i]
#
# from `start` before the first point.  Until 2026-10-07 this built a list of
# L small lists with lapply() and ran Reduce(accumulate = TRUE) over it, which
# is the same recurrence at several microseconds a point; with a dose four
# times a day for a year and twenty-odd states in the metabolite engine it was
# most of the run.  The loop does the same arithmetic in the same order --
# state * l, then + bolus, then + infusion -- so the result is bit-identical.
# (Claude Code, 2026-10-07, at the request of Steven L. Shafer; see
# R/simulationTimeGrid.R.)
advanceState <- function(l, bolus, infusion, start, L)
{
  out <- numeric(L)
  state <- start
  for (i in seq_len(L))
  {
    state  <- state * l[i] + bolus[i] + infusion[i]
    out[i] <- state
  }
  out
}

# Advance a single state variable over time, with the extravascular inputs:
# the same recurrence with PO, IM and IN added after the infusion, in that
# order, as the Reduce() it replaced added them.  Sublingual (SL), added
# later, is optional and comes last, so that every caller written before it
# keeps its arithmetic bit for bit.
advanceStatePO <- function(l, bolus, infusion, PO, IM, IN, L, SL = NULL)
{
  out <- numeric(L)
  state <- 0
  if (is.null(SL))
  {
    for (i in seq_len(L))
    {
      state  <- state * l[i] + bolus[i] + infusion[i] + PO[i] + IM[i] + IN[i]
      out[i] <- state
    }
  } else {
    for (i in seq_len(L))
    {
      state  <- state * l[i] + bolus[i] + infusion[i] + PO[i] + IM[i] + IN[i] + SL[i]
      out[i] <- state
    }
  }
  out
}
