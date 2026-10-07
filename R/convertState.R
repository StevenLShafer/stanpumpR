# Convert state variables from one PK set to another, when a clinical event
# switches PK set in advanceClosedForm1().
#
# The states are the plasma concentration's exponential terms,
#     Cp(t) = s_1 exp(-lambda_1 t) + s_2 exp(-lambda_2 t) + s_3 exp(-lambda_3 t),
# and what has to carry across a change in PK is the drug: the AMOUNT in each
# compartment.  One unit of state j is the model's mode with eigenvalue
# lambda_j, scaled to a central concentration of 1, which holds
#     v1                          in the central compartment,
#     v1 k12 / (k21 - lambda_j)   in peripheral compartment 2, and
#     v1 k13 / (k31 - lambda_j)   in peripheral compartment 3
# (from dA2/dt = k12 A1 - k21 A2 = -lambda_j A2, and likewise for 3; v1 k12 is
# v2 k21).  So the amounts are that matrix, built from the old set, times the
# old states, and the new states solve the same system built from the new set.
#
# Each matrix has only the compartments its own set has: one when lambda_2 is
# zero, two when lambda_3 is zero, otherwise three, the test getDrugPK() uses.
# The two sets need not agree:
#   * a compartment the new set has and the old one lacked starts empty;
#   * drug in a compartment the new set lacks goes to the new set's remaining
#     peripheral compartment (three to two), or to the central compartment when
#     there is no peripheral left (two or three to one).  The total is conserved
#     either way.  Three to two keeps it in the periphery so that plasma does
#     not jump, at the instant of the event, by the whole content of a deep
#     compartment, which after a long infusion can be many times the plasma's;
#     a one-compartment set has nowhere else to put it.
# A state beyond the old set's own compartments is zero by construction (its
# coefficients are zero, and this function zeroes it), so it is not read.
#
# Until 2026-10-07 this chose its branch from the OLD set alone and assumed the
# new set had the same structure.  Three to three and two to two give the
# states that closed form gave, to rounding; one to one kept the concentration
# instead of the amount, creating or destroying drug whenever v1 changed; one
# to two or three left the states unchanged, which in the new model put drug in
# peripheral compartments that had none; and two or three to one returned NaN.
# No drug in the library switches compartment count on an event, so the last
# three were latent.  (Claude Code, Claude Opus 5.5, 2026-10-07, at the request
# of Steven L. Shafer; checked against the old closed form, a mass balance and
# a matrix exponential by tests/testthat/test-convertState.R.)
convertState <- function(oldState, oldPK, newPK)
{
  nOld <- compartmentCount(oldPK)
  nNew <- compartmentCount(newPK)

  amount <- as.vector(stateToAmount(oldPK, nOld) %*% oldState[seq_len(nOld)])

  if (nNew > nOld)
  {
    amount <- c(amount, rep(0, nNew - nOld))
  }
  if (nNew < nOld)
  {
    # The new set's last compartment: peripheral 2 if it has one, else central
    amount[nNew] <- amount[nNew] + sum(amount[(nNew + 1):nOld])
    amount <- amount[seq_len(nNew)]
  }

  newState <- solve(stateToAmount(newPK, nNew), amount)
  return(state = c(newState, rep(0, 3 - nNew)))
}

# Number of compartments in a PK set
compartmentCount <- function(pk)
{
  if (pk$lambda_2 == 0) return(1)
  if (pk$lambda_3 == 0) return(2)
  3
}

# Amount in each of the set's n compartments (rows) per unit of each of its n
# states (columns); see convertState()
stateToAmount <- function(pk, n)
{
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)[seq_len(n)]
  U <- matrix(pk$v1, nrow = n, ncol = n)
  if (n >= 2) U[2, ] <- pk$v1 * pk$k12 / (pk$k21 - lambda)
  if (n == 3) U[3, ] <- pk$v1 * pk$k13 / (pk$k31 - lambda)
  U
}
