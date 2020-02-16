#Convert state variables from one PK set to another PK set
convertState <- function(oldState, oldPK, newPK)
{
  if (oldPK$lambda_2 == 0)
  {
    return(state = oldState) # In a one compartment model, state variable doesn't change
  }

  if (oldPK$lambda_3 == 0)
  {
    a1 <- (oldState[1] + oldState[2]) * oldPK$v1

    a2 <- (
      oldState[1] / (oldPK$k21 - oldPK$lambda_1) * oldPK$k21 +
      oldState[2] / (oldPK$k21 - oldPK$lambda_2) * oldPK$k21
    ) * oldPK$v2
    f1 <- newPK$k21 / (newPK$k21 - newPK$lambda_1)
    f2 <- newPK$k21 / (newPK$k21 - newPK$lambda_2)
    newState2 = (a2 / newPK$v2 -a1 / newPK$v1 * f1) / (f2 - f1)
    newState1 = a1 / newPK$v1 - newState2
    return(state = c(newState1, newState2, 0))
  }

  a1 <- (oldState[1] + oldState[2] + oldState[3]) * oldPK$v1

  a2 <- (
    oldState[1] / (oldPK$k21 - oldPK$lambda_1) * oldPK$k21 +
    oldState[2] / (oldPK$k21 - oldPK$lambda_2) * oldPK$k21 +
    oldState[3] / (oldPK$k21 - oldPK$lambda_3) * oldPK$k21
    ) * oldPK$v2

  a3 <- (
    oldState[1] / (oldPK$k31 - oldPK$lambda_1) * oldPK$k31 +
    oldState[2] / (oldPK$k31 - oldPK$lambda_2) * oldPK$k31 +
    oldState[3] / (oldPK$k31 - oldPK$lambda_3) * oldPK$k31
    ) * (oldPK$v1 * oldPK$k13 / oldPK$k31) # oldPK$v3


  # Set up intermediate variables
  f1 = newPK$v2 * newPK$k21 / (newPK$k21 - newPK$lambda_1)
  f2 = newPK$v2 * newPK$k21 / (newPK$k21 - newPK$lambda_2)
  f3 = newPK$v2 * newPK$k21 / (newPK$k21 - newPK$lambda_3)
  f4 = newPK$v3 * newPK$k31 / (newPK$k31 - newPK$lambda_1)
  f5 = newPK$v3 * newPK$k31 / (newPK$k31 - newPK$lambda_2)
  f6 = newPK$v3 * newPK$k31 / (newPK$k31 - newPK$lambda_3)
  f7 = f5 / f4
  f8 = f6 / f4
  f9 = a3 / f4
  f10 = f1 * f9
  f11 = f1 * f7
  f12 = f1 * f8
  f13 = (f3 - f12) / (f11 - f2)
  f14 = (f10 - a2) / (f11 - f2)
  f15 = a1 / newPK$v1 - f9 - f14 + f7 * f14
  f16 = 1 + f13 - f7 * f13 - f8

  newState3 = f15 / f16
  newState2 = newState3 * f13 + f14
  newState1 = a1 / newPK$v1 - newState2 - newState3

  return(state = c(newState1, newState2, newState3))
}
