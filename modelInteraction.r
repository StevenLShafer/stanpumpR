# model the interaction of an opioid with propofol
modelInteraction <- function(propofol, opioid)
{
  # Bouillon Anesthesiology 2004, v100, p1360, Table 4, Bayesian Predicted
  ce50Remi <- 1.01
  ce50Prop <- 6.68
  steepnessRemi <- 0.72
  steepnessProp <- 6.9
  preopioidIntensity <- 0.83  # Laryngoscopy
  
  # normalize all opioid to remi equivalents 
  remiMEAC <- 1  # Yes, this makes it pretty easy, but still necessary
  opioid <- opioid * remiMEAC # Convert opioid to remi equivalents
  
  opioid <- opioid^steepnessRemi
  propofol <- propofol^steepnessProp
  
  # Both drugs together
  postopioidIntensity <- preopioidIntensity * 
    (1 - opioid / (opioid + (ce50Remi * preopioidIntensity)^steepnessRemi))
  pNR <- 1 - propofol / (propofol + (ce50Prop * postopioidIntensity)^steepnessProp)
  
  # propofol only
  pNRpropofol <- 1 - propofol / (propofol + (ce50Prop * preopioidIntensity)^steepnessProp)
  
  # opioid only (have to have some propofol, or they will respond, based on the model)
  pNRopioid <- 1 
  
  return(
    list(
      pNR = pNR, 
      pNRpropofol = pNRpropofol,
      pNRopioid = pNRopioid
    )
  )
}
