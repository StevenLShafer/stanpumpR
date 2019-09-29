# Get the pharmacokinetic and pharmacodynamic value for a drug based on 
# patient covariates
getDrugPK <- function(
  drug = "propofol",
  weight = 70,
  height = 170,
  age = 50,
  sex = "woman",
  drugDefaults
)
{
#  cat("In getDrugPK\n")
  X <- eval(call(drug, weight, height, age, sex))
  tPeak <- X$tPeak 

  events <- names(X$PK)
  for (event in events)
  {
    v1  <- X$PK[[event]]$v1 
    v2  <- X$PK[[event]]$v2 
    v3  <- X$PK[[event]]$v3 
    cl1 <- X$PK[[event]]$cl1 
    cl2 <- X$PK[[event]]$cl2 
    cl3 <- X$PK[[event]]$cl3
    if (is.null(X$PK[[event]]$ka))
    {
      ka <- 0
      bioavailability <- 0
    } else {
      ka <- X$PK[[event]]$ka
      bioavailability <- X$PK[[event]]$bioavailability
    }
    lambda5 <- ka
      
    k10 <- cl1 / v1
    k12 <- cl2 / v1
    k13 <- cl3 / v1
    k21 <- cl2 / v2
    k31 <- cl3 / v3
    
    roots <- cube(k10, k12, k13, k21, k31)
    lambda_1 <- roots[1]
    lambda_2 <- roots[2]
    lambda_3 <- roots[3]
    
    p_coef_bolus_1 <- 0
    p_coef_bolus_2 <- 0
    p_coef_bolus_3 <- 0
    
    p_coef_infusion_1 <- 0
    p_coef_infusion_2 <- 0
    p_coef_infusion_3 <- 0
    
    p_coef_po_1 <- 0
    p_coef_po_2 <- 0
    p_coef_po_3 <- 0
    p_coef_po_4 <- 0
    
    e_coef_bolus_1 <- 0
    e_coef_bolus_2 <- 0
    e_coef_bolus_3 <- 0
    e_coef_bolus_4 <- 0
    e_coef_infusion_1 <- 0
    e_coef_infusion_2 <- 0
    e_coef_infusion_3 <- 0
    e_coef_infusion_4 <- 0
    e_coef_po_1 <- 0
    e_coef_po_2 <- 0
    e_coef_po_3 <- 0
    e_coef_po_4 <- 0
    e_coef_po_5 <- 0
    
    if (k31 > 0)
    {
      p_coef_bolus_1 <- (k21 - lambda_1) * (k31 - lambda_1) /
        (lambda_1 - lambda_2) /
        (lambda_1 - lambda_3) / v1
      p_coef_bolus_2 <- (k21 - lambda_2) * (k31 - lambda_2) /
        (lambda_2 - lambda_1) /
        (lambda_2 - lambda_3) /
        v1
      p_coef_bolus_3 <- (k21 - lambda_3) * (k31 - lambda_3) /
        (lambda_3 - lambda_2) /
        (lambda_3 - lambda_1) /
        v1
    } else {
      if (lambda_2 > 0)
      {
        p_coef_bolus_1 <- (k21 - lambda_1) / (lambda_2 - lambda_1) / v1
        p_coef_bolus_2 <- (k21 - lambda_2) / (lambda_1 - lambda_2) / v1
      } else {
        p_coef_bolus_1 <- 1 / lambda_1 / v1
      }
    }
    
    p_coef_infusion_1 <- p_coef_bolus_1 / lambda_1
    if (lambda_2 > 0) p_coef_infusion_2 <- p_coef_bolus_2 / lambda_2
    if (lambda_3 > 0) p_coef_infusion_3 <- p_coef_bolus_3 / lambda_3
    
    # find ke0 from tPeak
    if (tPeak > 0)
    {
      lambda_4 <- optimize(tPeakError, c(0,200),tPeak,
                           p_coef_bolus_1,
                           p_coef_bolus_2,
                           p_coef_bolus_3,
                           lambda_1,
                           lambda_2,
                           lambda_3
      )$minimum
    } else {
      lambda_4 <- 0
    }
    
    if (lambda_4 > 0)
    {
      e_coef_bolus_1 <- p_coef_bolus_1 / (lambda_4 - lambda_1) * lambda_4
      e_coef_infusion_1 <- e_coef_bolus_1 / lambda_1
      
      if (lambda_2 > 0)
      {
        e_coef_bolus_2 <-  p_coef_bolus_2 / (lambda_4 - lambda_2) * lambda_4
        e_coef_infusion_2 <- e_coef_bolus_2 / lambda_2
      }
      if (lambda_3 > 0)
      {
        e_coef_bolus_3 <- p_coef_bolus_3 / (lambda_4 - lambda_3) * lambda_4
        e_coef_infusion_3 <- if (lambda_3 > 0) e_coef_bolus_3 / lambda_3
      }
      e_coef_bolus_4 <- - e_coef_bolus_1 - e_coef_bolus_2 - e_coef_bolus_3
      e_coef_infusion_4 <- e_coef_bolus_4 / lambda_4
    }
    
    # cat("ka: ", ka, "\n")
    if (ka > 0)
    {
      # cat("bioavailability: ", bioavailability, "\n")
      p_coef_po_1 <- p_coef_bolus_1 / (ka - lambda_1) * ka * bioavailability
      p_coef_po_2 <- p_coef_bolus_2 / (ka - lambda_2) * ka * bioavailability
      p_coef_po_3 <- p_coef_bolus_3 / (ka - lambda_3) * ka * bioavailability
      p_coef_po_4 <- - p_coef_po_1 - p_coef_po_2 - p_coef_po_3
      
      e_coef_po_1 <- e_coef_bolus_1 / (ka - lambda_1) * ka * bioavailability
      e_coef_po_2 <- e_coef_bolus_2 / (ka - lambda_2) * ka * bioavailability
      e_coef_po_3 <- e_coef_bolus_3 / (ka - lambda_3) * ka * bioavailability
      e_coef_po_4 <- e_coef_bolus_4 / (ka - lambda_4) * ka * bioavailability
      e_coef_po_5 <- - e_coef_po_1 - e_coef_po_2 - e_coef_po_3 - e_coef_po_4
      
      # cat("p_coef_po_1", p_coef_po_1, "\n")
      # cat("p_coef_po_2", p_coef_po_2, "\n")
      # cat("p_coef_po_3", p_coef_po_3, "\n")
      # cat("p_coef_po_4", p_coef_po_4, "\n")
    }

    # Vd Peak Effect
    if (tPeak == 0)
    {
      vdPeakEffect <- 0
    } else {
      vdPeakEffect <- 
        1 /
        (
          e_coef_bolus_1 * exp(-lambda_1 * tPeak) +
            e_coef_bolus_2 * exp(-lambda_2 * tPeak) +
            e_coef_bolus_3 * exp(-lambda_3 * tPeak) +
            e_coef_bolus_4 * exp(-lambda_4 * tPeak)
        )
    }
    assign(
      event, 
      list(
        v1 = v1,
        k10 = k10,
        k12 = k12,
        k13 = k13,
        k21 = k21,
        k31 = k31,
        ka = ka,
        bioavailability = bioavailability,
        lambda_1 = lambda_1,
        lambda_2 = lambda_2,
        lambda_3 = lambda_3,
        lambda_4 = lambda_4,
        
        p_coef_bolus_1 = p_coef_bolus_1,
        p_coef_bolus_2 = p_coef_bolus_2,
        p_coef_bolus_3 = p_coef_bolus_3,
        
        p_coef_infusion_1 = p_coef_infusion_1,
        p_coef_infusion_2 = p_coef_infusion_2,
        p_coef_infusion_3 = p_coef_infusion_3,
        
        p_coef_po_1 = p_coef_po_1,
        p_coef_po_2 = p_coef_po_2,
        p_coef_po_3 = p_coef_po_3,
        p_coef_po_4 = p_coef_po_4,
        
        
        e_coef_bolus_1 = e_coef_bolus_1,
        e_coef_bolus_2 = e_coef_bolus_2,
        e_coef_bolus_3 = e_coef_bolus_3,
        e_coef_bolus_4 = e_coef_bolus_4,
        
        e_coef_infusion_1 = e_coef_infusion_1,
        e_coef_infusion_2 = e_coef_infusion_2,
        e_coef_infusion_3 = e_coef_infusion_3,
        e_coef_infusion_4 = e_coef_infusion_4,
        
        e_coef_po_1 = e_coef_po_1,
        e_coef_po_2 = e_coef_po_2,
        e_coef_po_3 = e_coef_po_3,
        e_coef_po_4 = e_coef_po_4,
        e_coef_po_5 = e_coef_po_5
      )
    )
  }
  
  PK <- sapply(events, function(x) list(get0(x)))
  thisDrug <- which(drugDefaults$Drug == drug)
  #  cat("Leaving getDrugPK\n")

  return(
    list(
      drug = drug,
      PK = PK,
      tPeak = tPeak,
      pkEvents = events,
      reference = "Not Available",
      weight = weight,
      height = height,
      age = age,
      sex = sex,
      upperTypical        = drugDefaults$Upper[thisDrug],
      lowerTypical        = drugDefaults$Lower[thisDrug],
      typical             = drugDefaults$Typical[thisDrug],
      MEAC                = drugDefaults$MEAC[thisDrug],
      Concentration.Units = drugDefaults$Concentration.Units[thisDrug],
      Bolus.Units         = drugDefaults$Bolus.Units[thisDrug],
      Infusion.Units      = drugDefaults$Infusion.Units[thisDrug],
      Units               = drugDefaults$Units[thisDrug],
      Default.Units       = drugDefaults$Default.Units[thisDrug],
      emerge              = drugDefaults$Emerge[thisDrug]
    )
  )
}
