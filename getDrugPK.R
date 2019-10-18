# Get the pharmacokinetic and pharmacodynamic value for a drug based on 
# patient covariates
getDrugPK <- function(
  drug = "propofol",
  weight = 70,
  height = 170,
  age = 50,
  sex = "male",
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
    
    # Note on Oral, IM, and IN route PK #
    # Oral is (for now) state 4 for plasma and state 5 for effect site #
    # IM is state 5 for plasma and state 6 for effect site #
    # IN is state 6 for plasma and state 7 for effect site #
    # Plan to change these to make the code clearer:       #
    # For IV, states 1-3 for plasma, states 1-4 for effect site #
    # PO will add state_PO, associated with ka_PO #
    # IM will add state_IM, associated with ka_IM #
    # IN will add state_IN, associated with ka_IN #
    
    # Set up PK for oral delivery
    if (is.null(X$PK[[event]]$ka_PO))
    {
      ka_PO <- 0
      bioavailability_PO <- 0
      tlag_PO <- 0
    } else {
      ka_PO <- X$PK[[event]]$ka_PO
      if (is.null(X$PK[[event]]$bioavailability_PO))
      {
        bioavailability_PO <- 1
      } else {
        bioavailability_PO <- X$PK[[event]]$bioavailability_PO
      }
      if (is.null(X$PK[[event]]$tlag_PO))
      {
        tlag_PO <- 0
      } else {
        tlag_PO <- X$PK[[event]]$tlag_PO
      }
    }
    
    # Set up PK for IM delivery
    if (is.null(X$PK[[event]]$ka_IM))
    {
      ka_IM <- 0
      bioavailability_IM <- 0
      tlag_IM <- 0
    } else {
      ka_IM <- X$PK[[event]]$ka_IM
      if (is.null(X$PK[[event]]$bioavailability_IM))
      {
        bioavailability_IM <- 1
      } else {
        bioavailability_IM <- X$PK[[event]]$bioavailability_IM
      }
      if (is.null(X$PK[[event]]$tlag_IM))
      {
        tlag_IM <- 0
      } else {
        tlag_IM <- X$PK[[event]]$tlag_IM
      }
    }
    
    # Set up PK for IM delivery
    if (is.null(X$PK[[event]]$ka_IN))
    {
      ka_IN <- 0
      bioavailability_IN <- 0
      tlag_IN <- 0
    } else {
      ka_IN <- X$PK[[event]]$ka_IN
      if (is.null(X$PK[[event]]$bioavailability_IN))
      {
        bioavailability_IN <- 1
      } else {
        bioavailability_IN <- X$PK[[event]]$bioavailability_IN
      }
      if (is.null(X$PK[[event]]$tlag_IN))
      {
        tlag_IN <- 0
      } else {
        tlag_IN <- X$PK[[event]]$tlag_IN
      }
    }
    
    if (is.null(X$PK[[event]]$customFunction))
    {
      customFunction <- ""
    } else {
      customFunction <- X$PK[[event]]$customFunction
    }
    
    k10 <- cl1 / v1
    k12 <- cl2 / v1
    k13 <- cl3 / v1
    k21 <- cl2 / v2
    k31 <- cl3 / v3
    
    roots <- cube(k10, k12, k13, k21, k31)
    lambda_1 <- roots[1]
    lambda_2 <- roots[2]
    lambda_3 <- roots[3]
    
    # Bolus Delivery
    p_coef_bolus_l1  <- 0
    p_coef_bolus_l2  <- 0
    p_coef_bolus_l3  <- 0

    e_coef_bolus_l1  <- 0
    e_coef_bolus_l2  <- 0
    e_coef_bolus_l3  <- 0
    e_coef_bolus_ke0 <- 0
    
    # Infusion delivery
    p_coef_infusion_l1  <- 0
    p_coef_infusion_l2  <- 0
    p_coef_infusion_l3  <- 0
    
    e_coef_infusion_l1  <- 0
    e_coef_infusion_l2  <- 0
    e_coef_infusion_l3  <- 0
    e_coef_infusion_ke0 <- 0
    
    # PO Delivery
    p_coef_PO_l1  <- 0
    p_coef_PO_l2  <- 0
    p_coef_PO_l3  <- 0
    p_coef_PO_ka <- 0
    
    e_coef_PO_l1  <- 0
    e_coef_PO_l2  <- 0
    e_coef_PO_l3  <- 0
    e_coef_PO_ke0 <- 0
    e_coef_PO_ka  <- 0
    
    # IM Delivery
    p_coef_IM_l1  <- 0
    p_coef_IM_l2  <- 0
    p_coef_IM_l3  <- 0
    p_coef_IM_ka  <- 0
    
    e_coef_IM_l1  <- 0
    e_coef_IM_l2  <- 0
    e_coef_IM_l3  <- 0
    e_coef_IM_ke0 <- 0
    e_coef_IM_ka  <- 0
    
    # IN Delivery
    p_coef_IN_l1  <- 0
    p_coef_IN_l2  <- 0
    p_coef_IN_l3  <- 0
    p_coef_IN_ka  <- 0

    e_coef_IN_l1  <- 0
    e_coef_IN_l2  <- 0
    e_coef_IN_l3  <- 0
    e_coef_IN_ke0 <- 0
    e_coef_IN_ka  <- 0

    if (k31 > 0)
    {
      p_coef_bolus_l1 <- (k21 - lambda_1) * (k31 - lambda_1) /
        (lambda_1 - lambda_2) /
        (lambda_1 - lambda_3) / v1
      p_coef_bolus_l2 <- (k21 - lambda_2) * (k31 - lambda_2) /
        (lambda_2 - lambda_1) /
        (lambda_2 - lambda_3) /
        v1
      p_coef_bolus_l3 <- (k21 - lambda_3) * (k31 - lambda_3) /
        (lambda_3 - lambda_2) /
        (lambda_3 - lambda_1) /
        v1
    } else {
      if (lambda_2 > 0)
      {
        p_coef_bolus_l1 <- (k21 - lambda_1) / (lambda_2 - lambda_1) / v1
        p_coef_bolus_l2 <- (k21 - lambda_2) / (lambda_1 - lambda_2) / v1
      } else {
        p_coef_bolus_l1 <- 1 / lambda_1 / v1
      }
    }
    
    p_coef_infusion_l1 <- p_coef_bolus_l1 / lambda_1
    if (lambda_2 > 0) p_coef_infusion_l2 <- p_coef_bolus_l2 / lambda_2
    if (lambda_3 > 0) p_coef_infusion_l3 <- p_coef_bolus_l3 / lambda_3
    
    # find ke0 from tPeak
    if (tPeak > 0)
    {
      ke0 <- optimize(tPeakError, c(0,200),tPeak,
                           p_coef_bolus_l1,
                           p_coef_bolus_l2,
                           p_coef_bolus_l3,
                           lambda_1,
                           lambda_2,
                           lambda_3
      )$minimum
    } else {
      ke0 <- 0
    }
    
    if (ke0 > 0)
    {
      e_coef_bolus_l1 <- p_coef_bolus_l1 / (ke0 - lambda_1) * ke0
      e_coef_infusion_l1 <- e_coef_bolus_l1 / lambda_1
      
      if (lambda_2 > 0)
      {
        e_coef_bolus_l2 <-  p_coef_bolus_l2 / (ke0 - lambda_2) * ke0
        e_coef_infusion_l2 <- e_coef_bolus_l2 / lambda_2
      }
      if (lambda_3 > 0)
      {
        e_coef_bolus_l3 <- p_coef_bolus_l3 / (ke0 - lambda_3) * ke0
        e_coef_infusion_l3 <- if (lambda_3 > 0) e_coef_bolus_l3 / lambda_3
      }
      e_coef_bolus_ke0 <- - e_coef_bolus_l1 - e_coef_bolus_l2 - e_coef_bolus_l3
      e_coef_infusion_ke0 <- e_coef_bolus_ke0 / ke0
    }
    
    # cat("ka PO: ", ka_PO, "\n")
    if (ka_PO > 0)
    {
      p_coef_PO_l1   <- p_coef_bolus_l1 / (ka_PO - lambda_1) * ka_PO * bioavailability_PO
      p_coef_PO_l2   <- p_coef_bolus_l2 / (ka_PO - lambda_2) * ka_PO * bioavailability_PO
      p_coef_PO_l3   <- p_coef_bolus_l3 / (ka_PO - lambda_3) * ka_PO * bioavailability_PO
      p_coef_PO_ka   <- - p_coef_PO_l1 - p_coef_PO_l2 - p_coef_PO_l3

      e_coef_PO_l1   <- e_coef_bolus_l1 / (ka_PO - lambda_1) * ka_PO * bioavailability_PO
      e_coef_PO_l2   <- e_coef_bolus_l2 / (ka_PO - lambda_2) * ka_PO * bioavailability_PO
      e_coef_PO_l3   <- e_coef_bolus_l3 / (ka_PO - lambda_3) * ka_PO * bioavailability_PO
      e_coef_PO_ke0  <- e_coef_bolus_ke0 / (ka_PO - ke0)     * ka_PO * bioavailability_PO
      e_coef_PO_ka   <- - e_coef_PO_l1 - e_coef_PO_l2 - e_coef_PO_l3 - e_coef_PO_ke0
    }

    if (ka_IM > 0)
    {
      p_coef_IM_l1  <- p_coef_bolus_l1 / (ka_IM - lambda_1) * ka_IM * bioavailability_IM
      p_coef_IM_l2  <- p_coef_bolus_l2 / (ka_IM - lambda_2) * ka_IM * bioavailability_IM
      p_coef_IM_l3  <- p_coef_bolus_l3 / (ka_IM - lambda_3) * ka_IM * bioavailability_IM
      p_coef_IM_ka  <- - p_coef_IM_l1 - p_coef_IM_l2 - p_coef_IM_l3
      
      e_coef_IM_l1  <- e_coef_bolus_l1 / (ka_IM - lambda_1) * ka_IM * bioavailability_IM
      e_coef_IM_l2  <- e_coef_bolus_l2 / (ka_IM - lambda_2) * ka_IM * bioavailability_IM
      e_coef_IM_l3  <- e_coef_bolus_l3 / (ka_IM - lambda_3) * ka_IM * bioavailability_IM
      e_coef_IM_ke0 <- e_coef_bolus_ke0 / (ka_IM - ke0)    * ka_IM * bioavailability_IM
      e_coef_IM_ka  <- - e_coef_IM_l1 - e_coef_IM_l2 - e_coef_IM_l3 - e_coef_IM_ke0
    }
    
    if (ka_IN > 0)
    {
      p_coef_IN_l1  <- p_coef_bolus_l1 / (ka_IN - lambda_1) * ka_IN * bioavailability_IN
      p_coef_IN_l2  <- p_coef_bolus_l2 / (ka_IN - lambda_2) * ka_IN * bioavailability_IN
      p_coef_IN_l3  <- p_coef_bolus_l3 / (ka_IN - lambda_3) * ka_IN * bioavailability_IN
      p_coef_IN_ka  <- - p_coef_IN_l1 - p_coef_IN_l2 - p_coef_IN_l3
      
      e_coef_IN_l1  <- e_coef_bolus_l1 / (ka_IN - lambda_1) * ka_IN * bioavailability_IN
      e_coef_IN_l2  <- e_coef_bolus_l2 / (ka_IN - lambda_2) * ka_IN * bioavailability_IN
      e_coef_IN_l3  <- e_coef_bolus_l3 / (ka_IN - lambda_3) * ka_IN * bioavailability_IN
      e_coef_IN_ke0 <- e_coef_bolus_ke0 / (ka_IN - ke0) *     ka_IN * bioavailability_IN
      e_coef_IN_ka  <- - e_coef_IN_l1 - e_coef_IN_l2 - e_coef_IN_l3 - e_coef_IN_ke0
    }
    
    # Vd Peak Effect
    if (tPeak == 0)
    {
      vdPeakEffect <- 0
    } else {
      vdPeakEffect <- 
        1 /
        (
          e_coef_bolus_l1 * exp(-lambda_1 * tPeak) +
            e_coef_bolus_l2 * exp(-lambda_2 * tPeak) +
            e_coef_bolus_l3 * exp(-lambda_3 * tPeak) +
            e_coef_bolus_ke0 * exp(-ke0 * tPeak)
        )
    }
    assign(
      event, 
      list(
        v1 = v1,
        v2 = v2,
        v3 = v3,
        cl1 = cl1,
        cl2 = cl2,
        cl3 = cl3,
        k10 = k10,
        k12 = k12,
        k13 = k13,
        k21 = k21,
        k31 = k31,
        
        ka_PO = ka_PO,
        bioavailability_PO = bioavailability_PO,
        tlag_PO = tlag_PO,
        
        ka_IM = ka_IM,
        bioavailability_IM = bioavailability_IM,
        tlag_IM = tlag_IM,
        
        ka_IN = ka_IN,
        bioavailability_IN = bioavailability_IN,
        tlag_IN = tlag_IN,
        
        customFunction = customFunction,

        lambda_1 = lambda_1,
        lambda_2 = lambda_2,
        lambda_3 = lambda_3,
        ke0 = ke0,
        
        # Bolus Coefficients
        p_coef_bolus_l1 = p_coef_bolus_l1,
        p_coef_bolus_l2 = p_coef_bolus_l2,
        p_coef_bolus_l3 = p_coef_bolus_l3,
        
        e_coef_bolus_l1 = e_coef_bolus_l1,
        e_coef_bolus_l2 = e_coef_bolus_l2,
        e_coef_bolus_l3 = e_coef_bolus_l3,
        e_coef_bolus_ke0 = e_coef_bolus_ke0,
        

        # Infusion Coefficients
        p_coef_infusion_l1 = p_coef_infusion_l1,
        p_coef_infusion_l2 = p_coef_infusion_l2,
        p_coef_infusion_l3 = p_coef_infusion_l3,
        
        e_coef_infusion_l1 = e_coef_infusion_l1,
        e_coef_infusion_l2 = e_coef_infusion_l2,
        e_coef_infusion_l3 = e_coef_infusion_l3,
        e_coef_infusion_ke0 = e_coef_infusion_ke0,
        
        # PO Coefficients
        p_coef_PO_l1 = p_coef_PO_l1,
        p_coef_PO_l2 = p_coef_PO_l2,
        p_coef_PO_l3 = p_coef_PO_l3,
        p_coef_PO_ka = p_coef_PO_ka,
        
        e_coef_PO_l1 = e_coef_PO_l1,
        e_coef_PO_l2 = e_coef_PO_l2,
        e_coef_PO_l3 = e_coef_PO_l3,
        e_coef_PO_ke0 = e_coef_PO_ke0,
        e_coef_PO_ka = e_coef_PO_ka,
        
        # IM Coefficients
        p_coef_IM_l1 = p_coef_IM_l1,
        p_coef_IM_l2 = p_coef_IM_l2,
        p_coef_IM_l3 = p_coef_IM_l3,
        p_coef_IM_ka = p_coef_IM_ka,
        
        e_coef_IM_l1 = e_coef_IM_l1,
        e_coef_IM_l2 = e_coef_IM_l2,
        e_coef_IM_l3 = e_coef_IM_l3,
        e_coef_IM_ke0 = e_coef_IM_ke0,
        e_coef_IM_ka = e_coef_IM_ka,
        
        # IN Coefficients
        p_coef_IN_l1 = p_coef_IN_l1,
        p_coef_IN_l2 = p_coef_IN_l2,
        p_coef_IN_l3 = p_coef_IN_l3,
        p_coef_IN_ka = p_coef_IN_ka,

        e_coef_IN_l1 = e_coef_IN_l1,
        e_coef_IN_l2 = e_coef_IN_l2,
        e_coef_IN_l3 = e_coef_IN_l3,
        e_coef_IN_ke0 = e_coef_IN_ke0,
        e_coef_IN_ka = e_coef_IN_ka
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
