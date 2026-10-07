# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-07, at the request of Steven L. Shafer, who
# set the rule: "For antibiotics, the threshold is the MIC for each drug",
# compared with FREE drug.  The MICs and free fractions were researched and
# independently fact-checked by separate agents against the sources listed
# below; numbers that could only be read from secondary sources say so.
#
# STATUS: verified on R 4.3.3 by tests/testthat/test-antibiotic-thresholds.R,
# which checks this table against drugDefaults_global.csv.
# -----------------------------------------------------------------------------
#
# THE ANTIBIOTICS' TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ===========================================================
# The antibiotics have no effect site, so "time until threshold" times their
# plasma curve (see "Which concentration is timed" in R/recoveryStates.R).  The
# question it answers is: if no more is given now, how long until the FREE
# concentration falls below the MIC?  That is the time left above the MIC
# (fT > MIC), which is when to redose.  Free drug is what reaches the organism,
# and both the susceptibility breakpoints and the PK/PD targets are stated
# against it.
#
# stanpumpR does not simulate protein binding; each model plots one fixed
# quantity.  Cefazolin's source model is written on UNBOUND concentration, so
# its curve is free drug and its threshold is the MIC.  The others plot TOTAL
# drug (what a laboratory reports), so their threshold is the TOTAL
# concentration at which free drug equals the MIC: MIC / fu, with fu the
# unbound fraction AT THAT LOW LEVEL.  Where binding is saturable the free
# fraction rises with concentration, and the value near the threshold is the
# one that matters, so it is the low-concentration value that is used.
#
# The organism is the one the drug is mainly given against in the setting the
# app models (perioperative prophylaxis, or the indication the drug file
# names), and the MIC is the value the PK/PD literature for that drug targets:
# a susceptible breakpoint or the MIC90 of that organism.  It is a default for
# a susceptible isolate.  Against a known isolate the threshold can be edited
# (Settings -> Drug Thresholds), scaling by Threshold / MIC.
#
# The defaults in inst/extdata/drugDefaults_global.csv (endCe) are this
# table's Threshold column; the test keeps them in step, and the drug's help
# page is generated from it (helpDrugPageHTML() in R/help-drugs.R).


#' The antibiotics' MIC thresholds
#'
#' One row per antibiotic: the organism, the MIC (free drug, mg/L), what the
#' model plots, the unbound fraction at MIC-level concentrations, and the
#' resulting threshold on the plotted curve, which is the drug's default
#' \code{endCe}.
#'
#' @returns a data frame of \code{Drug}, \code{Organism}, \code{MIC},
#'   \code{Plotted} ("unbound" or "total"), \code{FreeFraction} (1 when the
#'   curve is unbound), \code{Saturable}, \code{Threshold}, \code{MicSource}
#'   and \code{BindingSource}
#' @keywords internal
antibioticMicTable <- function()
{
  # The sources are written by name, so that each is checked to land on its
  # own drug.
  micSource <- c(
    cefazolin     = paste(
      "MIC: EUCAST S. aureus cefazolin ECOFF 2 mg/L (EUCAST guidance on cephalosporins for S. aureus",
      "infections, 2026); MSSA MIC90 2 mg/L at standard inoculum (Nannini EC et al., Antimicrob Agents",
      "Chemother 2009;53:3437-3441); CLSI Enterobacterales S <= 2 mg/L. The unbound target of Eley VA et",
      "al., Anesth Analg 2020;131:199-207."),
    cefalexin     = paste(
      "MIC: MSSA MIC90 4 mg/L (Haynes AS et al., Microbiol Spectr 2022;10:e01039-22), the target of",
      "Haynes AS et al., Antimicrob Agents Chemother 2024;68:e00182-24. No CLSI or EUCAST cefalexin",
      "breakpoint exists for staphylococci."),
    ceftriaxone   = paste(
      "MIC: CLSI M100, 36th ed., 2026 (Enterobacterales: S <= 1, I 2, R >= 4 mg/L) and EUCAST Clinical",
      "Breakpoint Tables v16.1, 2026 (S <= 1, R > 2 mg/L)."),
    clindamycin   = paste(
      "MIC: CLSI M100 / FDA STIC, Staphylococcus spp. (S <= 0.5, I 1-2, R >= 4 mg/L); EUCAST streptococci",
      "A, B, C, G (S <= 0.5). Diekema DJ et al., Open Forum Infect Dis 2019;6(Suppl 1):S47-S53 (96% of",
      "MSSA susceptible)."),
    gentamicin    = paste(
      "MIC: CLSI M100 (from 2023; FDA-recognised) and EUCAST, Enterobacterales S <= 2 mg/L; MIC90 2 mg/L",
      "in 9,809 US isolates (Sader HS et al., Open Forum Infect Dis 2023;10:ofad058)."),
    metronidazole = paste(
      "MIC: EUCAST Clinical Breakpoint Tables v16.1, 2026 (Bacteroides spp.: S <= 4, R > 4 mg/L), as used",
      "for target attainment by da Silva Neto MJJ et al., J Antimicrob Chemother 2021;76:3212-3219;",
      "wild-type B. fragilis MIC50/MIC90 about 0.5/1 mg/L (Boiten KE et al., J Antimicrob Chemother",
      "2024;79:868-874)."),
    vancomycin    = paste(
      "MIC: Rybak MJ et al., Am J Health Syst Pharm 2020;77:835-864 (the consensus AUC target assumes",
      "an MIC of 1 mg/L); Diekema DJ et al., Open Forum Infect Dis 2019;6(Suppl 1):S47-S53 (SENTRY:",
      "1 mg/L is the modal MIC and MIC90 of MSSA and MRSA).")
  )
  bindingSource <- c(
    cefazolin     = "Free fraction: not needed, the model plots unbound cefazolin (Komatsu T et al., Antimicrob Agents Chemother 2024;68:e00267-24).",
    cefalexin     = paste(
      "Free fraction: 0.85 (10-15% bound, linear): Keflex prescribing information sec. 12.3; Singhvi SM et",
      "al., J Lab Clin Med 1977;89:414-420 (12.4% by ultrafiltration)."),
    ceftriaxone   = paste(
      "Free fraction: Sanz-Codina M et al., J Antimicrob Chemother 2023;78:380-388, the ultrafiltration",
      "binding fit (Kd 23.7 mg/L, capacity back-calculated as 354 mg/L) in the same six men as the plotted",
      "model: free 1 mg/L at 15.3 mg/L total. Saturable; lower with equilibrium dialysis, higher with low",
      "albumin."),
    clindamycin   = paste(
      "Free fraction: Wulkersdorfer B et al., J Antimicrob Chemother 2021;76:2106-2113, a saturable",
      "alpha-1 acid glycoprotein site (Kd 0.85 mg/L in vivo; capacity 12.7 mg/L back-calculated from the",
      "reported AUC ratio): free 0.5 mg/L at 5.2 mg/L total. Son DS et al., J Vet Pharmacol Ther",
      "1998;21:34-40 (human plasma) gives 5.9."),
    gentamicin    = paste(
      "Free fraction: 1 (no serum binding demonstrable by ultrafiltration under physiological",
      "conditions): Gordon RC et al., Antimicrob Agents Chemother 1972;2:214-216."),
    metronidazole = paste(
      "Free fraction: Dorn C et al., J Antimicrob Chemother 2021;76:2114-2120 (0.964 by ultrafiltration",
      "in adults given 0.5 g for abdominal surgical prophylaxis, independent of concentration)."),
    vancomycin    = paste(
      "Free fraction: Dejaco A et al., Antimicrob Agents Chemother 2026;70:e01593-25",
      "(0.72 in 706 samples from 228 adult in-patients at 37 C and pH 7.4, independent of",
      "concentration and albumin; 0.70 recommended for clinical use); Stove V et al.,",
      "Ther Drug Monit 2015;37:180-187 (0.725 by equilibrium dialysis).")
  )

  x <- data.frame(
    Drug = c("cefazolin", "cefalexin", "ceftriaxone", "clindamycin",
             "gentamicin", "metronidazole", "vancomycin"),
    Organism = c(
      "Staphylococcus aureus, methicillin-susceptible (MSSA)",
      "Staphylococcus aureus, methicillin-susceptible (MSSA)",
      "Enterobacterales (E. coli, Klebsiella, Proteus)",
      "Staphylococcus aureus and other staphylococci",
      "Enterobacterales (E. coli, Klebsiella)",
      "Bacteroides fragilis group",
      "Staphylococcus aureus, including MRSA"
    ),
    MIC = c(2, 4, 1, 0.5, 2, 4, 1),
    Plotted = c("unbound", "total", "total", "total", "total", "total", "total"),
    FreeFraction = c(1, 0.85, 0.065, 0.096, 1, 0.96, 0.70),
    Saturable = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    Threshold = c(2, 4.7, 15, 5.2, 2, 4.2, 1.4),
    MicSource = unname(micSource),
    BindingSource = unname(bindingSource),
    stringsAsFactors = FALSE
  )
  stopifnot(identical(names(micSource), x$Drug),
            identical(names(bindingSource), x$Drug))
  x
}


#' One antibiotic's MIC threshold row
#'
#' @param drug a drug name
#' @returns the drug's row of \code{antibioticMicTable()} as a list, or NULL
#'   when the drug is not an antibiotic in the table
#' @keywords internal
antibioticMic <- function(drug)
{
  x <- antibioticMicTable()
  i <- match(drug, x$Drug)
  if (is.na(i)) return(NULL)
  as.list(x[i, ])
}
