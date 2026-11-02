# -----------------------------------------------------------------------------
# Build inst/extdata/antiseizureInteractions.csv
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-24, at the request of Steven L. Shafer, who
# asked for the literature on the PHARMACOKINETIC interactions among the
# epilepsy drugs, a model of them that can be switched on and off, and the
# list of papers to obtain.  The model is R/antiseizureInteractions.R; the
# help page inst/help/models/antiseizure-interactions.md.
#
# The search was run on 2026-10-24 against PubMed (abstracts) and PubMed
# Central (full text).  Every PMID below was checked against PubMed in this
# session.  `evidence_read` says what was read: "abstract", "full text", or
# "abstract; full text not in PMC".  `to_obtain` is "yes" where the full text
# would firm up a number or supply one the abstract omits; those papers are the
# reading list on the help page.
#
# status
#   applied     the simulation applies this row when the box is ticked and
#               both drugs are dosed
#   crosscheck  a second source for an applied row, or the source of its
#               shape or time course; never applied itself
#   recorded    an interaction the literature describes that the simulation
#               does not apply: unquantified, a victim or perpetrator outside
#               the library, a moiety not plotted (an epoxide, the unbound
#               drug), or a magnitude only the product label gives
#
# The numeric columns of an applied row:
#   parameter   CL (the elimination clearance, cl1, or CL/F), or for
#               phenytoin, Vmax or Km (total)
#   fold        the published factor on that parameter in patients on an
#               ordinary maintenance dose of the perpetrator
#   c_ref       the perpetrator concentration, mcg/mL, at which `fold` holds:
#               the typical value of the perpetrator's band
#   ec50        the perpetrator concentration of half the maximal effect,
#               mcg/mL; `ec50_basis` says whether a source gave it or it is
#               the lower edge of the perpetrator's band (the default; see
#               the header of R/antiseizureInteractions.R)
#   onset       "turnover" (induction, through the enzyme turnover model) or
#               "immediate" (inhibition, binding displacement)
#   turnover_half_life_h  the induced enzymes' degradation half-life, 86.1 h
#               (Punyawudho 2009, PMID 19566114), for every turnover row
#
# Run with: LC_ALL=C.UTF-8 Rscript data-raw/antiseizureInteractions.R
# Checked by tests/testthat/test-antiseizure-interactions.R.
# -----------------------------------------------------------------------------

STATUSES <- c("applied", "crosscheck", "recorded")
TURNOVER_HALF_LIFE_H <- 86.1

row <- function(perpetrator, victim, status, mechanism, enzyme,
                parameter = NA, fold = NA, c_ref = NA, ec50 = NA,
                ec50_basis = NA, onset = NA, finding, population = NA,
                source, source_short, pmid, pmcid = NA, doi = NA,
                evidence_read, to_obtain = "no", notes = NA)
  data.frame(
    perpetrator, victim, status, mechanism, enzyme, parameter, fold, c_ref,
    ec50, ec50_basis, onset,
    turnover_half_life_h = if (identical(onset, "turnover")) TURNOVER_HALF_LIFE_H else NA,
    finding, population, source, source_short, pmid, pmcid, doi,
    evidence_read, to_obtain, notes, stringsAsFactors = FALSE
  )

# The perpetrators' reference concentrations and the default EC50 (the lower
# edge of the band), from the library's drug models.
REF <- list(
  phenytoin     = c(c_ref = 15, ec50 = 10),
  carbamazepine = c(c_ref = 8,  ec50 = 4),
  valproate     = c(c_ref = 75, ec50 = 50),
  phenobarbital = c(c_ref = 20, ec50 = 10),
  zonisamide    = c(c_ref = 20, ec50 = 10),
  ethosuximide  = c(c_ref = 70, ec50 = 40)
)
LOWER_EDGE <- "assumed: lower edge of the perpetrator's reference range"

applied <- function(perpetrator, victim, mechanism, enzyme, parameter, fold,
                    onset, ..., ec50 = REF[[perpetrator]][["ec50"]],
                    ec50_basis = LOWER_EDGE)
  row(perpetrator, victim, "applied", mechanism, enzyme, parameter, fold,
      c_ref = REF[[perpetrator]][["c_ref"]], ec50 = ec50,
      ec50_basis = ec50_basis, onset = onset, ...)

RIVAS <- list(source = paste0(
  "Rivas N, Buelga DS, Elger CE, et al. Population pharmacokinetics of ",
  "lamotrigine with data from therapeutic drug monitoring in German and ",
  "Spanish patients with epilepsy. Ther Drug Monit 2008;30:483-489."),
  source_short = "Rivas 2008", pmid = "18641550",
  doi = "10.1097/FTD.0b013e31817fd4d4",
  population = "600 adults with epilepsy, 1699 steady-state levels (TDM, Germany and Spain)",
  evidence_read = "full text (read 2026-10-24): Table 3 gives each coefficient's SE%; co-medication coded 0/1, multiplicative in the exponentials")
TDS22 <- list(source = paste0(
  "Teixeira-da-Silva P, Perez-Blanco JS, Santos-Buelga D, Otero MJ, Garcia ",
  "MJ. Population pharmacokinetics of valproic acid in pediatric and adult ",
  "Caucasian patients. Pharmaceutics 2022;14:811."),
  source_short = "Teixeira-da-Silva 2022", pmid = "35456645",
  pmcid = "PMC9031051", doi = "10.3390/pharmaceutics14040811",
  population = "836 patients 0.1-93 y, 1751 levels; external set 368 patients",
  evidence_read = "full text")
GRAVES <- list(source = paste0(
  "Graves NM, Brundage RC, Wen Y, et al. Population pharmacokinetics of ",
  "carbamazepine in adults with epilepsy. Pharmacotherapy 1998;18:273-281."),
  source_short = "Graves 1998", pmid = "9545146",
  population = "829 adults on maintenance carbamazepine; validation set 50",
  evidence_read = "full text (read 2026-10-24): Table 4 gives the 95% CIs; the co-medication terms are 0/1 indicators")
TDS20 <- list(source = paste0(
  "Teixeira-da-Silva P, Santos-Buelga D, Otero MJ, Garcia MJ. Population ",
  "pharmacokinetics of phenobarbital in Caucasian patients with epilepsy. ",
  "Eur J Pharm Sci 2020;153:105484."),
  source_short = "Teixeira-da-Silva 2020", pmid = "32707172",
  doi = "10.1016/j.ejps.2020.105484",
  population = "395 patients over 15 y, 855 trough levels; external set 107",
  evidence_read = "abstract (the final equation is in it); full text not in PMC")
BAMGBOYE <- list(source = paste0(
  "Bamgboye et al. Topiramate pharmacokinetics after a stable-isotope-labelled ",
  "intravenous dose in adults. J Clin Pharmacol 2026."),
  source_short = "Bamgboye 2026", pmid = "41987705", pmcid = "PMC13084296",
  doi = "10.1002/jcph.70191",
  population = "20 adults with epilepsy or migraine, 246 samples",
  evidence_read = "full text (the library's topiramate source; read when the model was built)")
SAMARA <- list(source = paste0(
  "Samara EE, Gustavson LE, El-Shourbagy T, et al. Population analysis of ",
  "the pharmacokinetics of tiagabine in patients with epilepsy. Epilepsia ",
  "1998;39:868-873."),
  source_short = "Samara 1998", pmid = "9701378",
  doi = "10.1111/j.1528-1157.1998.tb01182.x",
  population = "511 patients, 2147 samples",
  evidence_read = "full text (read 2026-10-24): central CL/F 21.4 L/h induced vs 12.8 noninduced (CV 28%); inducers explicitly NOT additive")
FALCAO <- list(source = paste0(
  "Falcao A, Fuseau E, Nunes T, Almeida L, Soares-da-Silva P. ",
  "Pharmacokinetics, drug interactions and exposure-response relationship of ",
  "eslicarbazepine acetate in adult patients with partial-onset seizures. ",
  "CNS Drugs 2012;26:79-91."),
  source_short = "Falcao 2012", pmid = "22171585",
  doi = "10.2165/11596290-000000000-00000",
  population = "641 phase III patients",
  evidence_read = "abstract (the final equation is in it); full text not in PMC")
SILVA <- list(source = paste0(
  "Silva R, Colom H, Almeida A, et al. A new population pharmacokinetic model ",
  "for dosing optimization of zonisamide in patients with refractory epilepsy. ",
  "Eur J Pharm Sci 2025;207:107023."),
  source_short = "Silva 2025", pmid = "39848412",
  doi = "10.1016/j.ejps.2025.107023",
  population = "64 European adults with refractory epilepsy, 114 levels",
  evidence_read = "full text (read 2026-10-25): Table 2 gives INDDL_CL 1.63 (RSE 11.9%, bootstrap 1.33-2.27); CL/F = 0.761 x 1.63^INDDL, verified against the paper's +28/108/239% enhancements at INDDL 0.5/1.5/2.5")
RIVA96 <- list(source = paste0(
  "Riva R, Albani F, Contin M, Baruzzi A. Pharmacokinetic interactions between ",
  "antiepileptic drugs. Clinical considerations. Clin Pharmacokinet ",
  "1996;31:470-493."),
  source_short = "Riva 1996", pmid = "8968658",
  doi = "10.2165/00003088-199631060-00005",
  population = "review of the primary studies (Riva's refs 171-173)",
  evidence_read = "full text (read 2026-10-25): section 3.8 - inducers accelerate ethosuximide clearance; the ethosuximide dose must be 20-25% higher on carbamazepine/phenytoin/phenobarbital/primidone")
GIACCONE <- list(source = paste0(
  "Giaccone M, Bartoli A, Gatti G, et al. Effect of enzyme inducing ",
  "anticonvulsants on ethosuximide pharmacokinetics in epileptic patients. ",
  "Br J Clin Pharmacol 1996;41:575-579."),
  source_short = "Giaccone 1996", pmid = "8799524", pmcid = "PMC2042613",
  doi = "10.1046/j.1365-2125.1996.03601.x",
  population = "10 epileptic patients on inducers vs 12 healthy controls",
  evidence_read = "primary abstract (read 2026-10-26): CL/F 15.3 vs 9.2 ml/kg/h (x1.66), half-life 29 vs 54 h; inducers POOLED (phenobarbital/phenytoin/carbamazepine), so not a per-drug coefficient")
PISANI <- list(source = paste0(
  "Pisani F, Narbone MC, Trunfio C, et al. Valproic acid-ethosuximide ",
  "interaction: a pharmacokinetic study. Epilepsia 1984;25:229-233."),
  source_short = "Pisani 1984", pmid = "6423377",
  doi = "10.1111/j.1528-1157.1984.tb04180.x",
  population = "6 healthy volunteers, single-dose ethosuximide before/during valproate",
  evidence_read = "primary abstract (read 2026-10-26): total clearance 11.2 -> 9.5 ml/min (x0.85), half-life 44 -> 54 h; 2 of 6 showed no interaction")
HAGEMANN <- list(source = paste0(
  "Hagemann A, Herting A, Klimpel D, Bien CG, Polster T. Ethosuximide lowers ",
  "lamotrigine serum concentrations: evidence for a clinically relevant ",
  "interaction. Epilepsia 2024;65:e73-e78."),
  source_short = "Hagemann 2024", pmid = "38606683",
  doi = "10.1111/epi.17952",
  population = "523 samples, 209 patients (generalized estimating equations)",
  evidence_read = "full text (read 2026-10-26): Table 2 - ethosuximide lowers lamotrigine 14% in adults (e1 0.86, CL x1.16) and 37% in children (CL x1.59), via UGT1A4 induction")

app <- function(perpetrator, victim, mechanism, enzyme, parameter, fold,
                onset, src, finding, ..., to_obtain = "no", notes = NA)
  do.call(applied, c(list(perpetrator, victim, mechanism, enzyme, parameter,
                          fold, onset, finding = finding, to_obtain = to_obtain,
                          notes = notes), src, list(...)))

registry <- rbind(
  # --- Lamotrigine (victim) --------------------------------------------------
  app("phenytoin", "lamotrigine", "induction", "UGT1A4/UGT2B7", "CL",
      round(exp(0.663), 3), "turnover", RIVAS,
      "CL x e^0.663 = 1.94 with phenytoin (0.045-0.070 against 0.028 L/h/kg in monotherapy). Coefficient 0.663, SE 10.22% (Table 3).",
      to_obtain = "no",
      notes = "Read in full 2026-10-24. Lamotrigine half-life falls from 23-37 h to 13.5-15 h with an inducer (Rambeck 1993, PMID 8119045)."),
  app("phenobarbital", "lamotrigine", "induction", "UGT1A4/UGT2B7", "CL",
      round(exp(0.588), 3), "turnover", RIVAS,
      "CL x e^0.588 = 1.80 with phenobarbital or primidone. Coefficient 0.588, SE 8.66% (Table 3).",
      to_obtain = "no"),
  app("carbamazepine", "lamotrigine", "induction", "UGT1A4/UGT2B7", "CL",
      round(exp(0.467), 3), "turnover", RIVAS,
      "CL x e^0.467 = 1.60 with carbamazepine (coefficient 0.467, SE 29.6% - the least precise term, retained as a significant inducer); two or more inducers x e^0.864 = 2.37 (SE 12.17%).",
      to_obtain = "no",
      notes = "Read in full 2026-10-24. The two-inducer term 2.37 is the combination-rule data point: additive excess (3) gives 2.54 for phenytoin+carbamazepine and multiplicative 3.10, so additive excess is closer but ~7% above Rivas's own term. Rivas models inhibition x induction multiplicatively and reports they balance near monotherapy, which supports the model's multiply-inhibition-against-combined-induction rule."),
  app("valproate", "lamotrigine", "inhibition", "UGT1A4/UGT2B7", "CL",
      round(exp(-0.713), 3), "immediate", RIVAS,
      "CL x e^-0.713 = 0.49 with valproate (0.014 against 0.028 L/h/kg); coefficient -0.713, SE 7.66% (Table 3).",
      ec50 = 5.6,
      ec50_basis = "Gidal 2003 (PMID 15013050) Table 2: EC50 5.6 mcg/mL (SE 2.3, 95% CI 0.8-10.4), Emax 64.9% inhibition (SE 7.2, CI 50.1-79.7)",
      to_obtain = "no",
      notes = "Magnitude from Rivas (n = 600, categorical), shape from Gidal 2003 (both read in full 2026-10-24). REVIEW POINT: anchoring Fmax to Rivas's 0.49 at VPA 75 mcg/mL with EC50 5.6 gives a maximal inhibition of 54.8% (Fmax 0.452), shallower than Gidal's directly fitted Emax of 64.9% (fold 0.351). The two sources are Rivas TDM patients vs Gidal single-dose healthy volunteers; at VPA 75 Gidal's own Emax model predicts fold 0.40 against Rivas's 0.49. Whether to adopt Gidal's steeper Emax directly is a review choice (docs/antiseizure-interactions-review.md, assumption 1)."),
  # --- Valproate (victim) ----------------------------------------------------
  app("carbamazepine", "valproate", "induction", "UGT, CYP2C9/2C19 (beta-oxidation unaffected)", "CL",
      1.521, "turnover", TDS22,
      "CL/F x 1.521 with carbamazepine; confirmed by back-calculation from the paper's worked examples."),
  app("phenytoin", "valproate", "induction", "UGT, CYP2C9/2C19", "CL",
      1.640, "turnover", TDS22, "CL/F x 1.640 with phenytoin."),
  app("phenobarbital", "valproate", "induction", "UGT, CYP2C9/2C19", "CL",
      1.386, "turnover", TDS22, "CL/F x 1.386 with phenobarbital."),
  # --- Carbamazepine (victim) ------------------------------------------------
  row("carbamazepine", "carbamazepine", "applied", "autoinduction", "CYP3A4 (epoxide-diol pathway)",
      "CL", 2.0, c_ref = 8, ec50 = 4, ec50_basis = LOWER_EDGE, onset = "turnover",
      finding = paste0(
        "Clearance of tetradeuterium-labelled carbamazepine doubled after 17-32 ",
        "days of treatment in three children, with no further rise over four ",
        "months; in two adults the half-life of a single dose a week after the ",
        "last maintenance dose (45 and 46 h) was twice the steady-state half-life ",
        "(23 h in both)."),
      population = "3 children on long-term treatment; 2 adults",
      source = paste0("Bertilsson L, Tomson T, Tybring G. Pharmacokinetics: ",
                      "time-dependent changes--autoinduction of carbamazepine ",
                      "epoxidation. J Clin Pharmacol 1986;26:459-462."),
      source_short = "Bertilsson 1986", pmid = "3734137",
      doi = "10.1002/j.1552-4604.1986.tb03558.x",
      evidence_read = "abstract; full text not in PMC", to_obtain = "yes",
      notes = "The library's Graves model is the induced state, so the uninduced clearance is Graves's / 2: half-life 42 h in the reference man, inside the 25-65 h single-dose range."),
  app("phenytoin", "carbamazepine", "induction", "CYP3A4", "CL", 1.42,
      "turnover", GRAVES,
      "CL/F x 1.42 with phenytoin alone (Table 4, 95% CI 1.33-1.51); x 1.62 with phenytoin and phenobarbital or felbamate (CI 1.44-1.80).",
      to_obtain = "no",
      notes = "The library's own carbamazepine source; read in full 2026-10-24. The dual-inducer 1.62 is a FITTED interaction term (its own 0/1 indicator), not a composition of the single terms: additive excess (3) gives 1.59 and multiplicative 1.66, so the data sit between and slightly favour additive excess. Graves tested valproate and found it NOT significant on parent CBZ CL/F (Table 3); see the valproate->carbamazepine recorded row."),
  app("phenobarbital", "carbamazepine", "induction", "CYP3A4", "CL", 1.17,
      "turnover", GRAVES,
      "CL/F x 1.17 with phenobarbital or felbamate (Table 4, 95% CI 1.10-1.24).",
      to_obtain = "no"),
  # --- Phenytoin (victim) ----------------------------------------------------
  row("valproate", "phenytoin", "applied", "binding displacement", "albumin",
      "Km", round(9.6 / 15.6, 3), c_ref = 75, ec50 = 50, ec50_basis = LOWER_EDGE,
      onset = "immediate",
      finding = paste0(
        "Valproate 1200 mg/day (400 mg three times daily) raised the unbound ",
        "fraction of phenytoin from 9.6 +/- 0.9% to 15.6 +/- 1.4% (p<0.001; ",
        "Table I, % bound 90.1 -> 83.9 at 6 h). Total phenytoin clearance rose ",
        "~30% (Table II, Cl 33.8 -> 43.9 ml/h/kg, p<0.01) and total Vd rose, but ",
        "the FREE (intrinsic) clearance FELL ~18% (Cl_i 348 -> 286 ml/h/kg, ",
        "p<0.01) and free Vd fell, so unbound phenytoin was higher in all 7 ",
        "subjects. Steady-state valproate was 381/343 umol/L (~55/49 mg/L) at ",
        "6/12 h."),
      population = "7 healthy men, single intravenous phenytoin 250 mg (single-dose, non-saturating)",
      source = paste0("Perucca E, Hebdige S, Frigo GM, et al. Interaction ",
                      "between phenytoin and valproic acid: plasma protein ",
                      "binding and metabolic effects. Clin Pharmacol Ther ",
                      "1980;28:779-789."),
      source_short = "Perucca 1980", pmid = "6777108",
      doi = "10.1038/clpt.1980.235",
      evidence_read = "full text (read 2026-10-24): Tables I and II give the binding and clearance split",
      to_obtain = "no",
      notes = "Km on the total scale is Km(unbound)/fu: x 9.6/15.6 = 0.615. Only the DISPLACEMENT is applied (the plotted TOTAL falls). The METABOLIC-INHIBITION arm is now quantified from the full text but NOT applied: free intrinsic clearance fell ~18% (Cl_i 348 -> 286), so the active UNBOUND phenytoin actually RISES with valproate while the plotted total falls. This is a single-dose IV study with no saturation (the authors note saturation would amplify the effect at steady state); c_ref 75 mcg/mL stands for the ~55 mg/L valproate measured. REVIEW POINT: whether to plot free phenytoin (which rises) alongside the total (which falls) is docs/antiseizure-interactions-review.md assumption 3; this row now has the number to do it."),
  row("zonisamide", "phenytoin", "applied", "competitive inhibition", "not isozyme-identified",
      "Km", 1.16, c_ref = 20, ec50 = 10, ec50_basis = LOWER_EDGE,
      onset = "immediate",
      finding = "Km of phenytoin x theta_ZNS = 1.16 with zonisamide (Table 2, 95% CI 1.07-1.25); the serum phenytoin concentration rose ~16% (p<0.01). The model is Km_i = Km*(theta_ZNS)^ZNS, so zonisamide inhibits phenytoin metabolism competitively (the paper's wording); the isozyme is not identified.",
      population = "116 Japanese patients with epilepsy, 531 steady-state levels",
      source = paste0("Odani A, Hashimoto Y, Takayanagi K, et al. Population ",
                      "pharmacokinetics of phenytoin in Japanese patients with ",
                      "epilepsy: analysis with a dose-dependent clearance model. ",
                      "Biol Pharm Bull 1996;19:444-448."),
      source_short = "Odani 1996", pmid = "8924916", doi = "10.1248/bpb.19.444",
      evidence_read = "full text (read 2026-10-24): Table 2 gives theta_ZNS 1.16, CI 1.07-1.25; Vmax 9.80, Km 9.19, weight power 0.463 confirm the library's phenytoin model",
      to_obtain = "no",
      notes = "The library's own phenytoin source; the zonisamide term acts on Km (total scale), applied immediately like the valproate displacement."),
  # --- Phenobarbital (victim) ------------------------------------------------
  app("phenytoin", "phenobarbital", "inhibition", "CYP2C9/2C19", "CL", 0.822,
      "immediate", TDS20, "CL/F x 0.822 with phenytoin.", to_obtain = "yes",
      notes = "A different source from the library's phenobarbital model (the Munich status epilepticus cohort); the factor is applied to it as a relative change."),
  app("valproate", "phenobarbital", "inhibition", "CYP2C9/2C19, N-glucosidation", "CL",
      0.711, "immediate", TDS20, "CL/F x 0.711 with valproate.", to_obtain = "yes"),
  # --- Topiramate, tiagabine, lacosamide, eslicarbazepine, cenobamate --------
  app("carbamazepine", "topiramate", "induction", "CYP3A4", "CL", 1.63,
      "turnover", BAMGBOYE, "CL x 1.63 with enzyme-inducing co-medication."),
  app("phenytoin", "topiramate", "induction", "CYP3A4", "CL", 1.63,
      "turnover", BAMGBOYE, "CL x 1.63 with enzyme-inducing co-medication."),
  app("phenobarbital", "topiramate", "induction", "CYP3A4", "CL", 1.63,
      "turnover", BAMGBOYE, "CL x 1.63 with enzyme-inducing co-medication."),
  app("carbamazepine", "tiagabine", "induction", "CYP3A4", "CL", round(21.4 / 12.8, 3),
      "turnover", SAMARA, "Central CL/F 21.4 L/h with enzyme-inducing drugs against 12.8 L/h without (+67%); post-hoc CL/F with carbamazepine alone 24.6 L/h (Table 2).",
      to_obtain = "no",
      notes = "Read in full 2026-10-24. The model uses a single inducer indicator (one 1.67 factor for carbamazepine, phenytoin and phenobarbital); post-hoc Table 2 shows phenytoin (28.0) a little stronger than carbamazepine (24.6) and phenobarbital (20.4). COMBINATION DATA POINT: Samara states the inducers are NOT additive - carbamazepine alone 24.6 against carbamazepine+phenytoin 27.1 - the strongest evidence for a sub-additive combination rule (see docs/antiseizure-interactions-review.md assumption 2)."),
  app("phenytoin", "tiagabine", "induction", "CYP3A4", "CL", round(21.4 / 12.8, 3),
      "turnover", SAMARA, "Central CL/F 21.4 L/h with enzyme-inducing drugs against 12.8 L/h without (+67%); post-hoc CL/F with phenytoin alone 28.0 L/h (Table 2).",
      to_obtain = "no"),
  app("phenobarbital", "tiagabine", "induction", "CYP3A4", "CL", round(21.4 / 12.8, 3),
      "turnover", SAMARA, "Central CL/F 21.4 L/h with enzyme-inducing drugs against 12.8 L/h without (+67%); post-hoc CL/F with barbiturates 20.4 L/h (Table 2).",
      to_obtain = "no"),
  app("carbamazepine", "lacosamide", "induction", "CYP3A4/2C9/2C19", "CL", 1.48,
      "turnover",
      list(source = paste0("Yu L, Mao F, Chen S, et al. Development and validation ",
                           "of a population pharmacokinetic model for lacosamide in ",
                           "adult patients with epilepsy. BMC Pharmacol Toxicol 2026;27."),
           source_short = "Yu 2026", pmid = "41776608", pmcid = "PMC13067655",
           doi = "10.1186/s40360-026-01114-2",
           population = "180 Chinese adults, 294 troughs",
           evidence_read = "full text (the library's lacosamide source)"),
      "CL/F x 1.48^CBZ."),
  app("phenobarbital", "lacosamide", "induction", "CYP3A4/2C9/2C19", "CL", 1.42,
      "turnover",
      list(source = paste0("Wu R, Liu S, Zhao Z, Mei S. Population pharmacokinetic ",
                           "model of lacosamide in Chinese children and adults with ",
                           "epilepsy. Ther Drug Monit 2026;48:358-365."),
           source_short = "Wu 2025", pmid = "40960184",
           doi = "10.1097/FTD.0000000000001385",
           population = "391 Chinese children and adults, 482 samples",
           evidence_read = "abstract; full text not in PMC"),
      "CL x 1.42 with phenobarbital or carbamazepine (one indicator).",
      to_obtain = "yes",
      notes = "Applied for phenobarbital only: carbamazepine takes Yu 2026's own 1.48, the library's source."),
  app("phenytoin", "lacosamide", "induction", "CYP3A4/2C9/2C19", "CL",
      round(1.35 / 0.94, 3), "turnover",
      list(source = paste0("Contin M, Albani F, Riva R, Candela C, Mohamed S, ",
                           "Baruzzi A. Lacosamide therapeutic monitoring in ",
                           "patients with epilepsy: effect of concomitant ",
                           "antiepileptic drugs. Ther Drug Monit 2013;35:849-852."),
           source_short = "Contin 2013", pmid = "23942540",
           doi = "10.1097/FTD.0b013e318290eacc",
           population = "75 adults; 33 on strong inducers (11 phenytoin, 11 carbamazepine, 10 phenobarbital, 1 CBZ+PB) vs 42 without",
           evidence_read = "full text (read 2026-10-24): L/D 0.94 (0.75-1.27) vs 1.35 (1.16-1.74), P<0.001 (Fig 2); the carbamazepine, phenytoin and phenobarbital subgroups were comparable and were pooled"),
      "LCM concentration/dose ratio 0.94 with strong inducers (carbamazepine, phenobarbital or phenytoin) against 1.35 without, x1.44; the authors estimate a 30% higher lacosamide dose is needed.",
      to_obtain = "no",
      notes = "Fills the phenytoin gap: phenytoin is not a covariate in Yu 2026 (carbamazepine) or Wu 2025 (a phenobarbital/carbamazepine indicator), so having those two induce lacosamide while phenytoin did nothing was an asymmetry (Gemini review, 2026-10-24). Contin 2013 is the phenytoin-inclusive source, and its x1.44 sits between the carbamazepine (1.48) and phenobarbital (1.42) rows. The three inducer subgroups were analysed separately, found comparable and then pooled (so the pooled factor is supported for each, not a mixed average); still, applying it per inducer row over-predicts a multi-inducer regimen under additive-excess, the limitation shared with topiramate/tiagabine/ethosuximide. CROSS-CHECK: the Vimpat US label puts the grouped reduction at 15-20% (x1.18-1.25), below these TDM cohorts; the EU/NZ labels say 25% (x1.33)."),
  app("carbamazepine", "eslicarbazepine", "induction", "UGT", "CL",
      round((2.36 + 0.00149 * 800) / 2.36, 3), "turnover", FALCAO,
      "CL/F = (2.36 + 0.00149 D_CBZ + 1.41 AED_PB) (WT/70)^0.75 L/h; at 800 mg/day of carbamazepine, x 1.505.",
      to_obtain = "yes",
      notes = "Falcao's term is in the carbamazepine DOSE; 800 mg/day is taken as the dose that gives carbamazepine's reference 8 mcg/mL, and the concentration form (1) carries it from there."),
  app("phenytoin", "eslicarbazepine", "induction", "UGT", "CL",
      round((2.36 + 1.41) / 2.36, 3), "turnover", FALCAO,
      "Barbiturates or phenytoin add 1.41 L/h to 2.36 L/h: x 1.597.", to_obtain = "yes"),
  app("phenobarbital", "eslicarbazepine", "induction", "UGT", "CL",
      round((2.36 + 1.41) / 2.36, 3), "turnover", FALCAO,
      "Barbiturates or phenytoin add 1.41 L/h to 2.36 L/h: x 1.597.", to_obtain = "yes"),
  app("carbamazepine", "cenobamate", "induction", "CYP3A4/UGT", "CL", 1.15,
      "turnover",
      list(source = paste0("Vashi V et al. Population pharmacokinetics of cenobamate ",
                           "(sponsor model SK16007). Epilepsy Res 2023;195:107185."),
           source_short = "Vashi 2023", pmid = "37429218",
           population = "960 subjects, 8700 concentrations",
           evidence_read = "full text and supplement (the library's cenobamate source)"),
      "CL/F +15% with carbamazepine; clobazam -19% (clobazam is not in the library).",
      notes = "CONFLICT with the XCOPRI label (read 2026-10-24): the label's dedicated DDI study found carbamazepine did NOT significantly affect cenobamate exposure, while phenytoin LOWERED cenobamate AUC 27-28% (CYP2C19 induction of cenobamate). This Vashi pop-PK covariate (+15% with carbamazepine) is contradicted by the label's dedicated study. REVIEW: replace this row with phenytoin->cenobamate CL x1.38 (exposure -27.5%) and drop the carbamazepine term, per the label (the more authoritative DDI source)."),
  # --- Zonisamide (victim) ---------------------------------------------------
  # Silva 2025 (the library's own zonisamide source) puts the inducer effect on
  # CL/F as 1.63^INDDL, where INDDL is the summed dose/defined-daily-dose of the
  # enzyme-inducing drugs.  A single inducer at one defined daily dose is
  # INDDL 1, so CL x 1.63 at the perpetrator's reference; several inducers
  # combine by the model's additive-excess rule (milder than Silva's own
  # multiplicative-in-dose form; see the notes and the review doc).
  app("carbamazepine", "zonisamide", "induction", "CYP3A4 (reductive cleavage)", "CL",
      1.63, "turnover", SILVA,
      "CL/F = 0.761 x 1.63^INDDL (Silva 2025, Table 2); a single inducer at one defined daily dose is INDDL 1, so CL x 1.63 at the perpetrator's reference.",
      notes = "Silva 2025 is the library's own zonisamide model, so the factor is self-consistent to apply. CROSS-CHECK: Okada 2008 found carbamazepine/phenytoin/phenobarbital raised zonisamide CL/F 24/28/29% (x~1.25), which matches 1.63^INDDL at INDDL ~0.4-0.5 (a typical maintenance dose is below one DDD). COMBINATION: Silva's form is multiplicative in dose (two inducers at one DDD each give 1.63^2 = 2.66), steeper than the model's additive-excess (2.26); and Spina 1996 found phenytoin induces zonisamide more strongly than carbamazepine (zonisamide 40% higher on carbamazepine than phenytoin). The uniform 1.63 per inducer is the self-consistent first approximation; see docs/antiseizure-interactions-review.md."),
  app("phenytoin", "zonisamide", "induction", "CYP3A4 (reductive cleavage)", "CL",
      1.63, "turnover", SILVA,
      "CL/F = 0.761 x 1.63^INDDL (Silva 2025, Table 2); INDDL 1 per inducer at one defined daily dose, so CL x 1.63."),
  app("phenobarbital", "zonisamide", "induction", "CYP3A4 (reductive cleavage)", "CL",
      1.63, "turnover", SILVA,
      "CL/F = 0.761 x 1.63^INDDL (Silva 2025, Table 2); INDDL 1 per inducer at one defined daily dose, so CL x 1.63."),
  # --- Ethosuximide (victim and perpetrator) ---------------------------------
  # Inducers raise ethosuximide clearance: Giaccone 1996 measured CL/F 9.2 ->
  # 15.3 ml/kg/h (x1.66) in a direct controlled comparison.  The factor is
  # POOLED (phenobarbital/phenytoin/carbamazepine grouped), so applying it to
  # each inducer row combines by additive-excess and over-predicts a multi-
  # inducer regimen - the pooled-covariate limitation shared with topiramate
  # and tiagabine (docs/antiseizure-interactions-review.md).  Valproate inhibits
  # ethosuximide modestly but variably (Pisani 1984).  Ethosuximide in turn
  # induces lamotrigine (Hagemann 2024).
  app("carbamazepine", "ethosuximide", "induction", "CYP3A4", "CL",
      1.66, "turnover", GIACCONE,
      "Inducers raised ethosuximide CL/F from 9.2 to 15.3 ml/kg/h (x1.66) in a direct controlled comparison (Giaccone 1996, 10 patients vs 12 controls).",
      notes = "POOLED FACTOR: Giaccone grouped phenobarbital/phenytoin/carbamazepine, so x1.66 is the any-inducer effect, not a per-drug coefficient; applied to each inducer row the additive-excess combination over-predicts multi-inducer induction (a structural limitation shared with topiramate/tiagabine - see docs/antiseizure-interactions-review.md). Riva 1996 (dose 20-25% higher) is the concordant lower-bound review statement."),
  app("phenytoin", "ethosuximide", "induction", "CYP3A4", "CL",
      1.66, "turnover", GIACCONE,
      "Inducers raised ethosuximide CL/F 9.2 -> 15.3 ml/kg/h (x1.66); pooled inducer factor (Giaccone 1996)."),
  app("phenobarbital", "ethosuximide", "induction", "CYP3A4", "CL",
      1.66, "turnover", GIACCONE,
      "Inducers raised ethosuximide CL/F 9.2 -> 15.3 ml/kg/h (x1.66); pooled inducer factor (Giaccone 1996)."),
  app("valproate", "ethosuximide", "inhibition", "CYP3A4", "CL",
      0.85, "immediate", PISANI,
      "Valproate reduced ethosuximide total clearance 11.2 -> 9.5 ml/min (x0.85) in 6 healthy volunteers (Pisani 1984); half-life 44 -> 54 h.",
      notes = "VARIABLE in occurrence: 2 of 6 subjects in Pisani showed no change, and Bauer 1982 (6 subjects) found none - so x0.85 is a mean whose magnitude is uncertain (see the Bauer crosscheck row). Mattson & Cramer and the valproic acid label also report inhibition. Replaces the earlier 'no interaction' characterisation (ChatGPT/literature review 2026-10-26)."),
  app("ethosuximide", "lamotrigine", "induction", "UGT1A4", "CL",
      1.16, "turnover", HAGEMANN,
      "Ethosuximide lowered lamotrigine serum concentration 14% in adults (CL x1.16; Hagemann 2024, Table 2, e1 0.86), inducing UGT1A4 as methsuximide does.",
      notes = "Newly reported (2024); ethosuximide is both a victim of induction and a perpetrator on lamotrigine. AGE-DEPENDENT: the effect is 37% in children/adolescents (CL x1.59) vs 14% in adults; the model's concentration-only interaction form has no age term, so the adult factor (x1.16) is applied (the reference patient is an adult). A review point for the age dependence - docs/antiseizure-interactions-review.md."),

  # --- Cross-checks ------------------------------------------------------------
  row("valproate", "lamotrigine", "crosscheck", "inhibition", "UGT",
      finding = "Simple Emax fit (Table 2): Emax 64.9% inhibition of lamotrigine CL/F (SE 7.2, 95% CI 50.1-79.7), EC50 5.6 mcg/mL of valproate (SE 2.3, CI 0.8-10.4, ~38 umol/L). Mean inhibition 30.8, 51.5, 52.8 and 50.3% at 125, 250, 375 and 500 mg/day (mean troughs 6.5, 13.9, 16.1, 31.9 mcg/mL); 30 subjects, one arm per VPA dose.",
      population = "30 healthy volunteers, single 25 mg lamotrigine doses before and during low-dose VPA",
      source = "Gidal BE, Sheth R, Parnell J, Maloney K, Sale M. Evaluation of VPA dose and concentration effects on lamotrigine pharmacokinetics. Epilepsy Res 2003;57:85-93.",
      source_short = "Gidal 2003", pmid = "15013050", doi = "10.1016/j.eplepsyres.2003.09.008",
      evidence_read = "full text (read 2026-10-24): Table 2 is the Emax/EC50 fit", to_obtain = "no",
      notes = "Supplies the EC50 of the applied valproate-lamotrigine row. Its directly fitted Emax (64.9% max inhibition, fold 0.351 at saturation) is steeper than the applied row's Rivas-anchored calibration (54.8% max); the applied row's note records the review choice."),
  row("valproate", "lamotrigine", "crosscheck", "inhibition", "UGT",
      finding = "Lamotrigine CL/F 0.69 (monotherapy), 1.60 (with inducers) and 0.20 (with valproate) mL/kg/min; valproate inhibition maximal within the usual therapeutic range, and valproate concentration did not improve the fit.",
      population = "62 developmentally disabled adults with epilepsy",
      source = "Gidal BE, Anderson GD, Rutecki PR, Shaw R, Lanning A. Lack of an effect of valproate concentration on lamotrigine pharmacokinetics in developmentally disabled patients with epilepsy. Epilepsy Res 2000;42:23-31.",
      source_short = "Gidal 2000", pmid = "10996503", doi = "10.1016/s0920-1211(00)00160-1",
      evidence_read = "abstract; full text not in PMC"),
  row("valproate", "lamotrigine", "crosscheck", "inhibition (offset)", "UGT",
      finding = "After valproate is stopped, lamotrigine troughs fall from day 5 and the interaction is fully reversed 10-14 days later.",
      population = "10 healthy adults",
      source = "Leary E, Sheth RD, Gidal BE. Time course of reversal of valproate-mediated inhibition of lamotrigine. Seizure 2018;57:76-79.",
      source_short = "Leary 2018", pmid = "29574285", doi = "10.1016/j.seizure.2018.03.003",
      evidence_read = "abstract; full text not in PMC",
      notes = "Checks the immediate onset: the offset is lamotrigine's own slow approach to its uninhibited steady state."),
  # Consensus reviews and secondary studies that cross-check the applied set
  # (Steven Shafer supplied the full texts, 2026-10-25).
  row("valproate", "phenytoin", "crosscheck", "binding displacement + inhibition", "albumin / CYP2C9",
      finding = "Authoritative AED-AED consensus review. Displacement of phenytoin by valproate lowers TOTAL phenytoin while the unbound does not fall and can rise (metabolic inhibition); total-based monitoring underestimates the active drug. Corroborates the applied Km x0.615 and the free-vs-total caveat.",
      population = "consensus review (AED-AED interactions, Part I)",
      source = "Patsalos PN, Perucca E. Clinically important drug interactions in epilepsy: general features and interactions between antiepileptic drugs. Lancet Neurol 2003;2:347-356.",
      source_short = "Patsalos & Perucca 2003", pmid = "12849151", doi = "10.1016/s1474-4422(03)00409-5",
      evidence_read = "full text (read 2026-10-25): Table 2 and the displacement/inhibition sections",
      notes = "Corroborates the applied set broadly: inducers ~2x and valproate ~0.5x on lamotrigine; valproate +30-50% on phenobarbital; carbamazepine->phenytoin inconsistent (left unapplied); valproate->parent carbamazepine negligible (left unapplied); inducers lower zonisamide and ethosuximide."),
  row("phenytoin", "valproate", "crosscheck", "induction", "UGT, CYP2C9/2C19",
      finding = "May & Rambeck's primary data (259 inpatients) as quoted by Tanaka: valproate serum concentration is reduced TO 49.5% (phenytoin), 66.2% (carbamazepine), 76.3% (phenobarbital) of monotherapy - CL x2.02, x1.51, x1.31 - matching the applied Teixeira-da-Silva 2022 factors (1.64, 1.52, 1.39) in magnitude and rank order.",
      population = "259 inpatients (May & Rambeck 1985), via Tanaka 1999",
      source = "Tanaka E. Clinically significant pharmacokinetic drug interactions between antiepileptic drugs. J Clin Pharm Ther 1999;24:87-92.",
      source_short = "Tanaka 1999", pmid = "10380060", doi = "10.1046/j.1365-2710.1999.00201.x",
      evidence_read = "full text (read 2026-10-25): Case 3 and Table 3",
      notes = "CORRECTION: Patsalos & Perucca 2003 states valproate 'reduced BY 76/49/66%' with phenobarbital/phenytoin/carbamazepine, but those numbers are the May & Rambeck 'reduced TO' percentages (76.3/49.5/66.2% of monotherapy); read correctly they confirm the applied inducer->valproate factors. An earlier review note flagging an inducers->valproate divergence was withdrawn."),
  row("phenytoin", "valproate", "crosscheck", "induction", "UGT, CYP2C9/2C19",
      finding = "Primary source: valproate serum concentration reduced to 49.5% (phenytoin), 66.2% (carbamazepine), 76.3% (phenobarbital) of monotherapy.",
      population = "259 inpatients",
      source = "May T, Rambeck B. Serum concentrations of valproic acid: influence of dose and comedication. Ther Drug Monit 1985;7:387-390.",
      source_short = "May & Rambeck 1985", pmid = "3936236",
      evidence_read = "abstract; the figures are quoted in full by Tanaka 1999 and Patsalos & Perucca 2003"),
  row("carbamazepine", "lamotrigine", "crosscheck", "induction", "UGT",
      finding = "Carbamazepine accelerates lamotrigine glucuronidation, reducing its half-life from 24 +/- 6 h to 14 +/- 7 h (CL x1.71), close to the applied 1.595. Also: zonisamide was 40% higher on carbamazepine than phenytoin (phenytoin the stronger zonisamide inducer).",
      population = "consensus review (carbamazepine interactions)",
      source = "Spina E, Pisani F, Perucca E. Clinically significant pharmacokinetic drug interactions with carbamazepine. An update. Clin Pharmacokinet 1996;31:198-214.",
      source_short = "Spina 1996", pmid = "8877250", doi = "10.2165/00003088-199631030-00004",
      evidence_read = "full text (read 2026-10-25): sections 3.1-3.2"),
  row("valproate", "lamotrigine", "crosscheck", "inhibition", "UGT",
      finding = "Lamotrigine concentration-to-dose ratio (164 patients): C/D 0.84 (LTG alone), 0.37 (+inducers, CL ~x2.3), 3.41 (+valproate, CL ~x0.25). The valproate effect is at the strong end of the spread; the applied x0.49 is central across sources.",
      population = "164 patients (68 children, 96 adults), TDM",
      source = "Armijo JA, Bravo J, Cuadrado A, Herranz JL. Lamotrigine serum concentration-to-dose ratio: influence of age and concomitant antiepileptic drugs and dosage implications. Ther Drug Monit 1999;21:182-190.",
      source_short = "Armijo 1999", pmid = "10217338",
      evidence_read = "full text (read 2026-10-25): Table 2 and the abstract"),
  row("valproate", "lamotrigine", "crosscheck", "inhibition", "UGT",
      finding = "Single-dose lamotrigine in 19 children: CL/F 0.04 L/h/kg with valproate vs 0.11 with inducers (t1/2 41.7 vs 8.1 h). The valproate and inducer effects hold in children.",
      population = "19 children and young adults",
      source = "Battino D, Croci D, Granata T, et al. Single-dose pharmacokinetics of lamotrigine in children: influence of age and antiepileptic comedication. Ther Drug Monit 2001;23:217-222.",
      source_short = "Battino 2001", pmid = "11360028",
      evidence_read = "full text (read 2026-10-25): Table 1 and Figure 1"),
  row("valproate", "lamotrigine", "crosscheck", "inhibition", "UGT",
      finding = "Lamotrigine interactions in 31 children: median LTG t1/2 43.3 h (valproate), 14.1 h (carbamazepine/phenobarbital), 28.9 h (both). Inducers ~halve and valproate ~doubles the half-life; the applied x0.49 valproate factor sits at the ~x0.5 end of the LTG evidence spread.",
      population = "31 children with intractable epilepsy",
      source = "Eriksson AS, Hoppu K, Nergardh A, Boreus L. Pharmacokinetic interactions between lamotrigine and other antiepileptic drugs in children with intractable epilepsy. Epilepsia 1996;37:769-773.",
      source_short = "Eriksson 1996", pmid = "8764817",
      evidence_read = "full text (read 2026-10-25)"),
  row("carbamazepine", "zonisamide", "crosscheck", "induction", "CYP3A4",
      finding = "Carbamazepine, phenytoin and phenobarbital raised zonisamide CL/F by 24%, 28% and 29% (x~1.25) in a Japanese population model - the lower per-drug end against Silva 2025's 1.63 per defined daily dose, reconciled because a typical maintenance dose is below one DDD (INDDL ~0.4-0.5).",
      population = "Japanese epilepsy patients, population PK",
      source = "Okada Y, Seo T, Ishitsu T, et al. Population estimation regarding the effects of cytochrome P450 2C19 and 3A5 polymorphisms on zonisamide clearance. Ther Drug Monit 2008;30:540-543.",
      source_short = "Okada 2008", pmid = "18641551",
      evidence_read = "cited by Silva 2025 (read 2026-10-25); abstract",
      notes = "Cross-checks the applied inducers->zonisamide magnitude."),
  row("phenytoin", "valproate", "crosscheck", "binding (negative)", "albumin",
      finding = "Valproate protein binding is saturable (Langmuir Bm 130, Kd 7.8 mcg/mL). Phenytoin, carbamazepine and zonisamide do NOT alter VALPROATE protein binding (valproate is the victim here); only ethosuximide and polytherapy (enzyme induction raising the 2-en-valproate metabolite) shifted it. Negative cross-check: no phenytoin/carbamazepine/zonisamide -> valproate binding displacement to add.",
      population = "108 Japanese children, 342 total/unbound valproate pairs",
      source = "Ueshima S, Aiba T, Makita T, et al. Characterization of non-linear relationship between total and unbound serum concentrations of valproic acid in epileptic children. J Clin Pharm Ther 2008;33:31-38.",
      source_short = "Ueshima 2008", pmid = "18211614",
      evidence_read = "full text (read 2026-10-25): Tables 2 and 3",
      notes = "Supports the free-drug refinement material (valproate side) and confirms no binding interaction onto valproate from the inducers."),
  row("valproate", "ethosuximide", "crosscheck", "inhibition (discordant null)", "CYP3A4",
      finding = "Bauer 1982 (6 healthy adults): adding valproate to steady-state ethosuximide did NOT change total or nonrenal clearance (11.1 -> 11.2 and 8.3 -> 8.3 ml/h/kg; phases 2 and 3 superimposed). A null that, with Pisani's 2-of-6 non-responders, shows the valproate->ethosuximide inhibition is real but variable in occurrence.",
      population = "6 healthy adults",
      source = "Bauer LA, Harris C, Wilensky AJ, Raisys VA, Levy RH. Ethosuximide kinetics: possible interaction with valproic acid. Clin Pharmacol Ther 1982;31:741-745.",
      source_short = "Bauer 1982", pmid = "6804151",
      evidence_read = "full text (read 2026-10-25): Table I and Fig. 1",
      notes = "Discordant with the applied Pisani 1984 row; records the interindividual variability (the applied x0.85 is a mean)."),
  do.call(row, c(list("carbamazepine", "ethosuximide", "crosscheck", "induction", "CYP3A4",
      finding = "Consensus review: inducers accelerate ethosuximide clearance; the ethosuximide dose must be 20-25% higher on carbamazepine/phenytoin/phenobarbital/primidone (section 3.8). Concordant with, and a lower bound to, Giaccone 1996's measured x1.66."),
      RIVA96)),

  # --- Recorded: described but not applied -----------------------------------
  # Quantified only in a product label, a moiety not plotted, or a drug not in
  # the library.  Kept so the reference list is complete; never simulated.
  # The cenobamate magnitudes are now read from the XCOPRI label (NDA 212839,
  # section 12.3, cenobamate 200 mg/day). They are kept `recorded` pending a
  # focused implementation: cenobamate has no therapeutic band, so applying them
  # needs a defined reference concentration (from the model at 200 mg/day), and
  # the lamotrigine 21-52% across 100-400 mg/day gives the concentration anchor;
  # phenytoin is the saturable-engine victim (a Vmax factor). See
  # docs/antiseizure-interactions-review.md.
  row("cenobamate", "phenytoin", "recorded", "inhibition", "CYP2C19",
      finding = "XCOPRI label (12.3): cenobamate 200 mg/day increased phenytoin Cmax +70% and AUC +84%; the label advises decreasing the phenytoin dose by up to 50%. To APPLY on the saturable engine: Vmax x ~0.83 reproduces +84% total at the band midpoint (15 mcg/mL), but the effect is operating-point dependent (larger near saturation) - which is clinically real.",
      population = "dedicated DDI study (XCOPRI USPI, NDA 212839)",
      source = "XCOPRI (cenobamate) US prescribing information, NDA 212839, sections 7.1 and 12.3 (revised 8/2025).",
      source_short = "XCOPRI label", pmid = "33993416", doi = "10.1007/s40263-021-00819-8",
      evidence_read = "full text (label read 2026-10-24); Roberti 2021 (PMID 33993416) gives the CYP2C19 mechanism",
      to_obtain = "no",
      notes = "Not yet applied (see the block comment above). Mechanism CYP2C19 inhibition (immediate). Cenobamate->clobazam and ->N-desmethylclobazam are also raised, but clobazam is not in the library."),
  row("cenobamate", "lamotrigine", "recorded", "induction", "CYP3A4/UGT",
      finding = "XCOPRI label (12.3): lamotrigine exposure expected to decrease 21-52% across the cenobamate 100-400 mg/day dose range (so CL x1.27 to x2.08). At 200 mg/day the mid-range applies. The 100->400 mg/day span is the concentration anchor for a saturable fold.",
      population = "population PK (XCOPRI USPI, NDA 212839)",
      source = "XCOPRI (cenobamate) US prescribing information, NDA 212839, sections 7.1 and 12.3 (revised 8/2025).",
      source_short = "XCOPRI label", pmid = "36057761", pmcid = "PMC9588096",
      doi = "10.1007/s40120-022-00400-5",
      evidence_read = "full text (label read 2026-10-24); Smith 2022 (PMID 36057761) gives the clinical thresholds",
      to_obtain = "no",
      notes = "Not yet applied. Induction (turnover). The dose-range span (21% at 100, 52% at 400 mg/day) is exactly the two-point anchor the saturable fold(C) needs for a perpetrator that has no therapeutic band."),
  row("cenobamate", "carbamazepine", "recorded", "induction", "CYP3A4",
      finding = "XCOPRI label (12.3): cenobamate 200 mg/day decreased carbamazepine Cmax and AUC each 23% (CL x1.30). Dose increase as needed.",
      population = "dedicated DDI study (XCOPRI USPI, NDA 212839)",
      source = "XCOPRI (cenobamate) US prescribing information, NDA 212839, sections 7.1 and 12.3 (revised 8/2025).",
      source_short = "XCOPRI label", pmid = "33993416", doi = "10.1007/s40263-021-00819-8",
      evidence_read = "full text (label read 2026-10-24)",
      to_obtain = "no", notes = "Not yet applied (see the block comment above). Induction (turnover)."),
  row("cenobamate", "phenobarbital", "recorded", "inhibition", "CYP2C19",
      finding = "XCOPRI label (12.3): cenobamate 200 mg/day increased phenobarbital Cmax +34% and AUC +37% (CL x0.73). Reduce phenobarbital dose as needed.",
      population = "dedicated DDI study (XCOPRI USPI, NDA 212839)",
      source = "XCOPRI (cenobamate) US prescribing information, NDA 212839, sections 7.1 and 12.3 (revised 8/2025).",
      source_short = "XCOPRI label", pmid = "33993416", doi = "10.1007/s40263-021-00819-8",
      evidence_read = "full text (label read 2026-10-24)",
      to_obtain = "no", notes = "Not yet applied (see the block comment above). Inhibition (immediate). The label found valproate, lacosamide and levetiracetam NOT significantly changed by cenobamate (consistent with the applied set)."),
  row("valproate", "carbamazepine", "recorded", "epoxide hydrolase inhibition (epoxide); parent CL increase", "epoxide hydrolase",
      finding = "Valproate reduces carbamazepine-10,11-epoxide clearance x 0.693 (Table 5, theta4, 95% CI 0.615-0.771), raising the active epoxide by inhibiting epoxide hydrolase. On PARENT carbamazepine clearance, Jiao found phenytoin x 1.44 (Table 4, theta4; abstract; CI 1.28-1.60), phenobarbital x 1.26 (Table 4, theta5; CI 1.11-1.41) and valproate (only at a daily dose >18 mg/kg) x 1.23 (Table 4, theta6; CI 1.14-1.32), an INCREASE, in this Chinese cohort. (Eq. 3.1 in the text numbers the same three thetas in a different order than Table 4 and the abstract; Table 4 and the abstract agree with each other on which value belongs to which drug.)",
      population = "408 Chinese patients, 459 samples",
      source = "Jiao Z, Shi XJ, Zhao ZG, Zhong MK. Population pharmacokinetic modeling of steady state clearance of carbamazepine and its epoxide metabolite from sparse routine clinical data. J Clin Pharm Ther 2004;29:247-256.",
      source_short = "Jiao 2004", pmid = "15153086", doi = "10.1111/j.1365-2710.2004.00557.x",
      evidence_read = "full text (read 2026-10-24): Tables 4 and 5 give the CBZ and epoxide coefficients with CIs",
      to_obtain = "no",
      notes = "Not applied: the library does not plot carbamazepine-10,11-epoxide (the only moiety valproate clearly raises here). The valproate-on-PARENT term is itself contested: Jiao found an INCREASE gated at valproate >18 mg/kg (x 1.23), whereas Graves 1998 (read in full) found valproate not significant on parent CBZ CL/F at all. Jiao's phenytoin (1.44) term on CBZ parent is close to the applied Graves value (1.42), not a population difference; Jiao's phenobarbital term (1.26) does differ from the applied Graves value (1.17), a population difference (Chinese vs US) to weigh if these are ever revisited."),
  row("felbamate", "phenytoin", "recorded", "inhibition", "CYP2C19",
      finding = "Felbamate inhibits CYP2C19 and raises phenytoin; mechanism established, not quantified as a population factor.",
      population = "mechanistic review",
      source = "Levy RH. Cytochrome P450 isozymes and antiepileptic drug interactions. Epilepsia 1995;36 Suppl 5:S8-13.",
      source_short = "Levy 1995", pmid = "8806399", doi = "10.1111/j.1528-1157.1995.tb06007.x",
      evidence_read = "abstract", to_obtain = "no",
      notes = "Felbamate is not in the library. Topiramate, oxcarbazepine and eslicarbazepine raise phenytoin by the same CYP2C19 route at high doses; no clean population factor was found."),
  row("valproate", "rufinamide", "recorded", "inhibition", "unspecified",
      finding = "Valproate lowers rufinamide clearance; the recommended maximum daily rufinamide dose is 600 against 1000 mg/day in children under 30 kg also taking valproate.",
      population = "pooled Lennox-Gastaut population PK",
      source = "Marchand M, Fuseau E, Critchley DJ. Supporting the recommended paediatric dosing regimen for rufinamide in Lennox-Gastaut syndrome using clinical trial simulation. J Pharmacokinet Pharmacodyn 2010;37:99-118.",
      source_short = "Marchand 2010", pmid = "20084538", doi = "10.1007/s10928-009-9146-4",
      evidence_read = "abstract (percentage in the full text)", to_obtain = "yes",
      notes = "Rufinamide is not in the library."),
  row("stiripentol", "clobazam", "recorded", "inhibition", "CYP3A4, CYP2C19",
      finding = "Stiripentol inhibits CYP3A4 (Ki 1.59 uM) and CYP2C19 (Ki 0.52 uM for clobazam N-demethylation, 0.14 uM for N-desmethylclobazam 4'-hydroxylation), raising clobazam and N-desmethylclobazam.",
      population = "in vitro (cDNA-expressed P450) plus in vivo children",
      source = "Giraud C, Treluyer JM, Rey E, et al. In vitro and in vivo inhibitory effect of stiripentol on clobazam metabolism. Drug Metab Dispos 2006;34:608-611.",
      source_short = "Giraud 2006", pmid = "16415114", doi = "10.1124/dmd.105.007237",
      evidence_read = "abstract (Ki/IC50 values)", to_obtain = "yes",
      notes = "Neither stiripentol nor clobazam is in the library. The Ki/IC50 set would parameterise the inhibition if they are added."),
  row("carbamazepine", "phenytoin", "recorded", "induction", "CYP2C9/2C19 (Vmax)",
      finding = "Co-administered antiepileptics raised phenytoin's Vmax from 325 to 351 mg/day and Km from 2.41 to 3.18 mcg/mL at 60 kg (Table III, model 2); the covariate is a single phenytoin-alone-against-any-co-anticonvulsant indicator pooling phenobarbital, carbamazepine, valproate and, in 64 of the 334 patients, primidone, clonazepam, sultiame, ethotoin, ethosuximide, acetazolamide or diazepam. Rui 1995 found Vmax +8.4 mg/day with phenobarbital or a benzodiazepine.",
      population = "334 Japanese outpatients, 756 levels",
      source = "Yukawa E, Higuchi S, Aoyama T. Population pharmacokinetics of phenytoin from routine clinical data in Japan: an update. Chem Pharm Bull 1990;38:1973-1976.",
      source_short = "Yukawa 1990", pmid = "2268899", doi = "10.1248/cpb.38.1973",
      evidence_read = "full text (read 2026-10-29; Tables I and III confirmed on rendered pages)", to_obtain = "no",
      notes = "Not applied, for two reasons, neither of which is that the effect is small. (1) It REVERSES SIGN with concentration: raising Vmax 8% and Km 32% pulls opposite ways, so the dose needed to hold a level crosses over at 7.21 mcg/mL, and on an unchanged dose the concentration falls 6.5% at 10 mcg/mL and 24% at 20 but RISES below about 7. (2) The covariate is pooled across ten co-medications (see finding), so none of it is attributable to carbamazepine. Phenytoin's displacement by valproate (Km, applied) and zonisamide (Km, applied) are the quantified single-perpetrator effects. Rui 1995 (PMID 7639079) is the phenobarbital/benzodiazepine Vmax estimate. Digest: papers/digests/PMID_2268899.md."),
  row("brivaracetam", "brivaracetam", "recorded", "induction (victim)", "CYP2C19",
      finding = "The strong inducers carbamazepine, phenytoin and phenobarbital/primidone moderately lower brivaracetam plasma concentrations (about 26%), with no dose change advised.",
      population = "review",
      source = "Moseley BD, Chanteux H, Nicolas JM, et al. A review of the drug-drug interactions of the antiepileptic drug brivaracetam. Epilepsy Res 2020;163:106327.",
      source_short = "Moseley 2020", pmid = "32361205", doi = "10.1016/j.eplepsyres.2020.106327",
      evidence_read = "abstract", to_obtain = "no",
      notes = "Brivaracetam is not a selectable drug in the library. Listed so the inducer effect on it is recorded.")
)

stopifnot(all(registry$status %in% STATUSES))
utils::write.csv(registry, "inst/extdata/antiseizureInteractions.csv",
                 row.names = FALSE, na = "")
