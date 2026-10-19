# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer:
# "For all antibiotics in stanpumpR, create a therapeutic range based on the
# expected MBC for common pathogens for which the antibiotic is used."  The
# MICs and MBCs were researched by separate agents per drug, each number read
# in the source itself (PubMed abstract, PubMed Central full text or FDA label
# PDF) and quoted verbatim in the working notes; where only a class analogue,
# an abstract or a secondary summary was available the source string says so.
# Primary documents that could not be opened from the build environment
# (EUCAST rationale documents, CLSI M100, FDA STIC pages, EMA referral
# reports, several 1970s-1980s MBC tables) are listed in
# docs/antibiotic-mbc.md as the literature that would firm these bands up.
#
# STATUS: verified on R 4.3.3 by tests/testthat/test-antibiotic-mbc.R, which
# checks this table against drugDefaults_global.csv.
# -----------------------------------------------------------------------------
#
# THE ANTIBIOTICS' SHADED BAND: FREE DRUG AT THE MBC
# ==================================================
# The MIC (R/antibioticThresholds.R) is where growth stops; the MBC, the
# minimum bactericidal concentration, is where 99.9% of the inoculum is
# killed in 24 hours.  The shaded band marks the expected MBCs, as FREE drug,
# of the common pathogens the antibiotic is given against, placed on the
# plotted curve through the drug's binding (antibioticPlottedConcentration()).
#
# One rule for every drug:
#
#   Lower edge   the MBC50 of the PRINCIPAL pathogen: below it the drug no
#                longer kills a typical isolate of the organism it is mainly
#                given for.
#   Typical      the MBC90 of the principal pathogen: the free concentration
#                that kills 90% of its isolates.
#   Upper edge   the highest MBC90 among the principal and the other COMMON
#                pathogens the drug kills at attainable concentrations: above
#                it, free drug is bactericidal for 90% of isolates of every
#                one of them.
#
# The principal pathogen is the MIC table's organism wherever the drug kills
# it.  Clindamycin is the exception: it is bacteriostatic against
# Staphylococcus aureus (the FDA label: "Clindamycin is bacteriostatic"), so
# its band is built from the organisms it does kill, and staphylococci are
# listed as excluded.  Pathogens the drug does not kill at attainable
# concentrations (vancomycin and enterococci, tolerant strains, ESBL
# producers, an organism whose infection is treated through urine rather
# than plasma) are rows with Role "excluded" and a reason; they never move a
# band edge.
#
# Where a measured MBC50 or MBC90 exists it is used.  Most of these drugs
# have none that is modern: there the MBC is the MIC50 or MIC90 times the
# MBC/MIC ratio the literature reports for that drug or class against that
# organism, and the Basis column says so.  MBCs are reported on the doubling
# dilution scale, so are the values here.  Every MBC is an in-vitro value in
# protein-free broth, i.e. free drug.
#
# The CSV (Lower, Typical, Upper) carries the band on the plotted scale,
# rounded to two significant figures; the test keeps the two in step, and the
# drug's help page is generated from this table (helpDrugPageHTML() in
# R/help-drugs.R).


#' The antibiotics' MBC table
#'
#' One row per antibiotic and pathogen: the MIC50 and MIC90, the expected
#' MBC50 and MBC90 (free drug, mg/L), how the MBC was obtained, the pathogen's
#' role in the shaded band, and the source.
#'
#' @returns a data frame of \code{Drug}, \code{Pathogen}, \code{Role}
#'   ("principal", "core" or "excluded"), \code{MIC50}, \code{MIC90},
#'   \code{MBC50}, \code{MBC90} (mg/L, NA where not known), \code{Basis},
#'   \code{Note} and \code{Source}
#' @keywords internal
antibioticMbcTable <- function()
{
  row <- function(Drug, Pathogen, Role, MIC50 = NA, MIC90 = NA, MBC50 = NA, MBC90 = NA,
                  Basis = "", Note = "", Source = "")
    data.frame(Drug = Drug, Pathogen = Pathogen, Role = Role,
               MIC50 = MIC50, MIC90 = MIC90, MBC50 = MBC50, MBC90 = MBC90,
               Basis = Basis, Note = Note, Source = paste(Source, collapse = " "),
               stringsAsFactors = FALSE)

  rows <- list(
    # --- Cefazolin -----------------------------------------------------------
    row("cefazolin", "Staphylococcus aureus, methicillin-susceptible (MSSA)", "principal",
        MIC50 = 0.5, MIC90 = 2, MBC50 = 0.5, MBC90 = 2,
        Basis = "MIC x 1",
        Note = paste("No modern measured cefazolin MBC for MSSA could be read; the cephalosporin MBC equals",
                     "the MIC for most S. aureus strains. Standard inoculum only (see the inoculum effect below)."),
        Source = c(
          "MIC50 0.5: Haynes AS et al., Microbiol Spectr 2022;10:e01039-22 (47 isolates, CLSI broth",
          "microdilution). MIC90 2 at standard inoculum: Nannini EC et al., Antimicrob Agents Chemother",
          "2009;53:3437-3441 (98 isolates), also the EUCAST S. aureus ECOFF. MBC/MIC: cephalosporin MBCs",
          "'the same or within one dilution' of the MIC for most species (Jones RN et al., J Antibiot",
          "1977;30:583-592); cephalothin MBCs 'equal to MICs for most strains' of S. aureus and 'seldom more",
          "than fourfold greater' at 48 h (Pelletier LL, Baker CB, Antimicrob Agents Chemother",
          "1988;32:374-377).")),
    row("cefazolin", "Beta-haemolytic streptococci (groups A, B, C, G)", "core",
        MIC90 = 0.25, MBC90 = 0.25,
        Basis = "MIC x 1 (class analogue)",
        Note = "Cephalothin data; cefazolin's activity against group B streptococci is reported as similar to penicillin's.",
        Source = c(
          "Cephalothin MIC90 0.25 for 100 group B isolates, no tolerant strains (Jacobs MR et al., Antimicrob",
          "Agents Chemother 1982;22:897-900); 0.06 for 61 group C and G isolates, tolerance in one (Rolston KV",
          "et al., J Antimicrob Chemother 1984;13:389-392); cefazolin 'similar to that of penicillin' against",
          "group B streptococci (Backes RJ et al., Arch Intern Med 1985;145:693-696).")),
    row("cefazolin", "Enterobacterales (E. coli, Klebsiella, P. mirabilis), susceptible", "core",
        MIC50 = 1, MIC90 = 2, MBC50 = 1, MBC90 = 2,
        Basis = "MIC x 1",
        Note = "MIC90 taken at the CLSI systemic susceptible breakpoint, 2 mg/L (2 g every 8 hours).",
        Source = c(
          "Modal MIC 1, 79% at or below 2, in E. coli bacteraemia (Wang KC et al., J Microbiol Immunol Infect",
          "2016;49:768-774); CLSI M100 systemic breakpoint S <= 2 mg/L; MBC within one dilution of the MIC",
          "(Jones 1977), tolerance 'rarely observed' in 941 Gram-negative bacteraemia isolates",
          "(Chamberland S et al., Clin Infect Dis 1992;15:615-628).")),
    row("cefazolin", "Coagulase-negative staphylococci", "excluded",
        MIC90 = 4,
        Note = "MIC90 4 mg/L at 24 h and 32 at 48 h, and about a third of S. epidermidis tolerant to beta-lactams: no MBC90 the band can show.",
        Source = c(
          "Chin NX et al., Diagn Microbiol Infect Dis 1990;13:67-69; Watanakunakorn C, Scand J Infect Dis",
          "1985;17:59-61.")),
    row("cefazolin", "MSSA at high inoculum (the cefazolin inoculum effect)", "excluded",
        Note = paste("19 to 37% of MSSA have an MIC of 16 mg/L or more at 10^7 CFU/mL: a deep or high-burden",
                     "infection (endocarditis, osteomyelitis, abscess) needs more than the band shows."),
        Source = c(
          "Nannini 2009 (19%, MIC90 32 at high inoculum); Rincon S et al., J Antimicrob Chemother",
          "2013;68:2773-2778 (36% of 364); Rincon S et al., J Clin Microbiol 2021;59:e01938-20 (37.3% of 689).")),

    # --- Cefalexin -----------------------------------------------------------
    row("cefalexin", "Staphylococcus aureus, methicillin-susceptible (MSSA)", "principal",
        MIC50 = 2, MIC90 = 4, MBC50 = 2, MBC90 = 4,
        Basis = "MIC x 1",
        Note = "Other series put the MSSA MIC90 at 8 to 16 mg/L, which would double or quadruple these MBCs.",
        Source = c(
          "MIC50 2, MIC90 4 in 48 isolates (Haynes AS et al., Microbiol Spectr 2022;10:e01039-22, CLSI broth",
          "microdilution); four times the MIC of cephalexin '99.9 to 100% bactericidal to S. aureus in 24 h'",
          "(Lauderdale BL et al., Antimicrob Agents Chemother 1986;29:560-564); the cephalosporin MBC/MIC",
          "ratio as for cefazolin (Jones 1977; Pelletier and Baker 1988).")),
    row("cefalexin", "Beta-haemolytic streptococci (S. pyogenes)", "core",
        MIC90 = 2, MBC90 = 4,
        Basis = "MIC x 2 (class analogues)",
        Note = "MBC/MIC at most 2 in log phase; stationary-phase cultures were often tolerant.",
        Source = c(
          "MIC90 <= 2 for beta-haemolytic streptococci (Zhou CM et al., Zhonghua Jie He He Hu Xi Za Zhi",
          "2013;36:346-350); all group A strains inhibited by 3.1 (Thornhill TS et al., Appl Microbiol",
          "1969;17:457-461); MBC/MIC <= 2 for log-phase group A streptococci with cefadroxil and cefaclor",
          "(Kamme C et al., Scand J Infect Dis 1993;25:37-42).")),
    row("cefalexin", "E. coli, Klebsiella, P. mirabilis (uncomplicated urinary infection)", "excluded",
        MIC90 = 16,
        Note = paste("MICs of 4 to 16 mg/L; cefalexin treats cystitis through its urine concentration, which",
                     "is far above the plasma curve, so plasma is not the concentration to compare."),
        Source = c(
          "Thornhill 1969 (80% of E. coli inhibited by 12.5 mg/L); the EUCAST and CLSI uncomplicated-UTI",
          "breakpoint of 16 mg/L (secondary summary, to be checked).")),

    # --- Ceftriaxone ---------------------------------------------------------
    row("ceftriaxone", "Enterobacterales (E. coli, Klebsiella, Proteus), susceptible", "principal",
        MIC50 = 0.25, MIC90 = 1, MBC50 = 0.5, MBC90 = 2,
        Basis = "MIC x 2",
        Note = paste("MIC50 is the upper end of the modal MICs reported, so the lower edge is an upper",
                     "estimate; MIC90 is the susceptible breakpoint, 1 mg/L. No modern measured MBC was found."),
        Source = c(
          "All Enterobacteriaceae MICs <= 1, modal MICs 0.003 (Proteus) to 0.25, 'MBC/MIC less than or equal",
          "to 2' (Cluzel M et al., Pathol Biol 1985;33:473-476); MBCs 'generally being no more than twofold",
          "greater' than MICs (Eickhoff TC, Ehret J, Antimicrob Agents Chemother 1981;19:435-442); CLSI M100",
          "36th ed. and EUCAST v16.1 susceptible breakpoint 1 mg/L.")),
    row("ceftriaxone", "Streptococcus pneumoniae", "core",
        MIC50 = 0.06, MIC90 = 1, MBC50 = 1, MBC90 = 2,
        Basis = "measured",
        Note = "The measured MBCs are from collections enriched for penicillin-nonsusceptible strains, so are high for an unselected population; 4% tolerant.",
        Source = c(
          "MIC50/MIC90 <= 0.06/1 in 2,614 US isolates (Pfaller MA et al., Microb Drug Resist 2017;23:571-579);",
          "MIC90/MBC90 1/2 in 76 paediatric invasive isolates (Sakata H, Kansenshogaku Zasshi 2013;87:1-5);",
          "MBC50 1, MBC/MIC <= 8 in 96% of 50 strains (Pottumarthy S, Sader HS, Jones RN, Diagn Microbiol",
          "Infect Dis 2007;57:345-349).")),
    row("ceftriaxone", "Haemophilus influenzae", "core",
        MIC90 = 0.03, MBC90 = 0.25,
        Basis = "measured",
        Source = c(
          "MIC90 <= 0.03 (Biedenbach DJ et al., J Antimicrob Chemother 2016;71:3459-3464); MBC90 0.25 in",
          "18 paediatric blood isolates (Sakata H, J Infect Chemother 2006;12:338-342); MBC90 0.20-0.39",
          "(Deguchi K et al., Jpn J Antibiot 1984;37:1701-1713).")),
    row("ceftriaxone", "Neisseria meningitidis", "core",
        MBC90 = 0.25,
        Basis = "measured (pooled)",
        Note = "MBC90 pooled with pneumococci and H. influenzae from cerebrospinal fluid; no species-specific value was readable.",
        Source = "Bryan JP et al., Antimicrob Agents Chemother 1985;28:361-368 (54 isolates, MBC90 <= 0.06 to 0.25)."),
    row("ceftriaxone", "Beta-haemolytic streptococci (group B)", "core",
        MIC90 = 0.1, MBC90 = 0.4,
        Basis = "measured",
        Source = "MIC 0.05-0.1, MBC 0.1-0.4 in 24 strains (Delaplane D et al., J Antimicrob Chemother 1983;11:69-73)."),
    row("ceftriaxone", "Neisseria gonorrhoeae", "core",
        MIC50 = 0.007, MBC50 = 0.015,
        Basis = "MIC x 2",
        Note = "Single-dose intramuscular use; no MIC90 or MBC90 was readable.",
        Source = c(
          "MIC50 < 0.007 in 100 isolates, MICs and MBCs 'within a log2 concentration of each other'",
          "(Liebowitz LD et al., Antimicrob Agents Chemother 1982;22:598-603).")),
    row("ceftriaxone", "Staphylococcus aureus, methicillin-susceptible (MSSA)", "excluded",
        MIC90 = 4,
        Note = "MIC90 4 mg/L, and the MBC rises at large inocula: ceftriaxone is not a reliable bactericidal drug for staphylococcal infection.",
        Source = c(
          "Biedenbach 2016 (MIC90 4); Eickhoff and Ehret 1981 (MBC raised with large S. aureus inocula);",
          "EUCAST note on cefotaxime and ceftriaxone for S. aureus, 2023 (secondary summary, to be checked).")),
    row("ceftriaxone", "ESBL- or AmpC-producing Enterobacterales", "excluded",
        Note = "About 14% of US E. coli and 12.5% of K. pneumoniae produced an ESBL in 2016-2020; a TEM-3 strain had an MBC of 128 mg/L.",
        Source = c(
          "Castanheira M et al., Open Forum Infect Dis 2023;10:ofad038; Caron F et al., Antimicrob Agents",
          "Chemother 1990;34:2070-2074.")),

    # --- Clindamycin ---------------------------------------------------------
    # Bacteriostatic against S. aureus, the MIC table's organism, so the band
    # is built from the organisms it kills: group A streptococci (principal)
    # and susceptible Bacteroides.
    row("clindamycin", "Tonsillitis isolates (streptococci and mixed flora)", "principal",
        MIC50 = 0.25, MIC90 = 0.25, MBC50 = 0.25, MBC90 = 0.25,
        Basis = "measured",
        Note = paste("The one measured MBC found: 120 aerobic and anaerobic strains from recurrent tonsillitis,",
                     "not group A streptococci alone; 0.25 mg/L is probably the lowest concentration tested, and",
                     "modern group A MIC90s are lower (0.094 mg/L)."),
        Source = c(
          "Muller R, Wichmann G, Haufe E, Arzneimittelforschung 1996;46:534-538 ('MIC and MBC for 91.7% of",
          "the strains amounted to 0.25 micrograms/ml'); group A streptococcal MIC90 0.094 in 513 isolates",
          "(Melo-Cristino J et al., Rev Port Pneumol 2006;12:9-30).")),
    row("clindamycin", "Streptococcus pneumoniae", "core",
        Basis = "no MBC; serum bactericidal titres",
        Note = "No MBC was found, but serum after 300 mg every 8 hours was bactericidal for the whole dosing interval.",
        Source = c(
          "Klepser ME et al., Antimicrob Agents Chemother 1997;41:630-635 (serum bactericidal titres in",
          "12 volunteers).")),
    row("clindamycin", "Bacteroides fragilis group, clindamycin-susceptible", "core",
        MBC90 = 1.6,
        Basis = "measured, 80% of isolates (MBC80)",
        Note = paste("No MBC90 was found: 1.6 mg/L killed 80% of 29 isolates, the highest killing level",
                     "reported at a concentration serum reaches. The MBC is 4 to 128 times the MIC, killing is",
                     "slow and does not rise with concentration, and 25 to 60% of isolates are now resistant.",
                     "The FDA label no longer lists B. fragilis among the organisms clindamycin is active against",
                     "in clinical infections, but still doses for it: 1200 to 2700 mg a day intravenously."),
        Source = c(
          "Levison ME et al., J Infect Dis 1977;135 Suppl:S49-S53 ('bactericidal against 80%' at 1.6",
          "micrograms/ml); Zabransky RJ et al., Antimicrob Agents Chemother 1973;3:152-156 (MBC 4 to 128",
          "times the MIC); Klepser ME et al., Antimicrob Agents Chemother 1996;40:1941-1944 (no association",
          "of concentration with killing up to 64 times the MIC); Boiten KE et al., J Antimicrob Chemother",
          "2024;79:868-874 (MIC50 0.75, MIC90 > 256); FDA Cleocin Phosphate label, revised 12/2021, Dosage and",
          "Administration ('proven or suspected Bacteroides fragilis').")),
    row("clindamycin", "Beta-haemolytic streptococci, group G", "excluded",
        Note = "15 of 25 strains were tolerant: clindamycin's bactericidal activity was 'disappointing'.",
        Source = "Bayer AS, Lam K, Clin Ther 1983;5:391-397."),
    row("clindamycin", "Anaerobes other than Bacteroides (Fusobacterium, Prevotella, C. perfringens, peptostreptococci)", "excluded",
        Note = "At 2 mg/L clindamycin killed 99.9% of only 3 of 11 anaerobic strains in 24 hours: not reliably bactericidal.",
        Source = "Yagi BH, Zurenko GE, Anaerobe 2003;9:1-3."),
    row("clindamycin", "Staphylococcus aureus", "excluded",
        Note = paste("Clindamycin is bacteriostatic against S. aureus: the MBC is many times the MIC, and serum",
                     "at usual doses mostly does not kill it. The time-until-threshold line, free drug at the",
                     "MIC, is the measure for staphylococci."),
        Source = c(
          "FDA Cleocin Phosphate (revised 12/2021) and Cleocin HCl labels, Microbiology: 'Clindamycin is",
          "bacteriostatic.' Most serum samples after",
          "300 and 600 mg orally 'did not show bactericidal activity against S. aureus' (Dan M et al.,",
          "Chemotherapy 1997;43:227-231); serum bactericidal against S. aureus for 50 to 85% of the dosing",
          "interval against 100% for S. pneumoniae (Klepser ME et al., Antimicrob Agents Chemother",
          "1997;41:630-635).")),

    # --- Gentamicin ----------------------------------------------------------
    row("gentamicin", "Enterobacterales (E. coli, Klebsiella, Enterobacter)", "principal",
        MIC50 = 0.5, MIC90 = 2, MBC50 = 0.5, MBC90 = 2,
        Basis = "MIC x 1",
        Note = paste("No modern species-level MBC was found; the aminoglycosides are rapidly bactericidal and",
                     "the MBC is taken as the MIC. The MIC90 equals the CLSI (2023) and EUCAST susceptible breakpoint."),
        Source = c(
          "MIC50/MIC90 0.5/2 in 9,809 US isolates, 2017-2021 (Sader HS et al., Open Forum Infect Dis",
          "2023;10:ofad058), which describes the aminoglycosides' 'rapid, concentration-dependent,",
          "bactericidal action'; the FDA label: 'bactericidal in vitro against Gram-positive and",
          "Gram-negative bacteria'.")),
    row("gentamicin", "Pseudomonas aeruginosa", "core",
        MIC90 = 1, MBC90 = 8,
        Basis = "measured",
        Note = paste("The MIC90 was measured at a lower inoculum than the MBC90. CLSI withdrew its gentamicin",
                     "breakpoints for P. aeruginosa in 2023 (secondary summary): without this row the upper edge would be 2 mg/L."),
        Source = c(
          "40 clinical isolates: MIC90 1 at 5 x 10^3 CFU/mL, MBC90 8 at 5 x 10^5 and still 8 at 5 x 10^7",
          "(Corrado ML et al., Antimicrob Agents Chemother 1980;18:893-896).")),
    row("gentamicin", "ESBL-producing and carbapenem-resistant Enterobacterales", "excluded",
        Note = "MIC90 above 16 mg/L.",
        Source = "Sader 2023 (ESBL MIC50/MIC90 1/>16; CRE 4/>16)."),
    row("gentamicin", "Enterococci and staphylococci", "excluded",
        Note = "Gentamicin is given against them only for synergy with a cell-wall agent; the E. faecalis control strain's MIC is 4 to 16 mg/L.",
        Source = "FDA gentamicin injection label, 2013."),

    # --- Metronidazole -------------------------------------------------------
    row("metronidazole", "Bacteroides fragilis group", "principal",
        MIC50 = 0.5, MIC90 = 1, MBC50 = 1, MBC90 = 4,
        Basis = "MBC50: MIC x 2; MBC90: measured",
        Note = paste("Metronidazole is rapidly bactericidal: the MBC is the MIC to four times it. The MBC90",
                     "is the concentration that killed every one of 300 isolates."),
        Source = c(
          "MIC50/MIC90 0.5/1 (Boiten KE et al., J Antimicrob Chemother 2024;79:868-874); the FDA label:",
          "'generally bactericidal at concentrations equal to or slightly higher than the minimal",
          "inhibitory concentrations'; MBC one to two times the MIC (Garcia-Rodriguez JA et al., J",
          "Chemother 1990;2:11-16); 'all isolates being killed with <= 4 micrograms/ml at 24 hr' (Stratton",
          "CW et al., Diagn Microbiol Infect Dis 1991;14:377-382 and 1992;15:321-330); an MBC/MIC ratio of 4",
          "(Hu 2014, Int J Antimicrob Agents 2014;44:514-519, by comparison with levornidazole).")),
    row("metronidazole", "Other anaerobes (Prevotella, Fusobacterium, Clostridium, peptostreptococci)", "core",
        MIC90 = 1, MBC90 = 2,
        Basis = "MIC x 2",
        Source = c(
          "Prevotella MIC90 1 (Boiten 2024); MBC no more than 3 mg/L for susceptible anaerobes (Ralph ED,",
          "Clarke DA, Antimicrob Agents Chemother 1978;14:377-383); 114 anaerobes inhibited by 3.1 and",
          "killed by 6.3 (Wust J, Antimicrob Agents Chemother 1977;11:631-637).")),
    row("metronidazole", "Cutibacterium acnes, Actinomyces and other non-spore-forming Gram-positive rods", "excluded",
        Note = "Intrinsically resistant: the MBC for C. acnes exceeded 1,500 mg/L.",
        Source = "Ralph and Clarke 1978."),
    row("metronidazole", "Clostridioides difficile", "excluded",
        MIC90 = 2,
        Note = "Oral metronidazole acts in the colonic lumen, so plasma is not the concentration to compare.",
        Source = "MIC50/MIC90 0.5/2 in 593 US isolates (Gargis AS et al., Clin Infect Dis 2023;76:890-896)."),

    # --- Vancomycin ----------------------------------------------------------
    row("vancomycin", "Staphylococcus aureus, MRSA and MSSA", "principal",
        MIC50 = 1, MIC90 = 1, MBC50 = 2, MBC90 = 8,
        Basis = "measured",
        Note = paste("More than 70% of isolates had an MBC equal to the MIC; the MBC90 is raised by the tolerant",
                     "minority (4 to 27% of isolates by centre and method). MBCs are from MRSA; MSSA's are lower."),
        Source = c(
          "MBC50 2, MBC90 8 in 163 MRSA bacteraemia isolates (Honda H et al., BMC Infect Dis 2011;11:335);",
          "MBC90 8 in methicillin-resistant staphylococci (Rouse MS et al., Diagn Microbiol Infect Dis 2007;58:363-365);",
          "MIC 1 the modal MIC and",
          "MIC90 (Diekema DJ et al., Open Forum Infect Dis 2019;6(Suppl 1):S47-S53).")),
    row("vancomycin", "S. aureus, tolerant isolates, hVISA and VISA", "excluded",
        Note = "MBC 16 to more than 64 mg/L, which free vancomycin does not reach; tolerant in 69% of hVISA and all VISA.",
        Source = "Sader HS, Fritsche TR, Jones RN, Antimicrob Agents Chemother 2006;50:2330-2336 (tolerance in 14.7% of wild-type MRSA, 69.3% of hVISA, 100% of VISA)."),
    row("vancomycin", "Enterococci", "excluded",
        MIC90 = 4,
        Note = "Vancomycin does not kill enterococci (E. faecalis MBC90 above 128 mg/L, every isolate tolerant); the label combines it with an aminoglycoside for enterococcal endocarditis.",
        Source = "Sandoe JAT et al., J Antimicrob Chemother 2006;57:767-770 (58 catheter-related bloodstream isolates); FDA vancomycin injection label, 2024."),
    row("vancomycin", "Viridans group streptococci", "excluded",
        MIC90 = 1,
        Note = "MIC at most 1 mg/L but the MBC above 128 mg/L in 21 of 24 endocarditis isolates (1980s data).",
        Source = "Meylan PR, Francioli P, Glauser MP, Antimicrob Agents Chemother 1986;29:418-423."),
    row("vancomycin", "Coagulase-negative staphylococci", "excluded",
        Note = "No usable MBC50 or MBC90 was found; MICs run about one doubling dilution above S. aureus.",
        Source = "Rouse 2007 pools them with MRSA.")
  )
  x <- do.call(rbind, rows)
  rownames(x) <- NULL
  x
}


#' One antibiotic's MBC band
#'
#' Applies the band rule (see the header of R/antibioticMbc.R) to the drug's
#' rows of \code{antibioticMbcTable()}.
#'
#' @param drug a drug name
#' @returns NULL when the drug has no MBC rows; otherwise a list of
#'   \code{free} and \code{plotted}, each a named vector (lower, typical,
#'   upper) in mg/L, the plotted one rounded to two significant figures, and
#'   \code{principal}, the principal pathogen
#' @keywords internal
antibioticMbcBand <- function(drug)
{
  x <- antibioticMbcTable()
  x <- x[x$Drug == drug, ]
  if (!nrow(x)) return(NULL)
  principal <- x[x$Role == "principal", ]
  if (nrow(principal) != 1) stop("an antibiotic's MBC band needs exactly one principal pathogen: ", drug)
  killed <- x[x$Role %in% c("principal", "core"), ]
  free <- c(lower   = principal$MBC50,
            typical = principal$MBC90,
            upper   = max(killed$MBC90, na.rm = TRUE))
  list(free = free,
       plotted = signif(antibioticPlottedConcentration(drug, free), 2),
       principal = principal$Pathogen)
}
