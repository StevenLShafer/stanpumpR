## Antibiotics: the MIC line and the MBC band

Each of the seven antibiotics, [cefazolin](help:drugs/cefazolin), [cefalexin](help:drugs/cefalexin), [ceftriaxone](help:drugs/ceftriaxone), [clindamycin](help:drugs/clindamycin), [gentamicin](help:drugs/gentamicin), [metronidazole](help:drugs/metronidazole) and [vancomycin](help:drugs/vancomycin), is drawn against two pharmacodynamic markers, both written on **free** (unbound) drug, which is what reaches the organism:

- **The MIC**, the minimum inhibitory concentration, at which growth stops. It is the drug's threshold for *Time until threshold*: the line shows how long, with no further dose, until free drug falls below the MIC of the organism the drug is mainly given against. See [Time until threshold](help:models/recovery).
- **The MBC**, the minimum bactericidal concentration, at which 99.9 per cent of the organism is killed in 24 hours. The shaded band marks the expected MBCs of the common pathogens the drug is given against.

### The rule for the band

Every antibiotic's band follows one rule:

- **Lower edge**: the MBC50 of the principal pathogen. Below it the drug no longer kills a typical isolate of the organism it is mainly given for.
- **Typical** (the line drawn with *Show typical: Mid*): the MBC90 of the principal pathogen, the free concentration that kills 90 per cent of its isolates.
- **Upper edge**: the highest MBC90 among the principal pathogen and the other common pathogens the drug kills at concentrations the curve reaches. Above it, free drug kills 90 per cent of the isolates of each of them.

The principal pathogen is usually the hardest of the common pathogens to kill, so the typical value often sits at the upper edge.

The principal pathogen is the MIC line's organism wherever the drug kills it. **Clindamycin** is the exception: the FDA label calls it bacteriostatic, and against *Staphylococcus aureus* its MBC is many times the MIC. Its band is built from the organisms it does kill, and for staphylococci the MIC line is the measure.

Organisms the drug does not kill at attainable levels are listed on each drug's page but never move the band. Examples are enterococci and most viridans streptococci with vancomycin, tolerant staphylococci, and ESBL-producing Gram-negatives. So is an organism whose infection is treated through urine or the colonic lumen rather than plasma.

MBCs are measured in protein-free broth, so they are free-drug values. Cefazolin's curve is unbound drug and its band is drawn at the MBCs themselves. The others plot total drug, so each free MBC is placed on the curve through the drug's binding: divided by the free fraction, or, for ceftriaxone and clindamycin, whose binding saturates, through the one-site binding fit.

<!-- generated: antibiotic-mbc-table -->

### How much to trust it

Less than the MIC line. The MIC is the measurement breakpoints are set on; the MBC is a research measurement, and most of these numbers are not modern:

- **Most MBCs are inferred.** Only vancomycin (MRSA), ceftriaxone (pneumococci, *Haemophilus*, streptococci), gentamicin (*Pseudomonas*), metronidazole (*Bacteroides*) and clindamycin have measured MBC50s or MBC90s that could be read. The rest are an MIC times the MBC/MIC ratio reported for the drug or its class against that organism. A cephalosporin's MBC against a susceptible staphylococcus is the MIC or one dilution above it; an aminoglycoside's is about its MIC; metronidazole's is the MIC to four times it.
- **The data are old.** Many of the MBCs are from 1970s and 1980s broth studies, before current methods and before today's resistance.
- **The MBC is poorly reproducible.** It varies with the medium, the incubation time and, above all, the inoculum. Cefazolin is the extreme case: a fifth to a third of methicillin-susceptible staphylococci lose cefazolin activity at high inoculum. In biofilm the MBC is orders of magnitude higher.
- **Tolerance.** Some isolates of an otherwise susceptible organism have an MBC 32 times the MIC or more: up to a quarter of MRSA with vancomycin, and many stationary-phase streptococci with the oral cephalosporins.
- **Time and peak matter as well as level.** The beta-lactams and vancomycin kill by time above the MIC. Gentamicin kills by its peak, 8 to 10 times the MIC, and its trough is meant to fall below the band.

The band is orientation for a susceptible isolate, not a dosing target. An isolate's own MIC, where known, is the better guide: set it under **Settings → Drug Thresholds**.

### Sources that would firm up the bands

The MICs and MBCs were read from PubMed abstracts, PubMed Central full text and the FDA labels. The documents below could not be opened when the bands were built, or carry tables that were not readable, and are what a revision should read first.

**Product monographs and regulatory documents**

- **FDA labels.** The microbiology sections of the clindamycin (Cleocin Phosphate, revised 12/2021, Reference ID 4899100; Cleocin HCl, Reference ID 4572239), metronidazole (injection and tablets), vancomycin and gentamicin labels were read in full. The cefazolin, cefalexin (Keflex) and ceftriaxone labels were not: their section 12.4 and the FDA's interpretive criteria (STIC) pages for each drug at fda.gov/STIC.
- **EMA.** The Rocephin (ceftriaxone) Article 30 referral, EMEA/H/A-30/1302 (2014), with its harmonised SmPC; and the vancomycin Article 31 referral (2017), which moved European labelling towards AUC-guided dosing. Cefazolin, cefalexin, clindamycin, metronidazole and gentamicin are nationally authorised and have no centralised EMA document. Their national SmPCs (the UK SmPCs on medicines.org.uk, for example) carry section 5.1. The UK metronidazole infusion SmPC says efficacy depends mainly on the peak-to-MIC ratio; the Dalacin C SmPC calls clindamycin bacteriostatic at usual doses.
- **EUCAST.** The rationale documents for ceftriaxone (2023), vancomycin and the aminoglycosides (the 2020 revision and its bracketed breakpoints), and the anaerobe breakpoints. Also the 2025–2026 guidance on cephalosporins for *S. aureus* infection, which discusses the cefazolin inoculum effect, and the 2023 note on cefotaxime and ceftriaxone for *S. aureus*. The EUCAST MIC distributions (mic.eucast.org) give the wild-type MICs the inferred MBCs start from.
- **CLSI.** M100 (the current breakpoints, including the 2023 aminoglycoside revision and the cefazolin breakpoints for Enterobacterales); M11 for anaerobes; and M26-A, the reference method for the MBC itself.

**Papers whose MBC tables were not readable**

- Cefazolin against MSSA, streptococci and Enterobacterales: Reimer, Mirrett and Reller, *Antimicrob Agents Chemother* 1980;17:412-416 (420 blood isolates); Sweeney and colleagues, *Diagn Microbiol Infect Dis* 2017;87:121-128; Fabry and Kock, *J Hosp Infect* 2014;86:68-72.
- Cefalexin: Zabransky and colleagues, *Mayo Clin Proc* 1969;44:876.
- Ceftriaxone: the species tables of Bryan and colleagues, *Antimicrob Agents Chemother* 1985;28:361-368 (meningococcus alone), and a modern Enterobacterales and *Moraxella* MBC.
- Clindamycin against streptococci and anaerobes: Westerman and colleagues, *Antimicrob Agents Chemother* 1976;9:988-993; Chisari and colleagues, *J Chemother* 1995;7:515-518.
- Metronidazole: the full table of Hu and colleagues, *Int J Antimicrob Agents* 2014;44:514-519, whose abstract gives an MBC/MIC ratio of 4 but not the values.
- Vancomycin: Traczewski and colleagues 2009 (479 MRSA); Jones, *Clin Infect Dis* 2006 (SENTRY MBCs by organism); Hwang and colleagues, *Yonsei Med J* 2000;41:563 (coagulase-negative staphylococci and enterococci); Britt and colleagues, *Antimicrob Agents Chemother* 2016;60:3070-3075, the one clinical study to relate outcome to AUC over the MBC.

**Pharmacokinetic-pharmacodynamic targets beside the band**

- The French SFPT-SFAR guideline (Guilhaumou and colleagues, *Crit Care* 2019;23:104) targets free beta-lactam concentrations of 4 to 8 times the MIC for the whole interval, capped for neurotoxicity.
- Rybak and colleagues, *Am J Health Syst Pharm* 2020;77:835-864, set the vancomycin AUC target.
- Kashuba and colleagues, *Antimicrob Agents Chemother* 1999;43:623-629, and Moore, Lietman and Smith, *J Infect Dis* 1987;155:93-99, set the aminoglycoside peak-to-MIC target.
