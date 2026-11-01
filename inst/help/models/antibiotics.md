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

- **Most MBCs are inferred.** Only vancomycin (MRSA), ceftriaxone (pneumococci, *Haemophilus*, streptococci), metronidazole (*Bacteroides*) and clindamycin have measured MBC50s or MBC90s that could be read. The rest are an MIC times the MBC/MIC ratio reported for the drug or its class against that organism. A cephalosporin's MBC against a susceptible staphylococcus is usually the MIC or one dilution above it; an aminoglycoside's is about its MIC; metronidazole's is the MIC to four times it. Cefazolin is the exception: in the one study that measured its MBCs (Jones and colleagues, 1977), a seventh of *S. aureus* (methicillin status not reported) and of *E. coli*, *Klebsiella* and *Proteus* isolates, and more than a quarter of the *Klebsiella*, had an MBC four or more times the MIC, and cephalothin's tail against *S. aureus* is longer still (Pelletier and Baker, 1988). So cefazolin's MBC90s are taken as four times the MIC90s, which puts its typical line at 4 mg/L and its upper edge at 8.
- **The data are old.** Many of the MBCs are from 1970s and 1980s broth studies, before current methods and before today's resistance.
- **The MBC is poorly reproducible.** It varies with the medium, the incubation time and, above all, the inoculum. Cefazolin is the extreme case: a fifth to a third of methicillin-susceptible staphylococci lose cefazolin activity at high inoculum. In biofilm the MBC is orders of magnitude higher.
- **Tolerance.** Some isolates of an otherwise susceptible organism have an MBC 32 times the MIC or more: up to a quarter of MRSA with vancomycin, and many stationary-phase streptococci with the oral cephalosporins.
- **Plasma is not the site of infection.** EUCAST's endocarditis guidance (2025) notes that drug penetrates vegetations poorly, which is why endocarditis is treated with doses above the PK/PD-based ones. The same holds for abscesses, bone and biofilm.
- **Time and peak matter as well as level.** The beta-lactams and vancomycin kill by time above the MIC. Gentamicin kills by its peak, 8 to 10 times the MIC, and its trough is meant to fall below the band.

The band is orientation for a susceptible isolate, not a dosing target. An isolate's own MIC, where known, is the better guide: set it under **Settings → Drug Thresholds**.

### Sources that would firm up the bands

The MICs and MBCs were read from PubMed abstracts, PubMed Central full text and the FDA labels. The documents below could not be opened when the bands were built, or carry tables that were not readable, and are what a revision should read first.

**Product monographs and regulatory documents**

- **FDA labels, read.** Clindamycin (Cleocin Phosphate, revised 12/2021, Reference ID 4899100; Cleocin HCl, Reference ID 4572239), cefazolin (cefazolin injection, revised 1/2021, Reference ID 4739968), cefalexin (Keflex, revised 12/2018, Reference ID 4365308), metronidazole (injection and tablets), vancomycin and gentamicin. None gives an MBC: the cephalosporin labels say only that the drug is bactericidal, the clindamycin labels that it is bacteriostatic.
- **FDA, still to read.** The ceftriaxone label, and the interpretive criteria (STIC) page for each drug at fda.gov/STIC.
- **EMA MIC breakpoint table, read.** EMA/916812/2022 (version of 14 April 2025) carries the EUCAST breakpoints for every EU-authorised antibacterial. It gives 'insufficient evidence' for gentamicin against *Pseudomonas*, which is why *P. aeruginosa* is not in gentamicin's band. It confirms cefalexin's Enterobacterales breakpoint (16 mg/L, uncomplicated urinary infection only). It gives cefazolin no susceptible category for *E. coli* and *Klebsiella*, only 'susceptible, increased exposure' up to 4 mg/L in infections starting in the urinary tract. Its staphylococcal clindamycin breakpoint is 0.25 mg/L, one dilution below the CLSI value of 0.5 that sets clindamycin's MIC line.
- **EMA.** The public outcomes of both referrals have been read, and neither gives an MIC, an MBC or a PK/PD target. The Rocephin (ceftriaxone) Article 30 referral (EMEA/H/A-30/1302; questions and answers EMA/34577/2014, Commission decision 21 March 2014) harmonised the indications (meningitis, pneumonia, otitis media, intra-abdominal, complicated urinary, bone and joint and complicated skin infections, gonorrhoea and syphilis, endocarditis, surgical prophylaxis) and a dose of 1 to 4 g once daily. That matches the pathogens in ceftriaxone's band. The vancomycin Article 31 referral (EMA/645879/2017, Commission decision 21 September 2017) kept infused vancomycin for serious Gram-positive infection, MRSA above all. It found that the old doses 'often resulted in suboptimal vancomycin serum concentrations', so it based the starting dose on age and weight, with later doses adjusted to serum concentrations. It names no AUC target. Still to read are the harmonised SmPCs themselves, whose section 5.1 gives the PK/PD index and the breakpoints: Annex III of the Rocephin referral, and the vancomycin assessment report. Cefazolin, cefalexin, clindamycin, metronidazole and gentamicin are nationally authorised and have no centralised EMA document. Their national SmPCs (the UK SmPCs on medicines.org.uk, for example) carry section 5.1. The UK metronidazole infusion SmPC says efficacy depends mainly on the peak-to-MIC ratio; the Dalacin C SmPC calls clindamycin bacteriostatic at usual doses.
- **EUCAST, read.** The *Guidance Document on Cephalosporins for Staphylococcus aureus Infection* (27 April 2026) prints EUCAST's *S. aureus* MIC distributions. Cefazolin's MIC50 and MIC90 for MSSA come from it: 0.5 and 1 mg/L in 17,120 wild-type isolates, ECOFF 2. It infers cefazolin susceptibility in MSSA only at 2 g every 8 hours. It confirms that ceftriaxone does not kill MSSA even at 2 g twice daily (bacteriostasis only, in a hollow-fibre model), which is why MSSA is left out of ceftriaxone's band. And it reports the cephalosporins' PK/PD targets against *S. aureus* (below). The *Guidance Document on Infective Endocarditis* (November 2025) says endocarditis is treated with doses above the PK/PD-based ones because drug penetrates vegetations poorly, so no plasma band describes a vegetation. The guidance on daptomycin and on *Stenotrophomonas maltophilia* concerns drugs not in the library.
- **EUCAST, still to read.** The rationale documents for ceftriaxone (2023), vancomycin and the aminoglycosides (the 2020 revision and its bracketed breakpoints), and the anaerobe breakpoints. The EUCAST MIC distributions (mic.eucast.org) for the other drugs and organisms would put every inferred MBC on the same footing as cefazolin's.
- **CLSI.** M100 (the current breakpoints, including the 2023 aminoglycoside revision and the cefazolin breakpoints for Enterobacterales); M11 for anaerobes; and M26-A, the reference method for the MBC itself.

**Papers whose MBC tables were not readable**

- Cefazolin against MSSA, streptococci and Enterobacterales: Reimer, Mirrett and Reller, *Antimicrob Agents Chemother* 1980;17:412-416 (420 blood isolates); Sweeney and colleagues, *Diagn Microbiol Infect Dis* 2017;87:121-128; Fabry and Kock, *J Hosp Infect* 2014;86:68-72.
- Cefalexin: Zabransky and colleagues, *Mayo Clin Proc* 1969;44:876.
- Ceftriaxone: the species tables of Bryan and colleagues, *Antimicrob Agents Chemother* 1985;28:361-368 (meningococcus alone), and a modern Enterobacterales and *Moraxella* MBC.
- Clindamycin against streptococci and anaerobes: Westerman and colleagues, *Antimicrob Agents Chemother* 1976;9:988-993; Chisari and colleagues, *J Chemother* 1995;7:515-518.
- Metronidazole: the full table of Hu and colleagues, *Int J Antimicrob Agents* 2014;44:514-519, whose abstract gives an MBC/MIC ratio of 4 but not the values.
- Vancomycin: Traczewski and colleagues 2009 (479 MRSA); Jones, *Clin Infect Dis* 2006 (SENTRY MBCs by organism); Hwang and colleagues, *Yonsei Med J* 2000;41:563 (coagulase-negative staphylococci and enterococci); Britt and colleagues, *Antimicrob Agents Chemother* 2016;60:3070-3075, the one clinical study to relate outcome to AUC over the MBC.

**Pharmacokinetic-pharmacodynamic targets beside the band**

- For the cephalosporins against *S. aureus*, EUCAST's 2026 guidance gives the free time above the MIC needed for stasis, a 1-log and a 2-log kill in animal models as about 25, 30 and 35 per cent of the interval. An in-vitro model of cefazolin and ceftriaxone needed 55, 75 and 100 per cent (100 per cent for a 3-log kill).
- The French SFPT-SFAR guideline (Guilhaumou and colleagues, *Crit Care* 2019;23:104) targets free beta-lactam concentrations of 4 to 8 times the MIC for the whole interval, capped for neurotoxicity.
- Rybak and colleagues, *Am J Health Syst Pharm* 2020;77:835-864, set the vancomycin AUC target.
- Kashuba and colleagues, *Antimicrob Agents Chemother* 1999;43:623-629, and Moore, Lietman and Smith, *J Infect Dis* 1987;155:93-99, set the aminoglycoside peak-to-MIC target.
