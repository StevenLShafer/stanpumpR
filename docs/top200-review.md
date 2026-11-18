# The ClinCalc Top 200 drugs against the stanpumpR library

Reviewed 2026-11-15 by Claude Code at the request of Steven L. Shafer, who asked that most of the 200
most prescribed US drugs be in stanpumpR, that topical drugs and vitamins and minerals be left out, and
that drugs with narrow therapeutic windows come first. The list is ClinCalc DrugStats' Top 200
(https://clincalc.com/drugstats/Top200Drugs.aspx, read 2026-11-15; 2023 prescription counts from MEPS).

Each drug is added the way every library drug is (docs/adding-a-drug.md): a full-text source, the six
pieces, the FDA label. Drug classes that have a registry (antihypertensives, antidiabetics, Parkinson
disease, antiseizure) are added through it, and their registry rows say how far each drug has got.

## Summary

- In the library: 78 entries (some combinations only in part).
- Excluded (topical, inhaled, ophthalmic, vitamin, mineral, non-absorbed, supplement): 31.
- Missing: 91. Priority 0, requested now: 2 (sildenafil and tadalafil, with the pulmonary hypertension
  drugs). Priority 1, narrow therapeutic window: 4 (levothyroxine, methotrexate, insulin aspart, insulin
  degludec). Priority 2, perioperative or ICU use: 41. Priority 3, chronic outpatient use: 44.

## Missing, by priority

| Rank | Drug | Priority | Note |
|---|---|---|---|
| 119 | Tadalafil | 0 requested | added 2026-11-15 (`tadalafil`, Ferguson-Sells 2022, pulmonary hypertension; healthy-subject clearance pending a decision) |
| 160 | Sildenafil | 0 requested | added 2026-11-15 (`sildenafil`, FDA review of NDA 22-473, IV and oral, healthy volunteers) |
| 2 | Levothyroxine | 1 narrow window | added 2026-11-18 (`levothyroxine`, from papers Shafer supplied): one compartment from Inada 1975 (Table II, T4, normal subjects, extrapolation method: V 8.4 L and clearance 1.06 L/day per 60 kg), absorption rate and lag from Hays 1991 (Table 1, his abnormally low subject 1 excluded), bioavailability 0.48 from Surks 1973 (tablets at steady state), all on Shafer's decisions of 2026-11-18; filed under Other until stanpumpR has other hormones; oral and IV, total T4 above baseline. Inada's three-pool T4 model is not fully printed (two numbers missing), so the distribution phase is not carried; decisions pending in R/drugs_levothyroxine.R |
| 79 | Insulin Aspart | 1 narrow window | added 2026-11-18 on Shafer's request (`insulinAspart`, NovoLog; `insulinAspartFiasp`, Fiasp): the FDA review of Fiasp, NDA 208751, a depot then three transit compartments, run exactly by the transit-chain engine's new subcutaneous route; the unprinted reference weight (75 kg assumed) is a pending decision |
| 122 | Insulin Degludec | 1 narrow window | added 2026-11-18 (`insulinDegludec`, Tang 2026, joint INS068/degludec model, type 1 diabetes typical values; Shafer's decisions of 2026-11-18: type 2 factors not applied, weight held at 93.1 kg above and carried on below for children (FDA pediatric review, exponent 0.977), pregnancy CL/F x 1.25) |
| 145 | Methotrexate | 1 narrow window | in flight in PR #684 (Chemotherapy: Ibarra 2023 adults, Taylor 2023 children), so not added here; Blackman 2026 (Eur J Clin Pharmacol 82:156, 208 adults, read in full) is a candidate adult source for it |
| 4 | Amlodipine | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 7 | Losartan | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 10 | Omeprazole | 2 perioperative / ICU | acid suppression before anaesthesia |
| 16 | Pantoprazole | 2 perioperative / ICU | acid suppression before anaesthesia |
| 20 | Trazodone | 2 perioperative / ICU | sedating or antiemetic adjuncts |
| 22 | Amoxicillin | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 29 | Furosemide | 2 perioperative / ICU | diuretics: antihypertensive registry rows exist (descriptive or partial extraction) |
| 37 | Famotidine | 2 perioperative / ICU | acid suppression before anaesthesia |
| 39 | Buspirone | 2 perioperative / ICU | sedating or antiemetic adjuncts |
| 43 | Hydroxyzine | 2 perioperative / ICU | sedating or antiemetic adjuncts |
| 45 | Clopidogrel | 2 perioperative / ICU | antiplatelet before surgery; prodrug, active thiol metabolite, CYP2C19 (field exists); the perioperative question is platelet recovery after an irreversible effect, not a concentration window |
| 49 | Cyclobenzaprine | 2 perioperative / ICU | muscle relaxants (baclofen renal, tizanidine CYP1A2) |
| 50 | Spironolactone | 2 perioperative / ICU | diuretics: antihypertensive registry rows exist (descriptive or partial extraction) |
| 62 | Azithromycin | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 71 | Atenolol | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 73 | Doxycycline | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 74 | Amoxicillin; Clavulanate | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 83 | Clonidine | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 88 | Tizanidine | 2 perioperative / ICU | muscle relaxants (baclofen renal, tizanidine CYP1A2) |
| 91 | Hydrochlorothiazide; Losartan | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 103 | Valacyclovir | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 107 | Methocarbamol | 2 perioperative / ICU | muscle relaxants (baclofen renal, tizanidine CYP1A2) |
| 111 | Baclofen | 2 perioperative / ICU | muscle relaxants (baclofen renal, tizanidine CYP1A2) |
| 115 | Esomeprazole | 2 perioperative / ICU | acid suppression before anaesthesia |
| 120 | Nifedipine | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 133 | Irbesartan | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 135 | Sulfamethoxazole; Trimethoprim | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 137 | Nitrofurantoin | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 138 | Isosorbide | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 140 | Hydrochlorothiazide; Triamterene | 2 perioperative / ICU | diuretics: antihypertensive registry rows exist (descriptive or partial extraction) |
| 158 | Prazosin | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 159 | Ciprofloxacin | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 170 | Cefdinir | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 172 | Meclizine | 2 perioperative / ICU | sedating or antiemetic adjuncts |
| 174 | Fluconazole | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 184 | Sacubitril; Valsartan | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 186 | Nebivolol | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 187 | Acyclovir | 2 perioperative / ICU | anti-infectives: need the MIC line and MBC band (antifungal/antiviral: a threshold instead) |
| 189 | Doxazosin | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 190 | Amlodipine; Benazepril | 2 perioperative / ICU | antihypertensive registry rows exist; implementation follows docs/antihypertensives.md |
| 199 | Torsemide | 2 perioperative / ICU | diuretics: antihypertensive registry rows exist (descriptive or partial extraction) |
| 1 | Atorvastatin | 3 chronic outpatient | lipid-lowering; no concentration target |
| 9 | Rosuvastatin | 3 chronic outpatient | lipid-lowering; no concentration target |
| 21 | Montelukast | 3 chronic outpatient | allergy and cough |
| 24 | Tamsulosin | 3 chronic outpatient | urology |
| 26 | Simvastatin | 3 chronic outpatient | lipid-lowering; no concentration target |
| 44 | Allopurinol | 3 chronic outpatient | oxypurinol carries the effect; renal |
| 47 | Cetirizine | 3 chronic outpatient | allergy and cough |
| 52 | Estradiol | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 54 | Glipizide | 3 chronic outpatient | antidiabetic: the antidiabetic registry governs (only PK-FIT rows are selectable) |
| 63 | Pravastatin | 3 chronic outpatient | lipid-lowering; no concentration target |
| 65 | Ezetimibe | 3 chronic outpatient | lipid-lowering; no concentration target |
| 68 | Ethinyl Estradiol; Norethindrone | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 81 | Finasteride | 3 chronic outpatient | urology |
| 86 | Alendronate | 3 chronic outpatient | bisphosphonate: plasma half-life of hours, bone of years |
| 93 | Testosterone | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 97 | Fenofibrate | 3 chronic outpatient | lipid-lowering; no concentration target |
| 101 | Loratadine | 3 chronic outpatient | allergy and cough |
| 105 | Ethinyl Estradiol; Norgestimate | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 113 | Sumatriptan | 3 chronic outpatient | migraine |
| 118 | Norethindrone | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 121 | Progesterone | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 125 | Adalimumab | 3 chronic outpatient | monoclonal antibodies: target-mediated disposition |
| 127 | Sitagliptin | 3 chronic outpatient | antidiabetic: the antidiabetic registry governs (only PK-FIT rows are selectable) |
| 128 | Glimepiride | 3 chronic outpatient | antidiabetic: the antidiabetic registry governs (only PK-FIT rows are selectable) |
| 130 | Oxybutynin | 3 chronic outpatient | urology |
| 134 | Hydroxychloroquine | 3 chronic outpatient | very long half-life; retinal toxicity is cumulative over years; whole-blood monitoring proposed |
| 136 | Benzonatate | 3 chronic outpatient | allergy and cough |
| 141 | Levocetirizine | 3 chronic outpatient | allergy and cough |
| 143 | Atomoxetine | 3 chronic outpatient | stimulants and ADHD (atomoxetine CYP2D6) |
| 144 | Dexmethylphenidate | 3 chronic outpatient | stimulants and ADHD (atomoxetine CYP2D6) |
| 146 | Donepezil | 3 chronic outpatient | neurology (ropinirole is in the Parkinson registry) |
| 148 | Ropinirole | 3 chronic outpatient | neurology (ropinirole is in the Parkinson registry) |
| 152 | Phentermine | 3 chronic outpatient | stimulants and ADHD (atomoxetine CYP2D6) |
| 161 | Ethinyl Estradiol; Levonorgestrel | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 162 | Evolocumab | 3 chronic outpatient | lipid-lowering; no concentration target |
| 164 | Memantine | 3 chronic outpatient | neurology (ropinirole is in the Parkinson registry) |
| 165 | Ethinyl Estradiol; Etonogestrel | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 175 | Rimegepant | 3 chronic outpatient | migraine |
| 177 | Lovastatin | 3 chronic outpatient | lipid-lowering; no concentration target |
| 178 | Drospirenone; Ethinyl Estradiol | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |
| 191 | Nirmatrelvir; Ritonavir | 3 chronic outpatient | antivirals (ritonavir boosting; tenofovir as the prodrug) |
| 192 | Emtricitabine; Tenofovir | 3 chronic outpatient | antivirals (ritonavir boosting; tenofovir as the prodrug) |
| 195 | Dupilumab | 3 chronic outpatient | monoclonal antibodies: target-mediated disposition |
| 197 | Anastrozole | 3 chronic outpatient | sex hormones and contraceptives: many routes and products; endogenous baselines |

## In the library

| Rank | Drug | Library model | 
|---|---|---|
| 3 | Metformin | metformin |
| 5 | Lisinopril | lisinopril |
| 8 | Metoprolol | metoprolol |
| 11 | Gabapentin | gabapentin |
| 12 | Sertraline | sertraline |
| 13 | Escitalopram | escitalopram |
| 14 | Semaglutide | semaglutide, semaglutideOral |
| 15 | Dextroamphetamine; Dextroamphetamine Saccharate; Amphetamine; Amphetamine Aspartate | mixedAmphetamineSalts |
| 17 | Bupropion | bupropion |
| 18 | Hydrochlorothiazide | hydrochlorothiazide |
| 19 | Fluoxetine | fluoxetine |
| 25 | Apixaban | apixaban, apixabanVTE |
| 27 | Insulin Glargine | insulinGlargine |
| 28 | Empagliflozin | empagliflozin |
| 30 | Meloxicam | meloxicam |
| 31 | Acetaminophen; Hydrocodone | acetaminophen, hydrocodone |
| 32 | Tirzepatide | tirzepatide |
| 33 | Methylphenidate | methylphenidate, methylphenidatePediatric |
| 34 | Duloxetine | duloxetine |
| 35 | Prednisone | prednisone |
| 36 | Carvedilol | carvedilol |
| 38 | Ibuprofen | ibuprofen |
| 40 | Venlafaxine | venlafaxine |
| 41 | Tramadol | tramadol |
| 48 | Ondansetron | ondansetron |
| 51 | Oxycodone | oxycodone |
| 53 | Aspirin | aspirin |
| 55 | Zolpidem | zolpidem |
| 56 | Lamotrigine | lamotrigine |
| 57 | Alprazolam | alprazolam |
| 58 | Citalopram | citalopram |
| 59 | Pregabalin | pregabalin |
| 61 | Clonazepam | clonazepam |
| 64 | Valsartan | valsartan |
| 66 | Diclofenac | diclofenac, diclofenacEC |
| 67 | Insulin Lispro | insulinLispro (and the lispro mixes) |
| 69 | Propranolol | propranolol |
| 72 | Lisdexamfetamine | lisdexamfetamine |
| 75 | Dulaglutide | dulaglutide |
| 76 | Hydrochlorothiazide; Lisinopril | hydrochlorothiazide, lisinopril |
| 77 | Lorazepam | lorazepam |
| 80 | Celecoxib | celecoxib |
| 82 | Quetiapine | quetiapine |
| 84 | Aripiprazole | aripiprazole |
| 85 | Cephalexin | cefalexin |
| 87 | Topiramate | topiramate |
| 89 | Dapagliflozin | dapagliflozin, dapagliflozinKobuchi |
| 90 | Acetaminophen; Oxycodone | acetaminophen, oxycodone |
| 92 | Olmesartan | olmesartan |
| 94 | Amitriptyline | amitriptyline |
| 96 | Rivaroxaban | rivaroxaban |
| 99 | Paroxetine | paroxetine |
| 102 | Warfarin | warfarin |
| 108 | Acetaminophen | acetaminophen |
| 109 | Diltiazem | diltiazem |
| 112 | Naproxen | naproxen |
| 114 | Prednisolone | prednisolone |
| 116 | Mirtazapine | mirtazapine |
| 117 | Levetiracetam | levetiracetam |
| 124 | Hydralazine | hydralazine |
| 129 | Chlorthalidone | chlorthalidone |
| 131 | Pioglitazone | pioglitazone |
| 139 | Methylprednisolone | methylprednisolone |
| 142 | Clindamycin | clindamycin |
| 147 | Desvenlafaxine | desvenlafaxine |
| 155 | Verapamil | verapamil |
| 156 | Olanzapine | olanzapine |
| 157 | Metronidazole | metronidazole |
| 166 | Diazepam | diazepam |
| 167 | Lithium | lithiumCarbonate |
| 168 | Nortriptyline | nortriptyline |
| 173 | Cyclosporine | cyclosporine (systemic; the top-200 entry is mostly the ophthalmic emulsion) |
| 181 | Temazepam | temazepam |
| 182 | Benazepril | benazepril |
| 185 | Valproate | valproate |
| 188 | Morphine | morphine |
| 194 | Tacrolimus | tacrolimus |
| 196 | Enalapril | enalapril |

## Excluded

| Rank | Drug | Note |
|---|---|---|
| 6 | Albuterol | inhaled or nasal, acting locally |
| 23 | Fluticasone | inhaled or nasal, acting locally |
| 42 | Potassium Chloride | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 46 | Ergocalciferol | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 60 | Cholecalciferol | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 70 | Latanoprost | ophthalmic or otic |
| 78 | Fluticasone; Salmeterol | inhaled or nasal, acting locally |
| 95 | Folic Acid | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 98 | Triamcinolone | topical or nasal |
| 100 | Ferrous Sulfate | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 104 | Budesonide; Formoterol | inhaled or nasal, acting locally |
| 106 | Fluticasone; Umeclidinium; Vilanterol | inhaled or nasal, acting locally |
| 110 | Thyroid | dietary supplement (melatonin) or animal thyroid extract of variable T4/T3 content (Thyroid) |
| 123 | Cyanocobalamin | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 126 | Azelastine | topical or nasal |
| 132 | Ketoconazole | topical or nasal |
| 149 | Clobetasol | topical or nasal |
| 150 | Fluticasone; Vilanterol | inhaled or nasal, acting locally |
| 151 | Albuterol; Ipratropium | inhaled or nasal, acting locally |
| 153 | Ofloxacin | ophthalmic or otic |
| 154 | Mupirocin | topical or nasal |
| 163 | Mesalamine | not absorbed, or acting in the gut lumen |
| 169 | Timolol | ophthalmic or otic |
| 171 | Magnesium Salts | vitamin or mineral (intravenous magnesium sulfate is in the library) |
| 176 | Tretinoin | topical or nasal |
| 179 | Tiotropium | inhaled or nasal, acting locally |
| 180 | Docusate | not absorbed, or acting in the gut lumen |
| 183 | Brimonidine | ophthalmic or otic |
| 193 | Budesonide | inhaled or nasal, acting locally |
| 198 | Melatonin | dietary supplement (melatonin) or animal thyroid extract of variable T4/T3 content (Thyroid) |
| 200 | Polyethylene Glycol 3350 | not absorbed, or acting in the gut lumen |
