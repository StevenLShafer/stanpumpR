## Patient covariates

| Quantity | Entered as | Used internally | Conversion |
|---|---|---|---|
| Age | years or months | years | 1 month = 1/12 year |
| Weight | kg or lb | kg | 1 lb = 0.453592 kg |
| Height | in or cm | cm | 1 in = 2.54 cm |

## Concentrations

Each drug is plotted in the concentration units given in the drug library: **mcg/mL** (micrograms per millilitre, equal to mg/L) or **ng/mL** (nanograms per millilitre, equal to mcg/L); exenatide and lixisenatide, whose concentrations are a thousand times lower, are in **pg/mL** (picograms per millilitre, equal to ng/L). The inhaled agents are in **per cent of one atmosphere**, and MAC equivalents are dimensionless. Mannitol is plotted as the serum osmolality it produces, in **mOsm/kg**; calcium and magnesium sulfate as the rise in serum concentration, in **mmol/L**; the heparins as an activity, in **IU/mL**, or (heparin on bypass) as the activated clotting time in seconds; and warfarin, fluindione and acenocoumarol as the **INR**. The insulins and vasopressin, dosed in units, are plotted in **mcU/mL** (microunits per millilitre, equal to mU/L; about 6 pmol/L of human insulin). Each drug's page gives its unit; broadly:

| Drugs | Plotted in |
|---|---|
| propofol, ketamine, midazolam, etomidate, remimazolam, morphine, pethidine, methadone, rocuronium, sugammadex, the local anesthetics, the antibiotics, hydrocortisone, methylprednisolone, amiodarone, and the oral analgesics and antiseizure drugs other than celecoxib and tiagabine | mcg/mL |
| remifentanil, fentanyl, alfentanil, sufentanil, hydromorphone, oxycodone, codeine, hydrocodone, oxymorphone, buprenorphine, tramadol, oliceridine, dexmedetomidine, the benzodiazepines other than midazolam and remimazolam, zolpidem, neostigmine, glycopyrrolate, vecuronium, cisatracurium, naloxone, oxytocin, dexamethasone, prednisolone, prednisone, and the antidepressants, antipsychotics, stimulants and antiemetics; semaglutide, tirzepatide, liraglutide, empagliflozin, ertugliflozin, glyburide, linagliptin and vildagliptin | ng/mL |
| metformin | mcg/mL |
| exenatide, lixisenatide | pg/mL |
| the insulins, vasopressin | mcU/mL |
| heparin, enoxaparin | IU/mL |
| calcium, magnesium sulfate | mmol/L |
| warfarin, fluindione, acenocoumarol | INR |
| heparin on bypass (heparinCPB) | ACT, seconds |

Internally, doses of a drug plotted in mcg/mL are converted to milligrams, doses of one plotted in ng/mL to micrograms, doses of one plotted in pg/mL to nanograms, and insulin units to milliunits before simulation; volumes are in litres, so concentration comes out in the plotted unit.

## Dose units

| Family | Units | Meaning |
|---|---|---|
| Bolus | g, mg, mcg, ng; and each per kg | An amount at that time |
| Infusion | g/min, g/hr, mg/min, mg/hr, mcg/min, mcg/hr; and per kg | A rate from that time |
| Oral | g PO, mg PO, mcg PO; and each per kg | An oral dose at that time |
| Oral form | mg PO tablet, liquid, ER, DR or XR; and per kg | An oral dose of that product, with its own absorption where the drug's forms differ; see [absorption](help:models/absorption) |
| Oral rate | mg/day PO | A daily oral dose spread evenly over each day, from that time until the drug's next rate row (amiodarone; see [its page](help:drugs/amiodarone)) |
| Sublingual | mg SL, mcg SL; and each per kg | A dose under the tongue at that time (buprenorphine) |
| Intramuscular | the same with IM | |
| Intranasal | the same with IN | |
| Regional anesthesia | g RA, mg RA, mcg RA; and each per kg | An injection into tissue, absorbed from a depot (the local anesthetics) |
| Perineural infusion | mg/hr RA | A rate into the tissue depot, from that time until the drug's next mg/hr RA row |
| Subcutaneous | mg SC, mcg SC; and each per kg | An injection under the skin, absorbed from a depot (the incretin peptides) |
| Insulin | U SC, U/kg SC | Units of that insulin preparation, injected under the skin; a unit is a potency unit of the product, not a mass |
| Insulin pump | U/hr SC | A rate into the subcutaneous depot, from that time until the drug's next U/hr SC row (insulin lispro only); see [absorption](help:models/absorption) |
| Phenytoin equivalents | mg PE, mg PE IM, mg PE/min; and per kg | Fosphenytoin, prescribed in phenytoin sodium equivalents (see [phenytoin](help:drugs/phenytoin)) |
| Target | Plasma target, Effect site target | A target concentration in the plotted unit; see [Target-controlled infusion](help:tci) |
| Scheduled | a bolus, PO (including the oral forms), SL, IM, IN or SC unit followed by qd, bid, tid, qid or q4h, or an SC unit followed by qwk | Repeated every 24, 12, 8, 6 or 4 hours, or every week; see [The dose table](help:dose-table) |
| Gas flow | L/min | A flowmeter or ventilation setting |
| Vaporizer | % | A vaporizer setting |

Which of these a given drug offers is set in the drug library and shown on its page. Per-kilogram units use the weight in the Patient Profile.

Some conversions that come up:

- 1 mg/kg/hr = 16.67 mcg/kg/min
- 100 mcg/kg/min of propofol in a 70 kg patient = 7 mg/min = 420 mg/hr
- 0.1 mcg/kg/min of remifentanil in a 70 kg patient = 7 mcg/min = 420 mcg/hr
- 400 mg/day PO of amiodarone = 16.7 mg/hr = 0.278 mg/min, given continuously

## Time

The calculation works in minutes. Times are entered and displayed in the **Time units** chosen in the Time panel (minutes, hours, days or weeks), and the dose table also accepts `HH:MM`. Max time runs from five minutes to a year (365 days, or 52 weeks), with choices that follow the unit. Infusion rates stay per minute or per hour whatever the time unit. See [Time display](help:time-display).

## Rate constants and half-lives

Rate constants are per minute. The half-life of any first-order process is ln(2)/k = 0.693/k. The drug pages tabulate the half-lives of the three disposition phases and of effect-site equilibration.

## Volumes and clearances

Volumes in litres, clearances in litres per minute. Papers that report clearances in mL/min, L/h or L/day (amiodarone) are converted in the drug file; the conversion is noted there.
