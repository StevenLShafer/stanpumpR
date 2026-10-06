## Patient covariates

| Quantity | Entered as | Used internally | Conversion |
|---|---|---|---|
| Age | years or months | years | 1 month = 1/12 year |
| Weight | kg or lb | kg | 1 lb = 0.453592 kg |
| Height | in or cm | cm | 1 in = 2.54 cm |

## Concentrations

Each drug is plotted in the concentration units given in the drug library: **mcg/mL** (micrograms per millilitre, equal to mg/L) or **ng/mL** (nanograms per millilitre, equal to mcg/L). The inhaled agents are in **per cent of one atmosphere**, and MAC equivalents are dimensionless.

| Drug | Plotted in |
|---|---|
| propofol, morphine, pethidine, methadone, ketamine, midazolam, etomidate, lidocaine, rocuronium, remimazolam | mcg/mL |
| remifentanil, fentanyl, alfentanil, sufentanil, hydromorphone, dexmedetomidine, naloxone, oxytocin, oxycodone, oliceridine | ng/mL |

Internally, doses of a drug plotted in mcg/mL are converted to milligrams and doses of one plotted in ng/mL to micrograms before simulation; volumes are in litres, so concentration comes out in the plotted unit.

## Dose units

| Family | Units | Meaning |
|---|---|---|
| Bolus | g, mg, mcg, ng; and each per kg | An amount at that time |
| Infusion | mg/min, mg/hr, mcg/min, mcg/hr; and each per kg | A rate from that time |
| Oral | g PO, mg PO, mcg PO; and each per kg | An oral dose at that time |
| Intramuscular | the same with IM | |
| Intranasal | the same with IN | |
| Gas flow | L/min | A flowmeter or ventilation setting |
| Vaporizer | % | A vaporizer setting |

Which of these a given drug offers is set in the drug library and shown on its page. Per-kilogram units use the weight in the Patient Profile.

Some conversions that come up:

- 1 mg/kg/hr = 16.67 mcg/kg/min
- 100 mcg/kg/min of propofol in a 70 kg patient = 7 mg/min = 420 mg/hr
- 0.1 mcg/kg/min of remifentanil in a 70 kg patient = 7 mcg/min = 420 mcg/hr

## Time

Times are minutes. The dose table accepts `HH:MM` and converts it. Max time runs from 60 minutes to a year (525,600 minutes). See [Time display](help:time-display).

## Rate constants and half-lives

Rate constants are per minute. The half-life of any first-order process is ln(2)/k = 0.693/k. The drug pages tabulate the half-lives of the three disposition phases and of effect-site equilibration.

## Volumes and clearances

Volumes in litres, clearances in litres per minute. Papers that report clearances in mL/min or L/h are converted in the drug file; the conversion is noted there.
