## Chlorthalidone

Chlorthalidone is a long-acting thiazide-like diuretic. It lowers blood
pressure more than hydrochlorothiazide at the doses commonly used, and has
the better cardiovascular outcome evidence of the two.

The model is from **Tsai 2016**, the same phase 3 analysis that supplies
[azilsartan](help:drugs/azilsartan): 1059 subjects contributing 4807
concentrations, two compartments with first-order absorption. Its own
introduction makes the case for why it was needed — chlorthalidone had been
available for over fifty years on a pharmacokinetic literature of studies
with seven subjects or fewer.

Unlike azilsartan in the same paper, **every disposition parameter here is
estimated**, none fixed.

### Why the volumes look so large

An apparent central volume of 62 L and a peripheral volume of 120 L are large
for a small, highly bound molecule. The reason is in the paper's discussion:
chlorthalidone partitions markedly into red blood cells, which hold about 95%
of the drug in whole blood and 30% of the drug in the body at steady state.

The samples assayed were **plasma**, so this curve is a plasma concentration
and the volumes are apparent in that partitioning as well as in
bioavailability. Do not read a whole-blood figure against this plot.

### What the covariates do

Age lowers clearance; men clear faster and have a much larger apparent
central volume:

> CL/F = (4.13 L/h − 0.0451 × [age − 58 years]) × (1 + 0.253 if male)
>
> V1/F = 62.1 L × (1 + 1.19 if male)

The authors attribute the sex effect on volume to men's higher haematocrit,
which follows from the erythrocyte partitioning above.

### Two fitted covariates that this model does not apply

Both are left out deliberately, and in neither case because the number is in
doubt.

**Cystatin C on clearance** (−2.61 L/h per mg/L, about 0.73 mg/L). The
Patient Profile has no cystatin C field. Serum creatinine is *not* a
substitute: cystatin C is cleared only by glomerular filtration, and the
authors chose it over creatinine for precisely that reason. Feeding
creatinine into a term fitted to cystatin C would invent a covariate the
source never estimated. The term is dropped at its reference value, where it
contributes nothing.

**Body mass index on volume** (2.41 L per kg/m², about 30.9 kg/m²). Here the
obstacle is the equation rather than the number. The paper prints

> V1/F = 62.1 L × (1 + Sex × 1.19) × 2.41 × (BMI − 30.9 kg/m²)

which cannot be read as written — as a product it gives a volume of zero at a
body mass index of 30.9 and a negative volume below it. The surrounding text
says the volume increased *linearly* with body mass index, so an additive
term is the likely intent, but likely is not published, and the supplementary
material holds only a demographics table and a diagnostic figure with no
control stream to settle it. At the reference body mass index every candidate
reading gives 62.1 L, so the typical patient is unaffected.

### What is deliberately not here

**No blood pressure**, for the same reasons as azilsartan: the
exposure-response in this paper is driven by steady-state area under the
curve, not by a concentration. No effect site and no therapeutic band.

### Reference

Tsai MC, Wu J, Kupfer S, Vakilynejad M. Population pharmacokinetics and
exposure-response of a fixed-dose combination of azilsartan medoxomil and
chlorthalidone in patients with stage 2 hypertension. *J Clin Pharmacol*
2016;56:988–998.
