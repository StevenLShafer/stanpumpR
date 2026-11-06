Cytotoxic chemotherapy is new to stanpumpR. Five drugs are in the library, under **Chemotherapy** in the startup menu: [high-dose methotrexate in children and young adults](help:drugs/methotrexatePediatric) (Taylor 2023, the MTXPK.org model: three compartments by body surface area and serum creatinine), [busulfan](help:drugs/busulfan) (intravenous, McCune 2014: two compartments on normal fat mass, maturation from infancy, clearance falling during the course), [carboplatin](help:drugs/carboplatin) (unbound, Duffull 1997: two compartments, lean body weight and an adjusted Cockcroft-Gault creatinine clearance), [fluorouracil](help:drugs/fluorouracil) (5-FU, intravenous, Briki 2026: two compartments with saturable and linear elimination) and [cladribine](help:drugs/cladribine) (intravenous, subcutaneous and oral solution, Lindemalm 2005: three compartments). All are plasma only, and every parameter was read in the paper's PDF. The rest of this page is the evidence plan: every drug and product considered, the published population models that could describe it, what each would need from the engine, and what stands in the way. It is built from the registry in `inst/extdata/chemotherapyRegistry.csv` and cannot drift from it.

## What a chemotherapy curve can and cannot say

A curve here is a **population prediction of plasma concentration** for the typical patient with the covariates entered. It is not therapeutic drug monitoring, not a Bayesian estimate from a measured level, and not a dose recommendation. Several distinctions matter more for these drugs than for anaesthetics:

- **What is measured.** Carboplatin may be reported as intact drug, as ultrafilterable platinum or as total platinum; cisplatin as parent, as free platinum or as total platinum; irinotecan as the lactone or the carboxylate, with its metabolite SN-38; the liposomal products as encapsulated, released or total drug. A model describes only the species its source measured, and the help page of each implemented drug names it.
- **Exposure targets are not bands.** The established targets for these drugs are areas under the curve over a stated interval (carboplatin's Calvert AUC, 5-FU's AUC over a 46-hour infusion, busulfan's cumulative AUC or average steady-state concentration) or a time above a concentration (paclitaxel). None of them is a concentration band, so none is drawn as one. Methotrexate's glucarpidase and leucovorin thresholds are rescue criteria, not a therapeutic range, and a simulation never replaces a measured level.
- **No effect site.** No chemotherapy model here has a measured equilibration between plasma and effect, and toxicity (myelosuppression, cardiotoxicity, neuropathy) is cumulative and delayed. Every model is plasma only.
- **Genotype guidance is not a clearance.** The CPIC and DPWG guidelines for DPYD (fluoropyrimidines), UGT1A1 (irinotecan) and TPMT/NUDT15 (thiopurines) recommend dose changes; they do not estimate a change in clearance, so none is applied as one.
- **Dose normalisation is not a covariate.** Dosing in mg/m² does not mean a model's clearance scales with body surface area. Each model keeps the size relation its source fitted.

## The models considered

The **first wave** is carboplatin, high-dose methotrexate, 5-fluorouracil and busulfan, the drugs with the best-established exposure targets. The **second wave** is the rest of the drugs asked for (cisplatin, cyclophosphamide, doxorubicin, solvent-based paclitaxel, vincristine, irinotecan, etoposide), and the **expansion** list follows. The *Engine* column is judged from the abstract against the engines as they stand: a linear model of up to three compartments, a parent with one metabolite, saturable (Michaelis-Menten) elimination with one peripheral compartment, or a mechanism the engine does not carry (a chain of metabolites, enzyme autoinduction, saturable tissue binding, enterohepatic recirculation, release from a liposome, intracellular formation).

<!-- generated: chemotherapy-registry-table -->

## Where the papers stand

None of these papers is in the library's archive. For all but the two implemented, the numbers that fix a population model are printed in its tables and equations, which the text available here does not carry. So every other candidate needs its full text before it can be implemented. These are the papers to obtain, first wave first:

<!-- generated: chemotherapy-to-obtain -->

The guidelines behind the exposure targets and the genotype recommendations are recorded with them: the IATDMCT recommendations for 5-FU and paclitaxel, the EBMT busulfan guide, the glucarpidase consensus guidelines for methotrexate, the ADDIKD renal dosing guideline, the ASCO guideline for obese patients, and the CPIC and DPWG pharmacogenetic guidelines. How the plan was drawn up, and what in the original recommendations was corrected, is in `docs/chemotherapy.md`.
