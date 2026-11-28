### One name, the model chosen for the patient

stanpumpR holds 2 published models of oral quinine. You do not choose between them: stanpumpR picks the model whose study population best matches the patient you entered (a rule of Steven Shafer's, 9 November 2026). The References panel under the plot names the model in use.

| Model | Source | Studied in |
|---|---|---|
| [quinineKloprogge](help:drugs/quinineKloprogge) | Kloprogge 2014 | 22 pregnant women with uncomplicated falciparum malaria, 18-37 years, 44-71 kg |
| [quinineLeJouan](help:drugs/quinineLeJouan) | Le Jouan 2005 | 30 children with uncomplicated falciparum malaria, 0.55-6.7 years |

**The rule.** Among the models whose studied age range (and weight range, where the paper printed one) includes the patient, the one fitted to more patients is chosen, then the newer. If none includes the patient, the one whose ranges are nearest is chosen: years outside the age range first, then kilograms outside the weight range.

**What that means for quinine.** Children up to about 12 years get Le Jouan 2005, and everyone older gets Kloprogge 2014. **The adult model was fitted in pregnant women**, because no population model of quinine in non-pregnant adults is in the archive or the other libraries stanpumpR checks. Steven Shafer decided on 28 November 2026 to use it for every adult and to say so here. Two small controlled studies of intravenous quinine in Sudanese women with malaria found no significant difference between pregnant and non-pregnant women (see the Pregnancy section below), which supports that use.

**Both are malaria patients.** Acute malaria raises quinine's binding to plasma proteins and lowers its clearance, and both papers modelled that: Kloprogge through parasitaemia and fever, Le Jouan through a rising unbound fraction. stanpumpR holds those terms at each paper's reference, so the curve is neither the sickest day nor full recovery. Each model's page says where its reference lies.

**Doses.** The US product is quinine sulfate (Qualaquin and generics, 324 mg capsules), approved for uncomplicated falciparum malaria in adults 16 years and over at 648 mg every 8 hours for 7 days; the label says its safety and efficacy under 16 years have not been established, though quinine is given to children elsewhere, as in Le Jouan's study. Doses are entered as the sulfate, as the label gives them (`mg PO tid`, or `mg/kg PO tid` for a child); both papers modelled quinine base, so stanpumpR converts each dose by 0.8287 mg base per mg of sulfate. The concentration drawn is total quinine (base) in plasma.

**Routes not modelled.** Intravenous quinine is not marketed in the United States, and both models were fitted to oral doses, so only the oral route is offered.
