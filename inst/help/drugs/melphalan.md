### The model

High-dose melphalan comes from Nath and colleagues (*Br J Clin Pharmacol* 2010;69:484-497): 100 adults aged 36 to 73 with multiple myeloma, given about 200 mg/m² over half an hour before autologous stem-cell transplantation. Two compartments describe total plasma melphalan, its clearance the sum of a non-renal and a renal part:

- non-renal clearance 17 L/h × (fat-free mass / 50 kg)^0.75 (at the reference haematocrit of 34%);
- renal clearance 11.1 L/h × (creatinine clearance / 88), the creatinine clearance by Cockcroft-Gault in mL/min per 70 kg;
- central volume 13.2 L × fat-free mass / 50 kg, and one peripheral compartment of 15 L exchanging at 30.6 L/h.

At the reference values clearance is 28.1 L/h, 40% of it renal, and the half-lives are 7 and 55 minutes, as the paper reports.

### What is plotted

Total plasma melphalan in mcg/mL (mg/L). Doses are entered in mg or as an infusion rate; a dose prescribed per m² must be multiplied by the body surface area first. There is no effect site and no band: the exposures the paper related to toxicity are not a therapeutic range.

### Covariates

- **Fat-free mass**, from weight, height and sex (Janmahasatian's equation in adults, the paper's), on the non-renal clearance and the central volume.
- **Serum creatinine**, from the Patient Profile, in Cockcroft-Gault normalised to 70 kg, so weight does not enter the renal term. Left blank, the library's assumed normal value is used.
- **Haematocrit is held at 34%**, the study's median, because the Patient Profile has no field for it. A lower haematocrit lowers the non-renal clearance: by 22% at a haematocrit of 20%.

### Where to be careful

- **The reference fat-free mass** is printed as 50 kg in the final model, although the study's median was 53 kg; it is used as printed.
- **Adults only**, 36 to 73 years. Not a monitoring tool: the curve is a population prediction.

Every parameter was read in the paper's PDF.
