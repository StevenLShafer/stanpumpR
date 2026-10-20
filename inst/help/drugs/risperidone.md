### The model

Oral risperidone and its active metabolite, from Størset and colleagues (*Eur J Clin Pharmacol* 2024;80:1531-1541): 1565 paired **serum** concentrations from 512 genotyped adults in routine drug monitoring, mostly 9 to 30 hours after the dose. One compartment for each species, first-order absorption at 2.01 per hour (fixed), apparent volume 333 L. The parent's apparent clearance is

CL/F = [4.2 + 23.2 × (allele activities)] × [1 − 0.009 × (age − 34)] L/h,

with the age term applying only above 34. Risperidone forms [9-hydroxyrisperidone](help:drugs/hydroxyrisperidone), which carries the larger share of the active moiety.

The structure, the allele activities, both age terms, the volumes and the metabolite's clearance were all checked against the paper (Table 2).

### CYP 2D6

The **CYP 2D6** field is a phenotype, so it is mapped to the paper's own example genotypes: poor metaboliser 4.2 L/h (two non-functional alleles), intermediate 27.4 L/h (one functional and one non-functional), normal 50.6 L/h (two functional). The paper had no ultrarapid subjects; ultrarapid is taken as three functional copies (73.8 L/h), an extrapolation. Reduced-function alleles such as \*10 and \*41 fall between these, and the NFIB variant that raises CYP2D6 clearance by 41% is not applied.

### Oral only

All parameters are **apparent**, so risperidone is offered as **mg PO** only (also as a qd or bid [scheduled dose](help:dose-table)).

### Effect

There is **no effect site** and no band. The AGNP therapeutic reference range, 20 to 60 ng/mL 12 hours after a dose (Hiemke and colleagues, *Pharmacopsychiatry* 2018;51:9-62, unchanged in the 2026 update, Hart and colleagues, *Pharmacopsychiatry* 2026;59:166-295, Table 5, with a laboratory alert level of 80 ng/mL), is for the **active moiety**, and the share each row carries depends on the CYP2D6 phenotype this model applies, so it cannot be drawn on either row: add the two rows to compare. D2 occupancy depends on the **active moiety**, risperidone plus 9-hydroxyrisperidone in ng/mL, read from the two rows together. Uchida and colleagues' pooled PET analysis (*J Clin Psychopharmacol* 2011;31:318-325) gives two complete fits that must not be mixed: 88% × C / (4.9 + C), unconstrained, and 100% × C / (8.2 + C), with Emax fixed. The second has been checked in a paper that uses it; the first has not yet been checked. The PET work measured plasma, and this model predicts serum.

### Where to be careful

The samples were mostly troughs, so the early peak after a dose is weakly informed. Both age terms are linear and fitted in adults.
