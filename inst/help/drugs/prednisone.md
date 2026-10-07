### The model

Prednisone is the prodrug side of the Xu, Winkler and Derendorf reversible pair described under [prednisolone](help:drugs/prednisolone) (*J Pharmacokinet Pharmacodyn* 2007;34:355-372). The prednisone row is the exact two-compartment mammillary equivalent of that system for a prednisone dose, with the prednisolone pool as the peripheral compartment, written on **total** prednisone (free times four, Xu's constant free fraction of 0.25): V1 99.3 L, effective clearance 36.8 L/h, Q 11.1 L/h, V2 17.2 L.

### A prodrug

Prednisone has little glucocorticoid activity of its own; its effect is prednisolone's. The row therefore has **no effect site** and plots plasma only, and the prednisolone it forms appears on the prednisolone row through the [active metabolite](help:models/metabolites) link. That link is a one-way formation convolved through prednisolone's own model, which cannot carry the back-reaction directly, so the formation constant (0.175/h against the biochemical 0.228/h) is calibrated to make the free prednisolone exposure after a prednisone dose exactly the source's. Shape is approximate, exposure exact.

### Oral only

Xu's oral prednisone enters 0.105 as prednisone and 0.645 as prednisolone formed before reaching the circulation. Those cannot be used directly, because most of the prednisone in plasma after an oral dose is regenerated from that prednisolone and a one-way parent row cannot receive it; the code solves two effective coefficients from the source's AUC identities instead (oral bioavailability 0.42 for the prednisone row, first-pass fraction 0.50 for the prednisolone link), so that both the total prednisone and the free prednisolone exposures are exact. Intravenous prednisone has been given in research, but no routine product was verified, so the row is **mg PO** only.

### Covariates

None in the source. The parameters take the default [fat-free-mass scaling](help:models/fat-free-mass), identically to prednisolone because the formation constant is a clearance over a volume, and are used as published with the switch off.

### Typical concentrations

The shaded band (10 to 80 ng/mL total prednisone) covers what 20 to 40 mg produce; 20 mg peaks near 45 ng/mL at about 85 minutes, with free prednisolone peaking near 48 ng/mL on its own row.

### Where to be careful

Everything said about the Xu fit under prednisolone applies. Delayed-release prednisone is a different product this input does not describe.
