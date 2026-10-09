### Two models, by body mass index

For a patient with **BMI below 30** the parameters are the **Eleveld allometric model** (Eleveld DJ, Proost JH, Vereecke H, Absalom AR, Olofsen E, Vuyk J, Struys MMRF. *Anesthesiology* 2017;126:1005-1018): reference volumes of 5.81, 8.82 and 5.03 L and clearances of 2.58, 1.72 and 0.124 L/min at 70 kg and 35 years, scaled by Al-Sallami fat-free mass, with age terms on every parameter, a maturation term on clearance and a sex term that raises clearance and V2 in women between puberty and the menopause. It was fitted to pooled data from several earlier studies, including Minto's.

For a patient with **BMI of 30 or above** they are the **Kim model for obesity** (Kim TK, Obara S, Egan TD, et al. *Anesthesiology* 2017;126:1019-1032), with V1 and clearance scaled to weight, V2 to Janmahasatian fat-free mass, and age terms. Kim pooled data from 229 people, 107 surgical patients and 122 volunteers, aged 20 to 85 years with BMI from 16.1 to 73.7, so the model was not fitted to obese patients alone. Using it only at BMI 30 and above, the conventional threshold for obesity, is stanpumpR's rule, not a limit set by the source study. The citation the model function returns, and the References panel shows, follows the branch in use for the patient entered.

Minto and colleagues' 1997 model (*Anesthesiology* 1997;86:10-23), on which STANPUMP's remifentanil kinetics were based and which most commercial remifentanil TCI pumps implement, is kept in the drug file as comments but is not computed.

### Covariates

Weight, height, age and sex all enter through fat-free mass, the allometric terms, and the age and sex terms. The switch at BMI 30 is abrupt: the two models give somewhat different concentrations for the same dose on either side of it, which is why the "obese adult" reference patient on this page is at BMI 41.

### Effect site

The time to peak effect is 1.6 minutes, from the opioid simulation spreadsheet that accompanied the original comparisons; Minto's own ke0 of 0.595/min at age 40 corresponds to a similar peak. The resulting equilibration half-time of about a minute is the fastest in the library.

### MEAC and typical concentrations

The MEAC of 1 ng/mL is the unit in which the other opioids are expressed on the interaction panel. The shaded band is 0.8 to 2 MEAC, 0.8 to 2 ng/mL; concentrations of 4 to 8 ng/mL are typical during laryngoscopy and incision with propofol.

### Where to be careful

Remifentanil's metabolism by non-specific esterases makes its kinetics unusually independent of organ function, and its offset barely depends on infusion duration. That is the model's great teaching value; see [the context-sensitive decrement scenario](scenario:context-sensitive-opioids). The pharmacokinetics are the easy part. The hyperalgesia, the chest-wall rigidity after a fast bolus and the abrupt loss of analgesia at the end of a case are pharmacodynamic and are not in the model.
