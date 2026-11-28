### The model

Factor VIII travels in the blood bound to von Willebrand factor, which protects it from clearance. This panel is the factor VIII activity a von Willebrand factor concentrate leaves in the patient. It is **not dosed directly**: it has no dose units and does not appear in the dose table's drug list or the startup menu. Its whole curve comes from the concentrate given, [Vonvendi](help:drugs/vonicogAlfa), the same arrangement [enalaprilat](help:drugs/enalaprilat) has with enalapril.

Bauer and colleagues (*J Blood Med* 2023) fitted it as an indirect response in 41 adults given Vonvendi, 31 of them with type 3 von Willebrand disease: the VWF:RCo the patient has slows the elimination of factor VIII,

dF/dt = kout × F0 − kout × (1 − Imax × VWF/(IC50 + VWF)) × F,

with kout 15.9 /h, Imax 0.998, IC50 0.0658 IU/dL and F0 0.5 IU/dL × (haematocrit/0.39)^−0.571, held here at a haematocrit of 0.4 (0.49 IU/dL). With no von Willebrand factor the half-life of factor VIII is 2.6 minutes; at 50 IU/dL of VWF:RCo it is 13 hours (the source prints 11.5, from Imax rounded to three figures).

### What is plotted

Factor VIII activity in **IU/dL** (% of normal), the patient's own included: before the first dose the curve is the steady state on the patient's own VWF:RCo, the Patient Profile's **VWF:RCo baseline**. The Patient Profile's **Factor VIII baseline** is not read here; it belongs to the factor VIII concentrates.

The band is the Vonvendi label's factor VIII peak target for major surgery, 80 to 100 IU/dL; for minor surgery the target is 40 to 50, and after major surgery the trough is kept above 30 IU/dL.

### Not modelled

Factor VIII given as a concentrate of its own is not added here (see [Vonvendi](help:drugs/vonicogAlfa)). The patient's own production of factor VIII is the model's constant kout × F0, so a perioperative rise in it is not drawn.
