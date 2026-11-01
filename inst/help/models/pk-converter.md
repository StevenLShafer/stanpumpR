A one-, two- or three-compartment mammillary model can be written down in several equivalent ways: as volumes and clearances, as V1 and the micro rate constants, as the coefficients and exponents of the curve after a bolus, and so on. Papers report whichever their software printed, so comparing two models usually means converting one of them first. This page does the conversion. It reproduces Steven L. Shafer's spreadsheet *Convert.xls* (last updated December 2002), whose five tabs each take one complete definition of the model and list every other parameter, and adds the effect site and a graph.

**Units.** Volumes are in litres. Times are in the time unit chosen (minutes as the page opens), so rate constants and exponents are per that unit and clearances are litres per that unit. Changing the unit converts every value, entered or derived; the model does not change. The coefficients A1, A2, A3 are those of the unit disposition function, the plasma concentration after a bolus of one unit of drug, so they are per litre, and V1 = 1 / (A1 + A2 + A3).

<!-- generated: pk-converter -->

## How to use it

Choose the number of compartments, then fill in any one panel. Each panel is one of the spreadsheet's five ways of defining the model, and needs as many numbers as the model has parameters: two for one compartment, four for two and six for three. As soon as the panel is complete, every other panel is filled in. The values you entered are yellow and the derived ones aqua, as in the spreadsheet; a field in grey is one that panel derives but never takes (V1 and Vdss of one compartment, Vdss of two, the fractional coefficients).

To change the model, edit any number in any panel. That panel becomes the definition, holding the panel's other numbers at their current values, so changing CL2 in the first panel holds the volumes and the other clearances fixed, while changing k12 in the second holds V1 and the other rate constants fixed. **Load the spreadsheet's example** enters the example of its first tab (V1 = 10 L, V2 = 100 L, V3 = 1000 L, CL1 = 3, CL2 = 2 and CL3 = 1 L/min for three compartments). **Clear** starts again.

Not every set of numbers is a mammillary model. A rate constant that comes out negative or complex, two equal exponents, or coefficients that no model can give with the exponents entered are reported as **not admissible**, with the parameter that failed, and nothing is derived from them. The exponents can be entered in any order; they are sorted fastest first, each coefficient staying with its own exponent.

## The five definitions

Every definition is reduced to V1 and the micro rate constants, and everything else is computed from those. With k10 = CL1/V1, k12 = CL2/V1, k21 = CL2/V2, k13 = CL3/V1 and k31 = CL3/V3:

* **The exponents** λ1 > λ2 > λ3 are the roots of the characteristic polynomial: λ = k10 for one compartment, the quadratic λ² − (k10 + k12 + k21) λ + k10 k21 = 0 for two, and for three the cubic λ³ − a2 λ² + a1 λ − a0 = 0 with a0 = k10 k21 k31, a1 = k10 k31 + k21 k31 + k21 k13 + k10 k21 + k31 k12 and a2 = k10 + k12 + k13 + k21 + k31, solved by the trigonometric method that `cube()` uses in the simulator and then refined by Newton's method.
* **The coefficients** are A_i = (k21 − λ_i)(k31 − λ_i) / ((λ_j − λ_i)(λ_k − λ_i)) / V1 for three compartments (i, j, k the three exponents in turn), A_i = (k21 − λ_i) / (λ_j − λ_i) / V1 for two, and 1/V1 for one. The fractional coefficients A_i / ΣA are each exponent's share of the fall after a bolus.
* **Panel 3** (V1, Vdss, CL and the half-lives): with three compartments, g0 = λ1 λ2 λ3 / k10, f = (λ1 λ2 + λ1 λ3 + λ2 λ3 − (Vdss/V1) g0) / k10, and k21 and k31 are the roots of x² − f x + g0 = 0; then k13 = ((Vdss/V1 − 1) g0 − i k31) / (k21 − k31) and k12 = i − k13, where i = λ1 + λ2 + λ3 − k10 − k21 − k31. With two compartments, as in the spreadsheet, the panel takes V1, CL and both half-lives (k21 = λ1 λ2 / k10, k12 = λ1 + λ2 − k21 − k10) and derives Vdss; with one, it takes CL and the half-life and derives V1 = CL/λ.
* **Panel 4** (coefficients and exponents): V1 = 1/ΣA. With three compartments, k21 and k31 are the roots of x² − b x + c = 0, where b ΣA = λ1 (A2 + A3) + λ2 (A1 + A3) + λ3 (A1 + A2) and c ΣA = λ1 λ2 A3 + λ1 λ3 A2 + λ2 λ3 A1; with two, k21 = (A1 λ2 + A2 λ1) / (A1 + A2).
* **Panel 5** (V1, the exponents, k21 and k31): k10 = λ1 λ2 λ3 / (k21 k31), k12 = ((λ1 λ2 + λ1 λ3 + λ2 λ3) − k21 (λ1 + λ2 + λ3) − k10 k31 + k21²) / (k31 − k21) and k13 = λ1 + λ2 + λ3 − (k10 + k12 + k21 + k31); with two compartments, k10 = λ1 λ2 / k21 and k12 = λ1 + λ2 − k21 − k10.

When panel 3, 4 or 5 is the definition, the exponents (and in panel 4 the coefficients) are recomputed from the rate constants and must agree with what was entered; that check is how an inadmissible set of exponents is caught.

In panels 3 and 4 the larger root is taken as k21, so compartment 2 is the faster of the two peripheral compartments. In panel 5 the order is yours: k21 and k31 can be entered either way round, giving the same model with its peripheral compartments numbered the other way.

## The effect site

Once the model is defined, enter ke0, its half-life ln(2)/ke0, or the time to peak effect after a bolus, and the other two are computed. The effect-site concentration after a bolus is

```
Ce(t) = ke0 Σ A_i (exp(-λ_i t) - exp(-ke0 t)) / (ke0 - λ_i)
```

and it peaks at the moment it meets the falling plasma concentration, since dCe/dt = ke0 (Cp − Ce). The time to peak effect is found as that crossing; ke0 is found from a time to peak by the same search the simulator uses (`ke0FromTPeakBolus()`), the peak coming earlier as ke0 rises. [The effect site and ke0](help:models/effect-site) explains why the time to peak effect, not ke0, is the number to carry from one model to another.

## The graph

The graph is the unit disposition function, the plasma concentration after a bolus of one unit of drug, with the effect-site concentration once ke0 is known. It can show the concentrations themselves, or both curves divided by the peak plasma concentration (1/V1, at the moment of the bolus), or both divided by the peak effect-site concentration. The second shows how little of the plasma peak reaches the effect site: the effect-site peak is later and far lower than the plasma's, and its height relative to 1/V1 is printed under the effect-site panel. The third shows how high the plasma concentration must go to bring the effect site to its peak. A logarithmic axis shows the exponential phases as straight lines. The graph runs until the plasma concentration has fallen to 2% of its initial value, or to three times the time to peak effect if that is later; enter a time in **Plot until** to change it.

Everything on this page is computed in the browser; nothing is sent to the server and nothing changes the simulation.
