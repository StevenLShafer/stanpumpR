The **Physiology** panel in the left-hand column asks *what if*: what if hepatic
function were half of normal, the kidneys shut down, or cardiac output fell
by two thirds? It has four sliders. Each is a multiple of normal function, on
a log scale from a tenth (0.1) to ten times (10). All four start at 1, which
is normal and changes nothing. **Reset to normal** puts them back.

The sliders change each drug's pharmacokinetic **parameters**, once, before
the simulation runs. As with any covariate, the parameters then stay fixed
for the whole simulation. They do not change over time.

## The liver: flow-limited and capacity-limited drugs

Hepatic clearance is liver blood flow times the extraction ratio,
CL<sub>H</sub> = Q × E. In the well-stirred model (Wilkinson and Shand, 1975)
the extraction ratio comes from the liver's intrinsic clearance,
CL<sub>int</sub>, the metabolic capacity V<sub>m</sub>/K<sub>m</sub>:

E = CL<sub>int</sub> / (Q + CL<sub>int</sub>),
CL<sub>H</sub> = Q × CL<sub>int</sub> / (Q + CL<sub>int</sub>).

Change hepatic blood flow by a factor *q* and metabolic capacity by a factor
*l*. Hepatic clearance then changes by

CL<sub>H</sub>′ / CL<sub>H</sub> = *q l* / (*q* (1 − E) + *l* E),

which needs nothing but the drug's extraction ratio E at normal physiology.

- A drug with a **high** extraction ratio (propofol, E 0.87) is **flow
  limited**. The liver can metabolize far more than reaches it, so its
  clearance follows hepatic blood flow and hardly changes with metabolic
  capacity.
- A drug with a **low** extraction ratio (alfentanil, E 0.28) is **capacity
  limited**. Its clearance follows metabolic capacity, as in liver disease or
  enzyme induction, and hardly changes with hepatic blood flow.

The **Liver function** slider is *l*. The **Cardiac output** slider is *q*,
because liver blood flow is taken as a fixed share of cardiac output (as is
renal blood flow, below). For a
drug given by mouth whose bioavailability includes a hepatic first pass
(morphine), the new extraction ratio E′ = *l* E / (*q* (1 − E) + *l* E) also
changes the oral bioavailability, F′ = F (1 − E′)/(1 − E), never above 1.

## The kidneys

**Renal function** scales only the renal share of a drug's clearance. Each drug's clearance is split into a renal
share, a hepatic share, and the rest. The rest is elimination outside the liver
and kidneys, such as plasma and tissue esterases (remifentanil) and Hofmann
elimination (cisatracurium), and no slider changes it. Setting renal function
to 0.1 removes 90% of the renal share and nothing else.

Some drugs are **metabolized** by the kidney, not just filtered. Propofol is
the main example. Its total clearance is greater than hepatic blood flow, and
the kidneys are the likely reason: they metabolize about 27% of it, with a
renal extraction ratio of 0.58 measured against renal blood flow (Takizawa
2005). The renal share of such a drug follows the same well-stirred rule as
the liver, with renal blood flow a fixed share of cardiac output:
CL<sub>R</sub>′/CL<sub>R</sub> = *q r* / (*q* (1 − E<sub>R</sub>) + *r* E<sub>R</sub>).
So both renal function and cardiac output change it. A renal share with no
renal extraction ratio is filtration of unchanged drug, which changes in
proportion to renal function. A Patient Profile serum creatinine still
changes the drugs whose models read it, and the slider acts on top of that.

## Cardiac output and distribution

Besides hepatic blood flow, cardiac output scales two things:

- **The intercompartmental clearances** (Q2 and Q3), because distribution to
  the tissues is carried by blood flow.
- **The central volume** (V1), in the same direction. With a low cardiac
  output, a bolus mixes into less blood on its first pass, so early
  concentrations are higher, which a mammillary model expresses as a smaller
  V1. The size of this effect is an assumption, V1 in proportion to cardiac
  output, and is not fitted to data.

## The effect site in low cardiac output

**ke0** is the rate at which the effect site equilibrates with blood. It is set
mainly by the drug's relative solubility in blood and brain, not by brain
blood flow; if it were set by flow, every drug acting in the brain would share
one ke0. A drug that is poorly soluble in brain equilibrates quickly, because
almost nothing has to cross before equilibrium is reached. A highly soluble
drug equilibrates slowly, because a great deal has to cross.

Cardiac output still matters. Picture the same arterial concentration in
normal physiology and in shock. With less brain blood flow, less drug is
delivered each minute, so the effect site rises more slowly. That can be made
exact. If the brain is one well-stirred tissue fed by arterial blood,

V dC<sub>brain</sub>/dt = Q E (C<sub>arterial</sub> − C<sub>brain</sub>/λ), with
E = 1 − exp(−PS/Q),

where V is the brain's volume, Q its blood flow, λ the brain:blood partition
coefficient (the solubility) and PS the permeability-surface product of the
blood-brain barrier. Writing the effect site as C<sub>e</sub> = C<sub>brain</sub>/λ
turns this into the ke0 equation,
dC<sub>e</sub>/dt = ke0 (C<sub>arterial</sub> − C<sub>e</sub>), with

ke0 = Q (1 − exp(−PS/Q)) / (V λ).

Two cases follow. For a drug whose entry is limited by flow (PS much larger
than Q), ke0 ≈ Q / (V λ): it falls in exact proportion to brain blood flow,
whatever the solubility, and solubility sets how many minutes that costs. For
a drug limited by the blood-brain barrier (PS much smaller than Q),
ke0 ≈ PS / (V λ): flow hardly matters.

The simulation below shows this. It uses a brain of 1.4 L with a normal blood
flow of 0.75 L/min, holds one arterial curve fixed, and lowers only brain
blood flow. Each drug's λ is chosen to give its ke0 at normal flow.

<!-- generated: physiology-ke0-figure -->

<!-- generated: physiology-ke0-table -->

The fractional change in ke0 is the same for every flow-limited drug. The delay
it causes is not: at a third of normal brain flow, the fast drug's
equilibration half-time grows by about 3 minutes and the highly soluble drug's
by about half an hour. The permeability-limited drug barely changes.

What remains is how brain blood flow follows cardiac output. While blood
pressure is within the range of cerebral autoregulation, brain flow follows
cardiac output only partly, about as cardiac output to the power 0.35:

- Levine 1994 lowered cardiac output 29.9% with lower-body negative pressure
  without systemic hypotension, and middle cerebral artery velocity fell 15.5%
  (an exponent of 0.47).
- Ogoh 2005 lowered and raised cardiac output with lower-body negative pressure
  and albumin, at constant blood pressure and arterial CO<sub>2</sub>, and
  middle cerebral artery velocity rose linearly with it (an exponent of about
  0.35).
- Vlastra 2020 measured grey-matter blood flow by MRI before and after aortic
  valve replacement: 8.2% more flow per added L/min of cardiac output, from
  4.0 L/min (an exponent of 0.33).

The evidence is not strong. On cardiopulmonary bypass with blood pressure held,
halving pump flow did not change cerebral blood flow (Cook 1997), and a
systematic review of 41 studies judged them too weak to confirm that cardiac
output and cerebral blood flow are coupled (Castle-Kirszbaum 2021). Below the
autoregulatory floor, as in true shock, brain flow becomes pressure-passive and
falls further than this.

So stanpumpR takes ke0 at normal physiology and multiplies it by
CO<sup>0.35</sup>, for every drug whose entry into the brain is limited by
flow. Liver function, renal function and GI absorption leave ke0 alone.

**Morphine is the exception.** It is polar, and it crosses the blood-brain
barrier slowly: its equilibration is limited by the barrier (PS much smaller
than brain blood flow), not by delivery. Its ke0 is then
PS / (V λ), and brain blood flow drops out, as the lowest panel of the figure
above shows. So morphine's ke0 is left as it is at every cardiac output; its
clearance and central volume still change with cardiac output like any other
drug's. A drug is marked this way in the physiology table
(`ke0_permeability_limited`); morphine is the only one so far.

In the app the slower ke0 meets the smaller V1. A 2 mg/kg propofol bolus at
three cardiac outputs, simulated by stanpumpR itself:

<!-- generated: physiology-propofol-table -->

The higher plasma concentration raises the effect-site peak; the slower ke0
delays it.

## The gut

**GI absorption** scales the absorption rate constant (ka) of the oral routes.
At a higher setting the drug arrives sooner, and at a lower one later. The
amount absorbed (the bioavailability) does not change.

## Which drugs respond

The inhaled anesthetics do not respond to the sliders yet: they are simulated
by the gas engine, whose cardiac output is fixed at its default for the
patient's size.

Only drugs whose renal share, hepatic share and extraction ratio were read in
a published source respond. Any other drug ignores the sliders, and the panel
lists it under **Not changed**. Rows marked "not yet" are still waiting for a
readable source for a missing value.

<!-- generated: physiology-table -->

## References

Castle-Kirszbaum M, Parkin WG, Goldschlager T, Lewis PM. Cardiac output and
cerebral blood flow: a systematic review of cardio-cerebral coupling.
*J Neurosurg Anesthesiol* 2022;34:352-63. doi:10.1097/ANA.0000000000000768

Cook DJ, Proper JA, Orszulak TA, Daly RC, Oliver WC. Effect of pump flow rate
on cerebral blood flow during hypothermic cardiopulmonary bypass in adults.
*J Cardiothorac Vasc Anesth* 1997;11:415-9. doi:10.1016/s1053-0770(97)90047-1

Hiraoka H, Yamamoto K, Miyoshi S, et al. Kidneys contribute to the
extrahepatic clearance of propofol in humans, but not lungs and brain.
*Br J Clin Pharmacol* 2005;60:176-82. doi:10.1111/j.1365-2125.2005.02393.x

Levine BD, Giller CA, Lane LD, Buckey JC, Blomqvist CG. Cerebral versus
systemic hemodynamics during graded orthostatic stress in humans.
*Circulation* 1994;90:298-306. doi:10.1161/01.cir.90.1.298

Ogoh S, Brothers RM, Barnes Q, et al. The effect of changes in cardiac output
on middle cerebral artery mean blood velocity at rest and during exercise.
*J Physiol* 2005;569:697-704. doi:10.1113/jphysiol.2005.095836

Takizawa D, Hiraoka H, Goto F, Yamamoto K, Horiuchi R. Human kidneys play an
important role in the elimination of propofol. *Anesthesiology*
2005;102:327-30. doi:10.1097/00000542-200502000-00014

Vlastra W, van Nieuwkerk AC, Bronzwaer AGT, et al. Cerebral blood flow in
patients with severe aortic valve stenosis undergoing transcatheter aortic
valve implantation. *J Am Geriatr Soc* 2021;69:494-9. doi:10.1111/jgs.16882

Wilkinson GR, Shand DG. Commentary: a physiological approach to hepatic drug
clearance. *Clin Pharmacol Ther* 1975;18:377-90.
