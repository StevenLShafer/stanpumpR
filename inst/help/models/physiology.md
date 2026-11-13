# Physiology sliders: liver, kidney, heart and gut

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
because liver blood flow is taken as a fixed share of cardiac output. For a
drug given by mouth whose bioavailability includes a hepatic first pass
(morphine), the new extraction ratio E′ = *l* E / (*q* (1 − E) + *l* E) also
changes the oral bioavailability, F′ = F (1 − E′)/(1 − E), never above 1.

## The kidneys

**Renal function** scales only the renal share of a drug's clearance: the
unchanged drug excreted in urine. Each drug's clearance is split into a renal
share, a hepatic share, and the rest. The rest is elimination outside the liver
and kidneys, such as plasma and tissue esterases (remifentanil) and Hofmann
elimination (cisatracurium), and no slider changes it. Setting renal function
to 0.1 removes 90% of the renal share and nothing else.

The renal slider does not reach a kidney's **metabolism** of a drug. Propofol
is the example: the kidneys metabolize almost a third of it. The slider
reaches only unchanged drug in urine. A Patient Profile serum creatinine
still changes the drugs whose models read it. The slider acts on top of that.

## Cardiac output and distribution

Besides hepatic blood flow, cardiac output scales two things:

- **The intercompartmental clearances** (Q2 and Q3), because distribution to
  the tissues is carried by blood flow.
- **The central volume** (V1), in the same direction. With a low cardiac
  output, a bolus mixes into less blood on its first pass, so early
  concentrations are higher, which a mammillary model expresses as a smaller
  V1. The size of this effect is an assumption, V1 in proportion to cardiac
  output, and is not fitted to data.

**ke0**, the effect-site rate constant, is held at its normal value, because
it describes equilibration with the effect site rather than delivery to it.
So the time to peak effect moves with the changed disposition: a low cardiac
output reaches its peak plasma concentration sooner and higher.

## The gut

**GI absorption** scales the absorption rate constant (ka) of the oral routes.
At a higher setting the drug arrives sooner, and at a lower one later. The
amount absorbed (the bioavailability) does not change.

## Which drugs respond

Only drugs whose renal share, hepatic share and extraction ratio were read in
a published source respond. Any other drug ignores the sliders, and the panel
lists it under **Not changed**. Rows marked "not yet" are still waiting for a
readable source for a missing value.

<!-- generated: physiology-table -->

## Reference

Wilkinson GR, Shand DG. Commentary: a physiological approach to hepatic drug
clearance. *Clin Pharmacol Ther* 1975;18:377-90.
