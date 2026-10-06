**Allometric scaling.** Scaling a parameter with body weight to a power: clearances to the 0.75 power, volumes to the first. See [Covariates and body size](help:models/covariates).

**Alveolar concentration.** For an inhaled agent, the tension in alveolar gas, which the end-tidal monitor measures. Plotted as the gas's "plasma" line.

**Bioavailability (F).** The fraction of an oral, intramuscular or intranasal dose that reaches the systemic circulation.

**Bolus.** A dose given all at once. In the dose table, any row with a mass unit.

**Ce.** Effect-site concentration. See [The effect site and ke0](help:models/effect-site).

**Clearance (CL).** The volume of plasma cleared of drug per unit time, L/min. CL1 is elimination; CL2 and CL3 are intercompartmental.

**Compartment.** A notional volume in which drug is assumed to be uniformly mixed. See [The three-compartment model](help:models/three-compartment).

**Concentration effect.** The faster rise of an inhaled agent given at high inspired concentration, because its own uptake concentrates what remains in the alveolus.

**Context-sensitive half-time.** The time for the plasma concentration to halve after an infusion stops, which depends on how long the infusion ran. See [Time until threshold](help:models/recovery).

**Covariate.** A patient characteristic (age, weight, height, sex) that a model uses to adjust its parameters.

**Cp.** Plasma concentration.

**CYP2D6.** A liver enzyme with common genetic variants that forms the active metabolites of codeine, tramadol, hydrocodone and oxycodone. The field in the Patient Profile is not yet active.

**Dead space.** The part of each breath that does not reach the alveoli; 30 per cent of minute ventilation in the gas model.

**Effect site.** A hypothetical compartment whose concentration drives drug effect, linked to the plasma by ke0.

**Eigenvalue.** One of the three exponents (α, β, γ) of the tri-exponential concentration curve, obtained by solving the characteristic cubic of the compartment model.

**endCe.** The recovery threshold in the drug library: the effect-site concentration that *Time until threshold* counts down to.

**Fat-free mass (FFM).** Body mass excluding fat, by the Al-Sallami equations; used by the Eleveld models.

**Fresh gas flow.** The total flow from the flowmeters (air, oxygen, nitrous oxide) into the breathing circuit.

**Hysteresis.** The lag between plasma concentration and effect, visible as a loop when effect is plotted against plasma concentration; the effect compartment collapses it.

**Infusion.** Drug given at a rate. In the dose table, any row with a per-minute or per-hour unit; it runs until the next row for that drug changes it.

**ke0.** The rate constant for equilibration between plasma and effect site, 1/min. Its half-time is ln(2)/ke0.

**Lean body mass (LBM).** Body mass excluding fat, by the James equations.

**MAC.** Minimum alveolar concentration: the alveolar concentration of an inhaled agent at which half of patients do not move to incision. A property of the agent, falling with age.

**MAC equivalents.** The patient's alveolar concentration as a multiple of the age-adjusted MAC, summed over agents present.

**Mammillary.** A compartment model in which the peripheral compartments connect only to the central one, like a hub and spokes.

**Maturation.** The rise of clearance from birth towards adult values, described as a function of post-menstrual age.

**MEAC.** Minimum effective analgesic concentration. See [MEAC: comparing opioids](help:models/meac).

**Minute ventilation.** The volume breathed per minute; the "ventilation" entry in the dose table.

**Normalization.** Rescaling each curve to its own peak. See [Normalization](help:models/normalization).

**Partition coefficient.** The ratio of concentrations of a gas in two phases at equilibrium: blood:gas, tissue:gas.

**PK/PD.** Pharmacokinetics (what the body does to the drug: concentration over time) and pharmacodynamics (what the drug does to the body: effect at a given concentration).

**Rate constant (k).** A first-order rate, 1/min: k10 elimination, k12 and k21 transfer to and from the fast compartment, k13 and k31 to and from the slow one.

**Rebreathing.** Inspiring exhaled gas; in the gas model it stops once fresh gas flow reaches minute ventilation.

**Response surface.** A model of the combined effect of two drugs as a function of both concentrations. See [Propofol-opioid interaction](help:models/interaction).

**Second gas effect.** The faster rise of a second inhaled agent in the presence of nitrous oxide, because nitrous oxide's bulk uptake concentrates it.

**Steady state.** When the rate of drug in equals the rate out and the concentration is constant; reached in practice only after several terminal half-lives.

**TCI.** Target-controlled infusion: a pump that computes its own rate to reach and hold a target concentration. See [In development](help:in-development).

**Time until threshold.** At each moment, how long the concentration would take to fall to the threshold if delivery stopped then. See [Time until threshold](help:models/recovery).

**tPeak.** Time to peak effect after a bolus; what ke0 is solved from.

**Typical range.** The shaded band: a published therapeutic range for a typical indication, from the drug library.

**Vessel-rich group (VRG).** The highly perfused tissues (brain, heart, liver, kidneys) in the gas model; plotted as the gas's "effect site" line, labelled brain.

**Volume of distribution (V).** The apparent volume into which a dose distributes to give the observed concentration, L. V1 is the central compartment.
