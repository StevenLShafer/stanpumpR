Buprenorphine is the one opioid the ordinary [% MEAC](help:models/meac) scale cannot handle. That scale adds opioids as if each were a full agonist contributing in proportion to its concentration. Buprenorphine is a **partial agonist** that binds the µ-opioid receptor and lets go of it very slowly, and it is that slow release — not its affinity — that makes it behave unlike any other opioid: it blocks other opioids durably, it has a ceiling on respiratory depression, and giving it on top of a full agonist can precipitate withdrawal.

With **Graph Options → Buprenorphine / opioid-receptor interaction** ticked — offered only while buprenorphine or an opioid antagonist (naloxone) is in the dose table — stanpumpR adds panels built on receptor occupancy rather than on the additive scale.

## Why slow dissociation, not affinity

A drug's *affinity* is where it sits at equilibrium. Its *displaceability* — how readily another opioid can take its receptor — is set by how fast it lets go. Sufentanil binds the µ receptor at least as tightly as buprenorphine, yet another opioid displaces it easily, because sufentanil comes off quickly and the receptor is constantly free to be taken. Buprenorphine comes off with a half-life of about 37 minutes: once it is on, it stays, and a competing opioid can only take a receptor as fast as buprenorphine releases it. Equal affinity, opposite behaviour. Carrying that release rate explicitly is the whole point of this model; an equilibrium calculation, which depends only on affinity, cannot tell the two drugs apart.

## One occupancy, two effects

A µ receptor is a µ receptor. There is **one physical occupancy** for each opioid — the fraction of receptors it holds — and ventilation and analgesia are two different *transductions* of that single occupancy, differing only in how much spare receptor capacity (reserve) each has. So the model carries each opioid's true dissociation constant and computes a competitive occupancy of one shared receptor pool; buprenorphine binds slowly (and holds on), the full agonists and naloxone reach their balance quickly within whatever receptors buprenorphine leaves free.

The mathematics is given in full, for a reader who wants it, in the typeset note [`docs/buprenorphine-interaction.tex`](https://github.com/StevenLShafer/stanpumpR/blob/master/docs/buprenorphine-interaction.tex) (and `docs/buprenorphine-interactions-math.md`); the engine is `R/opioidReceptorInteraction.R`.

## The panels

- **Receptor occupancy** (%): a line for each opioid's physical occupancy, plus the free-receptor fraction. This is the picture to read first. On a maintenance background buprenorphine holds most of the receptor and an added full agonist reaches only a thin slice (**blockade**); when buprenorphine is stopped, watch its line fall over *hours*, not minutes, as the slow release lets the receptor reopen. Occupancy here is physical receptor occupancy; ventilation and analgesia are separate transductions of it.
- **Respiratory depression** (%): the share of baseline ventilation lost. Buprenorphine alone plateaus at its **ceiling** (about 0.81 of baseline in an opioid-naive patient, less in a tolerant one), and adding a full agonist on a high buprenorphine background barely moves it — the safety margin that makes buprenorphine respiratory-sparing.
- **Analgesia** (MEAC-equivalents): the analgesic drive in multiples of one MEAC, so a full agonist at its own MEAC reads 1. Buprenorphine contributes its own partial analgesia here. The **available-drive** line is the full-agonist drive that is actually getting through the blockade — the honest answer to "will my fentanyl work, and how much?"

## Reading the three clinical situations

- **Opioid use disorder, maintenance.** Buprenorphine occupancy is high and durable; the respiratory curve is flat even if a full agonist is added — the receptors are occupied, so the agonist cannot act. This is the therapeutic blockade.
- **Surgery on a maintenance patient.** The occupancy panel shows how little room a full agonist wins, and the available-drive line shows how much analgesic drive gets through. Because buprenorphine's own analgesia is counted, the analgesia panel is not zero at baseline.
- **Induction / precipitated withdrawal.** Give buprenorphine while a full agonist is on board and its occupancy ratchets up, evicting the agonist from receptors; because buprenorphine is a weaker agonist, the net effect can *fall* — the model shows the dip, and that it depends on *when* buprenorphine is introduced relative to the full agonist.

## Tolerance

The Patient Profile's **Opioid tolerance** field moves this model the way it moves [% MEAC](help:models/meac): a tolerant patient's receptors are *desensitised* (the transduction changes, and full agonists need more concentration), while buprenorphine's binding is unchanged — residence time is a property of the molecule, not the patient. The four tolerance levels are placed so the model and that field move together.

## Scope and cautions

This is a **µ-receptor** model only. Buprenorphine's κ and ORL-1/NOP actions, its active metabolite norbuprenorphine, and the bell-shaped high-dose antinociception sometimes seen in animals are deliberately out of scope. The transduction reserves and the analgesic ceiling (a band of 2–5× MEAC for buprenorphine) are calibration choices, documented in the math note and adjustable; a full agonist's effective dissociation constant in this model is larger than its in-vitro binding constant, the difference being receptor reserve. The panels are **illustrative, not a prescribing tool**: they are simulations for one typical patient with the covariates entered, not a dosing recommendation. See [Cautions](help:cautions).

## References

Olofsen E, Algera MH, Moss L, et al. Modeling buprenorphine reduction of fentanyl-induced respiratory depression. *JCI Insight* 2022;7(9):e156973. · Yassen A, et al. Mechanism-based PK/PD modelling of buprenorphine antinociception. *Anesthesiology* 2006;104:1232. · Yassen A, et al. Reversal of buprenorphine-induced respiratory depression by naloxone. *Clin Pharmacokinet* 2007;46:965. · Nasser AF, et al. *Clin Pharmacokinet* 2014;53:813. · Lutfy K, Cowan A. Buprenorphine: a unique drug with complex pharmacology. *Curr Neuropharmacol* 2004;2:395. · Black JW, Leff P. Operational models of pharmacological agonism. *Proc R Soc Lond B* 1983;220:141.
