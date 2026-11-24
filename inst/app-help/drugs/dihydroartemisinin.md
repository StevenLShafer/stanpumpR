### The model

Dihydroartemisinin is the active metabolite of [artemether](help:drugs/artemether). It cannot be entered in the dose table: no dihydroartemisinin product is approved in the United States. Its curve comes from the artemether in the [Coartem](help:drugs/Coartem) you give.

The parameters come from Ding and colleagues' joint model (*Br J Clin Pharmacol* 2026;92:589-605, Table 2). The model has one compartment, with values for a typical 45 kg patient:

- **Clearance:** CL/F 255 L/h × (weight / 45)<sup>0.75</sup>.
- **Volume:** V/F 64.1 L × weight / 45.

These are apparent values: artemether's bioavailability, fixed at 1, is built into them. The model assumes that all of artemether's elimination forms dihydroartemisinin.

### Its curve follows artemether's

Dihydroartemisinin's own half-life is about 10 minutes, much shorter than the time it takes to be formed from artemether. So its curve rises and falls with its formation: it peaks about 2 hours after each dose, as artemether does.

Because artemether's clearance rises with each dose, more of each dose becomes dihydroartemisinin sooner. Its peaks therefore rise over the course while artemether's fall. With all of artemether converted, the total area of the dihydroartemisinin curve is simply the dose over its clearance. For the typical 45 kg patient on six doses of 80 mg, that is 480 mg / 255 L/h = 1,882 h·ng/mL; Ding's median is 1,870.

Ding does not say whether the conversion was modelled in mass or in moles. Mass matches Ding's median area; moles would give 1,794.

### No band

No therapeutic range is established for dihydroartemisinin.
