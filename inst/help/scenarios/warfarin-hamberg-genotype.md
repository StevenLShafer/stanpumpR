## What to look for

A 75-year-old, 75 kg, 172 cm man is started on a fixed **warfarin 5 mg once a day**, and followed for eight weeks. He is a slow CYP2C9 metaboliser (**\*2/\*3**) and warfarin-sensitive (**VKORC1 AA**), the genotype set in the Patient Profile. With **Hamberg** ticked under Graph Options, the INR is drawn twice in the one *warfarin (INR)* panel: the library's model (Xue 2017) in warfarin's own colour, and the Hamberg (2007) genotype model in a lighter shade, named in the legend as *warfarin (Hamberg)*.

- The INR does not jump; it **climbs over days to weeks**, because the vitamin-K-dependent clotting factors already in the blood have to be cleared before the INR moves. In the Hamberg model this delay is two chains of transit compartments (mean transit times of 11.6 hours and 120 hours).
- A fixed 5 mg/day start is **well above what this patient needs**: Hamberg's own dose table predicts about 1 mg/day to reach an INR of 2.5 in a \*2/\*3, VKORC1 AA, elderly patient, so both lines climb past the usual 2-3 target. That is the teaching point — the same milligram dose means very different INRs in different patients.
- **Hover** over the panel and the readout gives both INR values, the library's model first and the Hamberg model by its name.

## Try next

- Untick **Hamberg** and the second line goes; tick it again and it returns. The box is enabled only while warfarin is in the dose table.
- Change **VKORC1** from AA to GG and apply. Hamberg's half-maximal S-warfarin concentration rises from 2.20 to 4.61 mg/L, so the patient is about half as sensitive and the Hamberg INR climbs **much less** for the same dose. The library line moves far less: its model lowers the C50 only for AA, so GG and GA are the same.
- Change **CYP2C9** from \*2/\*3 to \*1/\*1 (the wild type) and apply. S-warfarin clearance rises by about threefold, the S-warfarin concentration falls, and the Hamberg INR comes down toward the therapeutic range on this 5 mg/day dose.
- Set the **age** to 40 and apply. Both S- and R-warfarin clearances rise (about 9% and 10% per decade of youth), so the INR settles lower.

## Background

[The Hamberg genotype model](help:models/hamberg) explains the two models and how the second INR line is computed and drawn. The library's own model is [warfarin](help:drugs/warfarin) (Xue 2017). Both are research and teaching models: a simulated INR is **not** a dose recommendation, a bleeding risk or a procedure-safe time.
