### The model

Kunisawa and colleagues in Asahikawa studied nine women having gynaecologic surgery (*J Anesth* 2015;29:198-205). They were aged 37 to 71 and weighed 47.1 to 73.0 kg, and were anaesthetised with propofol and fentanyl. Each was given landiolol by target-controlled infusion to plasma targets of 500 and then 1,000 ng/mL, for 30 minutes each. In all, 126 arterial samples were assayed.

A population fit gave two compartments per kilogram:
- clearance 34.0 mL/min/kg
- central volume 74.9 mL/kg
- intercompartmental clearance 70.9 mL/min/kg
- peripheral volume 38.9 mL/kg

The fit also had a lag of 0.63 minutes, which stanpumpR does not draw.

### When stanpumpR uses it

This is one of [three landiolol models](help:drugs/landiolol). stanpumpR uses it from 36 to 71 years. Between 64 and 71 it is chosen over the peripheral arterial disease study because it has one more patient.

### Where to be careful

- Nine patients is a small study, and all were women.
- The intercompartmental clearance is barely estimated: its standard error, 68.9, is as large as the value itself. The clearance is well estimated.
- Weight, lean body mass and age were tested and none was retained.

See [the vasoactive and resuscitation drugs](help:models/vasoactive).
