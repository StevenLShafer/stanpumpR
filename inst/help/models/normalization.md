Drugs in the library span six orders of magnitude of concentration: propofol in micrograms per millilitre, sufentanil in hundredths of a nanogram. **Graph Options → Normalize to** rescales each curve to its own peak so that their *shapes* can be compared.

## The two options

**Peak plasma.** Each drug's plasma concentration is divided by its own peak plasma concentration over the simulation and shown as a percentage, so every curve peaks at 100. Only the plasma line is drawn. For mannitol, plotted as serum osmolality, it is the rise above the baseline osmolality that is normalised.

**Peak effect site.** Each drug's effect-site concentration is divided by its own peak effect-site concentration, as a percentage. Only the effect-site line is drawn, so a drug with no effect site reads zero throughout; normalise it to its plasma peak instead.

In either case the dose no longer matters to the picture, only the time course: a bolus of fentanyl and a bolus of remifentanil normalised to their effect-site peaks differ only in how fast they rise and fall.

## Why only one line is shown

The plasma and effect-site curves have different peaks. Normalising each to its own peak would put both at 100 and lose the relationship between them; normalising both to the plasma peak would be a different plot from normalising both to the effect-site peak. The program shows one line, normalised to its own peak, to keep the picture unambiguous.

## What is turned off

While normalization is on, the time-until-threshold line, the MEAC panel and the interaction panel are not shown, because they depend on absolute concentrations. The shaded band is not rescaled with the curves, so it means nothing on the normalised axis; the scenarios that use normalization set the band to none.

## What to look for

[The context-sensitive decrement scenario](scenario:context-sensitive-opioids) normalises four opioids to their effect-site peaks after three-hour infusions. The infusion rates were chosen arbitrarily; the comparison is of offset, and normalization is what makes it fair. [The naloxone scenario](scenario:naloxone-morphine) uses it to put naloxone and morphine, which differ in concentration a thousandfold, on one picture.
