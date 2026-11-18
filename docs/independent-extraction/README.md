# Independent readings of stanpumpR's source papers

stanpumpR's models were transcribed from their papers by Claude, and the repository's rule is that every number is read in the PDF. The protocol here has the same papers read again, **blind**, by models from other companies: Gemini and ChatGPT, each the most capable model and mode the paid plan offers (the one Steven calls "Carbon" is a Gemini model). It then compares their numbers with stanpumpR's.

What each comparison means:
- **Two independent readers agree on a number that stanpumpR prints nowhere.** stanpumpR has probably missed or misread something, and the number is a strong lead.
- **One reader alone reports a number.** It is usually that reader's error, or a part of the paper stanpumpR did not use.

Where nlmixr2lib transcribed the same paper (`docs/nlmixr2lib/`), its reading counts as one more independent reader.

As with every external review, the readers' output is **leads, not findings** (`AGENTS.md`, *Adversarial review by another LLM*). A lead is settled only by reading the PDF.

## Files

| File | What it is |
|---|---|
| `PROMPT.md` | The instructions given to each reader, the same for every paper. They require, for each number, its page, its table or section, and a short exact quote. They never show stanpumpR's numbers. |
| `index.csv` | The 481 source papers: digest cards of kind `pk-model` or `pkpd-model` that name a model file of a Drug Library drug. Each row gives the archive file, the citation and identifiers, and stanpumpR's drugs. The 82 papers nlmixr2lib also transcribed come first, since three readings can be compared for them. `repliesSaved` lists the readers already done. |
| `replies/` | The readers' answers, `<stem>.<reader>.md` (for example `PMID_31338356.gemini.md`). **Not committed** (`.gitignore`): they quote the papers, which the digest cards may not. |
| `comparisons/` | Written by the comparison script: one CSV per paper and `summary.csv`. They contain numbers, parameter names and locations only, never a quote, so they can be committed. |

## Steps for each paper

1. **Choose a paper** from `index.csv`, in order.
2. **Get the PDF and its supplements** from the stanpumpR archive on Google Drive. Use the name in the `pdf` column. Supplements are files with the same stem and a suffix such as `_mmc1`, `_sup1`, `_ESM1` or `_SDC1`.
3. **Ask Gemini, in a new conversation.** Attach the PDF, its supplements and `PROMPT.md`, and send: *"Follow the instructions in PROMPT.md for the attached paper."*
   - Use one paper per conversation.
   - Turn off web search, so the reader uses only the paper.
4. **Ask ChatGPT, in a new conversation, the same way.** Also turn off memory and personalisation, so that an earlier reading cannot leak into this one.
5. **Save each reply in full**, unedited, as `<stem>.gemini.md` and `<stem>.chatgpt.md`, and add one first line naming the model and mode the app showed (for example `Reader: <model name>, <mode>`), so a reading can be traced to the model that made it. The script reads only the CSV block, so that line changes nothing else. Put the files in a Google Drive folder of their own, *not* inside the paper archive (the archive's pipeline owns that folder), or upload them to a Claude session.
6. **Claude takes over from there:**
   - copies the replies into `docs/independent-extraction/replies/`;
   - runs:

     ```bash
     Rscript --vanilla data-raw/independentExtractionCompare.R .
     ```

   - checks every lead (`LEAD`, then `in stanpumpR only after a unit factor (check)`) on the PDF;
   - records the outcome in the paper's digest card. A correction goes into the model with its test, by the repository's ordinary rules.

Rebuild the index after adding replies, or after cards are added:

```bash
Rscript --vanilla data-raw/independentExtractionIndex.R .
```

## What the verdicts mean

| Verdict | Meaning |
|---|---|
| `in stanpumpR` | The number, as printed, is in stanpumpR's digest card or in a model file the card names. |
| `in stanpumpR only after a unit factor (check)` | Found only as x60, /60, x1000, /1000, x100 or /100 of the value, to 0.5%. This may be stanpumpR's own conversion, or a coincidence: a card prints hundreds of numbers. |
| `LEAD: another reader agrees, not in stanpumpR` | At least two readers (nlmixr2lib counts as one) report the same number, and stanpumpR prints it nowhere. Readers are matched exactly, since the prompt forbids converting; only the comparison with stanpumpR and nlmixr2lib allows the unit factors. **Read the PDF.** |
| `one reading only, not in stanpumpR` | Probably that reader's error, or a model or table stanpumpR did not use. |
| `variability (not compared)` | Between-subject variability or residual error. stanpumpR carries these for only a few models. |
| `derived by the reader (not compared)` | A value the reader computed (`estimate_type` `derived`, from replies to the prompt's first wording). It is not in the paper, and two readers applying one formula agree, so it is never a lead. A value the paper prints but computed (`secondary`) is compared like any other. |
| `no number` | `value_as_printed` is not one number (a range, a word, two numbers). Minus signs, `x 10^n` and superscripts are read. |

A match means the number was transcribed, not that it sits in the right place: a small integer can match by coincidence. A lead can also be a model in the paper that stanpumpR read but deliberately did not implement. The card usually says so.

## Could Claude drive Gemini and ChatGPT directly?

Not with the subscriptions alone. A Claude Code session reaches another company's model only through its API (api.openai.com, generativelanguage.googleapis.com). As far as I know, the API is billed separately from ChatGPT's and Gemini's consumer plans and needs its own key.

With a key for each, saved in the environment's network secrets, a script could send all 481 papers, each with `PROMPT.md`, and collect the replies with no manual steps. That would mean:
- adding the two hosts to the environment;
- a script that sends each PDF and saves the reply into `replies/`.

Until then, the steps above do the same by hand.
