# Single Paper Review: what happens to an uploaded manuscript

> **DRAFT for Steven L. Shafer and for legal review. This is not a
> compliance certification, a GDPR assessment or a data processing
> agreement.** It describes what the code on branch
> `claude/single-paper-review-app-cbf738` does, and what Anthropic's published
> terms said on 2026-10-30. Before this text is shown to users it needs: a
> named controller and contact, a lawful basis, a decision on each item under
> "Open decisions" in `docs/paper-review.md`, and a check of every Anthropic
> quotation below against the live page (the quotations were read through an
> automated fetch that summarises pages, so each must be confirmed by eye).

## 1. Who is involved

| Party | Role (GDPR terms, provisional) | What it holds |
|---|---|---|
| The person who uploads the manuscript | Data subject where the manuscript contains personal data (authors' names, and any patient data the manuscript reports); usually also a reviewer bound by a journal's confidentiality rules | The manuscript |
| The operator of the Single Paper Review app (today: Steven L. Shafer, running it locally on his own Anthropic account) | Controller | The manuscript and its extraction, in the app's memory, for the session only |
| Anthropic, PBC | Processor, under the Data Processing Addendum incorporated into its Commercial Terms | The request (manuscript included) and the response, under Anthropic's retention policy (section 4) |
| The host of the app: shinyapps.io (Posit), on the operator's professional account (decided 2026-10-30) | Processor or sub-processor of the operator, under Posit's own terms, which are not yet reviewed here | The app's process memory, its temporary files until they are deleted, and its logs |

Anthropic's Data Processing Addendum states that "Customer is the controller
and Anthropic is Customer's processor", and that it is "incorporated into and
forms part of the Anthropic Commercial Terms of Service"
([Data Processing Addendum](https://www.anthropic.com/legal/data-processing-addendum),
effective 24 February 2025). The Commercial Terms say that data submitted
"will be processed in accordance with the Anthropic Data Processing Addendum"
([Commercial Terms of Service](https://www.anthropic.com/legal/commercial-terms),
effective 17 June 2025). Nothing has to be signed separately for this to apply.

## 1a. Requirements on the deployment (Shafer, 2026-10-30)

- **Nothing held on the server** (Shafer, 2026-11-10). stanpumpR's server
  never stores a manuscript, its supplement, the extracted model or the
  review: not on disk, not in a database, not in a log or a backup, and not
  after the user's session ends. They exist only in the server's memory, for
  the one browser session that uploaded them, and are gone when that session
  ends. In Shafer's words: "I am uncomfortable with leaving a finished review
  on the server, because I believe that 'single paper review' will be used to
  review submissions to peer reviewed journals. I don't want those papers, or
  the review, held on the server." So a review that finishes after its page
  was closed is dropped unread, never kept for the user to collect; the only
  copy that can outlive a session is the report the user downloads to their
  own computer. A change that would keep any of these on the server (a cache,
  a queue of finished reviews, a resumable job) is ruled out by this
  requirement, not merely deferred.

Steven L. Shafer, as operator: "Claude is not allowed to learn from my
models. Claude is allowed to see coarse metadata."

- **No training.** The content of an uploaded manuscript, its supplement, and
  the model extracted from them must not be used to train any model. On
  Anthropic's side this rests on Anthropic's terms, not on anything the app
  does. The Commercial Terms say "Anthropic may not train models on Customer
  Content from Services"
  ([Commercial Terms of Service](https://www.anthropic.com/legal/commercial-terms),
  effective 17 June 2025). The privacy center lists the exceptions: explicit
  feedback or bug reports, the customer choosing to allow it, or joining the
  Development Partner Program
  ([Is my data used for training?](https://privacy.claude.com/en/articles/7996868-is-my-data-used-for-training),
  dated 18 August 2026;
  [Does Anthropic act as a data processor or controller?](https://privacy.claude.com/en/articles/9267385-does-anthropic-act-as-a-data-processor-or-controller)).
  So the requirement holds only while the deployment stays outside every
  exception. The app sends no feedback and no bug reports. The organisation
  that owns the API key must not opt in to training or join the Development
  Partner Program; those are account settings for the operator to keep, not
  guarantees this document can give. A request the server-side refusal
  fallback re-runs on another model is the same API call under the same
  account and terms.
- **Coarse metadata, defined.** The only information about a review that may
  ever be recorded, or seen outside the user's own session, is coarse
  metadata, defined here as exactly:
  - that a review request was made (a count), and when;
  - its token usage (input and output token counts, as the API reports them);
  - its timing (how long the request took);
  - its outcome class: success, or the error code the app assigns
    (`http_429`, `refusal`, `max_tokens`, ...), never the error's text.

  Never content: not the manuscript or any part of it, its file name or size,
  its title, authors or journal, the drug's name, any parameter value,
  covariate, regimen or figure point, the extraction, the review's findings,
  or the API key. Since 2026-11-10 the app records exactly this list, and
  only for a review sent with the operator's own key (Shafer: "track usage
  and let me know how often it is used"): one line on the server log and one
  email to the operator per review (`R/paper-review-usage.R`). A review sent
  with a user's own key records nothing. **This definition is for Shafer to
  confirm or narrow.**

  The rule governs what is recorded and reused. Anthropic necessarily
  processes the whole of each request to answer it, and its automated safety
  systems and, on a flag, its staff may examine it (section 4). The app cannot
  prevent that; the operator accepts it by sending the request.

## 2. What the app does with a manuscript, step by step

These are statements about this code. Each names the function that does it.

1. **Upload.** Shiny writes the uploaded file to a temporary directory on the
   server. The app reads it into the session's memory and deletes the file at
   once, and the directory with it when it is left empty
   (`paperReviewReadUpload()`, `R/paper-review-app.R`). A file that is not a
   PDF is deleted the same way and not kept. The file's name is never stored.
2. **Held in memory.** The bytes live in an R environment private to that
   browser session (`paperReviewStore()`). They are not written to disk, not
   cached, not put in the URL, and not bookmarked (the app is started with
   bookmarking disabled).
3. **Sent to Anthropic, only on request.** The manuscript leaves the server
   only when the user ticks the confirmation box and presses **Review the
   paper**. It is sent once, inline (base64) in one request to the Anthropic
   Messages API, over HTTPS, from a short-lived background R process on the
   same server (`paperReviewExtractAsync()` and `paperReviewTransport()`,
   `R/paper-review-api.R`), which receives the request in memory, writes
   nothing, and exits when the reply has been passed back.
   The app does **not** use Anthropic's Files API (which stores files until
   they are deleted), the Batch API, prompt caching, or any user identifier.
   The request may be re-run inside the same API call on another Anthropic
   model if the first declines it (Anthropic's server-side refusal fallback,
   `fallbacks: "default"`).
4. **Discarded from the app.** As soon as the response returns, successful or
   not, the manuscript's bytes are removed from the session. What remains is
   the extraction (the model's parameters, figures' digitised points and the
   locations Claude cited) and the review computed from it. If the session
   has already ended when the reply returns (the page was closed while Claude
   read), the reply is dropped unread: there is no session to give it to, and
   nothing keeps it for later (section 1a, *Nothing held on the server*).
5. **Discarded at the end.** The extraction and review are removed when the
   user presses **Discard the paper now**, or when the browser session ends
   (closing the tab, or a disconnection), whichever comes first
   (`paperReviewDiscard()`, called from `session$onSessionEnded()`).
6. **No logging.** The app writes nothing about the manuscript, its
   extraction or its review to the console, the server log or any file; the
   only line it writes is the coarse metadata of a review made with the
   operator's key (section 1a). Its
   computations run with stanpumpR's comment log switched off, and warnings
   are captured rather than printed (`paperReviewQuietly()`). Errors are shown
   to the user only, through Shiny's validation messages, which Shiny does not
   write to the server log. The API key is sent only in the request header,
   which the HTTP library is told to redact, and is never printed.
7. **Plots and downloads.** A plot is drawn to a temporary PNG file that is
   read and deleted at once, as Shiny's own plot output does. If downloads are
   enabled (Open decision 5), the report is written to a temporary file that
   Shiny marks as owned and deletes after sending it to the browser. Once
   downloaded, the report is on the user's own computer, outside the app.
8. **No digest, no archive.** Nothing from an uploaded manuscript is added to
   stanpumpR's paper archive, its digests or its drug library.

## 3. What the app cannot control

- **Anthropic's copy.** Once sent, the request and response are held by
  Anthropic under its policy (section 4), not by this app. Deleting them from
  the app does not delete them from Anthropic.
- **Memory.** R frees memory but cannot overwrite it. After a discard, the
  bytes are unreachable from the app, and the operating system reuses the
  memory in time; they are not cryptographically erased. A memory dump of the
  running server process taken before reuse could contain them.
- **The host.** The hosting provider controls the machine, its swap space,
  its backups and its network. The app will run on shinyapps.io, a shared
  host, where process memory, swap, temporary files until they are deleted,
  and the logs of stdout and stderr are under Posit's control and terms (not
  yet reviewed here).
- **The user's browser.** The report is drawn in the user's browser, which
  may cache the page. A downloaded report is the user's.
- **A crash.** If the R process is killed, the session's end handler does not
  run; the memory is released with the process.
- **Claude's reading.** The extraction is Claude's reading of the paper and
  may be wrong. That is a question of accuracy, not of privacy, but the report
  says it on every page.

The app never claims that no trace exists anywhere. It claims only what it
controls: that it keeps no copy after the session, writes none to disk, and
logs none.

## 4. What Anthropic does with the request

Every quotation below must be checked against the live page before use.

- **Retention.** "For Anthropic API users, we automatically delete inputs and
  outputs on our backend within 30 days of receipt or generation", with
  exceptions including a zero data retention agreement, enforcement of the
  Usage Policy, and compliance with the law
  ([How long do you store my organization's data?](https://privacy.claude.com/en/articles/7996866-how-long-do-you-store-my-organization-s-data),
  dated 1 July 2026). If Anthropic's automated trust and safety systems flag a
  request, "We retain inputs and outputs for up to 2 years and trust and
  safety classification scores for up to 7 years" (same page).
  - *Conflict to resolve:* Anthropic's developer documentation says
    "Conversation content (your prompts and Claude's outputs) is not retained
    by default; the exception is Covered Models, which require 30-day
    retention" ([API and data retention](https://platform.claude.com/docs/en/manage-claude/api-and-data-retention),
    undated). Until Anthropic confirms which applies, this document assumes
    the privacy center's 30 days. The app's default model, Claude Opus 5.5, is
    not listed among the Covered Models.
- **Zero data retention (ZDR).** Under ZDR, "Anthropic does not store
  customer prompts or responses at rest after the API response is returned";
  it is arranged per organisation through Anthropic's sales team; inline PDFs
  on the Messages API are eligible; the Files API and Batch API are not (same
  documentation page). Even under ZDR, "Anthropic may retain data where
  required by law or where it has been flagged by Anthropic's automated trust
  and safety systems." The models Anthropic calls Covered Models (Claude
  Fable 5.1, Claude Mythos 5.1, Claude Fable 5, Claude Mythos 5) are not
  available under ZDR unless expressly authorised
  ([Covered Models](https://support.claude.com/en/articles/15425695), dated
  1 September 2026). This app uses none of them. Whether to request ZDR is
  Open decision 2.
- **Training.** "Anthropic may not train models on Customer Content from
  Services" ([Commercial Terms](https://www.anthropic.com/legal/commercial-terms)).
  The privacy center adds the exceptions of explicit feedback and of opting in
  ([Is my data used for training?](https://privacy.claude.com/en/articles/7996868-is-my-data-used-for-training),
  dated 18 August 2026), and of the Development Partner Program. The app sends
  no feedback and the operator's organisation must not join that programme;
  section 1a makes this a requirement of the deployment.
- **Who at Anthropic sees it.** "Access occurs only for safety review or
  incident response", and "Model serving, safety classifiers, and
  abuse-detection pipelines process your content as part of normal operation"
  ([Access Transparency](https://platform.claude.com/docs/en/manage-claude/access-transparency)).
  Anthropic personnel are "subject to an appropriate duty of confidentiality"
  (DPA).
- **Where.** By default inference "may run in any available geography"; with
  `inference_geo: "us"` it runs only in the United States, at a higher price.
  Data at rest is stored in the United States; there is no EU data-residency
  option ([Data residency](https://platform.claude.com/docs/en/manage-claude/data-residency)).
  The app can send `inference_geo` through the `paper_review_inference_geo`
  setting; it is unset (the API's default) today.
- **International transfer.** The DPA incorporates the EU Standard
  Contractual Clauses (Modules Two and/or Three), with the UK and Swiss
  addenda, "deemed to have been executed by the parties". Anthropic's
  sub-processors are listed at <https://trust.anthropic.com/subprocessors>
  (the list could not be read automatically and must be checked).
- **Certifications.** Anthropic reports SOC 2 Type I and II, ISO 27001:2022
  and ISO/IEC 42001:2023
  ([What certifications has Anthropic obtained?](https://privacy.claude.com/en/articles/10015870-what-certifications-has-anthropic-obtained));
  its trust portal is <https://trust.anthropic.com/>.

## 5. What a GDPR notice still needs (for legal review)

- The controller's identity and contact, and a data protection contact if one
  is required.
- The lawful basis (consent by the uploader is the obvious candidate; the
  confirmation box records it for the request only and is not a consent
  record).
- The purposes, and confirmation that the manuscript is not used for anything
  else.
- The recipients: Anthropic (processor) and the host.
- The transfer outside the EEA (United States) and its safeguard (the SCCs in
  Anthropic's DPA, and the host's).
- Retention: none by the app beyond the session; Anthropic's, as above.
- The data subject's rights, and how they apply when the app keeps nothing
  after the session (the request to Anthropic is the operator's to pursue).
- A data protection impact assessment, which may be warranted given
  unpublished manuscripts under peer review and any patient data they contain.
- A journal's own confidentiality rules for reviewers, many of which forbid
  uploading a manuscript under review to an AI service at all. That is the
  reviewer's obligation; the app should say so (it does, in its help page).

## 6. Bring your own key (built 2026-11-10)

A user may enter their own Anthropic key in the tab; `paperReviewApiKey()`
takes it before the operator's. It is held only in that session's input
(never stored, logged or put in a bookmark), sent only in the request header,
and discarded with the session. The user's organisation then becomes the
Anthropic customer for that request, under its own terms with Anthropic, and
this document would need a second column for that case. Reviews made with a
user's key are not counted or reported.
