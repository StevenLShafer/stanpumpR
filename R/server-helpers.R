showIntroModal <- function() {
  showModal(
    modalDialog(
      title = "Welcome to stanpumpR",
      p(
        "stanpumpR, derived from the original STANPUMP program developed at
        Stanford University,  performs pharmacokinetic simulations
        based on mathematical models published in the peer-reviewed
        literature. stanpumpR is intended to help clinicians and investigators
        better understand the mathematical implications of published models.
        stanpumpR is only an advisory program. How these models are applied to
        individual patients is a matter of clinical judgment by the health care
        provider."
      ),
      tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-dismiss` = "modal",
        "OK"
      ),
      footer = NULL,
      easyClose = TRUE,
      fade = TRUE,
      size = "s"
    )
  )
}

