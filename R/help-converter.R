# -----------------------------------------------------------------------------
# The PK parameter converter: the markup of the help page models/pk-converter
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5.5), 2026-11-01, at the request of
# Steven L. Shafer, who wrote the spreadsheet it reproduces, Convert.xls (last
# updated 12/02): five ways of defining a one-, two- or three-compartment
# mammillary model, each giving every other parameter.
#
# The page is the generated block "pk-converter" (R/help-generated.R).  This
# file only writes the HTML; inst/www/pk-convert.js does the arithmetic and
# answers the fields, in the browser, through delegated handlers, so nothing
# reaches the Shiny server.  To keep Shiny from binding the fields as inputs,
# none has an id or a name (Shiny binds a text input by either) and each
# carries data-shiny-no-bind-input; the radio buttons have a name, which
# Shiny's bindings ignore for a bare radio.  The block is one HTML block with
# no blank line, so commonmark passes it through whole.
# -----------------------------------------------------------------------------

# One row per field: the field's name as pk-convert.js knows it, its label
# (HTML), its dimension and the numbers of compartments that have it.
PK_CONVERTER_GROUPS <- list(
  list(id = "vc", title = "1. Volumes and clearances", fields = list(
    c("v1", "V<sub>1</sub>", "vol", "1,2,3"),
    c("v2", "V<sub>2</sub>", "vol", "2,3"),
    c("v3", "V<sub>3</sub>", "vol", "3"),
    c("cl1", "CL<sub>1</sub>", "clear", "1,2,3"),
    c("cl2", "CL<sub>2</sub>", "clear", "2,3"),
    c("cl3", "CL<sub>3</sub>", "clear", "3"))),
  list(id = "micro", title = "2. V1 and the rate constants", fields = list(
    c("v1", "V<sub>1</sub>", "vol", "1,2,3"),
    c("k10", "k<sub>10</sub>", "rate", "1,2,3"),
    c("k12", "k<sub>12</sub>", "rate", "2,3"),
    c("k13", "k<sub>13</sub>", "rate", "3"),
    c("k21", "k<sub>21</sub>", "rate", "2,3"),
    c("k31", "k<sub>31</sub>", "rate", "3"))),
  list(id = "half", title = "3. V1, Vdss, CL and the half-lives", fields = list(
    c("v1", "V<sub>1</sub>", "vol", "1,2,3"),
    c("vdss", "V<sub>dss</sub>", "vol", "1,2,3"),
    c("cl", "CL", "clear", "1,2,3"),
    c("t1", "t<sub>&frac12;</sub> &lambda;<sub>1</sub>", "time", "1,2,3"),
    c("t2", "t<sub>&frac12;</sub> &lambda;<sub>2</sub>", "time", "2,3"),
    c("t3", "t<sub>&frac12;</sub> &lambda;<sub>3</sub>", "time", "3"))),
  list(id = "coef", title = "4. Coefficients and exponents", fields = list(
    c("a1", "A<sub>1</sub>", "coef", "1,2,3"),
    c("a2", "A<sub>2</sub>", "coef", "2,3"),
    c("a3", "A<sub>3</sub>", "coef", "3"),
    c("l1", "&lambda;<sub>1</sub>", "rate", "1,2,3"),
    c("l2", "&lambda;<sub>2</sub>", "rate", "2,3"),
    c("l3", "&lambda;<sub>3</sub>", "rate", "3"))),
  list(id = "exp", title = "5. V1, the exponents, k21 and k31", fields = list(
    c("v1", "V<sub>1</sub>", "vol", "1,2,3"),
    c("l1", "&lambda;<sub>1</sub>", "rate", "1,2,3"),
    c("l2", "&lambda;<sub>2</sub>", "rate", "2,3"),
    c("l3", "&lambda;<sub>3</sub>", "rate", "3"),
    c("k21", "k<sub>21</sub>", "rate", "2,3"),
    c("k31", "k<sub>31</sub>", "rate", "3")))
)

# The unit each dimension is shown in, with time in minutes as the page opens;
# pk-convert.js rewrites the ones that carry time when the unit changes.
PK_CONVERTER_UNITS <- c(vol = "L", clear = "L/min", rate = "1/min", time = "min",
                        coef = "1/L")

#' One field of the converter, as an HTML string
#'
#' The ordinary keyboard, not inputmode="decimal": in a locale whose decimal
#' mark is a comma the decimal keypad may offer no point, which is the only
#' decimal mark the page reads.
#' @noRd
pkConverterInput <- function(attr, label) {
  sprintf(paste0('<input type="text" inputmode="text" autocomplete="off" spellcheck="false" ',
                 'class="form-control form-control-sm" %s aria-label="%s" data-shiny-no-bind-input>'),
          attr, label)
}

#' A unit cell; the units that carry time are rewritten by pk-convert.js
#' @noRd
pkConverterUnit <- function(dim) {
  timed <- dim %in% c("clear", "rate", "time")
  sprintf('<td class="pkconv-unit"%s>%s</td>',
          if (timed) sprintf(' data-pk-unit="%s"', dim) else "", PK_CONVERTER_UNITS[[dim]])
}

#' The converter, as one HTML block
#'
#' The generated block \code{pk-converter} on the help page
#' \code{models/pk-converter}.
#' @param drugDefaults unused; every block takes it
#' @returns one HTML string with no blank line
#' @noRd
helpPKConverterHTML <- function(drugDefaults = NULL) {
  radios <- function(name, values, labels, checked) {
    paste0(vapply(seq_along(values), function(i) {
      sprintf(paste0('<label class="pkconv-choice"><input type="radio" name="%s" value="%s"%s ',
                     'data-shiny-no-bind-input> %s</label>'),
              name, values[i], if (values[i] == checked) " checked" else "", labels[i])
    }, character(1)), collapse = "")
  }
  # Tags only: the entities (&lambda;, &frac12;) are decoded in the attribute
  plain <- function(html) gsub("<[^>]+>", "", html)

  groups <- vapply(PK_CONVERTER_GROUPS, function(g) {
    rows <- vapply(g$fields, function(f) {
      sprintf('<tr data-pk-n="%s"><th scope="row">%s</th><td>%s</td>%s</tr>',
              f[4], f[2],
              pkConverterInput(sprintf('data-pk-field="%s" data-pk-dim="%s"', f[1], f[3]),
                               paste(plain(f[2]), "-", plain(g$title))),
              pkConverterUnit(f[3]))
    }, character(1))
    paste0(sprintf('<section class="pkconv-group" data-pk-group="%s"><h4>%s</h4>', g$id, g$title),
           '<table class="pkconv-table"><tbody>', paste(rows, collapse = ""),
           "</tbody></table></section>")
  }, character(1))

  effectRows <- c(
    ke0 = "k<sub>e0</sub>|rate",
    tke0 = "t<sub>&frac12;</sub> k<sub>e0</sub>|time",
    tpeak = "Time to peak effect after a bolus|time"
  )
  effect <- vapply(names(effectRows), function(k) {
    parts <- strsplit(effectRows[[k]], "|", fixed = TRUE)[[1]]
    sprintf('<tr><th scope="row">%s</th><td>%s</td>%s</tr>', parts[1],
            pkConverterInput(sprintf('data-pk-effect="%s"', k), plain(parts[1])),
            pkConverterUnit(parts[2]))
  }, character(1))

  paste(c(
    '<div class="pkconv">',
    paste0('<div class="pkconv-controls">',
           '<fieldset><legend>Compartments</legend>',
           radios("pkconv-n", c("1", "2", "3"), c("1", "2", "3"), "3"), "</fieldset>",
           '<fieldset><legend>Time unit</legend>',
           radios("pkconv-unit", c("s", "min", "h"), c("s", "min", "h"), "min"), "</fieldset>",
           '<div class="pkconv-actions">',
           '<button type="button" class="btn btn-outline-primary btn-sm" data-pk-action="example">',
           "Load the spreadsheet&rsquo;s example</button> ",
           '<button type="button" class="btn btn-outline-secondary btn-sm" data-pk-action="clear">Clear</button>',
           "</div></div>"),
    paste0('<p class="pkconv-status" role="status" aria-live="polite" data-kind="info">',
           "Enter a complete set of parameters in any one panel; the others are filled in.</p>"),
    paste0('<div class="pkconv-legend small"><span class="pkconv-key pkconv-entered">entered</span> ',
           '<span class="pkconv-key pkconv-derived">derived</span> ',
           '<span class="pkconv-key pkconv-readonly">derived only</span> ',
           "The decimal mark is a point (0.5); a comma separates thousands (1,110).</div>"),
    paste0('<div class="pkconv-groups">', paste(groups, collapse = ""), "</div>"),
    # The model in one fixed layout, whichever panel defined it (pk-convert.js)
    paste0('<section class="pkconv-summary"><h4>The model</h4>',
           '<div class="pkconv-summary-body"><p class="pkconv-empty">',
           "The model appears here when it is defined.</p></div></section>"),
    paste0('<section class="pkconv-effect"><h4>The effect site</h4>',
           '<p class="small">Enter k<sub>e0</sub>, its half-life or the time to peak effect after a bolus; ',
           "the others are computed from the model above.</p>",
           '<table class="pkconv-table"><tbody>', paste(effect, collapse = ""), "</tbody></table>",
           '<p class="pkconv-effect-result small"></p></section>'),
    paste0('<section class="pkconv-plot"><h4>The unit disposition function</h4>',
           '<div class="pkconv-plot-options">',
           '<fieldset><legend>Show</legend>',
           # The effect-site normalisation waits for a ke0 (pk-convert.js)
           sub('value="effect"', 'value="effect" disabled',
               radios("pkconv-norm", c("none", "plasma", "effect"),
                      c("Concentration", "Normalised to the peak plasma concentration",
                        "Normalised to the peak effect-site concentration"), "none"),
               fixed = TRUE),
           "</fieldset>",
           '<label class="pkconv-choice"><input type="checkbox" data-pk-plot="log" data-shiny-no-bind-input> ',
           "Logarithmic concentration axis</label>",
           '<label class="pkconv-until">Plot until ',
           pkConverterInput('data-pk-plot="until" placeholder="auto"', "Plot until"),
           ' <span data-pk-unit="time">min</span></label>',
           "</div>",
           '<div class="pkconv-svg"><p class="pkconv-empty">The graph appears when the model is defined.</p></div>',
           "</section>"),
    # Decrement times against the duration of a TCI infusion (pk-convert.js)
    paste0('<section class="pkconv-decrement"><h4>Decrement times</h4>',
           '<p class="small">The time for the concentration to fall by the percentage below once a ',
           "target-controlled infusion that held it constant is stopped, against how long that ",
           "infusion ran: the plasma after holding the plasma, the effect site after holding the ",
           "effect site.</p>",
           '<div class="pkconv-plot-options">',
           '<label class="pkconv-until">Decrease ',
           pkConverterInput('data-pk-dec="pct" value="50"', "Decrease, percent"),
           " %</label>",
           '<label class="pkconv-until">Infusions up to ',
           pkConverterInput('data-pk-dec="until" placeholder="600"', "Longest infusion"),
           ' <span data-pk-unit="time">min</span></label>',
           "</div>",
           '<div class="pkconv-dec-svg"><p class="pkconv-empty">The decrement times appear when the model is defined.</p></div>',
           '<p class="pkconv-dec-note small"></p>',
           "</section>"),
    "</div>"
  ), collapse = "\n")
}
