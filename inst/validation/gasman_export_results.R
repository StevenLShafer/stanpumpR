# =============================================================================
# Export our Gas Man baseline results over the validation grid, as a workbook
# =============================================================================
#
# Produces gasman_baseline_results.xlsx: our answers for every case in the
# five-case grid, at one-second resolution, with provenance recorded so the run
# can be reproduced or challenged.
#
# The point is to give a fixed reference to diff against.  Two independent
# checks become possible:
#
#   1. Run gasman_validation_grid.R yourself.  Your numbers should reproduce the
#      Case_n sheets exactly, to the last digit, on any machine.  If they do not,
#      something differs in the R environment before Gas Man is even involved.
#   2. Run the same scenarios through the Gas Man API and compare against the
#      Case_n sheets, or use gasman_verify() on the live objects.
#
# Requires the openxlsx package, which is only needed for this export -- the
# baseline and grid scripts themselves are base R.
# =============================================================================

if (!requireNamespace("openxlsx", quietly = TRUE))
  stop("openxlsx is needed for the export. install.packages('openxlsx')")

if (!exists("gasman_simulate")) {
  GASMAN_QUIET <- TRUE
  source("gasman_baseline_standalone.R")
}

OUTFILE <- "gasman_baseline_results.xlsx"
DT_MS   <- 6000
MINUTES <- 30

grid <- data.frame(
  case    = 1:5,
  label   = c("sevoflurane, high flow",
              "sevoflurane + 70% N2O (isolates the second gas effect)",
              "isoflurane, low flow (soluble agent, rebreathing dominates)",
              "desflurane, 0.5 L/min (near-closed circuit)",
              "sevoflurane + N2O, low cardiac output, raised ventilation"),
  agent1  = c("Sevoflurane", "Sevoflurane", "Isoflurane", "Desflurane", "Sevoflurane"),
  del1    = c(2.0,            2.0,           1.2,          6.0,          2.0),
  agent2  = c("",             "Nitrous Oxide", "",         "",           "Nitrous Oxide"),
  del2    = c(0,              70,             0,           0,            70),
  fgf     = c(8.0,            8.0,           2.0,          0.5,          2.0),
  va      = c(4,              4,             4,            4,            6),
  co      = c(5.0,            5.0,           5.0,          5.0,          2.5),
  weight  = 70,
  minutes = MINUTES,
  circuit = "semi-closed",
  dt_ms   = DT_MS,
  stringsAsFactors = FALSE
)

cat("Running the grid...\n")
run <- gasman_grid(grid, outdir = "scenarios", every_seconds = 1)

# ---- provenance --------------------------------------------------------------
gitrev <- tryCatch(
  trimws(system("git rev-parse --short HEAD", intern = TRUE)[1]),
  error = function(e) NA_character_, warning = function(w) NA_character_)

about <- data.frame(item = character(), value = character(),
                    stringsAsFactors = FALSE)
add <- function(k, v) about[nrow(about) + 1, ] <<- c(k, as.character(v))

add("Generated",        format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
add("Machine",          Sys.info()[["nodename"]])
add("Platform",         R.version$platform)
add("R version",        R.version.string)
add("stanpumpR commit", if (is.na(gitrev)) "not a git checkout" else gitrev)
add("", "")
add("Source",           "gasman_baseline_standalone.R + gasman_validation_grid.R")
add("What it is",       paste("An R restatement of Gas Man's own integration",
                              "scheme, transcribed from GasDoc.cpp::Calc and",
                              "CalcUptake (GPL-3.0, github.com/rasman/gasmanonline)."))
add("", "")
add("dt_ms",            DT_MS)
add("",                 "Gas Man's breath period, GasGlobal.h: #define DT 6000")
add("Circuit",          "semi-closed for every case")
add("Weight",           "70 kg for every case (the reference weight)")
add("uptake_effect",    "TRUE  (m_bUptEnb; the concentration and second gas effect)")
add("recirculation",    "TRUE  (m_bRtnEnb)")
add("vaporizer_effect", "FALSE (m_bVapEnb; confirmed false in InitDocument)")
add("", "")
add("Tensions",         "percent of one atmosphere")
add("Uptake",           "cumulative litres of agent taken up by the tissues")
add("Delivered",        "cumulative litres of agent delivered")
add("VA",               paste("INSPIRED ventilation, matching Gas Man's GetVA:",
                              "the setting plus the volume drawn in to replace",
                              "uptake. Not the setting itself."))
add("ART",              "reported as ALV; not verified against GetART()")
add("", "")
add("Sheet Grid",       "the five case definitions")
add("Sheet Summary",    "key timepoints, for eyeballing")
add("Sheet Case_n",     "full series at one-second resolution")

# ---- a compact summary -------------------------------------------------------
tt <- c(1, 2, 5, 10, 15, 20, 30)
summary <- do.call(rbind, lapply(seq_len(nrow(grid)), function(r) {
  o <- run$ours[run$ours$Case == r, ]
  do.call(rbind, lapply(unique(o$Agent), function(a) {
    d <- o[o$Agent == a, ]
    at <- function(col, t) stats::approx(d$Time, d[[col]], t)$y
    data.frame(Case = r, Label = grid$label[r], Agent = a, Minute = tt,
               CKT = at("CKT", tt), ALV = at("ALV", tt), VRG = at("VRG", tt),
               MUS = at("MUS", tt), FAT = at("FAT", tt), VEN = at("VEN", tt),
               VA  = at("VA",  tt),
               Uptake = at("Uptake", tt), Delivered = at("Delivered", tt),
               stringsAsFactors = FALSE)
  }))
}))

sheets <- list(About = about, Grid = grid, Summary = summary)
for (r in seq_len(nrow(grid)))
  sheets[[paste0("Case_", r)]] <- run$ours[run$ours$Case == r, ]

openxlsx::write.xlsx(sheets, file = OUTFILE, overwrite = TRUE)

cat("Wrote", OUTFILE, "--", length(sheets), "sheets,",
    nrow(run$ours), "rows of results\n")
cat("Scenario files for Gas Man are in scenarios/\n")
