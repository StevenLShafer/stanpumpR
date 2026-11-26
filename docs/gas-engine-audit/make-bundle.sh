#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# Build the attachment for the external audit of the inhaled-gas engine:
# round 1 (docs/gas-engine-audit/PROMPT.md, stanpumpR c90c33e, 2026-11-23) or
# round 2 (PROMPT-2.md, stanpumpR 8c1ea1b, 2026-11-26), chosen by ROUND.
# Round 2 adds the files the changes since round 1 touched and
# CHANGES-SINCE-ROUND-1.diff, the diff of the gas engine's code and pages from
# c90c33e to the round-2 commit.
#
# Writes, from PINNED commits so that every line number the auditors cite can
# be checked against the same text:
#   stanpumpR/...                the gas engine and everything around it, as
#                                committed at STANPUMPR_COMMIT
#   gasman/...                   GasMan's API source at GASMAN_COMMIT
#                                (github.com/rasman/gasmanonline, GPL-3.0)
#   CORE-NUMBERED.txt            the files an auditor must read, concatenated,
#                                each line numbered, the path repeated every 100
#                                lines (about 1 MB: fits a long-context model)
#   SUPPLEMENT-NUMBERED.txt      the rest of the stanpumpR files the same way
#                                (AGENTS.md, app_server.R, the IV physiology,
#                                the drug table, results CSVs, ...)
#   INDEX.md                     every file with its permalink and line count,
#                                and every function and constant with its line
#   MANIFEST.sha256              checksums
# then zips the folder.  The Eger chapter is not in the repository (it is a
# manuscript on the maintainer's Drive); pass its PDF as the second argument
# to include it.
#
# Usage:
#   docs/gas-engine-audit/make-bundle.sh OUTDIR [EGER_CHAPTER.pdf]
# Environment:
#   ROUND             1 or 2 (default 2)
#   STANPUMPR_COMMIT  default the commit that round's prompt names
#   GASMAN_COMMIT     default the commit both prompts name
#   GASMAN_REPO       a local clone of rasman/gasmanonline to read from instead
#                     of fetching (it must contain GASMAN_COMMIT)
# -----------------------------------------------------------------------------
set -euo pipefail

ROUND="${ROUND:-2}"
ROUND1_COMMIT="c90c33e50346380960d5d86c5a991ca1bf35a1ff"
case "$ROUND" in
  1) DEFAULT_COMMIT="$ROUND1_COMMIT" ;;
  2) DEFAULT_COMMIT="8c1ea1bd3610181226f087c2598ae48f5a17b0aa" ;;
  *) echo "ROUND must be 1 or 2" >&2; exit 1 ;;
esac
STANPUMPR_COMMIT="${STANPUMPR_COMMIT:-$DEFAULT_COMMIT}"
GASMAN_COMMIT="${GASMAN_COMMIT:-d3a2dd36f6be4582cdb7c26ccb860164d9f9983a}"
GASMAN_URL="https://github.com/rasman/gasmanonline"
STANPUMPR_URL="https://github.com/StevenLShafer/stanpumpR"

OUT="${1:?usage: make-bundle.sh OUTDIR [EGER_CHAPTER.pdf]}"
CHAPTER="${2:-}"
REPO="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
SHORT="${STANPUMPR_COMMIT:0:7}"
if [ "$ROUND" = 1 ]; then
  NAME="stanpumpR-gas-audit-${SHORT}"
else
  NAME="stanpumpR-gas-audit-r${ROUND}-${SHORT}"
fi
DEST="$OUT/$NAME"

if [ -e "$DEST" ]; then
  echo "$DEST already exists; remove it or choose another OUTDIR" >&2
  exit 1
fi
mkdir -p "$DEST/stanpumpR" "$DEST/gasman"

git -C "$REPO" cat-file -e "${STANPUMPR_COMMIT}^{commit}" 2>/dev/null || {
  echo "stanpumpR commit $STANPUMPR_COMMIT is not in $REPO; fetch it first" >&2
  exit 1
}

# --- stanpumpR files ---------------------------------------------------------
# The engine, the physiology that reaches it, the MAC interactions, the code
# that carries the Patient Profile to it, its documentation, its validation
# record and its tests.
STANPUMPR_FILES=(
  AGENTS.md
  DESCRIPTION
  R/advanceClosedFormGas.R
  R/advanceGasManBaseline.R
  R/gasProperties.R
  R/gasDrugEntries.R
  R/gasRecovery.R
  R/altitude.R
  R/arterialOxygen.R
  R/endobronchial.R
  R/physiology.R
  R/physiologyEvents.R
  R/opioidMacInteraction.R
  R/modelInteraction.R
  R/simulateDrugsWithCovariates.R
  R/pkSizeFactors.R
  R/lbmJames.R
  R/viewerRun.R
  R/constants.R
  R/app_server.R
  R/help-drugs.R
  inst/extdata/drugDefaults_global.csv
  inst/extdata/pregnancyRegistry.csv
  inst/app-help/inhaled-agents.md
  inst/app-help/patient-profile.md
  inst/app-help/validation.md
  inst/app-help/models/overview.md
  inst/app-help/models/gas-engine.md
  inst/app-help/models/gas-differences.md
  inst/app-help/models/opioid-mac.md
  inst/app-help/models/interaction.md
  inst/app-help/models/meac.md
  inst/app-help/models/physiology.md
  inst/app-help/models/physiology-and-pharmacokinetics.md
  inst/app-help/models/recovery.md
  inst/app-help/drugs/sevoflurane.md
  inst/app-help/drugs/isoflurane.md
  inst/app-help/drugs/desflurane.md
  inst/app-help/drugs/nitrousOxide.md
  inst/app-help/drugs/oxygen.md
  inst/app-help/drugs/air.md
  inst/app-help/drugs/ventilation.md
  docs/weight-adjustment.md
  tests/testthat/helpers.R
  tests/testthat/test-gas-engine.R
  tests/testthat/test-gas-convergence.R
  tests/testthat/test-gas-entries.R
  tests/testthat/test-gas-recovery.R
  tests/testthat/test-gas-routing.R
  tests/testthat/test-gas-scenarios.R
  tests/testthat/test-gasman-baseline.R
  tests/testthat/test-altitude.R
  tests/testthat/test-arterial-oxygen.R
  tests/testthat/test-endobronchial.R
  tests/testthat/test-physiology.R
  tests/testthat/test-physiology-events.R
  tests/testthat/test-opioid-mac.R
  tests/testthat/test-ui-audit-carbon3-gas.R
  tests/testthat/test-ui-audit-carbon4-gas.R
  tests/testthat/test-coderabbit-730-gas.R
)
# Round 2: what the changes since round 1 added or touched, the record of
# round 1, and the gas scenarios' pages (James Philip will read them)
if [ "$ROUND" -ge 2 ]; then
  STANPUMPR_FILES+=(
    R/physiologyRibbon.R
    R/derivedSeries.R
    R/simulationPlot.R
    R/help-scenarios.R
    inst/app-help/models/fat-free-mass.md
    inst/app-help/scenarios/emergence-desflurane.md
    inst/app-help/scenarios/emergence-sevoflurane.md
    inst/app-help/scenarios/mac-and-age.md
    inst/app-help/scenarios/opioid-mac-interaction.md
    inst/app-help/scenarios/second-gas-effect.md
    inst/app-help/scenarios/sevoflurane-washin.md
    inst/app-help/scenarios/visualizer-nitrous-oxide.md
    inst/app-help/scenarios/visualizer-sevoflurane-emergence.md
    inst/app-help/scenarios/visualizer-sevoflurane-overpressure.md
    inst/app-help/scenarios/visualizer-sevoflurane-washin.md
    docs/gas-engine-audit/chatgpt-2026-11-23-assessment.md
    docs/gas-engine-audit/gemini-2026-11-23-assessment.md
    tests/testthat/test-brain-mac.R
    tests/testthat/test-gas-body-composition.R
    tests/testthat/test-physiology-ribbon.R
    tests/testthat/test-gas-audit-chatgpt.R
    tests/testthat/test-gas-audit-gemini.R
    tests/testthat/test-coderabbit-858.R
  )
fi
# Whole directories
mapfile -t VALIDATION < <(git -C "$REPO" ls-tree -r --name-only "$STANPUMPR_COMMIT" -- inst/validation)
STANPUMPR_FILES+=("${VALIDATION[@]}")

for f in "${STANPUMPR_FILES[@]}"; do
  if ! git -C "$REPO" cat-file -e "$STANPUMPR_COMMIT:$f" 2>/dev/null; then
    echo "missing at $STANPUMPR_COMMIT: $f" >&2
    exit 1
  fi
  mkdir -p "$DEST/stanpumpR/$(dirname "$f")"
  git -C "$REPO" show "$STANPUMPR_COMMIT:$f" > "$DEST/stanpumpR/$f"
done

# --- GasMan files ------------------------------------------------------------
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
if [ -n "${GASMAN_REPO:-}" ]; then
  GM="$GASMAN_REPO"
else
  GM="$TMP/gasmanonline"
  git init -q "$GM"
  git -C "$GM" remote add origin "$GASMAN_URL"
  GIT_LFS_SKIP_SMUDGE=1 git -C "$GM" fetch -q --depth 1 origin "$GASMAN_COMMIT"
fi
git -C "$GM" cat-file -e "${GASMAN_COMMIT}^{commit}" || {
  echo "GasMan commit $GASMAN_COMMIT not found in $GM" >&2
  exit 1
}
mapfile -t GASMAN_FILES < <(
  git -C "$GM" ls-tree -r --name-only "$GASMAN_COMMIT" -- LICENSE README.md gasman_api |
    grep -v '/importLibraries/' |
    grep -E '\.(cpp|h|ini|md|json|R|py|m|txt)$|/LICENSE$|^LICENSE$|CMakeLists\.txt$|Makefile$'
)
for f in "${GASMAN_FILES[@]}"; do
  mkdir -p "$DEST/gasman/$(dirname "$f")"
  git -C "$GM" show "$GASMAN_COMMIT:$f" > "$DEST/gasman/$f"
done

# --- The chapter -------------------------------------------------------------
if [ -n "$CHAPTER" ]; then
  cp "$CHAPTER" "$DEST/$(basename "$CHAPTER")"
fi

# --- Round 2: what changed since round 1 -----------------------------------
# The diff, from the round-1 commit to this one, of the bundled files that
# existed at both (a file added since is in the bundle whole), leaving out the
# CSVs and the files whose changes are mostly elsewhere in the program
# (AGENTS.md, docs/, constants.R, help-drugs.R), so the auditor can see what
# the fixes and the features since round 1 changed.
if [ "$ROUND" -ge 2 ]; then
  git -C "$REPO" cat-file -e "${ROUND1_COMMIT}^{commit}" 2>/dev/null || {
    echo "round-1 commit $ROUND1_COMMIT is not in $REPO; fetch it first" >&2
    exit 1
  }
  mapfile -t DIFFED < <(printf '%s\n' "${STANPUMPR_FILES[@]}" |
    grep -v -E '\.csv$|^AGENTS\.md$|^docs/|^R/constants\.R$|^R/help-drugs\.R$')
  {
    echo "# Changes to the gas engine's files from round 1 to round 2"
    echo "# git diff --diff-filter=M ${ROUND1_COMMIT:0:7} ${STANPUMPR_COMMIT:0:7}"
    echo "# Files added since round 1 are in the bundle whole and not repeated here."
    echo "# Paths are repository paths; in the bundle they are under stanpumpR/."
    echo "# The commits that touched these files:"
    git -C "$REPO" log --format='#   %h %s' "$ROUND1_COMMIT..$STANPUMPR_COMMIT" -- "${DIFFED[@]}"
    echo
    git -C "$REPO" diff --diff-filter=M "$ROUND1_COMMIT" "$STANPUMPR_COMMIT" -- "${DIFFED[@]}"
  } > "$DEST/CHANGES-SINCE-ROUND-1.diff"
fi

# --- INDEX.md ----------------------------------------------------------------
permalink() {  # bundle path -> permalink at the pinned commit
  case "$1" in
    stanpumpR/*) echo "$STANPUMPR_URL/blob/$STANPUMPR_COMMIT/${1#stanpumpR/}" ;;
    gasman/*)    echo "$GASMAN_URL/blob/$GASMAN_COMMIT/${1#gasman/}" ;;
  esac
}
{
  echo "# Index of the gas-engine audit bundle (round $ROUND)"
  echo
  echo "- stanpumpR at \`$STANPUMPR_COMMIT\` ($STANPUMPR_URL/tree/$STANPUMPR_COMMIT)"
  echo "- GasMan at \`$GASMAN_COMMIT\` ($GASMAN_URL/tree/$GASMAN_COMMIT)"
  echo
  echo "Cite a line as \`stanpumpR/R/file.R:123\` or \`gasman/gasman_api/gasmanAPI/GasDoc.cpp:123\`."
  echo
  echo "## Files"
  echo
  echo "| File | Lines | Permalink |"
  echo "|---|---|---|"
  (cd "$DEST" && find stanpumpR gasman -type f | LC_ALL=C sort) | while read -r p; do
    echo "| \`$p\` | $(wc -l < "$DEST/$p") | $(permalink "$p") |"
  done
  echo
  echo "## stanpumpR functions and constants (file:line: definition)"
  echo
  echo '```'
  (cd "$DEST" && grep -n -E '^[A-Za-z.][A-Za-z0-9._]* *(<-|=) *function|^[A-Z][A-Z0-9_]{2,} *<-' \
     $(find stanpumpR/R -name '*.R' | LC_ALL=C sort) | cut -c1-160)
  echo '```'
  echo
  echo "## GasMan functions (file:line: definition)"
  echo
  echo '```'
  (cd "$DEST" && grep -n -E '^[A-Za-z].*[A-Za-z_]+::~?[A-Za-z_]+\(' \
     $(find gasman -name '*.cpp' | LC_ALL=C sort) | cut -c1-160)
  echo '```'
} > "$DEST/INDEX.md"

# --- Numbered text ----------------------------------------------------------
# Files left out of CORE-NUMBERED.txt: large, or read only for one check.
# They are in the zip, in INDEX.md and, for stanpumpR, in SUPPLEMENT-NUMBERED.txt.
SUPPLEMENT_RE='^stanpumpR/(AGENTS\.md|R/app_server\.R|R/viewerRun\.R|R/constants\.R|R/help-drugs\.R|R/simulationPlot\.R|R/help-scenarios\.R|docs/gas-engine-audit/.*|inst/extdata/drugDefaults_global\.csv|inst/extdata/pregnancyRegistry\.csv|inst/validation/.*\.csv|inst/app-help/models/physiology-and-pharmacokinetics\.md|docs/weight-adjustment\.md|tests/testthat/test-physiology\.R|tests/testthat/test-physiology-events\.R|tests/testthat/helpers\.R)$'
# GasMan files never numbered: the licence text, build tooling, examples
UNNUMBERED_RE='^gasman/(LICENSE|gasman_api/(examples|run|convert|GasManWeb)/.*|.*CMake.*|.*Makefile)$'

numbered() {  # $1 output file, $2 title, then the bundle paths
  local out="$1" title="$2"; shift 2
  {
    echo "stanpumpR gas-engine audit bundle: $title. Every line numbered."
    echo "stanpumpR commit $STANPUMPR_COMMIT; GasMan commit $GASMAN_COMMIT."
    echo "Cite lines as <path>:<line>, e.g. stanpumpR/R/advanceClosedFormGas.R:572."
    for p in "$@"; do
      echo
      echo "===== FILE: $p ($(wc -l < "$DEST/$p") lines) ====="
      awk -v p="$p" '
        NR > 1 && (NR - 1) % 100 == 0 { print "----- " p " continues -----" }
        { printf "%d: %s\n", NR, $0 }' "$DEST/$p"
    done
  } > "$DEST/$out"
}
mapfile -t ALLPATHS < <(cd "$DEST" && find stanpumpR gasman -type f | LC_ALL=C sort)
CORE=(); SUPP=()
for p in "${ALLPATHS[@]}"; do
  if [[ "$p" =~ $UNNUMBERED_RE ]]; then continue
  elif [[ "$p" =~ $SUPPLEMENT_RE ]]; then SUPP+=("$p")
  else CORE+=("$p"); fi
done
numbered CORE-NUMBERED.txt "core files (engine, GasMan source, documentation, gas tests)" "${CORE[@]}"
numbered SUPPLEMENT-NUMBERED.txt "supplementary stanpumpR files" "${SUPP[@]}"

# --- Manifest and zip --------------------------------------------------------
(cd "$DEST" && find . -type f ! -name MANIFEST.sha256 | LC_ALL=C sort | xargs sha256sum > MANIFEST.sha256)
(cd "$OUT" && zip -qr "$NAME.zip" "$NAME")
echo "$OUT/$NAME.zip"
echo "$(find "$DEST" -type f | wc -l) files; CORE-NUMBERED.txt $(wc -c < "$DEST/CORE-NUMBERED.txt") bytes, SUPPLEMENT-NUMBERED.txt $(wc -c < "$DEST/SUPPLEMENT-NUMBERED.txt") bytes"
