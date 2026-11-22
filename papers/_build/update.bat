@echo off
rem Download the free PMC copy of any paper the reference list or the download queue wants (_build\fetch_pmc.py),
rem file journals' supplementary material (Word, CSV, .doc, zip, PDF ...) beside its article (_build\file_supplements.py; unplaced ones are listed in digests\STATUS.md),
rem move stanpumpR reference PDFs from Downloads (and subfolders) into this folder - originals go to the Recycle Bin -
rem OCR any scanned PDF in place (_build\ocr_scans.py: Tesseract + Ghostscript via ocrmypdf), then rebuild stanpumpR-references.xlsx and stanpumpr-queue.csv, and list the PDFs still needing a digest (digests\STATUS.md).
rem Uses C:\Python314 (the install with openpyxl), not whatever "python" is first on PATH.
set PYTHONIOENCODING=utf-8
set PY=py -3
if exist "C:\Python314\python.exe" set PY="C:\Python314\python.exe"
pushd "%~dp0_build"
%PY% -c "import openpyxl" 2>nul
if errorlevel 1 (
  echo openpyxl is not installed for %PY%. Install it with:  %PY% -m pip install openpyxl
  popd
  exit /b 1
)
rem Fetching runs FIRST so a paper off PMC is filed, OCR'd, counted and queued for a card in
rem this same run -- but deliberately NOT in the && chain: a PMC outage must not stop the
rem filing of papers already in Downloads.  Same reason check_in_step.py sits outside it.
%PY% fetch_pmc.py
%PY% file_supplements.py && %PY% file_downloads.py && %PY% ocr_scans.py && %PY% merge.py && %PY% build.py && %PY% digest_prep.py && %PY% arrivals.py
rem arrivals.py closes the loop: it reports which papers a model is WAITING for that have
rem now arrived (every registry's `to_obtain`, and papers\pending-work.csv in the repo).
rem Added 2026-11-17 -- until then the skill said update.bat ran it and nothing did.
rem It also ADDS any wanted paper the download queue does not yet carry (2026-11-18).
%PY% check_in_step.py
rem These scripts are version-controlled in the repository at papers\_build; the copies
rem here are the ones that run.  check_in_step.py says when the two have drifted apart, so
rem the record cannot quietly stop describing what actually runs.  It only reports and
rem always succeeds, so it is deliberately NOT part of the && chain above (2026-11-18).
%PY% check_index.py
rem check_index.py is the LAST word on the state the steps above leave behind: every PDF
rem in the archive must be in digest_manifest.json, have an extracted text file, and have
rem readable text.  On 2026-11-21 none of those held and nothing said so: 85 PDFs had been
rem absent from the manifest since October, so they appeared in no report at all, and 36 had
rem no text layer because ocr_scans.py had never been run over them.  Both were found by
rem looking, not by being told.  It only reports, so it is outside the && chain -- and it
rem sits after check_in_step.py deliberately: when an upstream step DID fail, the accounting
rem is exactly when you most want to hear about it.
popd
rem Then write up to 20 missing digests unattended, in a separate minimized window so update.bat returns at once
rem (digests.bat -> _build\digest_run.py; it exits immediately if a run is already going). Added 2026-10-28.
start "stanpumpR digests" /min cmd /c call "%~dp0digests.bat" 20
