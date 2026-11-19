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
%PY% fetch_pmc.py && %PY% file_supplements.py && %PY% file_downloads.py && %PY% ocr_scans.py && %PY% merge.py && %PY% build.py && %PY% digest_prep.py && %PY% arrivals.py
rem arrivals.py closes the loop: it reports which papers a model is WAITING for that have
rem now arrived (every registry's `to_obtain`, and papers\pending-work.csv in the repo).
rem Added 2026-11-17 -- until then the skill said update.bat ran it and nothing did.
rem It also ADDS any wanted paper the download queue does not yet carry (2026-11-18).
%PY% check_in_step.py
rem These scripts are version-controlled in the repository at papers\_build; the copies
rem here are the ones that run.  check_in_step.py says when the two have drifted apart, so
rem the record cannot quietly stop describing what actually runs.  It only reports and
rem always succeeds, so it is deliberately NOT part of the && chain above (2026-11-18).
popd
rem Then write up to 20 missing digests unattended, in a separate minimized window so update.bat returns at once
rem (digests.bat -> _build\digest_run.py; it exits immediately if a run is already going). Added 2026-10-28.
start "stanpumpR digests" /min cmd /c call "%~dp0digests.bat" 20
