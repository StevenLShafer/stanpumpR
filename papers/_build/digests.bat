@echo off
rem Write missing or stale paper digests unattended: _build\digest_run.py runs Claude Code headless ("claude -p") on the
rem digests\STATUS.md queue, two papers at a time, most load-bearing first, then refreshes STATUS.md and REVIEW.md.
rem Usage: digests.bat [cap]   (default 20 papers). update.bat starts it in its own minimized window; a run that finds
rem another one still going exits at once. Reports: _build\digest_runs\<time>.md
set PYTHONIOENCODING=utf-8
set PY=py -3
if exist "C:\Python314\python.exe" set PY="C:\Python314\python.exe"
set CAP=%1
if "%CAP%"=="" set CAP=20
pushd "%~dp0_build"
%PY% digest_run.py --cap %CAP%
popd
