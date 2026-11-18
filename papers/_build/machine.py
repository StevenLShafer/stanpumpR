"""Machine-local working space, kept OUT of the archive folder (2026-10-30).

The archive lives in Google Drive: My Drive\\stanpumpRPapers, mirrored to each computer by Drive for
desktop and reached through the junction C:\\dev\\stanpumpRPapers, so everything inside it is uploaded
and synced to every computer.  Caches and scratch that are regenerable, belong to one machine and are
made of thousands of small files would only churn that sync, so they live here instead:

    LOCAL = %LOCALAPPDATA%\\stanpumpRPapers
        http_cache\\        ncbi.py and unpaywall_fetch.py responses
        master_snapshot\\   digest_run.py's `git archive` of origin/master, the code a digest is checked against
        digest_scratch\\    each unattended digest batch's scratchpad
"""
import os

LOCAL = os.path.join(os.environ.get("LOCALAPPDATA") or os.path.expanduser("~"), "stanpumpRPapers")
os.makedirs(LOCAL, exist_ok=True)
