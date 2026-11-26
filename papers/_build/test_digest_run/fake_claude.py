"""Stands in for `claude -p`: reads the prompt from stdin, writes a card, leaves a helper holding the
inherited stdout/stderr handles, and either exits (the 20261119-1542 case) or never returns (the hang case).

argv: <mode: exit|hang> <grandchild seconds> <cards dir> <stem...>
"""
import os, subprocess, sys, time

mode, secs, cards = sys.argv[1], sys.argv[2], sys.argv[3]
stems = sys.argv[4:]
prompt = sys.stdin.read()                      # exactly what the real CLI does with -p and no argument
print("fake claude: mode=%s, prompt %d chars, first line %r" % (mode, len(prompt), prompt.split("\n")[0][:40]))
sys.stdout.flush()
here = os.path.dirname(os.path.abspath(__file__))
g = subprocess.Popen([sys.executable, "-E", os.path.join(here, "grandchild.py"), secs], close_fds=False)
open(os.path.join(os.path.dirname(cards), "grandchild.pid"), "w").write(str(g.pid))
time.sleep(1.5)                                # the cards appear while the run is still going
for s in stems:
    with open(os.path.join(cards, s + ".md"), "w", encoding="utf-8") as f:
        f.write("card for %s\n" % s)
print("cards written: %s" % ", ".join(stems))
sys.stdout.flush()
sys.stderr.write("fake claude: a line on stderr\n")
if mode == "hang":
    time.sleep(100000)                         # never returns: only the deadline can end this
