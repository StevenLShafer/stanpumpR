from pathlib import Path
import re

import cards                       # the cards are in the stanpumpR repository (2026-10-29), not in this archive
root = Path(__file__).resolve().parent.parent
digests = Path(cards.CARDS)
texts = root / "_build" / "text"

def canonical(s):
    return s.replace(",", "").replace("−", "-").replace("–", "-").replace("µ", "u").replace("μ", "u")

def as_number(s):
    try:
        return float(canonical(s))
    except ValueError:
        return None

def numbers(s):
    return [as_number(m.group()) for m in re.finditer(r"(?<![A-Za-z0-9])[-+]?\d[\d,]*(?:\.\d+)?(?![A-Za-z0-9])", s)]

rows = []
for f in sorted(digests.glob("PMID_*.md"), key=lambda x: int(x.stem[5:])):
    n = int(f.stem[5:])
    if not 15000000 <= n < 35000000:
        continue
    digest = f.read_text(encoding="utf-8", errors="replace")
    source = (texts / (f.stem + ".txt")).read_text(encoding="utf-8", errors="replace")
    source_num = [v for v in numbers(source) if v is not None]
    params = []
    for line in digest.splitlines():
        if "- {name:" not in line or "value:" not in line:
            continue
        m = re.search(r"name:\s*([^,]+),\s*value:\s*([^,}\]]+)", line)
        if not m:
            continue
        label, value = m.group(1), m.group(2).strip().strip('"\'')
        parsed = as_number(value)
        if parsed is None:
            continue
        hits = [v for v in source_num if abs(v - parsed) <= max(abs(parsed)*0.0001, 0.000001)]
        if not hits:
            params.append((label, value, line.strip()))
    rows.append((f.stem, len(params), params))

for stem,count,params in sorted(rows,key=lambda x:-x[1]):
    if count:
        print(stem, count, "|", "; ".join(f"{p[0]}={p[1]}" for p in params[:12]))
print("total",len(rows),"with unmatched params",sum(bool(x[1]) for x in rows),"unmatched values",sum(x[1] for x in rows))
