from pathlib import Path
from collections import defaultdict
import re

import cards                       # the cards are in the stanpumpR repository (2026-10-29), not in this archive
root = Path(__file__).resolve().parents[1]
digests = Path(cards.CARDS)
texts = root / '_build' / 'text'

def is_present(value, paper):
    v = value.strip().strip('"\'')
    if not re.fullmatch(r'-?\d+(?:\.\d+)?', v):
        return True
    f = float(v)
    if f == 0:
        return True
    digits = re.findall(r'\d+', paper.replace(',', ''))
    vnos = [v.replace('.', '')]
    if '.' in v:
        a, b = v.split('.')
        vnos.append(str(int(a)) + b.rstrip('0'))
    if any(n in digits for n in vnos):
        return True
    # number with decimal point normalized, accounting for arbitrary spacing
    simple = re.sub(r'\s+', '', paper).replace(',', '')
    return v in simple

out = []
for digest in sorted(digests.glob('PMID_*.md')):
    if int(digest.stem[5:]) >= 15_000_000:
        continue
    card = digest.read_text(encoding='utf-8')
    paper = (texts / f'{digest.stem}.txt').read_text(encoding='utf-8', errors='replace')
    if len(paper) < 1000:
        continue
    kind = re.search(r'^kind:\s*(.*)$', card, re.M).group(1)
    for line in card.splitlines():
        if not line.strip().startswith('- {name:'):
            continue
        n = re.search(r'name:\s*(.*?),\s*value:', line)
        v = re.search(r'\bvalue:\s*(.*?),\s*unit:', line)
        if not (n and v):
            continue
        if not is_present(v.group(1), paper):
            out.append((digest.stem, kind, n.group(1), v.group(1), line[:250]))

with (root / '_build' / 'audit_2026-10-27_early_triage.tsv').open('w', encoding='utf-8') as f:
    for row in out:
        f.write('\t'.join(row) + '\n')
print('unmatched parameter values:', len(out), 'across', len(set(r[0] for r in out)), 'digests')
for row in out[:100]:
    print(' | '.join(row[:4]))
