"""Citations in the registry tables' PROSE columns (notes, finding, verification, blocking_issue, evidence_read ...).

extract_registries.py reads only the source, source_url and to_obtain columns; the other columns cite papers too, in
running text: "Fuenmayor 1992 (Drugs 44 Suppl 1:1-11)", "Grasela & Sheiner 1984", "Hart 2026 read in full", a bare
PMID or DOI.  This finds those, and writes a record only for a paper the works list does not already hold: a PMID or
DOI not on it, or an author-year whose (surname, year) no work has.  Author-year mentions are noisy (journal names,
places), so every record it writes is reviewed by hand before it counts.
Usage: python extract_registry_prose.py <repo checkout> <output path>
"""
import csv, glob, json, os, re, sys, unicodedata
from collections import Counter

sys.stdout.reconfigure(encoding="utf-8")
REPO, OUT = sys.argv[1], sys.argv[2]
HERE = os.path.dirname(os.path.abspath(__file__))

READ = re.compile(r"^source$|source_url|to_obtain")  # what extract_registries.py already reads
SKIP_COLS = {"model_id", "drug", "Drug", "route", "analyte", "matrix", "units", "value"}
PMID = re.compile(r"(?:PMID[:\s]*|pubmed\.ncbi\.nlm\.nih\.gov/)(\d{5,9})", re.I)
DOI = re.compile(r"\b(10\.\d{4,9}/[^\s;,\"<>]+)")
PMC = re.compile(r"\b(PMC\d{5,9})\b")
PARTICLE = r"(?:van der|van den|van de|van|von|de la|de|da|dos|del|du|le|la|el|al|ter|ten)"
NAME = r"[A-Z][a-zà-ÿ'’]*(?:[A-Z][a-zà-ÿ'’]+)?(?<=[a-zà-ÿ])(?:-[A-Za-z][a-zà-ÿ'’]+)*"  # McQuay, MacGregor, O'Hanlon too
SURNAME = r"(?:(?:%s|%s)[ -])?%s(?: %s)?" % (PARTICLE, PARTICLE.title(), NAME, NAME)  # "de Bruijn", "Van Wart", "Otero Torres"
AY = re.compile(r"(?<![A-Za-z])(?P<au>%s)(?: (?:et al\.?|and %s|& %s))?,? \(?(?P<yr>(?:19|20)\d\d)[a-z]?\b(?![-/]\d)" % (SURNAME, NAME, NAME))
JOURNAL_AFTER = re.compile(r"^[a-z]?\)?[^.;]{0,60}?[(,]\s*(?P<jn>[A-Z][A-Za-z .&]+?)\s+(?P<vol>\d+)(?:\s*Suppl\.?\s*\w+)?"
                           r"(?:\(\d+\))?\s*:\s*(?P<pg>[A-Za-z]?\d+(?:-[A-Za-z]?\d+)?)")
# words that end up in front of a year without being anyone's name
STOP = set("""In The From Since Table Figure Until Before After Section Version Update Label Labels January February
March April May June July August September October November December Supplement Errata Erratum See Read Its This That
Both With Revised Above Below Between During Through Approved Withdrawn Marketed Discontinued Launched Published
Accessed Retrieved Issued Edition Guideline Guidelines Consensus Society Foundation Association Committee Group
Institute Agency Norway Sweden Denmark Germany France Japan China Korea Canada Australia Munich London Paris Basel
Boston Chicago Note Notes Also Only Until Prior Pharmacol Pharmacokinet Pharmacodyn Ther Toxicol Epilepsia Anesthesiology
Anaesthesia Anesth Analg Lancet Neurology Circulation Drugs Chest Pediatrics Blood Clin Br Eur Am Int Ann Arch Acta
Thromb Haemost Psychiatry Pharmacopsychiatry Seizure Diabetes Obstet Gynecol Med Res Sci Rev Cardiol Hypertension
EMA FDA EUCAST CLSI ILAE AGNP ISHLT AHA ACC CDC WHO NICE Carnexiv PIONEER PubMed GitHub McNemar MedDRA PharmGKB DailyMed""".split())
JOURNALISH = re.compile(r"(?:\b(?:J|Clin|Br|Eur|Am|Int|Ann|Arch|Acta|Pharmacol|Ther|Res|Med|Sci|Rev|Pharm|Drug|Exp)\s*)$")


def fold(s):
    return re.sub(r"[^a-z]", "", unicodedata.normalize("NFKD", s or "").encode("ascii", "ignore").decode().lower())


def variants(surname):
    """'Otero Torres' -> {oterotorres, torres, otero}; 'de Bruijn' -> {debruijn, bruijn}."""
    surname = re.sub(r"['’]s$", "", surname or "")  # "Cook's 1993 smoking study"
    toks = [t for t in re.split(r"[ \-]", surname) if t]
    core = [t for t in toks if not re.fullmatch(PARTICLE, t, re.I)]
    out = {fold(surname)}
    if core:
        out |= {fold(core[-1]), fold(core[0]), fold("".join(core))}
    return {v for v in out if len(v) > 1}


def work_surname(name):
    parts = (name or "").split()
    while len(parts) > 1 and re.fullmatch(r"[A-Z]{1,4}|Jr|Sr|\d+(?:st|nd|rd|th)", parts[-1]):
        parts = parts[:-1]
    return " ".join(parts)


works = json.load(open(os.path.join(HERE, "works.json"), encoding="utf-8"))
have_id, have_ay = set(), set()
for w in works:
    pm, cr = w.get("pm") or {}, w.get("cr") or {}
    if w["key"].startswith("pmid:"):
        have_id.add(w["key"][5:])
    for d in (w.get("doi"), pm.get("doi"), cr.get("doi")):
        if d:
            have_id.add(d.lower())
    if pm.get("pmcid"):
        have_id.add(pm["pmcid"])
    yr = w.get("year") or pm.get("year")
    try:
        yr = int(str(yr)[:4])
    except Exception:
        continue
    for au in [w.get("first_author"), pm.get("first_author")] + list(pm.get("authors") or [])[:1]:
        for v in variants(work_surname(au)):
            have_ay.add((v, yr))
            have_ay.add((v, yr - 1))  # online-first against print year
            have_ay.add((v, yr + 1))


def sentence_around(text, i, j):
    k = text.rfind(". ", 0, i)
    a = max(k + 2 if k >= 0 else 0, i - 220)
    b = text.find(". ", j)
    b = len(text) if b < 0 else b + 1
    return text[a:min(b, j + 220)].strip()


recs, stats = {}, Counter()


def add(key, rec):
    if key in recs:
        recs[key]["locations"] = sorted(set(recs[key]["locations"] + rec["locations"]))[:12]
        recs[key]["drugs"] = sorted(set(recs[key]["drugs"] + rec["drugs"]))
        for k in ("pmid", "doi", "journal", "volume", "pages"):
            recs[key][k] = recs[key][k] or rec[k]
    else:
        recs[key] = rec


def record(**kw):
    rec = {"first_author": None, "authors": None, "year": None, "title": None, "journal": None, "volume": None,
           "issue": None, "pages": None, "pmid": None, "doi": None, "pmcid": None, "url": None,
           "kind": "journal-article", "citation_text": None, "locations": [], "drugs": [], "used_for": None,
           "search_hint": None, "notes": "extracted by extract_registry_prose.py (prose column)"}
    rec.update(kw)
    return rec


for f in sorted(glob.glob(os.path.join(REPO, "inst", "extdata", "*.csv"))):
    base = os.path.basename(f)
    rel = "inst/extdata/" + base
    with open(f, encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    if not rows:
        continue
    cols = [c for c in rows[0] if c and not READ.search(c) and c not in SKIP_COLS]
    drugcol = next((c for c in ("drug", "ingredient", "molecule", "Drug", "victim") if c in rows[0]), None)
    for i, r in enumerate(rows, start=2):
        drug = (r.get(drugcol) or "").strip() if drugcol else ""
        for c in cols:
            v = (r.get(c) or "").strip()
            if not v or v.upper() == "NA":
                continue
            loc = "%s:%d" % (rel, i)
            used = "cited in the %s column of %s" % (c, base)
            taken = set()
            for m in AY.finditer(v):
                au, yr = m.group("au"), int(m.group("yr"))
                if len(au.split()) == 2 and au.split()[0] in STOP:  # "Table Hart 2026" -> Hart
                    au = au.split()[1]
                if au in STOP or au.split()[0] in STOP or JOURNALISH.search(v[:m.start()]) or yr > 2027:
                    stats["stop"] += 1
                    continue
                tail = v[m.end():m.end() + 160]
                pm = PMID.search(tail[:120])
                nxt = AY.search(tail)
                if pm and nxt and nxt.start() < pm.start():
                    pm = None
                jm = JOURNAL_AFTER.match(tail)
                if pm:
                    taken.add(pm.group(1))
                    if pm.group(1) in have_id:
                        stats["pmid on list"] += 1
                        continue
                elif any((x, yr) in have_ay for x in variants(au)):
                    stats["author-year on list"] += 1
                    continue
                rec = record(first_author=au, year=yr, pmid=pm.group(1) if pm else None,
                             journal=jm.group("jn").strip() if jm else None, volume=jm.group("vol") if jm else None,
                             pages=jm.group("pg") if jm else None, citation_text=sentence_around(v, m.start(), m.end())[:400],
                             locations=[loc], drugs=[drug] if drug else [], used_for=used)
                add(pm.group(1) if pm else "%s:%d:%s" % (fold(au), yr, (jm.group("vol") if jm else "")), rec)
                stats["new author-year"] += 1
            for p in PMID.findall(v):
                if p in taken or p in have_id:
                    continue
                add(p, record(pmid=p, citation_text=v[:400], locations=[loc], drugs=[drug] if drug else [], used_for=used))
                stats["new PMID"] += 1
            for d in DOI.findall(v):
                d = d.rstrip(".,;:)")
                if d.lower() in have_id:
                    continue
                add(d.lower(), record(doi=d, citation_text=v[:400], locations=[loc], drugs=[drug] if drug else [], used_for=used))
                stats["new DOI"] += 1
            for p in PMC.findall(v):
                if p not in have_id:
                    add(p, record(pmcid=p, citation_text=v[:400], locations=[loc], drugs=[drug] if drug else [], used_for=used))
                    stats["new PMCID"] += 1

with open(OUT, "w", encoding="utf-8") as fh:
    for rec in recs.values():
        fh.write(json.dumps(rec, ensure_ascii=False) + "\n")
print(len(recs), "records ->", OUT)
print(dict(stats))
