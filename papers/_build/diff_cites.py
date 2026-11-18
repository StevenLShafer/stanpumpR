"""Citations on lines added to stanpumpR between two commits (code, help, tests, docs), not yet on the works list.

Usage: python diff_cites.py <repo> <base> <head> <out.jsonl> <round letter>
Written for round U (2026-10-28); kept in _build since round V (2026-10-30), which it found 16 new works for.
A new round: base = the last round's head (README.txt says which), head = origin/master; then the two registry
extractors, review every record into overrides.json, resolve.py, merge.py, build.py, and update.bat.
Reads `git diff -U0 base head`, scans each added line together with the lines around it (citations in R are often a
paste0() spread over several strings), and writes one record per citation whose PMID/DOI/PMCID, or (surname, year +-1),
no work on the list has.  Every record is reviewed by hand before it counts.
"""
import json, os, re, subprocess, sys, unicodedata
from collections import Counter

sys.stdout.reconfigure(encoding="utf-8")
REPO, BASE, HEAD, OUT, ROUND = sys.argv[1:6]
B = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, B)
PATHS = ["R", "inst/help", "inst/apidocs", "inst/www", "tests", "docs", "data-raw", "vignettes", "README.md", "AGENTS.md", "NEWS.md"]

PMID = re.compile(r"(?:PMID[:\s]*|pubmed\.ncbi\.nlm\.nih\.gov/|PMID_)(\d{5,9})", re.I)
DOI = re.compile(r"\b(10\.\d{4,9}/[^\s;,\"'<>\]\)]+(?:\([^\s)]*\)[^\s;,\"'<>\]\)]*)?)")
PMC = re.compile(r"\b(PMC\d{5,9})\b")
URL = re.compile(r"https?://[^\s\"'<>)\]]+")
# "Journal 1985;74:703-710" / "Journal 2018;84(9):2075" / "Journal. 2018 Sep;84(9):2075"
JREF = re.compile(r"(?P<jn>[A-Z][A-Za-z.&\- ]{2,60}?)\.?\s+(?P<yr>(?:19|20)\d\d)(?:\s+[A-Z][a-z]{2}(?:\s+\d{1,2})?)?\s*;\s*(?P<vol>\d+)(?:\s*\([^)]*\))?\s*:\s*(?P<pg>[A-Za-z]?\d+)")
VAN_AU = re.compile(r"(?:^|[\"(;]\s*|\.\s+)(?P<au>(?:(?:van|von|de|da|dos|del|le|la|el|ter)\s)?[A-Z][a-zà-ÿ'’\-]+(?: [A-Z][a-zà-ÿ'’\-]+)?) [A-Z]{1,3}[,.]")
PARTICLE = r"(?:van der|van den|van de|van|von|de la|de|da|dos|del|du|le|la|el|al|ter|ten)"
NAME = r"[A-Z][a-zà-ÿ'’]*(?:[A-Z][a-zà-ÿ'’]+)?(?<=[a-zà-ÿ])(?:-[A-Za-z][a-zà-ÿ'’]+)*"  # McQuay, MacGregor, O'Hanlon too
SURNAME = r"(?:(?:%s|%s)[ -])?%s(?: %s)?" % (PARTICLE, PARTICLE.title(), NAME, NAME)
AY = re.compile(r"(?<![A-Za-z])(?P<au>%s)(?: (?:et al\.?|and %s|& %s))?,? \(?(?P<yr>(?:19|20)\d\d)[a-z]?\b(?![-/]\d)" % (SURNAME, NAME, NAME))
STOP = set("""In The From Since Table Figure Until Before After Section Version Update Label Labels January February March April
May June July August September October November December Supplement Errata Erratum See Read Its This That Both With Revised
Above Below Between During Through Approved Withdrawn Marketed Discontinued Launched Published Accessed Retrieved Issued
Edition Guideline Guidelines Consensus Society Foundation Association Committee Group Institute Agency Norway Sweden Denmark
Germany France Japan China Korea Canada Australia Munich London Paris Basel Boston Chicago Note Notes Also Only Prior
Pharmacol Pharmacokinet Pharmacodyn Ther Toxicol Epilepsia Anesthesiology Anaesthesia Anesth Analg Lancet Neurology
Circulation Drugs Chest Pediatrics Blood Clin Br Eur Am Int Ann Arch Acta Thromb Haemost Psychiatry Pharmacopsychiatry
Seizure Diabetes Obstet Gynecol Med Res Sci Rev Cardiol Hypertension EMA FDA EUCAST CLSI ILAE AGNP ISHLT AHA ACC CDC WHO NICE
Shafer Copyright Date Updated Added Fixed Merged Rev Revised Release Released Tests Test Since Year Years Q Q1 Q2 Q3 Q4
TRUE FALSE NULL NA Stanford Windows R CRAN GitHub PubMed McNemar MedDRA PharmGKB DailyMed""".split())
JOURNALISH = re.compile(r"(?:\b(?:J|Clin|Br|Eur|Am|Int|Ann|Arch|Acta|Pharmacol|Ther|Res|Med|Sci|Rev|Pharm|Drug|Exp)\s*)$")
SKIP_URL = re.compile(r"doi\.org|pubmed\.ncbi|ncbi\.nlm\.nih\.gov/pmc|github\.com|stanpumpr|shinyapps|localhost|127\.0\.0|cran\.r-project|"
                      r"posit\.co|rstudio|example\.(com|org)|w3\.org|schema|fonts\.|cdn|jsdelivr|unpkg|anthropic|claude\.", re.I)


def fold(s):
    return re.sub(r"[^a-z]", "", unicodedata.normalize("NFKD", s or "").encode("ascii", "ignore").decode().lower())


def variants(surname):
    surname = re.sub(r"['’]s$", "", surname or "")
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


works = json.load(open(os.path.join(B, "works.json"), encoding="utf-8"))
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
    for u in (w.get("urls") or []) + ([w.get("url")] if w.get("url") else []):
        have_id.add(u.rstrip("/.").lower())
    yr = w.get("year") or pm.get("year")
    try:
        yr = int(str(yr)[:4])
    except Exception:
        continue
    for au in [w.get("first_author"), pm.get("first_author")] + list(pm.get("authors") or [])[:1]:
        for v in variants(work_surname(au)):
            for d in (-1, 0, 1):
                have_ay.add((v, yr + d))
# every URL any earlier extraction recorded counts as known too
for f in os.listdir(os.path.join(B, "extract")):
    for l in open(os.path.join(B, "extract", f), encoding="utf-8"):
        if l.strip():
            u = json.loads(l).get("url")
            if u:
                have_id.add(u.rstrip("/.").lower())

diff = subprocess.run(["git", "-C", REPO, "diff", "-U0", "--no-color", BASE, HEAD, "--"] + PATHS, capture_output=True).stdout.decode("utf-8", "replace")
added = {}  # file -> {lineno: text}
cur, ln = None, 0
for line in diff.splitlines():
    if line.startswith("+++ "):
        cur = line[6:] if line.startswith("+++ b/") else None
        continue
    m = re.match(r"@@ -\S+ \+(\d+)(?:,(\d+))? @@", line)
    if m:
        ln = int(m.group(1))
        continue
    if cur and line.startswith("+") and not line.startswith("+++"):
        added.setdefault(cur, {})[ln] = line[1:]
        ln += 1
print("files with added lines:", len(added), "| added lines:", sum(len(v) for v in added.values()))

recs, stats = {}, Counter()


def drug_of(path):
    m = re.search(r"drugs?_([A-Za-z0-9]+)\.(?:R|md)$|test-drugs-([A-Za-z0-9]+)\.R$|help/drugs/([A-Za-z0-9]+)\.md$", path)
    return [g for g in (m.groups() if m else []) if g][:1]


def add(key, rec):
    if key in recs:
        r = recs[key]
        r["locations"] = sorted(set(r["locations"] + rec["locations"]))[:12]
        r["drugs"] = sorted(set(r["drugs"] + rec["drugs"]))
        for k in ("pmid", "doi", "journal", "volume", "pages", "first_author", "year"):
            r[k] = r[k] or rec[k]
    else:
        recs[key] = rec


def record(**kw):
    rec = {"first_author": None, "authors": None, "year": None, "title": None, "journal": None, "volume": None, "issue": None,
           "pages": None, "pmid": None, "doi": None, "pmcid": None, "url": None, "kind": "journal-article", "citation_text": None,
           "locations": [], "drugs": [], "used_for": None, "search_hint": None,
           "notes": "extracted by diff_cites.py (round %s, %s..%s)" % (ROUND, BASE[:7], HEAD[:7])}
    rec.update(kw)
    return rec


for path, lines in added.items():
    if path.startswith("man/") or path.endswith((".Rd", ".json", ".css", ".svg", ".png")):
        continue
    full = subprocess.run(["git", "-C", REPO, "show", "%s:%s" % (HEAD, path)], capture_output=True).stdout.decode("utf-8", "replace").splitlines()
    drugs = drug_of(path)
    used = "cited in %s" % path
    for n in sorted(lines):
        # the added line with two lines either side, joined: a paste0() citation spans strings
        win = " ".join(re.sub(r'^\s*#\'?\s*|"\s*,?\s*$|^\s*"', " ", full[i]) for i in range(max(0, n - 3), min(len(full), n + 2)))
        win = re.sub(r"\s+", " ", win)
        line = lines[n]
        loc = "%s:%d" % (path, n)
        taken = set()
        for p in PMID.findall(line):
            taken.add(p)
            if p not in have_id:
                add(p, record(pmid=p, citation_text=win[:400], locations=[loc], drugs=drugs, used_for=used)); stats["PMID"] += 1
        for d in DOI.findall(line):
            d = d.rstrip(".,;:")
            while d.endswith(")") and d.count(")") > d.count("("):
                d = d[:-1]
            if d.lower() not in have_id:
                add(d.lower(), record(doi=d, citation_text=win[:400], locations=[loc], drugs=drugs, used_for=used)); stats["DOI"] += 1
        for p in PMC.findall(line):
            if p not in have_id:
                add(p, record(pmcid=p, citation_text=win[:400], locations=[loc], drugs=drugs, used_for=used)); stats["PMCID"] += 1
        for u in URL.findall(line):
            u = u.rstrip(".,;:")
            if SKIP_URL.search(u) or u.rstrip("/").lower() in have_id:
                continue
            add(u.rstrip("/").lower(), record(url=u, kind="website", citation_text=win[:400], locations=[loc], drugs=drugs, used_for=used)); stats["URL"] += 1
        # journal citations: "... Gundert-Remy U, Schmidlin O, J Pharm Sci 1985;74:703-710"
        for m in JREF.finditer(line):
            yr = int(m.group("yr"))
            before = line[max(0, m.start() - 300):m.start()]
            k = before.rfind(". ")
            au_m = None
            for am in VAN_AU.finditer(before):
                au_m = au_m or am  # the first author-initials pair in the citation
            au = au_m.group("au") if au_m else None
            if any(p in taken for p in PMID.findall(line[m.end():m.end() + 80])):
                continue
            if au and any((v, yr) in have_ay for v in variants(au)):
                stats["jref on list"] += 1
                continue
            key = "%s:%d:%s:%s" % (fold(au or m.group("jn")), yr, m.group("vol"), m.group("pg"))
            add(key, record(first_author=au, year=yr, journal=m.group("jn").strip(), volume=m.group("vol"), pages=m.group("pg"),
                            citation_text=line.strip()[:400], locations=[loc], drugs=drugs, used_for=used)); stats["jref"] += 1
        for m in AY.finditer(line):
            au, yr = m.group("au"), int(m.group("yr"))
            if len(au.split()) == 2 and au.split()[0] in STOP:
                au = au.split()[1]
            if au in STOP or au.split()[0] in STOP or JOURNALISH.search(line[:m.start()]) or yr > 2027:
                stats["stop"] += 1
                continue
            if any((v, yr) in have_ay for v in variants(au)):
                stats["author-year on list"] += 1
                continue
            add("%s:%d" % (fold(au), yr), record(first_author=au, year=yr, citation_text=line.strip()[:400], locations=[loc], drugs=drugs, used_for=used))
            stats["author-year"] += 1

with open(OUT, "w", encoding="utf-8") as fh:
    for rec in recs.values():
        fh.write(json.dumps(rec, ensure_ascii=False) + "\n")
print(len(recs), "records ->", OUT)
print(dict(stats))
