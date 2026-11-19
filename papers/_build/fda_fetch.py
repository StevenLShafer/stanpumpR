"""Download FDA-approved labels and FDA reviews cited by stanpumpR from accessdata.fda.gov (Drugs@FDA).

Labels: the most recent label PDF Drugs@FDA lists for each application.  Reviews: the clinical pharmacology
review(s) linked from the approval's review table of contents.  Files are saved in the archive as
NJ_<group>_<applno>_<date>.pdf and recorded in nonjournal_manifest.json (group -> files, source URL, date).
Drugs@FDA index: fda/drugsatfda.zip from https://www.fda.gov/media/89850/download (unzipped in fda/).
"""
import csv, json, os, re, sys, time, urllib.parse, urllib.request
sys.stdout.reconfigure(encoding="utf-8")
HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
MAN = os.path.join(HERE, "nonjournal_manifest.json")
UA = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) stanpumpR-archive"}

# reference group -> FDA applications whose label is cited (brand chosen; generics -> the reference listed drug)
LABELS = {
    "label-calciumgluconate": ["208418"], "label-calciumchloride": ["021117"],  # reference NDAs (Fresenius Kabi, Hospira)
    "label-ambien": ["019908"], "label-amiodarone": ["018972"], "label-amiodarone-iv": ["022325"],
    "label-aptiom": ["022416"], "label-ativan": ["018140", "017794"], "label-bridion": ["022225"],
    "label-cerebyx": ["020450"], "label-concerta": ["021121"], "label-cymbalta": ["021427"],
    "label-depakoteer": ["021168"], "label-dilantin": ["084349"], "label-dilantin125": ["008762"],
    "label-dilantininfatabs": ["084427"], "label-dilaudid": ["019892"], "label-hydromorphone": ["019034"],
    "label-gabitril": ["020646"], "label-glycopyrrolate": ["017558"], "label-keflex": ["050405"],
    "label-keppra": ["021035"], "label-ketorolactromethamine": ["019645"], "label-klonopin": ["017533"],
    "label-lexapro": ["021323"], "label-lidocaine": ["006488"], "label-lyrica": ["021446"],
    "label-mannitol": ["013684"], "label-methylprednisolone": ["011153"], "label-metronidazole": ["012623"],
    "label-mirtazapine": ["020415"], "label-naproxen": ["017581"], "label-narcan": ["208411"],
    "label-neurontin": ["020235"], "label-numorphan": ["011707"], "label-opana": ["021611"],
    "label-paroxetine": ["020031"], "label-phenytoinsodiuminjection": ["010151"], "label-prozac": ["018936"],
    "label-restoril": ["018163"], "label-roxicodone": ["021011"], "label-sertraline": ["019839"],
    "label-tegretolxr": ["020234"], "label-topamax": ["020505"], "label-tramadol": ["020281", "022370", "021692"],
    "label-valproatesodiuminjection": ["020593"], "label-vancomycin": ["050671"], "label-vimpat": ["022253"],
    "label-xanax": ["018276"], "label-zonisamides": ["020789", "214273"],
    "t:olanzapineprescribinginformationusla": ["020592"], "label-acetaminophen": ["022450"],
    "fda-l1-5": ["020938"], "fda-l1-4": ["210583"],
    # 2026-10-17 (master fe63e39): illicit drugs, anticoagulants, antidiabetics, antihypertensives, relaxants
    "label-xyrem": ["021196"], "label-angiomax": ["020873"], "label-eliquis": ["202155"],
    "label-argatroban": ["020883"], "label-heparin": ["017037"], "label-xarelto": ["022406"],
    "label-coreg": ["020297"], "label-chlorthalidone": ["019574"], "label-corlopam": ["019922"],
    "label-edarbi": ["200796"], "label-farxiga": ["202293"], "label-humalog": ["020563"],
    "label-invokana": ["204042"], "label-cardeneiv": ["019734"], "label-ozempictablets": ["213051"],
    "label-symlin": ["021332"], "label-victoza": ["022341"], "label-vecuronium": ["018776"],
    "label-bumetanideinjection": ["018226"], "label-bumex": ["018225"], "label-captopril": ["018343"],
    "label-catapres": ["017407"], "label-catapresstts": ["018891"], "label-furosemideinjection": ["018579"],
    "label-lasix": ["016273"], "label-toprolxl": ["019962"], "label-lopressor": ["017963"],
    "label-lopressorinjection": ["018704"], "label-inderal": ["016418"], "label-inderalla": ["018553"],
    "label-aldactone": ["012151"], "label-demadex": ["020136"], "label-calansr": ["019152"],
    # 2026-10-19 (master 41d2ec9): vasoactive and cardiac drugs, cenobamate, antibiotic MBC, pregnancy
    "label-hydrochlorothiazide": ["020504"], "label-rybelsus": ["213051"], "label-arixtra": ["021345"],
    "label-xcopri": ["212839"], "label-cleocinphosphate": ["050441"], "label-cleocinhcl": ["050162"],
    "label-cefazolin": ["050779"], "label-ceftriaxone": ["050585"], "label-brevibloc": ["019386"],
    "label-nimodipine": ["018869"], "label-fragmin": ["020287"], "label-tinzaparin": ["020484"],
    "label-norepinephrine": ["007513"],
    # 2026-10-21 (master e708619): ACE inhibitors and ARBs; verapamil injection (Pfizer's, 019038)
    "label-benicar": ["021286"], "label-diovan": ["021283"], "label-verapamilinjection": ["019038"],
    # 2026-10-24 (master a2c596e): enalapril, enalaprilat (VASOTEC I.V. NDA 019309 has no label online; ANDA 075458 does), vasopressin
    "label-vasotec": ["018998"], "label-vasotec-iv": ["075458"], "label-vasostrict": ["204485"],
    # 2026-10-27 (master 7d8b50b): hydralazine, therapeutic ranges (antiseizure and digoxin labels)
    "label-depakote": ["018723"], "label-zarontin": ["012380"],
    "label-sezaby": ["215910"], "label-depakene": ["018081"],
    # 2026-10-27 (master 83b9b20): therapeutic-range source labels, where the cited revision is the newest
    "label-cleviprex": ["022156"], "label-primacor": ["019436"], "label-cardizem": ["018602"],
    "label-trandate": ["018716"], "label-calan": ["018817"], "label-nitroglycerin-d5w": ["019970"], "label-apresoline": ["008303"],
    "label-adrenalin": ["204640"], "label-atropine": ["021146"], "label-dopamine": ["018132"], "label-lidocaine-d5w": ["018461"],
    "label-gentamicin": ["062366"],  # Fresenius Kabi gentamicin injection (ANDA), cited rev. 10/2013
}
# A group whose cited document is NOT the application's newest label names it exactly: (application, URL, date).
# The newest "label" document can be something else entirely (2026-10-27: Lanoxin's newest was its carton and foil
# labels, with no PI text, and Apresoline's only one the 1995 tablets PI where an injection label was cited; the
# hydralazine injection label is archived from the registry's own DailyMed link instead, group dailymed-4d1f55aa).
PINNED = {
    "label-lanoxin": ("020405", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/020405s015lbl.pdf", "2019-02-25"),
    # 2026-10-27 (master 83b9b20): therapeutic-range sources cite an older revision than the newest (the first posting on or after
    # the cited revision; a group also in LABELS keeps its newest beside it)
    "label-savaysa": ("206316", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/206316s015lbl.pdf", "2019-08-12"),
    "label-pradaxa": ("022512", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2024/022512s047lbl.pdf", "2024-01-12"),
    "label-coumadin": ("009218", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2017/009218s017lbl.pdf", "2017-05-24"),
    "label-sensorcaine": ("018304", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2018/018304s049lbl.pdf", "2018-11-05"),
    "label-carbocaine": ("012250", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2018/012250s037lbl.pdf", "2018-11-05"),
    "label-vazculep": ("204300", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2019/204300s008lbl.pdf", "2019-10-21"),
    "label-magnesiumsulfate": ("020309", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2013/020309s028lbl.pdf", "2013-06-04"),
    "label-eliquis": ("202155", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/202155Orig1s039,s040lbl.pdf", "2025-12-12"),
    "label-xarelto": ("022406", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/022406s044,215859s005lbl.pdf", "2025-06-20"),
    "label-angiomax": ("020873", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2016/020873s036lbl.pdf", "2016-03-22"),
    "label-vasostrict": ("204485", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2022/204485Orig1s027lbl.pdf", "2022-12-08"),
    # Naropin's newest posting (2022-09-21, s044) is bag and carton labeling only, no prescribing information
    # (found by the unattended digest of it, 2026-10-28); the cited 2022 PI is s038, "Revised 04/2022"
    "label-naropin": ("020533", "https://www.accessdata.fda.gov/drugsatfda_docs/label/2022/020533s038lbl.pdf", "2022-06-16"),
}
ALIASES = {"fda-l6-33": "fda-l1-4", "fda-l1-24": "label-naproxen"}  # the same document cited twice
NOTES = {
    "label-numorphan": "Cited as Endo, NUMORPHAN prescribing information, PDR 54th ed. 2000 (not online); this is the current label of the same application (NDA 011707, now OPANA injection).",
    "label-amiodarone": "Code cites the amiodarone 'product monograph' (as simulated by Pollak 2000); US CORDARONE tablet label used.",
    "label-amiodarone-iv": "CORDARONE IV (NDA 020377) has no label on Drugs@FDA; NEXTERONE (NDA 022325) gives the same 24-hour regimen.",
    "label-phenytoinsodiuminjection": "Reference listed drug ANDA 084307 has no label on Drugs@FDA; DILANTIN injection (NDA 010151) used.",
    "label-vancomycin": "Product not named in the code; reference listed vancomycin injection NDA 050671 used.",
    "label-mannitol": "Product not named in the code; OSMITROL (NDA 013684, reference listed drug) used.",
    "label-adrenalin": "Cited as ADRENALIN rev. 10/2026; this is R0 06/2026 (approved 2026-07-20), the newest in the local Drugs@FDA tables. Replace when the 10/2026 revision is posted.",
    "label-primacor": "Cited as PRIMACOR rev. 01/2003; the only label on Drugs@FDA (posted 2019-01-24) prints January 2003.",
}
# review groups: (group, application, direct PDF URL or TOC URL, regex for the PDFs to take from a TOC)
BASE = "https://www.accessdata.fda.gov/drugsatfda_docs/nda/"
REVIEWS = [
    ("fda-review-nda202880", "202880", BASE + "2013/202880Orig1s000ClinPharmR.pdf", None),
    ("fda-bla125469", "125469", BASE + "2014/125469Orig1s000TOC.html", r"ClinPharm"),
    ("fda-nda212839", "212839", BASE + "2019/212839Orig1s000TOC.html", r"ClinPharm|MultidisciplineR|IntegratedR"),
    ("fda-nda204630", "204630", BASE + "2016/204630Orig1_toc.html", r"ClinPharm|clinpharm"),
    ("fda-nda215866", "215866", BASE + "2022/215866Orig1s000TOC.html", r"ClinPharm|MultidisciplineR|IntegratedR"),
    ("fda-nda210583", "210583", BASE + "2020/210583Orig1s000TOC.html", r"ClinPharm"),
    ("fda-nda22023", "022023", "https://www.accessdata.fda.gov/drugsatfda_docs/pediatric/261_22023S017-fosaprepitant-clinpharm-bpca.pdf", None),
    ("fda-review-naloxone", "208411", BASE + "2015/208411Orig1s000TOC.html", r"ClinPharm"),
    ("fda-review-naloxone", "208411", "https://www.accessdata.fda.gov/drugsatfda_docs/pediatric/208411 naloxone clinpharm prea.pdf", None),
    ("fda-nda200535", "200535", BASE + "2010/200535s000_oxycodone_toc.html", r"ClinPharm|BioPharm|clinpharm|biopharm"),
    ("fda-l1-14", "022063", BASE + "2017/022063Orig1s000TOC.html", r"ClinPharm"),
    ("fda-l4-21", "022253", BASE + "2008/022253s000_022254s000TOC.html", r"ClinPharm|clinpharm"),
]


def rows(f):
    return list(csv.DictReader(open(os.path.join(HERE, "fda", f), encoding="latin-1"), delimiter="\t"))


def get(url, binary=True):
    for i in range(3):
        try:
            safe_url = urllib.parse.quote(url, safe=":/?=&%#")  # some Drugs@FDA file names contain spaces
            with urllib.request.urlopen(urllib.request.Request(safe_url, headers=UA), timeout=120) as r:
                b = r.read()
            time.sleep(1.0)
            return b if binary else b.decode("utf-8", "replace")
        except Exception as e:
            err = e
            time.sleep(3 * (i + 1))
    raise err


man = json.load(open(MAN, encoding="utf-8")) if os.path.exists(MAN) else {}


def save(group, appl, url, date, kind, title=""):
    tag = date or re.sub(r"[^A-Za-z0-9]+", "-", os.path.splitext(os.path.basename(url))[0])[-40:]
    fn = "NJ_%s_%s_%s.pdf" % (group.replace("t:", "").replace(":", "-"), appl, tag)
    dst = os.path.join(ARCHIVE, fn)
    if not os.path.exists(dst):
        b = get(url)
        if not b.startswith(b"%PDF") or len(b) < 5000:
            print("  NOT A PDF:", url)
            return None
        open(dst, "wb").write(b)
    entry = {"file": fn, "url": url, "appl": appl, "date": date, "kind": kind, "title": title, "note": NOTES.get(group, "")}
    lst = man.setdefault(group, [])
    if not any(e["file"] == fn for e in lst):
        lst.append(entry)
    print("  %-40s %s  (%d KB)" % (group, fn, os.path.getsize(dst) // 1024))
    return fn


docs = rows("ApplicationDocs.txt")
labels = {}
for d in docs:
    if d["ApplicationDocsTypeID"] == "2":
        labels.setdefault(d["ApplNo"], []).append(d)
print("Labels:")
for group, (appl, url, date) in PINNED.items():
    try:
        save(group, appl, url, date, "label")
    except Exception as e:
        print("  FAILED", group, appl, url, e)
for group, appls in LABELS.items():
    for a in appls:
        L = sorted(labels.get(a, []), key=lambda d: d["ApplicationDocsDate"])
        if not L:
            print("  no label on Drugs@FDA:", group, a)
            continue
        d = L[-1]
        url = d["ApplicationDocsURL"].replace("http://", "https://")
        try:
            save(group, a, url, d["ApplicationDocsDate"][:10], "label")
        except Exception as e:
            print("  FAILED", group, a, url, e)
print("Reviews:")
for group, a, url, pat in REVIEWS:
    try:
        if pat:
            html = get(url, binary=False)
            # the TOC pages list their files in script variables as well as links: take every *.pdf name
            names = re.findall(r"[\w%\-./]+\.pdf", html, re.I)
            hrefs = list(dict.fromkeys(urllib.parse.urljoin(url, n) for n in names if re.search(pat, n)))
            if not hrefs:
                print("  no clinical pharmacology PDF in", url)
            for i, h in enumerate(hrefs):
                save(group, a + ("" if i == 0 else "-%d" % (i + 1)), h, "", "review", os.path.basename(h))
        else:
            save(group, a, url, "", "review", os.path.basename(url))
    except Exception as e:
        print("  FAILED", group, url, e)
for alias, target in ALIASES.items():
    if target in man:
        man[alias] = man[target]
json.dump(man, open(MAN, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
print("manifest groups:", len(man))
