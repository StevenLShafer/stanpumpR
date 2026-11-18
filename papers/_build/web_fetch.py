"""Archive the non-journal web sources: direct PDFs are downloaded; web pages are printed to PDF with Microsoft
Edge (headless; the printed header/footer keep the URL and the date).  Recorded in nonjournal_manifest.json."""
import datetime, json, os, re, subprocess, sys, time, urllib.parse, urllib.request
sys.stdout.reconfigure(encoding="utf-8")
HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
MAN = os.path.join(HERE, "nonjournal_manifest.json")
EDGE = r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe"
UA = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130 Safari/537.36"}
TODAY = datetime.date.today().isoformat()

PDFS = [  # group, url, tag
    ("holford2014", "https://www.jscimedcentral.com/public/assets/articles/pharmacology-spidpharmacokinetics-1023.pdf", "jpct2014"),
    ("hartog-thesis", "https://repub.eur.nl/pub/93139/y-hartog-2.pdf", "thesis"),
    ("page2010-bartels", "https://www.page-meeting.org/wp-content/uploads/pdf_assets/3455-213_v5_ChristianBartels_final.pdf", "poster"),
    ("fda-zolpidem-2013", "https://www.fda.gov/files/drugs/published/Drug-Safety-Communication--Risk-of-next-morning-impairment-after-use-of-insomnia-drugs--FDA-requires-lower-recommended-doses-for-certain-drugs-containing-zolpidem-%28Ambien--Ambien-CR--Edluar--and-Zolpimist%29.pdf", "dsc-2013-01-10"),
    ("fda-gabapentinoids-2019", "https://www.fda.gov/media/133681/download", "dsc-2019-12-19"),
]
PAGES = [  # group, url, tag
    ("apsf2022", "https://www.apsf.org/article/is-rebreathing-prevented-when-fgf-equals-mv/", "web"),
    ("fda-stic", "https://www.fda.gov/drugs/development-resources/antibacterial-susceptibility-test-interpretive-criteria", "web"),
    ("fda-gabapentinoids-2019", "https://www.fda.gov/safety/medical-product-safety-information/neurontin-gralise-horizant-gabapentin-and-lyrica-lyrica-cr-pregabalin-drug-safety-communication", "web"),
    ("page2010-brogren", "https://www.page-meeting.org/?abstract=1686", "abstract"),
]


def get(url, binary=True):
    with urllib.request.urlopen(urllib.request.Request(urllib.parse.quote(url, safe=":/?=&%#"), headers=UA), timeout=120) as r:
        b = r.read()
    time.sleep(1)
    return b if binary else b.decode("utf-8", "replace")


def ok(url):
    try:
        get(url, binary=False)
        return True
    except Exception as e:
        print("  unreachable:", url, e)
        return False


man = json.load(open(MAN, encoding="utf-8")) if os.path.exists(MAN) else {}


def record(group, fn, url, kind):
    lst = man.setdefault(group, [])
    if not any(e["file"] == fn for e in lst):
        lst.append({"file": fn, "url": url, "appl": "", "date": TODAY if kind.startswith("web") else "", "kind": kind, "title": "", "note": ""})
    print("  %-26s %s (%d KB)" % (group, fn, os.path.getsize(os.path.join(ARCHIVE, fn)) // 1024))


for group, url, tag in PDFS:
    fn = "NJ_%s_%s.pdf" % (group, tag)
    dst = os.path.join(ARCHIVE, fn)
    try:
        if not os.path.exists(dst):
            b = get(url)
            if not b.startswith(b"%PDF"):
                print("  not a PDF:", url)
                continue
            open(dst, "wb").write(b)
        record(group, fn, url, "pdf")
    except Exception as e:
        print("  FAILED", group, url, e)

# EUCAST: find the current breakpoint-table PDF on the clinical breakpoints page
try:
    html = get("https://www.eucast.org/clinical_breakpoints", binary=False)
    links = [urllib.parse.urljoin("https://www.eucast.org/", h) for h in re.findall(r'href="([^"]+?\.pdf)"', html, re.I)]
    bp = [l for l in links if re.search(r"breakpoint", l, re.I) and re.search(r"16[._]1", l)] or [l for l in links if re.search(r"Breakpoint_Tables", l, re.I)]
    print("  EUCAST candidates:", bp[:3])
    if bp:
        PDFS_EU = [("eucast-breakpoints", bp[0], "v16-1" if re.search(r"16[._]1", bp[0]) else "current")]
        for group, url, tag in PDFS_EU:
            fn = "NJ_%s_%s.pdf" % (group, tag)
            if not os.path.exists(os.path.join(ARCHIVE, fn)):
                b = get(url)
                if b.startswith(b"%PDF"):
                    open(os.path.join(ARCHIVE, fn), "wb").write(b)
            if os.path.exists(os.path.join(ARCHIVE, fn)):
                record(group, fn, url, "pdf")
except Exception as e:
    print("  EUCAST page failed:", e)

for group, url, tag in PAGES:
    fn = "NJ_%s_%s_%s.pdf" % (group, tag, TODAY)
    dst = os.path.join(ARCHIVE, fn)
    if not os.path.exists(dst):
        if not ok(url):
            continue
        try:
            r = subprocess.run([EDGE, "--headless=new", "--disable-gpu", "--no-first-run", "--virtual-time-budget=15000",
                                "--print-to-pdf=" + dst, url], capture_output=True, timeout=180)
        except subprocess.TimeoutExpired:
            print("  print timed out:", group, url)
            continue
        if not os.path.exists(dst) or os.path.getsize(dst) < 5000:
            print("  print failed:", group, url, r.stderr.decode("utf-8", "replace")[-300:])
            continue
    record(group, fn, url, "web page printed %s" % TODAY)

# ClinicalTrials.gov is a JavaScript app that does not print headless; keep the registry record (with results) as JSON
fn = "NJ_nct04526197_api_%s.json" % TODAY
if not os.path.exists(os.path.join(ARCHIVE, fn)):
    try:
        open(os.path.join(ARCHIVE, fn), "wb").write(get("https://clinicaltrials.gov/api/v2/studies/NCT04526197"))
    except Exception as e:
        print("  FAILED NCT04526197", e)
if os.path.exists(os.path.join(ARCHIVE, fn)):
    record("nct04526197", fn, "https://clinicaltrials.gov/study/NCT04526197", "registry record (JSON, API v2)")
json.dump(man, open(MAN, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
print("manifest groups:", len(man))
