"""Archive the documents the registry tables link to (groups dailymed-*, fda-*, web-* in overrides.json).

DailyMed label pages are fetched as the label PDF (downloadpdffile.cfm?setId=...); any other link is saved only if it
returns a PDF.  Files are NJ_<group>.pdf, recorded in nonjournal_manifest.json like fda_fetch.py and web_fetch.py.
Web pages that are not PDFs are left for printing by hand.
"""
import datetime, json, os, re, sys, time, urllib.parse, urllib.request

sys.stdout.reconfigure(encoding="utf-8")
HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
MAN = os.path.join(HERE, "nonjournal_manifest.json")
UA = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130 Safari/537.36"}
TODAY = datetime.date.today().isoformat()


def get(url):
    with urllib.request.urlopen(urllib.request.Request(urllib.parse.quote(url, safe=":/?=&%#~"), headers=UA), timeout=120) as r:
        b = r.read()
    time.sleep(1)
    return b


ov = json.load(open(os.path.join(HERE, "overrides.json"), encoding="utf-8"))
links = {}
for o in ov.values():
    g = o.get("group", "")
    if g.startswith(("dailymed-", "fda-", "web-")) and (o.get("note") or "").startswith("http"):
        links.setdefault(g, o["note"].split()[0])
man = json.load(open(MAN, encoding="utf-8")) if os.path.exists(MAN) else {}
done, failed, pages = 0, [], []
for g, url in sorted(links.items()):
    fn = "NJ_%s.pdf" % g
    dst = os.path.join(ARCHIVE, fn)
    if not os.path.exists(dst):
        m = re.search(r"setid=([0-9a-f-]{36})", url, re.I)
        fetch = "https://dailymed.nlm.nih.gov/dailymed/downloadpdffile.cfm?setId=%s" % m.group(1) if m else url
        try:
            b = get(fetch)
        except Exception as e:
            failed.append((g, url, str(e)[:60]))
            continue
        if not b.startswith(b"%PDF") or len(b) < 5000:
            pages.append((g, url))
            continue
        open(dst, "wb").write(b)
    lst = man.setdefault(g, [])
    if not any(e["file"] == fn for e in lst):
        lst.append({"file": fn, "url": url, "appl": "", "date": TODAY, "kind": "label" if g.startswith(("dailymed", "fda")) else "document",
                    "title": "", "note": "linked from a stanpumpR registry table (inst/extdata)"})
    done += 1
json.dump(man, open(MAN, "w", encoding="utf-8"), indent=1, ensure_ascii=False)
print("archived", done, "of", len(links))
for g, u in pages:
    print("  not a PDF (print by hand):", g, u)
for g, u, e in failed:
    print("  FAILED", g, u, e)
