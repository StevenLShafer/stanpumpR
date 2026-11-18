"""For scanned (no text layer) PDFs in Downloads: guess the reference from the file name, compare page counts,
and export page 1 as an image so it can be read by eye.  Writes scan_check.json; images go to scan_pages/."""
import json, os, re, subprocess, sys
import pypdf
sys.stdout.reconfigure(encoding="utf-8")
from ncbi import norm

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "scan_pages")
os.makedirs(OUT, exist_ok=True)
DL = os.path.join(os.path.expanduser("~"), "Downloads")


def words(s):
    return set(w for w in re.findall(r"[a-z0-9]+", (s or "").lower()) if len(w) > 3)


refs = []
for w in json.load(open(os.path.join(HERE, "works.json"), encoding="utf-8")):
    pm = w.get("pm") or {}
    if pm.get("pmid"):
        refs.append(pm)

cache = json.load(open(os.path.join(HERE, "filing_cache.json"), encoding="utf-8"))
scans = sorted({k.split("|")[0] for k, v in cache.items() if (v.get("review") or "").startswith("no text layer")})
scans = [p for p in scans if os.path.exists(p)]


def expected_pages(pages):
    m = re.match(r"\s*(\d+)\s*-\s*(\d+)", pages or "")
    if not m:
        return None
    a, b = m.group(1), m.group(2)
    if len(b) < len(a):
        b = a[: len(a) - len(b)] + b
    return int(b) - int(a) + 1


res = []
for i, path in enumerate(scans):
    name = os.path.basename(path)
    fw = words(os.path.splitext(name)[0].replace("-", " ").replace("_", " "))
    best, sc = None, 0
    for pm in refs:
        tw = words(pm["title"])
        if not tw:
            continue
        s = len(tw & fw) / len(tw)
        if pm["first_author"] and norm(pm["first_author"].split()[0]) in norm(name):
            s += 0.5
        if s > sc:
            best, sc = pm, s
    r = pypdf.PdfReader(path)
    n = len(r.pages)
    img = None

    def find_image(res_dict, depth=0):
        xo = res_dict.get("/XObject") if res_dict else None
        if not xo or depth > 3:
            return None
        xo = xo.get_object()
        for k in xo:
            o = xo[k].get_object()
            if o.get("/Subtype") == "/Form":
                hit = find_image(o.get("/Resources", {}).get_object() if o.get("/Resources") else None, depth + 1)
                if hit:
                    return hit
                continue
            if o.get("/Subtype") == "/Image" and int(o.get("/Width", 0)) > 1000:
                return o
        return None

    try:
        o = find_image(r.pages[0].get("/Resources").get_object() if r.pages[0].get("/Resources") else None)
        if o is not None:
            f = o.get("/Filter")
            f = f[0] if isinstance(f, list) else f
            ext = {"/DCTDecode": "jpg", "/CCITTFaxDecode": "tif", "/JPXDecode": "jp2"}.get(f, "bin")
            img = os.path.join(OUT, "scan%02d.%s" % (i, ext))
            open(img, "wb").write(o.get_data() if f == "/CCITTFaxDecode" else o._data)
    except Exception as e:
        img = "error: %s" % e
    res.append({"file": path, "name": name, "pages": n, "img": img,
                "pmid": best["pmid"] if best else None, "score": round(sc, 2),
                "cand": "%s %s %s %s:%s | %s" % (best["first_author"], best["year"], best["journal"], best["volume"], best["pages"], best["title"]) if best else "",
                "expected_pages": expected_pages(best["pages"]) if best else None})
json.dump(res, open(os.path.join(HERE, "scan_check.json"), "w", encoding="utf-8"), indent=1, ensure_ascii=False)
for x in res:
    print("%-2s %-60s pages=%s exp=%s score=%s pmid=%s\n     %s\n     img=%s" % (
        res.index(x), x["name"][:60], x["pages"], x["expected_pages"], x["score"], x["pmid"], x["cand"][:150], x["img"]))
