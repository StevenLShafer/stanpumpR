"""Cached, rate-limited helpers for NCBI E-utilities and Crossref."""
import json, os, re, time, unicodedata, urllib.parse, urllib.request, hashlib
import xml.etree.ElementTree as ET

SP = os.path.dirname(os.path.abspath(__file__))
from machine import LOCAL  # the cache is per-machine, kept out of the Drive-synced archive (2026-10-30)
CACHE_DIR = os.path.join(LOCAL, "http_cache")
os.makedirs(CACHE_DIR, exist_ok=True)
EUTILS = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
TOOL = "&tool=stanpumpR-archive"
_last = {"ncbi": 0.0, "crossref": 0.0}


def _get(url, kind="ncbi", tries=6):
    key = hashlib.sha1(url.encode()).hexdigest()
    path = os.path.join(CACHE_DIR, key + ".txt")
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            return f.read()
    gap = 0.4 if kind == "ncbi" else 0.2
    for i in range(tries):
        wait = _last[kind] + gap - time.time()
        if wait > 0:
            time.sleep(wait)
        _last[kind] = time.time()
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "stanpumpR-archive/1.0"})
            with urllib.request.urlopen(req, timeout=60) as r:
                txt = r.read().decode("utf-8", "replace")
            with open(path, "w", encoding="utf-8") as f:
                f.write(txt)
            return txt
        except Exception as e:  # 429 / 5xx / timeouts
            if i == tries - 1:
                raise
            time.sleep(2 * (i + 1))


def norm(s):
    if s is None:
        return ""
    s = unicodedata.normalize("NFKD", str(s))
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = s.replace("ø", "o").replace("Ø", "O").replace("ß", "ss").replace("æ", "ae")
    return re.sub(r"[^a-z0-9]", "", s.lower())


def esearch(term, retmax=10):
    url = EUTILS + "esearch.fcgi?db=pubmed&retmode=json&retmax=%d&term=%s%s" % (
        retmax, urllib.parse.quote(term), TOOL)
    j = json.loads(_get(url))["esearchresult"]
    return int(j.get("count", 0)), j.get("idlist", [])


def esummary(pmids):
    out = {}
    pmids = [p for p in dict.fromkeys(pmids) if p]
    for i in range(0, len(pmids), 150):
        chunk = pmids[i:i + 150]
        url = EUTILS + "esummary.fcgi?db=pubmed&retmode=json&id=%s%s" % (",".join(chunk), TOOL)
        j = json.loads(_get(url))["result"]
        for p in chunk:
            if p in j and "error" not in j[p]:
                out[p] = summarize(j[p])
    return out


def summarize(r):
    ids = {a["idtype"]: a["value"] for a in r.get("articleids", [])}
    pmc = ids.get("pmc") or ids.get("pmcid", "")
    m = re.search(r"PMC\d+", pmc or "")
    year = re.match(r"(\d{4})", r.get("pubdate", "") or r.get("epubdate", "") or "")
    authors = [a["name"] for a in r.get("authors", []) if a.get("authtype") == "Author"]
    return {
        "pmid": r["uid"],
        "title": (r.get("title") or "").rstrip("."),
        "authors": authors,
        "first_author": authors[0] if authors else "",
        "journal": r.get("source", ""),
        "journal_full": r.get("fulljournalname", ""),
        "year": int(year.group(1)) if year else None,
        "volume": r.get("volume", ""),
        "issue": r.get("issue", ""),
        "pages": r.get("pages", ""),
        "doi": ids.get("doi", ""),
        "pmcid": m.group(0) if m else "",
        "pubtype": r.get("pubtype", []),
    }


def ecitmatch(cites):
    """cites: list of dicts with journal, year, volume, first_page, author, key."""
    out = {}
    for i in range(0, len(cites), 40):
        chunk = cites[i:i + 40]
        lines = []
        for c in chunk:
            f = [c.get("journal") or "", str(c.get("year") or ""), str(c.get("volume") or ""),
                 str(c.get("first_page") or ""), c.get("author") or "", c["key"]]
            f = [x.replace("|", " ") for x in f]
            lines.append("|".join(f) + "|")
        url = EUTILS + "ecitmatch.cgi?db=pubmed&retmode=xml&bdata=" + urllib.parse.quote("\r".join(lines)) + TOOL
        txt = _get(url)
        for line in txt.strip().splitlines():
            parts = line.split("|")
            if len(parts) >= 7:
                out[parts[5]] = parts[6].strip()
    return out


def crossref_query(q, rows=3):
    url = ("https://api.crossref.org/works?rows=%d&select=DOI,title,author,issued,container-title,"
           "volume,page,type,score&query.bibliographic=%s") % (rows, urllib.parse.quote(q))
    j = json.loads(_get(url, "crossref"))
    items = []
    for it in j["message"]["items"]:
        yr = None
        try:
            yr = it["issued"]["date-parts"][0][0]
        except Exception:
            pass
        au = it.get("author") or []
        items.append({
            "doi": it.get("DOI", ""),
            "title": (it.get("title") or [""])[0],
            "first_author": (au[0].get("family") or au[0].get("name") or "") if au else "",
            "year": yr,
            "journal": (it.get("container-title") or [""])[0],
            "volume": it.get("volume", ""),
            "pages": it.get("page", ""),
            "type": it.get("type", ""),
            "score": it.get("score", 0),
        })
    return items


def crossref_doi(doi):
    url = "https://api.crossref.org/works/" + urllib.parse.quote(doi)
    try:
        j = json.loads(_get(url, "crossref"))["message"]
    except Exception:
        return None
    au = j.get("author") or []
    yr = None
    for k in ("published-print", "issued", "published-online"):
        try:
            yr = j[k]["date-parts"][0][0]
            break
        except Exception:
            pass
    return {
        "doi": j.get("DOI", doi),
        "title": (j.get("title") or [""])[0],
        "first_author": (au[0].get("family") or "") if au else "",
        "authors": ["%s %s" % (a.get("family", ""), "".join(w[0] for w in (a.get("given") or "").replace("-", " ").split() if w)) for a in au],
        "year": yr,
        "journal": (j.get("short-container-title") or j.get("container-title") or [""])[0],
        "volume": j.get("volume", ""),
        "issue": j.get("issue", ""),
        "pages": j.get("page", ""),
        "type": j.get("type", ""),
    }
