"""Resolve every extracted citation to a PMID and/or DOI, with a verification grade."""
import glob, json, os, re, sys
from ncbi import *

SP = os.path.dirname(os.path.abspath(__file__))
recs = []
for f in sorted(glob.glob(os.path.join(SP, "extract", "*.jsonl"))):
    for i, line in enumerate(open(f, encoding="utf-8")):
        if line.strip():
            r = json.loads(line)
            r["_src"] = "%s#%d" % (os.path.basename(f), i)
            recs.append(r)
print("records:", len(recs))


def first_page(p):
    if not p:
        return ""
    m = re.match(r"\s*([A-Za-z]?\d+)", str(p))
    return m.group(1) if m else ""


def year_of(r):
    try:
        return int(str(r.get("year"))[:4])
    except Exception:
        return None


def surname(name):
    # "Eleveld DJ" -> Eleveld ; "van den Berg JP" -> vandenBerg
    parts = (name or "").split()
    if len(parts) > 1 and re.fullmatch(r"[A-Z]{1,4}", parts[-1]):
        parts = parts[:-1]
    return norm("".join(parts))


def author_ok(rec_author, pm_authors):
    a = norm(rec_author)
    if not a or not pm_authors:
        return False
    s0 = surname(pm_authors[0])
    if a == s0 or s0.startswith(a) or a.startswith(s0) or (len(a) > 3 and a in s0):
        return True
    return False


def author_any(rec_author, pm_authors):
    a = norm(rec_author)
    return bool(a) and any(a == surname(x) for x in pm_authors)


def title_term(title, au=None, yr=None):
    words = [w for w in re.sub(r"[^A-Za-z0-9 ]", " ", title).split() if len(w) > 3][:9]
    if not words:
        return None
    t = " AND ".join("%s[ti]" % w for w in words)
    if au:
        t += " AND %s[1au]" % re.sub(r"[^A-Za-z\- ]", "", au)
    if yr:
        t += " AND %d:%d[dp]" % (yr - 1, yr + 1)
    return t


DRUG_SYNS = {"pethidine": "(pethidine OR meperidine)", "nitrousOxide": "nitrous oxide",
             "cefalexin": "(cefalexin OR cephalexin)", "desmetramadol": "(desmetramadol OR O-desmethyltramadol)"}

# ------------------------------------------------------------------ pass 1: ids + ecitmatch
cm_in = []
for k, r in enumerate(recs):
    r["_k"] = str(k)
    r["res"] = {"pmid": None, "doi": None, "method": None, "candidates": [], "notes": []}
    yr = year_of(r)
    if r.get("journal") and yr and (r.get("volume") or r.get("pages")) and r.get("kind") in (None, "journal-article", "abstract", "other", "guideline"):
        cm_in.append(dict(journal=r["journal"], year=yr, volume=r.get("volume") or "",
                          first_page=first_page(r.get("pages")), author=r.get("first_author") or "", key=str(k)))
cm = ecitmatch(cm_in)
print("ecitmatch hits:", sum(1 for v in cm.values() if v.isdigit()), "of", len(cm_in))

for k, r in enumerate(recs):
    res = r["res"]
    yr = year_of(r)
    pm = (str(r.get("pmid") or "")).strip()
    if pm.isdigit():
        res.update(pmid=pm, method="repo PMID")
        continue
    if r.get("pmcid"):
        n, ids = esearch("%s[pmcid]" % r["pmcid"])
        if n == 1:
            res.update(pmid=ids[0], method="repo PMCID")
            continue
    if r.get("doi"):
        doi = r["doi"].strip().rstrip(".")
        n, ids = esearch('"%s"[doi]' % doi)
        if n == 1:
            res.update(pmid=ids[0], method="repo DOI")
            continue
        res.update(doi=doi, method="repo DOI (not in PubMed)")
        continue
    v = cm.get(str(k), "")
    if v.isdigit():
        res.update(pmid=v, method="citation match (ecitmatch)")
        continue
    if r.get("title"):
        t = title_term(r["title"], r.get("first_author"), yr)
        if t:
            n, ids = esearch(t)
            if n == 1:
                res.update(pmid=ids[0], method="title search")
                continue
            if n == 0 and r.get("first_author"):
                n, ids = esearch(title_term(r["title"], None, yr))
                if n == 1:
                    res.update(pmid=ids[0], method="title search (no author)")
                    continue
            if 1 < n <= 8:
                res["candidates"] += ids
    if r.get("kind") in ("book", "website", "label", "thesis", "software"):
        continue
    terms = []
    if r.get("search_hint"):
        terms.append(r["search_hint"])
    if r.get("first_author") and yr:
        au = re.sub(r"[^A-Za-z\- ]", "", r["first_author"]).strip()
        base = "%s[1au] AND %d[dp]" % (au, yr)
        if r.get("journal"):
            terms.append(base + ' AND "%s"[ta]' % r["journal"])
        drugs = [DRUG_SYNS.get(d, d) for d in (r.get("drugs") or []) if d]
        if drugs:
            terms.append(base + " AND (" + " OR ".join(drugs) + ")")
    for t in terms:
        try:
            n, ids = esearch(t)
        except Exception as e:
            res["notes"].append("search error %s" % e)
            continue
        if n == 1:
            res.update(pmid=ids[0], method="search: " + t)
            break
        if 1 < n <= 10:
            res["candidates"] += [i for i in ids if i not in res["candidates"]]
    if not res["pmid"] and res["candidates"]:
        res["method"] = "ambiguous"

# ------------------------------------------------------------------ pass 2: metadata + grading
allp = set(r["res"]["pmid"] for r in recs if r["res"]["pmid"]) | set(c for r in recs for c in r["res"]["candidates"][:10])
S = esummary(sorted(allp))
print("summaries:", len(S))

for r in recs:
    res = r["res"]
    yr = year_of(r)
    if res["pmid"]:
        s = S.get(res["pmid"])
        if not s:
            res["grade"] = "check"
            res["notes"].append("PMID not found in PubMed")
            continue
        r["pm"] = s
        aok = author_ok(r.get("first_author"), s["authors"]) if r.get("first_author") else None
        yok = (abs(s["year"] - yr) <= 1) if (yr and s["year"]) else None
        res["author_ok"], res["year_ok"] = aok, yok
        m = res["method"]
        if m.startswith("repo"):
            res["grade"] = "ok" if (aok is not False and yok is not False) else "check"
        elif aok and yok:
            res["grade"] = "ok" if (m.startswith("citation") or m.startswith("title")) else "probable"
        elif aok is None and yok:
            res["grade"] = "probable"
        else:
            res["grade"] = "check"
            if aok is False:
                res["notes"].append("first author mismatch: repo %s vs PubMed %s" % (r.get("first_author"), s["first_author"]))
            if yok is False:
                res["notes"].append("year mismatch: repo %s vs PubMed %s" % (yr, s["year"]))
    else:
        # rank candidates by author/year agreement
        good = []
        for c in res["candidates"]:
            s = S.get(c)
            if s and author_ok(r.get("first_author"), s["authors"]) and yr and s["year"] and abs(s["year"] - yr) <= 1:
                good.append(c)
        res["candidates_good"] = good
        if len(good) == 1:
            res["pmid"] = good[0]
            r["pm"] = S[good[0]]
            res["method"] = "search, one of %d hits agreed on author+year" % len(res["candidates"])
            res["grade"] = "probable"
        else:
            res["grade"] = "unresolved" if not res.get("doi") else "doi-only"

# ------------------------------------------------------------------ pass 3: Crossref for what PubMed lacks
for r in recs:
    res = r["res"]
    if res["pmid"]:
        continue
    if res.get("doi"):
        c = crossref_doi(res["doi"])
        if c:
            r["cr"] = c
            res["grade"] = "ok"
        continue
    if r.get("kind") in ("website", "label", "software"):
        continue
    q = r.get("citation_text") or " ".join(str(x) for x in [r.get("first_author"), r.get("year"), r.get("title"), r.get("journal")] if x)
    try:
        items = crossref_query(q, rows=3)
    except Exception as e:
        res["notes"].append("crossref error %s" % e)
        continue
    yr = year_of(r)
    for it in items:
        aok = norm(r.get("first_author")) and norm(it["first_author"]) and (
            norm(it["first_author"]).startswith(norm(r.get("first_author"))[:5]))
        yok = yr and it["year"] and abs(it["year"] - yr) <= 1
        if aok and yok:
            res["doi"] = it["doi"]
            r["cr"] = it
            res["method"] = "Crossref bibliographic query (score %.0f)" % it["score"]
            res["grade"] = "probable"
            # does PubMed know this DOI after all?
            n, ids = esearch('"%s"[doi]' % it["doi"])
            if n == 1:
                res["pmid"] = ids[0]
                r["pm"] = esummary([ids[0]]).get(ids[0])
                res["method"] += " -> PubMed via DOI"
            break

json.dump(recs, open(os.path.join(SP, "resolved.json"), "w", encoding="utf-8"), indent=1, ensure_ascii=False)
from collections import Counter
print(Counter(r["res"].get("grade") for r in recs))
print(Counter((r["res"].get("method") or "none").split(":")[0] for r in recs))
