"""Collapse resolved records into distinct works. Applies manual overrides first."""
import json, os, re, sys
from ncbi import norm, esummary, crossref_doi

SP = os.path.dirname(os.path.abspath(__file__))
recs = json.load(open(os.path.join(SP, "resolved.json"), encoding="utf-8"))

# overrides.json: {"<_src>": {"pmid": "...", "doi": "...", "grade": "...", "method": "...", "drop": true, "note": "..."}}
ov_path = os.path.join(SP, "overrides.json")
OV = json.load(open(ov_path, encoding="utf-8")) if os.path.exists(ov_path) else {}
need_pm = [o["pmid"] for o in OV.values() if o.get("pmid")]
S = esummary(need_pm) if need_pm else {}
for r in recs:
    o = OV.get(r["_src"])
    if not o:
        continue
    res = r["res"]
    if o.get("drop"):
        r["_drop"] = o.get("note", "dropped")
        continue
    if "pmid" in o:
        res["pmid"] = o["pmid"]
        if o["pmid"]:
            r["pm"] = S.get(o["pmid"])
        else:
            r.pop("pm", None)
    if "doi" in o:
        res["doi"] = o["doi"]
        if o["doi"] and not res.get("pmid"):
            r["cr"] = crossref_doi(o["doi"]) or r.get("cr")
        if not o["doi"]:
            r.pop("cr", None)
    if o.get("group"):
        r["_group"] = o["group"]
    if o.get("kind"):
        r["kind"] = o["kind"]
    if o.get("repo_error"):
        r["_repo_error"] = o["repo_error"]
    res["grade"] = o.get("grade", "ok")
    res["method"] = o.get("method", "manual review")
    if o.get("note"):
        res["notes"] = (res.get("notes") or []) + [o["note"]]

dropped = [r for r in recs if r.get("_drop")]
json.dump(dropped, open(os.path.join(SP, "dropped.json"), "w", encoding="utf-8"), indent=1, ensure_ascii=False)
recs = [r for r in recs if not r.get("_drop")]


def key_of(r):
    res = r["res"]
    if res.get("pmid"):
        return "pmid:" + res["pmid"]
    if r.get("_group"):
        return "g:" + r["_group"]
    doi = res.get("doi") or (r.get("cr") or {}).get("doi")
    if doi:
        return "doi:" + doi.lower()
    t = norm(r.get("title"))[:40]
    if t:
        return "t:" + t
    return "a:%s:%s:%s" % (norm(r.get("first_author")), r.get("year"), norm(r.get("journal")))


groups = {}
for r in recs:
    groups.setdefault(key_of(r), []).append(r)

# second pass: DOI-keyed groups whose DOI maps to a PMID group
pmid_by_doi = {}
for k, g in groups.items():
    if k.startswith("pmid:"):
        for r in g:
            d = (r.get("pm") or {}).get("doi")
            if d:
                pmid_by_doi[d.lower()] = k
for k in list(groups):
    if k.startswith("doi:") and k[4:] in pmid_by_doi:
        groups[pmid_by_doi[k[4:]]] += groups.pop(k)

works = []
for k, g in groups.items():
    pm = next((r["pm"] for r in g if r.get("pm")), None)
    cr = next((r["cr"] for r in g if r.get("cr")), None)
    grades = [r["res"].get("grade") for r in g]
    best = "non-journal" if "non-journal" in grades and not pm else "ok" if "ok" in grades else ("probable" if "probable" in grades else ("check" if "check" in grades else grades[0]))
    locs, drugs, used, notes, texts, kinds, methods = [], [], [], [], [], [], []
    for r in g:
        for l in r.get("locations") or []:
            if l not in locs:
                locs.append(l)
        for d in r.get("drugs") or []:
            if d and d not in drugs:
                drugs.append(d)
        if r.get("used_for") and r["used_for"] not in used:
            used.append(r["used_for"])
        for n in (r["res"].get("notes") or []):
            if n not in notes:
                notes.append(n)
        if r.get("notes") and r["notes"] not in notes:
            notes.append(r["notes"])
        if r.get("citation_text"):
            texts.append(r["citation_text"])
        kinds.append(r.get("kind") or "")
        m = r["res"].get("method")
        if m and m not in methods:
            methods.append(m)
    repo_errors = [r["_repo_error"] for r in g if r.get("_repo_error")]
    works.append({"repo_errors": repo_errors, "records": g, "key": k, "n": len(g), "pm": pm, "cr": cr, "grade": best, "srcs": [r["_src"] for r in g],
                  "first_author": g[0].get("first_author"), "year": g[0].get("year"), "journal": g[0].get("journal"),
                  "volume": g[0].get("volume"), "pages": g[0].get("pages"), "title": next((r.get("title") for r in g if r.get("title")), None),
                  "kind": max(set(kinds), key=kinds.count), "locations": locs, "drugs": drugs, "used_for": used,
                  "notes": notes, "citation_text": max(texts, key=len) if texts else "", "methods": methods,
                  "doi": (pm or {}).get("doi") or (cr or {}).get("doi") or next((r["res"].get("doi") for r in g if r["res"].get("doi")), None)})

json.dump(works, open(os.path.join(SP, "works.json"), "w", encoding="utf-8"), indent=1, ensure_ascii=False)
from collections import Counter
print("works:", len(works), Counter(w["grade"] for w in works))
if "-v" in sys.argv:
    for w in sorted(works, key=lambda w: (w["grade"], norm(w["first_author"]))):
        if w["grade"] == "ok":
            continue
        pm = w["pm"] or {}
        print("\n[%s] %s %s %s %s:%s | srcs=%s" % (w["grade"], w["first_author"], w["year"], w["journal"], w["volume"], w["pages"], ",".join(w["srcs"])))
        print("   cite:", w["citation_text"][:220])
        print("   drugs:", w["drugs"], "| used:", "; ".join(w["used_for"])[:160])
        if pm:
            print("   -> PMID %s: %s %s %s %s:%s %s" % (pm["pmid"], pm["first_author"], pm["year"], pm["journal"], pm["volume"], pm["pages"], pm["title"][:90]))
        elif w["cr"]:
            c = w["cr"]
            print("   -> DOI %s: %s %s %s %s" % (c["doi"], c["first_author"], c["year"], c["journal"], c["title"][:90]))
        print("   method:", w["methods"], "| notes:", "; ".join(w["notes"])[:300])
