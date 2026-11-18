"""Tiered Lane download page for the references not yet in the archive.

Tier 1  model source: named in a drug function's `reference` string (what the app displays)
Tier 2  a value used in the R code (cited in R/*.R, not the model source)
Tier 3  cited in help pages or tests (explanation, validation, comparison)
Tier 4  bibliography, history and docs only
Usage: python tiers.py <path to a checkout of current master>
"""
import glob, html, json, os, re, sys
from ncbi import norm

HERE = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = os.path.dirname(HERE)
repo = sys.argv[1]

# every drug function's reference string, as text
ref_text = []
for f in glob.glob(os.path.join(repo, "R", "drugs_*.R")):
    src = open(f, encoding="utf-8", errors="replace").read()
    for m in re.finditer(r"reference\s*<-\s*(paste0?\((?:[^()]|\([^()]*\))*\)|\"[^\"]*\")", src):
        ref_text.append(" ".join(re.findall(r"\"([^\"]*)\"", m.group(1))))
REFS = "\n".join(ref_text)
REFS_L, REFS_N = REFS.lower(), norm(REFS)


NJ_MAN = json.load(open(os.path.join(HERE, "nonjournal_manifest.json"), encoding="utf-8")) if os.path.exists(os.path.join(HERE, "nonjournal_manifest.json")) else {}


def nj_archived(key):
    return any(os.path.exists(os.path.join(ARCHIVE, e["file"])) for e in NJ_MAN.get(key[2:], []))


def in_archive(pmid, doi):
    if pmid and os.path.exists(os.path.join(ARCHIVE, "PMID_%s.pdf" % pmid)):
        return True
    return bool(doi) and os.path.exists(os.path.join(ARCHIVE, "DOI_" + re.sub(r"[^A-Za-z0-9.]+", "_", doi) + ".pdf"))


def tier(w, pm, doi):
    pmid = pm.get("pmid", "")
    sur = norm((pm.get("first_author") or w.get("first_author") or "").split()[0]) if (pm.get("first_author") or w.get("first_author")) else ""
    yr = str(pm.get("year") or w.get("year") or "")
    if (pmid and pmid in REFS) or (doi and doi.lower() in REFS_L):
        return 1
    if sur and len(sur) > 3 and yr and re.search(re.escape(sur) + r".{0,40}" + yr, REFS_N + REFS.lower().replace(" ", "")):
        return 1
    locs = w.get("locations") or []
    if any(l.startswith("R/") for l in locs):
        return 2
    if any(l.startswith("tests/") or l.startswith("inst/help/drugs/") or l.startswith("inst/help/models/")
           or l.startswith("inst/help/scenarios/") for l in locs):
        return 3
    return 4


W = json.load(open(os.path.join(HERE, "works.json"), encoding="utf-8"))
# open copies a script could not download (2026-10-28): PMC, and Unpaywall's locations from unpaywall_fetch.py
UPW_LOG = os.path.join(HERE, "unpaywall_fetch_log.json")
UPW = {e["doi"].lower(): e for e in json.load(open(UPW_LOG, encoding="utf-8"))} if os.path.exists(UPW_LOG) else {}


def free_links(pm, doi):
    out = []
    if pm.get("pmcid"):
        out.append(("free at PMC", "https://pmc.ncbi.nlm.nih.gov/articles/%s/" % pm["pmcid"]))
    for l in (UPW.get((doi or "").lower()) or {}).get("locations") or []:
        url = l.get("pdf") or l.get("landing")
        if not url or "ncbi.nlm.nih.gov/pmc" in url or "pmc.ncbi" in url or ("europepmc" in url and pm.get("pmcid")):
            continue
        kind = "open, %s" % ("accepted manuscript" if l.get("version") == "acceptedVersion" else (l.get("host") or "open"))
        out.append((kind, url))
        break
    return out
rows, other = [], []
for w in W:
    pm = w.get("pm") or {}
    cr = w.get("cr") or {}
    pmid = pm.get("pmid", "")
    doi = pm.get("doi") or cr.get("doi") or w.get("doi") or ""
    if in_archive(pmid, doi) or (not pmid and not doi and nj_archived(w["key"])):
        continue
    src = pm or cr
    label = "%s %s" % (src.get("first_author") or w.get("first_author") or "?", src.get("year") or w.get("year") or "")
    if w["grade"] in ("non-journal", "unresolved") or not (pmid or doi):
        other.append((w["grade"], label, w.get("citation_text") or "", ", ".join(w.get("drugs") or [])))
        continue
    t = tier(w, pm, doi)
    link = ("https://doi-org.laneproxy.stanford.edu/" + doi) if doi else ("https://pubmed.ncbi.nlm.nih.gov/%s/" % pmid)
    rows.append({"tier": t, "label": label, "title": src.get("title") or w.get("title") or "",
                 "journal": src.get("journal") or w.get("journal") or "", "vol": src.get("volume") or "",
                 "pages": src.get("pages") or "", "pmid": pmid, "doi": doi, "link": link,
                 "via": "Lane" if doi else "PubMed (no DOI)", "grade": w["grade"], "free": free_links(pm, doi),
                 "aam": bool(pmid) and os.path.exists(os.path.join(ARCHIVE, "PMID_%s_aam.pdf" % pmid)),
                 "drugs": ", ".join(w.get("drugs") or []), "used": " | ".join(w.get("used_for") or [])[:220]})

NAMES = {1: "Tier 1 — model sources (the citation the app shows for a drug's model)",
         2: "Tier 2 — values used in the R code",
         3: "Tier 3 — cited in help pages and tests",
         4: "Tier 4 — bibliography, history and docs only"}
rows.sort(key=lambda r: (r["tier"], norm(r["journal"]), str(r["label"])))
e = html.escape
parts = ["""<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Lane Download Tiers</title><style>
:root{--bg:#fff;--fg:#1a1a1a;--mut:#666;--line:#ddd;--g:#f2f2f2;--a:#0b5cad;--warn:#b36b00}
@media (prefers-color-scheme:dark){:root{--bg:#161616;--fg:#e8e8e8;--mut:#9a9a9a;--line:#333;--g:#222;--a:#6fb0ff;--warn:#e0a040}}
body{background:var(--bg);color:var(--fg);font:14px/1.45 system-ui,sans-serif;margin:0 auto;max-width:1040px;padding:16px}
h1{font-size:20px;margin:0 0 4px}h2{font-size:16px;margin:24px 0 6px;padding-top:8px;border-top:2px solid var(--line)}
table{border-collapse:collapse;width:100%}td{border-bottom:1px solid var(--line);padding:5px 8px;vertical-align:top}
.t{color:var(--mut);font-size:12px}.p{font-size:11px;color:var(--warn);border:1px solid var(--warn);border-radius:3px;padding:0 3px;white-space:nowrap}
a{color:var(--a)}td.n{color:var(--mut);width:28px}td.l{white-space:nowrap}.j{color:var(--mut);font-size:12px;font-weight:600;background:var(--g)}
</style></head><body>"""]
counts = {t: sum(1 for r in rows if r["tier"] == t) for t in (1, 2, 3, 4)}
parts.append("<h1>Lane download tiers</h1><p class='t'>%d stanpumpR references not yet in the archive, from the reference list "
             "(stanpumpR-references.xlsx). Tier 1: %d · Tier 2: %d · Tier 3: %d · Tier 4: %d. "
             "Save PDFs anywhere in Downloads, then run update.bat: it files each as PMID_&lt;pmid&gt;.pdf and recycles the original. "
             "Links open through the Lane proxy by DOI; papers without a DOI link to PubMed. Under a Lane link, <i>free at PMC</i> "
             "and <i>open</i> are free copies a script could not download (PMC and most publishers refuse scripts); "
             "<i>accepted manuscript</i> is the authors' version, not the typeset paper. "
             "<span class='p'>verify</span> marks a paper identified from a short-form citation (Author Year): "
             "check it is the one the code means.</p>" % (len(rows), counts[1], counts[2], counts[3], counts[4]))
n = 0
for t in (1, 2, 3, 4):
    sub = [r for r in rows if r["tier"] == t]
    parts.append("<h2>%s <span class='t'>(%d)</span></h2><table>" % (e(NAMES[t]), len(sub)))
    last_j = None
    for r in sub:
        if r["journal"] != last_j:
            parts.append("<tr><td colspan='3' class='j'>%s</td></tr>" % e(r["journal"] or "—"))
            last_j = r["journal"]
        n += 1
        flag = " <span class='p'>verify</span>" if r["grade"] != "ok" else ""
        flag += " <span class='p'>accepted ms archived</span>" if r["aam"] else ""
        free = "".join("<br><a href='%s' target='_blank' rel='noopener'>%s</a>" % (e(u), e(k)) for k, u in r["free"])
        parts.append("<tr><td class='n'>%d</td><td><b>%s</b> %s%s<br><span class='t'>%s %s:%s · PMID %s · %s%s</span></td>"
                     "<td class='l'><a href='%s' target='_blank' rel='noopener'>%s</a>%s</td></tr>" % (
                         n, e(r["label"]), e(r["title"]), flag, e(r["journal"]), e(r["vol"]), e(r["pages"]), e(r["pmid"] or "—"),
                         e(r["drugs"] or "—"), (" · " + e(r["used"])) if r["used"] else "", e(r["link"]), e(r["via"]), free))
    parts.append("</table>")
if other:
    parts.append("<h2>Not on Lane <span class='t'>(%d: labels, guidelines, books, web pages, unidentified)</span></h2><table>" % len(other))
    for g, label, cite, drugs in sorted(other, key=lambda x: (x[0], x[1])):
        parts.append("<tr><td>%s</td><td><b>%s</b> <span class='t'>%s</span><br><span class='t'>%s</span></td></tr>" % (
            e(g), e(label), e(drugs), e(cite[:200])))
    parts.append("</table>")
parts.append("</body></html>")
out = os.path.join(ARCHIVE, "lane-download-tiers.html")
open(out, "w", encoding="utf-8").write("\n".join(parts))
print(out, counts, "not on Lane:", len(other))
