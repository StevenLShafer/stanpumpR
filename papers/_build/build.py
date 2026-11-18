"""Build the stanpumpR reference archive: workbook, download queue CSV, and PDF copies."""
import csv, glob, html, json, os, re, shutil, urllib.parse
from openpyxl import Workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.table import Table, TableStyleInfo
from ncbi import esummary, norm
from holdings import build as build_holdings, SHAFER, REPRINTS

SP = os.path.dirname(os.path.abspath(__file__))
ARCHIVE = r"C:\dev\stanpumpRPapers"
os.makedirs(ARCHIVE, exist_ok=True)
BUILT = "2026-10-07"
import datetime
NOW = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

W = json.load(open(os.path.join(SP, "works.json"), encoding="utf-8"))
dropped = json.load(open(os.path.join(SP, "dropped.json"), encoding="utf-8"))
reprints = json.load(open(os.path.join(SP, "reprints_resolved.json"), encoding="utf-8"))
by_pmid, by_doi, queue = build_holdings()

NONJ = {
    "g:clsi-m100": ("CLSI", "2026", "Performance Standards for Antimicrobial Susceptibility Testing, 36th ed. CLSI supplement M100", "Clinical and Laboratory Standards Institute", "https://clsi.org/"),
    "g:eucast-breakpoints": ("EUCAST", "2026", "Breakpoint tables for interpretation of MICs and zone diameters, version 16.1", "EUCAST", "https://www.eucast.org/clinical_breakpoints"),
    "g:eucast-ecoff": ("EUCAST", "2026", "EUCAST guidance on cephalosporins for S. aureus infections; MIC distributions and ECOFFs (cefazolin, clindamycin)", "EUCAST", "https://mic.eucast.org/"),
    "g:fda-stic": ("FDA", "", "Antibacterial Susceptibility Test Interpretive Criteria (STIC): clindamycin, gentamicin", "U.S. Food and Drug Administration (web page)", "https://www.fda.gov/drugs/development-resources/antibacterial-susceptibility-test-interpretive-criteria"),
    "g:label-keflex": ("Keflex label", "", "Keflex (cephalexin) prescribing information, section 12.3 (protein binding)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=keflex"),
    "g:label-numorphan": ("Endo", "2000", "NUMORPHAN (oxymorphone) prescribing information. Physicians' Desk Reference, 54th ed.", "Physicians' Desk Reference (book)", ""),
    "g:label-narcan": ("Narcan label", "", "NARCAN (naloxone HCl) nasal spray prescribing information (AUC, Cmax, bioavailability relative to IM)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=narcan+nasal"),
    "g:fda-review-naloxone": ("FDA", "", "FDA clinical pharmacology review of naloxone nasal spray (product and NDA not named in the repo)", "FDA review", "https://www.accessdata.fda.gov/scripts/cder/daf/"),
    "g:fda-review-nda202880": ("FDA", "2013", "Clinical pharmacology and biopharmaceutics review, NDA 202880 (hydrocodone ER); numerical model on p.93", "FDA review", None),
    "g:label-opana": ("Oxymorphone label", "", "Oral oxymorphone prescribing information (product not named; oral F 0.10)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=oxymorphone"),
    "g:label-bridion": ("Bridion label", "", "BRIDION (sugammadex) prescribing information (renal impairment; dose as sugammadex)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=bridion"),
    "g:label-vancomycin": ("Vancomycin label", "", "Vancomycin injection prescribing information (protein binding ~55%)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=vancomycin+injection"),
    "g:label-tramadol": ("Tramadol label", "", "Tramadol prescribing information (oral F 0.70 capsules; IR tablet 0.75; ER relative F)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=tramadol"),
    "g:label-hydromorphone": ("Hydromorphone label", "", "Hydromorphone injection prescribing information (same IM and IV dose)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=hydromorphone+injection"),
    "g:label-mannitol": ("Mannitol label", "", "Mannitol injection prescribing information (half-life up to 36 h in renal failure)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=mannitol"),
    "g:label-methylprednisolone": ("Methylprednisolone label", "", "Methylprednisolone tablets prescribing information (time to peak 1-2 h)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=methylprednisolone+tablets"),
    "g:label-metronidazole": ("Metronidazole label", "", "Metronidazole prescribing information (<20% protein bound)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=metronidazole"),
    "g:label-glycopyrrolate": ("Glycopyrrolate label", "", "Glycopyrrolate injection prescribing information (onset; dose as glycopyrronium bromide)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=glycopyrrolate+injection"),
    "g:holford2014": ("Holford S", "2014", "Parent-metabolite pharmacokinetic models for tramadol", "J Pharmacol Clin Toxicol 2014;2(1):1023 (not PubMed-indexed)", None),
    "g:james1976": ("James WPT", "1976", "Research on Obesity: A Report of the DHSS/MRC Group", "London: HMSO (book)", ""),
    "g:apsf2022": ("Feldman JM", "2022", "Is rebreathing prevented when FGF equals MV?", "APSF Newsletter, 20 October 2022 (web article)", None),
    "g:oxymorphone-crossover-abstract": ("Unidentified", "", "Crossover abstract reporting oral oxymorphone bioavailability 3.8-7.2%", "Abstract (not identified in repo)", ""),
    "a:moen:2013:": ("Moen", "2013", "'Moen 2013 review' (ketamine analgesic concentrations 0.1-0.16 mcg/mL)", "Not found", ""),
    "g:numerical-recipes": ("Press WH", "", "Numerical Recipes (rtsafe root finder); edition not stated in the repo", "Cambridge University Press (book)", ""),
    "g:label-amiodarone-iv": ("Amiodarone IV label", "", "Intravenous amiodarone prescribing information (first-24-hour loading regimen)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=amiodarone+injection"),
    "g:label-amiodarone": ("Amiodarone label", "", "Oral amiodarone product monograph / prescribing information (loading regimens)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=amiodarone+tablets"),
    "g:label-acetaminophen": ("Acetaminophen label", "", "Acetaminophen labelling (maximum 4 g a day)", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=acetaminophen"),
    "g:label-ambien": ("Ambien label", "", "Ambien (zolpidem tartrate) prescribing information", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=ambien"),
    "g:label-ativan": ("Ativan label", "", "Ativan (lorazepam) injection and tablets prescribing information", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=ativan"),
    "g:label-klonopin": ("Klonopin label", "", "Klonopin (clonazepam) prescribing information", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=klonopin"),
    "g:label-lyrica": ("Lyrica label", "", "Lyrica (pregabalin) US prescribing information, section 12.3", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=lyrica"),
    "g:label-neurontin": ("Neurontin label", "", "Neurontin (gabapentin) US prescribing information, section 12.3", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=neurontin"),
    "g:label-restoril": ("Restoril label", "", "Restoril (temazepam) prescribing information", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=restoril"),
    "g:label-xanax": ("Xanax label", "", "Xanax (alprazolam) prescribing information", "Product label", "https://dailymed.nlm.nih.gov/dailymed/search.cfm?labeltype=all&query=xanax"),
    "g:fda-zolpidem-2013": ("FDA", "2013", "Drug Safety Communication on zolpidem (next-morning impairment; 50 ng/mL driving level), 10 January 2013", "U.S. Food and Drug Administration (web page)", "https://www.fda.gov/drugs/drug-safety-and-availability"),
    "g:fda-gabapentinoids-2019": ("FDA", "2019", "Drug Safety Communication, December 2019 (gabapentinoids and respiratory depression)", "U.S. Food and Drug Administration (web page)", "https://www.fda.gov/drugs/drug-safety-and-availability"),
}

PMC_LOG = {e["pmid"]: e for e in json.load(open(os.path.join(SP, "pmc_fetch_log.json"), encoding="utf-8"))} if os.path.exists(os.path.join(SP, "pmc_fetch_log.json")) else {}
FILED = {e["to"]: e["from"] for e in json.load(open(os.path.join(SP, "filing_log.json"), encoding="utf-8"))} if os.path.exists(os.path.join(SP, "filing_log.json")) else {}
NJ_MAN = json.load(open(os.path.join(SP, "nonjournal_manifest.json"), encoding="utf-8")) if os.path.exists(os.path.join(SP, "nonjournal_manifest.json")) else {}
DOI_FILL = json.load(open(os.path.join(SP, "doi_fill.json"), encoding="utf-8"))
CONF = {"ok": "Verified", "probable": "Probable (inferred from short form)", "check": "Verify (candidate only)",
        "unresolved": "Unresolved", "non-journal": "Non-journal source"}

rows = []
for w in W:
    pm = w["pm"] or {}
    cr = w["cr"] or {}
    k = w["key"]
    pmid = pm.get("pmid", "")
    doi = pm.get("doi") or cr.get("doi") or w.get("doi") or ""
    urls = [r.get("url") for r in w["records"] if r.get("url")]
    if not doi and pmid in DOI_FILL:
        doi = DOI_FILL[pmid]
        w["notes"].append("DOI from Crossref (PubMed record has none); matched on volume and first page.")
    if pm:
        fa = pm["first_author"]; yr = pm["year"]; ti = pm["title"]; jo = pm["journal"]
        vo, iss, pg = pm["volume"], pm["issue"], pm["pages"]
        authors = ", ".join(pm["authors"][:6]) + (", et al." if len(pm["authors"]) > 6 else "")
    elif cr:
        fa = cr["first_author"]; yr = cr["year"]; ti = cr["title"]; jo = cr["journal"]
        vo, iss, pg = cr.get("volume", ""), cr.get("issue", ""), cr.get("pages", "")
        authors = ", ".join((cr.get("authors") or [])[:6])
        if k == "doi:10.1177/106002807400801104":
            fa, ti, authors = "Devine BJ", "Gentamicin therapy (Devine ideal body weight)", "Devine BJ"
        if k.startswith("g:dubois") or "archinte.1916" in k:
            fa, authors = "Du Bois D", "Du Bois D, Du Bois EF"
            ti = "Clinical calorimetry, tenth paper: a formula to estimate the approximate surface area if height and weight be known"
            jo, vo, pg = "Arch Intern Med", "17", "863-871"
        fa, ti, jo = html.unescape(fa or ""), html.unescape(ti or ""), html.unescape(jo or "")
    else:
        d = NONJ.get(k)
        fa, yr, ti, jo = (d[0], d[1], d[2], d[3]) if d else (w["first_author"] or "", w["year"] or "", w["title"] or "", w["journal"] or "")
        vo, iss, pg, authors = "", "", "", ""
        if d and d[4]:
            urls = [d[4]] + urls
    kind = w["kind"] or ""
    if pm and "Review" in (pm.get("pubtype") or []):
        kind = "journal-article (review)"
    conf = CONF.get(w["grade"], w["grade"])
    # local copies
    hits = list(by_pmid.get(pmid, [])) if pmid else []
    if doi:
        for e in by_doi.get(doi.lower(), []):
            if e not in hits:
                hits.append(e)
    qrow = queue.get(pmid) if pmid else None
    archive_name = ("PMID_%s.pdf" % pmid) if pmid else (("DOI_" + re.sub(r"[^A-Za-z0-9.]+", "_", doi) + ".pdf") if doi else "")
    local = ""
    if hits:
        e = hits[0]
        local = "%s\\%s" % (e["source"], os.path.basename(e["path"]))
        dst = os.path.join(ARCHIVE, archive_name)
        if not os.path.exists(dst):
            shutil.copy2(e["path"], dst)
    in_archive = bool(archive_name) and os.path.exists(os.path.join(ARCHIVE, archive_name))
    # non-journal sources (labels, FDA reviews, guidelines, web pages) are archived as NJ_*.pdf, listed in
    # nonjournal_manifest.json by group (fda_fetch.py, web_fetch.py)
    nj = [e for e in NJ_MAN.get(k[2:] if k.startswith("g:") else k[2:], []) if os.path.exists(os.path.join(ARCHIVE, e["file"]))] if not pmid and not doi else []
    if nj:
        in_archive = True
        archive_name = "; ".join(e["file"] for e in nj)
        local = "; ".join(e["url"] for e in nj if e.get("url"))
        w["notes"] += [e["note"] for e in nj if e.get("note")]
    if in_archive and not local and archive_name in FILED:
        local = os.path.join("Downloads", os.path.basename(FILED[archive_name]))
    if in_archive:
        status = "In archive"
    elif w["grade"] == "non-journal":
        status = "Obtain separately (non-journal)"
    elif w["grade"] == "unresolved" or (not pmid and not doi):
        status = "Unresolved"
    elif pm.get("pmcid"):
        status = "To download (free at PMC)"
    else:
        status = "To download"
    notes = list(w["notes"]) + w.get("repo_errors", [])
    pl = PMC_LOG.get(pmid)
    if pl:
        if pl["result"].startswith("downloaded"):
            local = "PMC Article Datasets (%s)" % (pl.get("license") or "licence not stated")
        elif pl["result"].startswith("not in PMC"):
            status = "To download (free at PMC, by hand)" if not in_archive else status
            notes.append("Free to read at PMC but not in the PMC Article Datasets, so not fetched by script; save the PDF from the PMC page by hand.")
        elif "no PDF" in pl["result"]:
            status = "To download (free at PMC, by hand)" if not in_archive else status
            notes.append("PMC Article Datasets hold only XML/text for this author manuscript; save the PDF from the PMC page or the publisher.")
    if qrow:
        notes.append("In ShaferPapers\\shafer-queue.csv (status: %s)." % qrow["status"])
    rows.append({
        "First author": fa, "Year": str(yr or ""), "Title": ti, "Journal / source": jo, "Volume": vo, "Issue": iss, "Pages": pg,
        "PMID": pmid, "DOI": doi, "PMCID": pm.get("pmcid", ""),
        "Archive status": status, "Local copy found": local, "Archive file": archive_name if in_archive or pmid or doi else "",
        "Confidence": conf, "How identified": "; ".join(w["methods"]), "Kind": kind,
        "Drugs / topics": ", ".join(w["drugs"]), "Used for": " | ".join(w["used_for"]),
        "Cited at (file:line)": "\n".join(w["locations"]), "Citation as written in repo": w["citation_text"],
        "Notes": "\n".join(dict.fromkeys(n for n in notes if n)), "Authors": authors,
        "_url": urls[0] if urls else "", "_journal_full": pm.get("journal_full", ""), "_srcs": w["srcs"],
    })

rows.sort(key=lambda r: (r["Confidence"] == "Non-journal source", norm(r["First author"]), r["Year"]))
for i, r in enumerate(rows, 1):
    r["Ref"] = "R%03d" % i

# ---------------------------------------------------------------- workbook
F = "Arial"
HDR_FILL = PatternFill("solid", fgColor="1F3864")
HDR_FONT = Font(name=F, bold=True, color="FFFFFF", size=10)
BODY = Font(name=F, size=10)
LINK = Font(name=F, size=10, color="0563C1", underline="single")
WRAP = Alignment(wrap_text=True, vertical="top")
TOP = Alignment(vertical="top")
STATUS_FILL = {"In archive": "E2EFDA", "To download (free at PMC)": "DDEBF7", "To download (free at PMC, by hand)": "DDEBF7", "To download": "FFFFFF",
               "Obtain separately (non-journal)": "F2F2F2", "Unresolved": "FCE4D6"}
CONF_FILL = {"Verify (candidate only)": "FFF2CC", "Probable (inferred from short form)": "FFF2CC", "Unresolved": "FCE4D6"}


def sheet(ws, headers, data, widths, wrap_cols=(), link_cols=(), table_name=None):
    ws.append(headers)
    for c in range(1, len(headers) + 1):
        cell = ws.cell(row=1, column=c)
        cell.fill, cell.font = HDR_FILL, HDR_FONT
        cell.alignment = Alignment(wrap_text=True, vertical="center")
    for row in data:
        ws.append([row.get(h, "") for h in headers])
    for ri in range(2, ws.max_row + 1):
        for ci, h in enumerate(headers, 1):
            cell = ws.cell(row=ri, column=ci)
            cell.font = BODY
            cell.alignment = WRAP if h in wrap_cols else TOP
            if h in link_cols and isinstance(cell.value, str) and cell.value.startswith("http"):
                cell.hyperlink = cell.value
                cell.font = LINK
    for i, wdt in enumerate(widths, 1):
        ws.column_dimensions[get_column_letter(i)].width = wdt
    ws.freeze_panes = "B2"
    ws.row_dimensions[1].height = 30
    if table_name and data:
        t = Table(displayName=table_name, ref="A1:%s%d" % (get_column_letter(len(headers)), ws.max_row))
        t.tableStyleInfo = TableStyleInfo(name="TableStyleLight1", showRowStripes=False)
        ws.add_table(t)


wb = Workbook()
summary = wb.active
summary.title = "Summary"

# References sheet
ref_hdr = ["Ref", "Archive status", "Confidence", "First author", "Year", "Title", "Journal / source", "Volume", "Issue", "Pages",
           "PMID", "DOI", "PMCID", "PubMed", "DOI link", "Lane proxy", "Free full text / source URL", "Local copy found", "Archive file",
           "Drugs / topics", "Used for", "Cited at (file:line)", "Citation as written in repo", "How identified", "Kind", "Authors", "Notes"]
for r in rows:
    r["PubMed"] = "https://pubmed.ncbi.nlm.nih.gov/%s/" % r["PMID"] if r["PMID"] else ""
    r["DOI link"] = "https://doi.org/%s" % r["DOI"] if r["DOI"] else ""
    r["Lane proxy"] = "https://doi-org.laneproxy.stanford.edu/%s" % r["DOI"] if r["DOI"] else ""
    r["Free full text / source URL"] = ("https://pmc.ncbi.nlm.nih.gov/articles/%s/" % r["PMCID"]) if r["PMCID"] else r["_url"]
ws = wb.create_sheet("References")
sheet(ws, ref_hdr, rows,
      [6, 16, 18, 16, 6, 50, 22, 7, 6, 10, 10, 26, 11, 14, 14, 14, 18, 26, 18, 18, 40, 28, 45, 26, 16, 30, 45],
      wrap_cols={"Title", "Used for", "Cited at (file:line)", "Citation as written in repo", "Notes", "How identified", "Authors", "Journal / source", "Drugs / topics"},
      link_cols={"PubMed", "DOI link", "Lane proxy", "Free full text / source URL"}, table_name="References")
for ri in range(2, ws.max_row + 1):
    st = ws.cell(row=ri, column=2).value
    if st in STATUS_FILL:
        ws.cell(row=ri, column=2).fill = PatternFill("solid", fgColor=STATUS_FILL[st])
    cf = ws.cell(row=ri, column=3).value
    if cf in CONF_FILL:
        ws.cell(row=ri, column=3).fill = PatternFill("solid", fgColor=CONF_FILL[cf])
    for col in (14, 15, 16):  # show short text for long link columns
        c = ws.cell(row=ri, column=col)
        if c.value:
            c.value = {14: "PubMed", 15: "doi.org", 16: "Lane"}[col]

# Download queue: free first, then subscription grouped by journal, then DOI-only, then non-journal
dl = [r for r in rows if r["Archive status"].startswith("To download")]
dl.sort(key=lambda r: (not r["Archive status"].startswith("To download (free at PMC"), not r["PMID"], norm(r["Journal / source"]), r["Year"]))
other = [r for r in rows if r["Archive status"] in ("Obtain separately (non-journal)", "Unresolved")]
for r in dl + other:
    r["Get it from"] = (r["Free full text / source URL"] if r["Archive status"].startswith("To download (free at PMC")
                        else r["Lane proxy"] or r["PubMed"] or r["_url"] or "")
q_hdr = ["Ref", "Archive status", "Journal / source", "Year", "Volume", "Pages", "First author", "Title", "PMID", "DOI", "Get it from", "Archive file", "Confidence", "Notes"]
wq = wb.create_sheet("Download queue")
sheet(wq, q_hdr, dl + other, [6, 18, 24, 6, 7, 10, 16, 50, 10, 28, 40, 18, 18, 50],
      wrap_cols={"Title", "Notes", "Journal / source"}, link_cols={"Get it from"}, table_name="DownloadQueue")
for ri in range(2, wq.max_row + 1):
    st = wq.cell(row=ri, column=2).value
    if st in STATUS_FILL:
        wq.cell(row=ri, column=2).fill = PatternFill("solid", fgColor=STATUS_FILL[st])

# Local files
ref_by_pmid = {r["PMID"]: r["Ref"] for r in rows if r["PMID"]}
sp_pmids = [re.search(r"PMID_(\d+)", p).group(1) for p in sorted(glob.glob(os.path.join(SHAFER, "PMID_*.pdf")))]
S = esummary(sp_pmids + [x["pmid"] for x in reprints if x.get("pmid")])
loc = []
for x in reprints:
    s = S.get(x.get("pmid") or "", {})
    cite = "%s %s. %s %s;%s:%s" % (s.get("first_author", ""), s.get("year", ""), s.get("journal", ""), s.get("year", ""), s.get("volume", ""), s.get("pages", "")) if s else ""
    loc.append({"Folder": "Reprints", "File": x["file"], "PMID": x.get("pmid") or "", "Identified as": cite, "Title": s.get("title", ""),
                "Cited by stanpumpR": ref_by_pmid.get(x.get("pmid") or "", ""), "Copied to archive as": ("PMID_%s.pdf" % x["pmid"]) if x.get("pmid") in ref_by_pmid else "",
                "Notes": x.get("note", "") + (" Same file as ShaferPapers\\PMID_11818774.pdf." if x["file"] == "Interaction Trial Design.pdf" else "")})
for p in sp_pmids:
    s = S.get(p, {})
    loc.append({"Folder": "ShaferPapers", "File": "PMID_%s.pdf" % p, "PMID": p,
                "Identified as": "%s %s. %s;%s:%s" % (s.get("first_author", ""), s.get("year", ""), s.get("journal", ""), s.get("volume", ""), s.get("pages", "")),
                "Title": s.get("title", ""), "Cited by stanpumpR": ref_by_pmid.get(p, ""),
                "Copied to archive as": ("PMID_%s.pdf" % p) if p in ref_by_pmid else "", "Notes": ""})
for qp, qrow in queue.items():
    if qp.startswith("doi:") or qp not in ref_by_pmid or qp in sp_pmids:
        continue
    loc.append({"Folder": "ShaferPapers queue", "File": "(not yet downloaded)", "PMID": qp,
                "Identified as": "%s %s;%s:%s" % (qrow["journal"], qrow["year"], qrow["volume"], qrow["pages"]), "Title": "",
                "Cited by stanpumpR": ref_by_pmid[qp], "Copied to archive as": "", "Notes": "shafer-queue.csv status: %s" % qrow["status"]})
loc.sort(key=lambda x: (x["Folder"], x["Cited by stanpumpR"] == "", x["File"]))
wl = wb.create_sheet("Local files")
sheet(wl, ["Folder", "File", "PMID", "Identified as", "Title", "Cited by stanpumpR", "Copied to archive as", "Notes"], loc,
      [18, 42, 10, 40, 60, 12, 20, 40], wrap_cols={"Title", "Notes", "Identified as"}, table_name="LocalFiles")

# Repo citation errors
errs = [
    ("R/drugs_codeine.R:245", "Wrong PMID", "pubmed.ncbi.nlm.nih.gov/39300028 (a 2024 retraction note in Environ Sci Pollut Res Int)", "PMID 39441506"),
    ("R/drugs_codeine.R:142, :244", "Wrong pages", "Clin Pharmacokinet 2024;63:1377-1391", "Clin Pharmacokinet 2024;63(11):1547-1560 (the DOI at :143, 10.1007/s40262-024-01433-9, is correct)"),
    ("inst/help/references.md:19", "Wrong authors, title and pages", "Ashraf MW, Mndala BM, Rao RN, et al. Pharmacometric modelling of codeine and morphine disposition in a CYP2D6-stratified population. Clin Pharmacokinet 2024;63:1377-1391.",
     "Ashraf MW, Poikola S, Neuvonen M, et al. Population pharmacokinetic quantification of CYP2D6 activity in codeine metabolism in ambulatory surgical patients for model-informed precision dosing. Clin Pharmacokinet 2024;63:1547-1560."),
    ("inst/help/drugs/codeine.md:17", "Wrong pages", "Clin Pharmacokinet 2024;63:1377-1391", "Clin Pharmacokinet 2024;63:1547-1560"),
    ("R/drugs_tramadol.R:198", "Wrong PMID", "pubmed.ncbi.nlm.nih.gov/25014373 (an IET Syst Biol editorial)", "PMID 25014367 (Brvar N et al., Int J Pharm 2014;473:170-178)"),
    ("docs/weight-adjustment.md:261", "Wrong PMID", "PMID 36794347 (Pan C 2023, JAK inhibitors and hepatitis B)", "PMID 36691877 (O'Hanlon CJ et al., CPT Pharmacometrics Syst Pharmacol 2023;12:401-412)"),
    ("docs/weight-adjustment.md:268", "Wrong PMID", "PMID 28506869 (Voller S 2017, phenobarbital in neonates)", "PMID 28552478 (Holford NHG, Anderson BJ, Eur J Pharm Sci 2017;109S:S59-S64)"),
    ("R/drugs_fentanyl.R:8", "Wrong journal, year and pages", "2 = Scott and Stanski, Anesthesiology. 1990;73:1091-102. (the citation of source 5, Shafer 1990, at :11)", "2 = Scott and Stanski, J Pharmacol Exp Ther. 1987;240:159-66. (PMID 3100765, the paper the fentanyl reference string cites)"),
    ("inst/help/references.md:77", "Title belongs to another paper", "Adams MP, Ahdieh H. Pharmacokinetics and dose-proportionality of oxymorphone extended release and its metabolites. Drugs R D 2005;6:91-99.",
     "Title of Drugs R D 2005;6:91-99 (PMID 15777102): 'Single- and multiple-dose pharmacokinetic and dose-proportionality study of oxymorphone immediate-release tablets.' The title given is that of Adams & Ahdieh, Pharmacotherapy 2004;24:468-476 (PMID 15098800)."),
    ("inst/help/references.md:71", "Paraphrased title", "Population pharmacokinetics of an extended-release hydrocodone formulation.", "Population pharmacokinetic analysis for hydrocodone following the administration of hydrocodone bitartrate extended-release capsules. (PMID 23719682)"),
    ("inst/help/references.md:73", "Paraphrased title", "Effects of paroxetine on the pharmacokinetics of hydrocodone and its metabolites.", "Effects of paroxetine, a CYP2D6 inhibitor, on the pharmacokinetic properties of hydrocodone after coadministration with a single-entity, once-daily, extended-release hydrocodone tablet. (PMID 26350273)"),
    ("R/drugs_ketamine.R:14-16", "Source not found", "'Moen 2013 review'", "No Moen 2013 ketamine review in PubMed. Possibly Mion G, Villevieille T, CNS Neurosci Ther 2013;19:370-80 (PMID 23575437); unconfirmed."),
    ("R/drugs_dexmedetomidine.R:195", "Incomplete reference string (shown in app)", "Zuppa BJA 2019", "Zuppa AF et al., Br J Anaesth 2019;123:839-852. PMID 31623840 (identified from the help page's description; probable)."),
    ("R/drugs_oxytocin.R:37", "Incomplete reference string (shown in app)", "Tanaka et al", "Not identified. Weak candidate: Tanaka A et al., Mol Pharm 2018;15:1105-1111 (PMID 29338251)."),
]
UNFIXED = {"R/drugs_ketamine.R:14-16", "R/drugs_oxytocin.R:37"}
erows = []
for e in errs:
    d = dict(zip(["Location", "Problem", "Repo says", "Should be"], e))
    ON_MASTER = {"R/drugs_codeine.R:142, :244", "inst/help/references.md:19", "inst/help/drugs/codeine.md:17"}
    d["Status"] = ("Not fixed: source unknown" if e[0] in UNFIXED
                   else "Fixed on master (ba813bc, 2026-10-09)" if e[0] in ON_MASTER
                   else "Fixed on master (PR #448, 8f3f169, 2026-10-10)")
    erows.append(d)
we = wb.create_sheet("Repo citation errors")
sheet(we, ["Location", "Problem", "Repo says", "Should be", "Status"], erows,
      [30, 24, 60, 70, 30], wrap_cols={"Repo says", "Should be", "Problem", "Location", "Status"}, table_name="RepoErrors")

# Excluded
exc = [{"Item": r.get("citation_text") or r.get("first_author"), "Where": "\n".join(r.get("locations") or []), "Why excluded": r.get("_drop")} for r in dropped]
exc += [
    {"Item": "Gemini-commissioned research summary on opioid MAC reduction", "Where": "R/opioidMacInteraction.R", "Why excluded": "Unpublished research summary, not a paper"},
    {"Item": "Gas Man (software), source code and API README", "Where": "R/gas*.R, inst/validation", "Why excluded": "Software; the Gas Man paper (Philip 1986) is in References"},
    {"Item": "'opioid simulation spreadsheet' / t_peaks.xls", "Where": "R/drugs_remifentanil.R; help pages", "Why excluded": "Unpublished spreadsheet; the Shafer & Varvel 1991 analysis it supports is in References"},
    {"Item": "HIPAA Safe Harbor rule", "Where": "inst/help", "Why excluded": "Regulation, not a literature source"},
    {"Item": "STANPUMP (software); Vieta's relations (mathematics)", "Where": "R/cube.R, help pages", "Why excluded": "Not literature sources"},
    {"Item": "'older small study' (glycopyrrolate); 'older ex vivo ultrafiltration studies' (clindamycin)", "Where": "inst/help/drugs/glycopyrrolate.md:19; R/drugs_clindamycin.R:60", "Why excluded": "No author, year or venue given"},
    {"Item": "McEwan AI et al. 1993 (isoflurane MAC reduction by fentanyl)", "Where": "R/opioidMacInteraction.R (Brunner 1994 record)", "Why excluded": "Not cited; noted as the likely primary source behind 'Brunner 1994 (cited)'. Consider adding."},
    {"Item": "Original Stanpump/Stanpump.zip (legacy C source)", "Where": "Original Stanpump/", "Why excluded": "Predecessor program, not scanned for citations"},
]
wx = wb.create_sheet("Excluded")
sheet(wx, ["Item", "Where", "Why excluded"], exc, [60, 45, 60], wrap_cols={"Item", "Where", "Why excluded"})

# Summary (formulas over the References table)
summary["A1"] = "stanpumpR reference archive"
summary["A1"].font = Font(name=F, bold=True, size=14)
summary["A2"] = "Built %s from the stanpumpR repository (master @ 72d079b): R/, inst/help/, docs/, vignettes/, tests/, inst/validation/." % BUILT
summary["A3"] = "Archive folder: %s  (PDFs named PMID_<pmid>.pdf, as in ShaferPapers)" % ARCHIVE
lines = [("Distinct works cited", '=COUNTA(References[Ref])'),
         ("  In archive (PDF on disk)", '=COUNTIF(References[Archive status],"In archive")'),
         ("  To download, free at PMC (save by hand)", '=COUNTIF(References[Archive status],"To download (free at PMC*")'),
         ("  To download (Lane proxy)", '=COUNTIF(References[Archive status],"To download")'),
         ("  Non-journal (labels, guidelines, books, web)", '=COUNTIF(References[Archive status],"Obtain separately (non-journal)")'),
         ("  Unresolved", '=COUNTIF(References[Archive status],"Unresolved")'),
         ("With a PMID", '=COUNTIF(References[PMID],"?*")'),
         ("With a DOI", '=COUNTIF(References[DOI],"?*")'),
         ("Confidence: verified", '=COUNTIF(References[Confidence],"Verified")'),
         ("Confidence: probable (inferred from short form)", '=COUNTIF(References[Confidence],"Probable*")'),
         ("Confidence: verify (candidate only)", '=COUNTIF(References[Confidence],"Verify*")'),
         ("Repo citation errors found", '=COUNTA(RepoErrors[Location])')]
for i, (lab, f) in enumerate(lines, 5):
    summary.cell(row=i, column=1, value=lab).font = Font(name=F, size=11, bold=not lab.startswith("  "))
    summary.cell(row=i, column=2, value=f).font = Font(name=F, size=11)
r0 = 5 + len(lines) + 1
notes = [
    "Sheets",
    "References: one row per distinct work, with every file:line where stanpumpR cites it.",
    "Download queue: what is still missing; free PMC copies first, then subscription papers grouped by journal for a Lane proxy sweep, then non-journal sources.",
    "Local files: every PDF in Reprints and ShaferPapers, identified and marked if stanpumpR cites it.",
    "Repo citation errors: wrong PMIDs, titles and pages found in the stanpumpR source while resolving.",
    "Excluded: things in the code that look like citations but are not papers.",
    "",
    "Confidence",
    "Verified: PMID/DOI given in the repo, or journal/volume/page matched in PubMed, with first author and year agreeing.",
    "Probable: the repo gives only 'Author Year'; the paper was identified by search on author, year and drug. Check before relying on it.",
    "Verify: a weak candidate only.",
    "",
    "stanpumpr-queue.csv in the archive folder uses the same columns as ShaferPapers\\shafer-queue.csv.",
]
for i, t in enumerate(notes):
    c = summary.cell(row=r0 + i, column=1, value=t)
    c.font = Font(name=F, size=10, bold=t in ("Sheets", "Confidence"))
summary.column_dimensions["A"].width = 52
summary.column_dimensions["B"].width = 10
wb.calculation.fullCalcOnLoad = True
out = os.path.join(ARCHIVE, "stanpumpR-references.xlsx")
LOCKED = []
try:
    wb.save(out)
except PermissionError:
    LOCKED.append(out)

# Queue CSV in the shafer-queue.csv format
import io
qp = os.path.join(ARCHIVE, "stanpumpr-queue.csv")
old = {}
if os.path.exists(qp):
    with open(qp, encoding="utf-8") as f:
        old = {row["pmid"] or row["doi"]: row for row in csv.DictReader(f)}
buf = io.StringIO(newline="")
if True:
    wr = csv.writer(buf)
    wr.writerow(["pmid", "doi", "journal", "year", "volume", "pages", "download_link", "status", "downloaded_at", "notes"])
    for r in sorted((r for r in rows if r["PMID"] or r["DOI"]), key=lambda r: (r["Archive status"] != "In archive", r["Year"])):
        key = r["PMID"] or r["DOI"]
        o = old.get(key, {})
        status = "done" if r["Archive status"] == "In archive" else (o.get("status") or "pending")
        if status == "done" and not o.get("downloaded_at"):
            o["downloaded_at"] = NOW
        link = r["Lane proxy"] or r["PubMed"]
        note = ("from " + r["Local copy found"]) if r["Local copy found"] else ("free at PMC %s" % r["PMCID"] if r["PMCID"] else "")
        wr.writerow([r["PMID"], r["DOI"], r["_journal_full"] or r["Journal / source"], r["Year"], r["Volume"], r["Pages"], link, status,
                     o.get("downloaded_at", ""), note])
    # Rows no citation produced: papers a registry's `to_obtain` asks for, put here by
    # arrivals.py.  This writer rebuilds the queue from the works the stanpumpR SOURCE
    # cites, so without this they would be dropped on the next run -- and a request
    # nobody has filled yet is precisely what must not evaporate.  A paper can be wanted
    # by a model before anything in the code cites it; that is the normal case for a
    # registry row whose readiness is `needs_full_text`.
    # Marked by the notes prefix: keep the literal in step with REQUEST_NOTE in arrivals.py.
    written = {r["PMID"] or r["DOI"] for r in rows if r["PMID"] or r["DOI"]}
    for key, o in old.items():
        if key in written or not (o.get("notes") or "").startswith("wanted by "):
            continue
        wr.writerow([o.get("pmid", ""), o.get("doi", ""), o.get("journal", ""), o.get("year", ""),
                     o.get("volume", ""), o.get("pages", ""), o.get("download_link", ""),
                     o.get("status") or "pending", o.get("downloaded_at", ""), o.get("notes", "")])
# Write the whole CSV, then swap it in: the queue is on a synced drive, and writing in place
# meant a crash or a full disk mid-write left it truncated -- and this file is the only record
# of what has been asked for but not yet obtained (CodeRabbit on PR #780, 2026-11-18).
_qtmp = qp + ".new"
try:
    with open(_qtmp, "w", newline="", encoding="utf-8") as f:
        f.write(buf.getvalue())
    os.replace(_qtmp, qp)                        # atomic within the volume
except PermissionError:
    if os.path.exists(_qtmp):
        os.remove(_qtmp)
    LOCKED.append(qp)

from collections import Counter
for p in LOCKED:
    print("NOT UPDATED: %s is open in another program (probably Excel). Close it and run update.bat again." % p)
print(out)
print(Counter(r["Archive status"] for r in rows))
print(Counter(r["Confidence"] for r in rows))
print("PMID:", sum(1 for r in rows if r["PMID"]), "DOI:", sum(1 for r in rows if r["DOI"]), "rows:", len(rows))
