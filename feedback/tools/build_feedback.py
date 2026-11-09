"""Build feedback material from the AI-ready tables of ClimMob-analysis.

    python build_feedback.py <ai-ready folder> <trial.json> <output folder> [--png]

Reads trial-summary.csv, results-by-trait.csv, participants.csv and
participant-answers.csv, plus trial.json with what the tables do not hold
(names, contacts, website, QR links, signatories, optional items table).
Writes feedback.json (all the data the templates receive), one HTML and PDF
per product, and PNG previews with --png.

Same data contract as render.R, so either renderer can be used.
"""
import os, sys, json, re, base64, csv, html
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from mustache import render

HERE = os.path.dirname(os.path.abspath(__file__))
TPL = os.path.join(os.path.dirname(HERE), "templates")
TEXT = os.path.join(os.path.dirname(HERE), "text")
L = ["A", "B", "C"]


def read_csv(path):
    with open(path, encoding="utf-8") as f:
        return list(csv.DictReader(f))


def num(x, default=None):
    try:
        return float(x)
    except (TypeError, ValueError):
        return default


def truthy(x):
    return str(x).strip().lower() in ("true", "1", "yes", "t")


def fmt(s, **kw):
    """{name} substitution that leaves unknown braces alone."""
    for k, v in kw.items():
        s = s.replace("{" + k + "}", str(v))
    return s


def qr_data_uri(text):
    """QR code as an SVG data URI (reportlab's encoder, no extra package)."""
    from reportlab.graphics.barcode import qrencoder
    for v in range(1, 20):
        try:
            q = qrencoder.QRCode(v, qrencoder.QRErrorCorrectLevel.M)
            q.addData(text); q.make(); break
        except Exception:
            continue
    n = q.getModuleCount(); b = 2
    path = "".join(f"M{c + b},{r + b}h1v1h-1z" for r in range(n) for c in range(n) if q.isDark(r, c))
    svg = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {n + 2 * b} {n + 2 * b}" shape-rendering="crispEdges">'
           f'<rect width="100%" height="100%" fill="#fff"/><path d="{path}" fill="#000"/></svg>')
    return "data:image/svg+xml;base64," + base64.b64encode(svg.encode()).decode()


def file_data_uri(path):
    ext = path.rsplit(".", 1)[-1].lower()
    mime = {"png": "image/png", "jpg": "image/jpeg", "jpeg": "image/jpeg", "svg": "image/svg+xml"}[ext]
    return f"data:{mime};base64," + base64.b64encode(open(path, "rb").read()).decode()


def split_trait(name):
    """'Yield [1st cut]' -> ('Yield', '1st cut')."""
    m = re.match(r"^(.*?)\s*\[(.*)\]\s*$", name)
    return (m.group(1), m.group(2)) if m else (name, "")


def symbol(worth, mean):
    """Chance that the item beats an average item, as a symbol."""
    p = worth / (worth + mean) if worth + mean > 0 else 0.5
    if p > 0.70: return "++", "pp"
    if p > 0.58: return "+", "p"
    if p < 0.30: return "−−", "mm"
    if p < 0.42: return "−", "m"
    return "·", "z"


def join_and(items, and_word):
    items = [str(i) for i in items]
    if len(items) <= 1: return "".join(items)
    return ", ".join(items[:-1]) + f" {and_word} " + items[-1]


def build(ai_dir, trial_json):
    T = json.load(open(trial_json, encoding="utf-8"))
    lang = T.get("language", "en")
    t = json.load(open(os.path.join(TEXT, f"{lang}.json"), encoding="utf-8"))
    t.update(T.get("text", {}))  # trial-specific overrides
    and_word = {"nb": "og", "es": "y", "fr": "et", "pt": "e"}.get(lang, "and")

    summ = read_csv(os.path.join(ai_dir, "trial-summary.csv"))[0]
    results = read_csv(os.path.join(ai_dir, "results-by-trait.csv"))
    parts = read_csv(os.path.join(ai_dir, "participants.csv"))
    answers = read_csv(os.path.join(ai_dir, "participant-answers.csv"))

    ref_trait = summ["reference_trait"]
    items = [x.strip() for x in summ["items"].split(";")]
    traits = []
    for a in answers:
        if a["trait"] not in traits: traits.append(a["trait"])
    has_group = "group" in parts[0]
    groups = sorted({p["group"] for p in parts}) if has_group else [None]
    group_col = "group" if has_group else None

    # worth and rank per (group, trait, item). results-by-trait has no group, so
    # the same trial-wide results are used for every group unless results carry a group column.
    res_by_group = {}
    for r in results:
        g = r.get("group") if "group" in r else None
        res_by_group.setdefault(g, {}).setdefault(r["trait"], {})[r["item"]] = dict(
            worth=num(r["worth_percent"], 0.0), rank=int(num(r["rank"], 0)), n=int(num(r["n_answers"], 0)))
    res_grouped = None not in res_by_group  # results-by-trait carries a group column
    def res_for(g):
        return res_by_group.get(g) or res_by_group.get(None) or {}

    # season moments from the bracket part of trait names, in order of appearance
    moments = []
    for tr in traits:
        _, when = split_trait(tr)
        if when and when not in moments: moments.append(when)
    optional = set(T.get("optional_moments", []))
    moment_sub = T.get("moment_labels", {})

    ans_by_p = {}
    for a in answers:
        ans_by_p.setdefault(a["participant_id"], []).append(a)

    trial = dict(name=T["name"], crop=summ.get("crop") or T.get("crop", ""), year=T.get("year", summ.get("report_date", "")[:4]),
                 season_label=T.get("season_label", ""), language=lang, contact=T.get("contact", ""),
                 partners=T.get("partners", ""), website=T.get("website", ""), n_items=len(items),
                 n_registered=int(num(summ.get("participants_registered"), len(parts))),
                 n_answered=int(num(summ.get("participants_reference_trait"), 0)), reference_trait=split_trait(ref_trait)[0],
                 signatories=T.get("signatories", []), cert_place_date=T.get("cert_place_date", ""))

    out_parts = []
    for p in parts:
        g = p[group_col] if group_col else None
        R = res_for(g)
        g = g if res_grouped else None  # only name the group when results are by group
        ref = R.get(ref_trait, {})
        mine = {Lx: p[f"item_{Lx}"] for Lx in L}
        pa = ans_by_p.get(p["participant_id"], [])
        n_farms = sum(1 for q in parts if (q[group_col] if group_col and res_grouped else None) == g)
        ref_n = next((v["n"] for v in ref.values()), trial["n_answered"])
        group_in = f" {t['in']} {g}" if g else ""

        season = []
        for m in moments:
            done = any(truthy(a["answered"]) for a in pa if split_trait(a["trait"])[1] == m)
            core = m not in optional
            season.append(dict(label=m, sub=moment_sub.get(m, ""), done=done, core=core,
                               cls="done" if done else ("opt" if not core else "miss")))
        ncore = sum(1 for s in season if s["core"]); ndone = sum(1 for s in season if s["done"])
        ncore_done = sum(1 for s in season if s["core"] and s["done"])
        missing = [s["label"] for s in season if s["core"] and not s["done"]]
        answered_ref = truthy(p.get("answered_reference_trait"))
        status = "active" if (ncore_done == ncore and ncore > 0) or (not season and answered_ref) else ("partial" if ndone > 0 or answered_ref else "new")
        first = p["name"].split()[0] if p.get("name") else ""
        miss_txt = join_and([m.lower() for m in missing], and_word)
        kw = dict(first_name=first, n_done=ncore_done, n_core=ncore, missing=miss_txt, n_farms=n_farms, group_in=group_in,
                  ref_n=ref_n, ref_trait=trial["reference_trait"].lower(), year=trial["year"])
        if status == "active":
            title = fmt(t["title_active"], **kw); thanks = fmt(t["thanks_active"], **kw) + (fmt(t["thanks_active_missing"], **kw) if missing else "")
        elif status == "partial":
            title = fmt(t["title_partial"], **kw); thanks = fmt(t["thanks_partial"], **kw)
        else:
            title = fmt(t["title_new"], **kw); thanks = fmt(t["thanks_new"], **kw)

        options = []
        for Lx in L:
            it = mine[Lx]; pts = num(p.get(f"points_{Lx}"), None)
            options.append(dict(letter=Lx, item=it, rank=ref.get(it, {}).get("rank", ""), points=int(pts) if pts is not None else 0, group=g))
        rows = []
        for a in pa:
            trn, when = split_trait(a["trait"])
            you = a["best_option"] if truthy(a["answered"]) else ""
            Rt = R.get(a["trait"], {})
            others = max(L, key=lambda Lx: Rt.get(mine[Lx], {}).get("worth", -1)) if Rt else ""
            rows.append(dict(trait=trn, when=when, you=you, you_item=mine.get(you, ""), others=others,
                             others_item=mine.get(others, ""), agree=bool(you) and you == others))
        pos = {Lx: ref.get(mine[Lx], {}).get("rank") for Lx in L}
        inv = {v: k for k, v in pos.items() if v}
        strip = [dict(pos=k, letter=inv.get(k, "")) for k in range(1, len(items) + 1)]
        qr_url = fmt(T.get("qr_url_template", ""), package=p.get("package_id", p["participant_id"]), participant_id=p["participant_id"],
                     **{k: v for k, v in p.items() if k.startswith("qr_") or k == "package_code"})
        qr_url = p.get("qr_url") or qr_url
        if status == "active" or not missing:
            cta_title, cta_text, steps = t["cta_title_next"], t["cta_text_next"], []
        else:
            cta_title, cta_text = t["cta_title_missing"], t["cta_text_missing"]
            steps = [dict(n=i + 1, text=t[f"cta_step{i + 1}"]) for i in range(3)]

        item_tab = None
        if T.get("items_table"):
            it = T["items_table"]; shown = [c for c in it["columns"] if c.get("group") in (None, g)] if g else it["columns"]
            mine_inv = {v: k for k, v in mine.items()}
            cols = [dict(label=c["label"], tag=c.get("tag", ""), mine=mine_inv.get(c["item"], "")) for c in shown]
            rws = []
            for r in it["rows"]:
                cells = [dict(value=r["values"].get(c["item"], ""), mine=bool(mine_inv.get(c["item"]))) for c in shown]
                if any(c["value"] != "" for c in cells):
                    rws.append(dict(label=r["label"], cells=cells))
            item_tab = dict(row_label=it.get("row_label", ""), columns=cols, rows=rws)

        out_parts.append(dict(
            id=p["participant_id"], name=p.get("name", ""), location=p.get("location", ""), package_id=p.get("package_id", p["participant_id"]),
            group=g, status=status, title=title, thanks_html=thanks, has_season=bool(season), season=season, options=options,
            points_note=fmt(t["points_note"], **kw), traits=rows, you_head=t["you_said_best"] if status != "new" else t["what_do_you_say"],
            strip=strip, ranking_note=fmt(t["ranking_note"], **kw), cta_title=cta_title, cta_text=cta_text, has_steps=bool(steps), cta_steps=steps,
            qr_url=qr_url, qr_src=qr_data_uri(qr_url) if qr_url else "",
            items_table=item_tab, items_intro_html=fmt(t["items_intro"], group_in=group_in, mine=", ".join(f"<b>{Lx}</b> = {mine[Lx]}" for Lx in L)),
            items_note_html=T.get("items_note", ""),
            cert_for_html=fmt(t["cert_for"], trial_name=trial["name"], year=trial["year"], n_items=len(items), crop=trial["crop"].lower()),
        ))

    # flyer
    F = T.get("flyer", {})
    fl_groups, tables = [], []
    for g in (groups if res_grouped else [None]):
        R = res_for(g); ref = R.get(ref_trait, {})
        order = sorted(ref, key=lambda i: ref[i]["rank"]) if ref else items
        fl_groups.append(dict(name=g or "", top3=[dict(pos=i + 1, item=it, first=i == 0) for i, it in enumerate(order[:3])]))
        rows = []
        for tr in traits:
            Rt = R.get(tr)
            if not Rt: continue
            mean = sum(v["worth"] for v in Rt.values()) / len(Rt)
            cells = []
            for it in order:
                s, c = symbol(Rt.get(it, {}).get("worth", mean), mean); cells.append(dict(sym=s, cls=c))
            n = next(iter(Rt.values()))["n"]
            rows.append(dict(trait=tr if len(traits) < 8 else split_trait(tr)[0], n=n, cells=cells))
        tables.append(dict(name=g or "", items=[T.get("short_item_names", {}).get(i, i) for i in order], rows=rows))
    flyer = dict(kicker=F.get("kicker", trial["season_label"]), headline=F.get("headline", ""), lead_html=F.get("lead", ""),
                 kpis=F.get("kpis", [dict(value=trial["n_registered"], label="farms in the network"), dict(value=trial["n_answered"], label="farms sent their answers")]),
                 map_src=file_data_uri(F["map"]) if F.get("map") else "", map_note=F.get("map_note", ""),
                 top_title=fmt(t["flyer_top_title"], ref_trait=trial["reference_trait"].lower()), groups=fl_groups,
                 callout_title=F.get("callout_title", ""), callout_html=F.get("callout", ""), basis_note=F.get("basis_note", ""),
                 band_title=F.get("band_title") or (t["flyer_band_title_missing"] if F.get("audience") == "non-responders" else t["flyer_band_title_all"]),
                 band_html=F.get("band") or fmt(t["flyer_band_missing" if F.get("audience") == "non-responders" else "flyer_band_all"], website=trial["website"].replace("https://", "")), site_qr_src=qr_data_uri(trial["website"]) if trial["website"] else "",
                 site_label=trial["website"].replace("https://", ""), page2_kicker=F.get("page2_kicker", ""), page2_title=F.get("page2_title", ""),
                 page2_intro=F.get("page2_intro", ""), tables=tables, symbols_note=t["flyer_symbols_note"], how_title=t["flyer_how_title"],
                 how_html=fmt(t["flyer_how"], website=trial["website"].replace("https://", "")))

    W = T.get("workshop") or json.load(open(os.path.join(TEXT, f"workshop-default.{lang}.json"), encoding="utf-8"))
    kw = dict(n_items=len(items), n_registered=trial["n_registered"], n_answered=trial["n_answered"], crop=trial["crop"])
    for s in W["steps"]:
        s["intro"] = fmt(s.get("intro", ""), **kw)
        s["bullets"] = [fmt(b, **kw) for b in s.get("bullets", [])]
        s["has_bullets"] = bool(s["bullets"])
    return dict(trial=trial, t=t, participants=out_parts, flyer=flyer, workshop=W)


def render_all(data, out_dir, png=False):
    os.makedirs(out_dir, exist_ok=True)
    head = open(os.path.join(TPL, "_head.html"), encoding="utf-8").read()
    foot = open(os.path.join(TPL, "_foot.html"), encoding="utf-8").read()
    css = open(os.path.join(TPL, "theme.css"), encoding="utf-8").read()
    head = render(head, data).replace('<link rel="stylesheet" href="theme.css">', f"<style>{css}</style>")
    tp = {k: open(os.path.join(TPL, f"{k}.html"), encoding="utf-8").read() for k in
          ["participant-sheet", "items-page", "community-flyer", "certificate", "workshop-manual"]}
    base = {"trial": data["trial"], "t": data["t"]}
    docs = {}
    sheets = "".join(render(tp["participant-sheet"], {**base, **p}) + (render(tp["items-page"], {**base, **p}) if p["items_table"] else "")
                     for p in data["participants"])
    docs["participant-sheets"] = sheets
    docs["certificates"] = "".join(render(tp["certificate"], {**base, **p}) for p in data["participants"])
    docs["community-flyer"] = render(tp["community-flyer"], {**base, "flyer": data["flyer"]})
    docs["workshop-manual"] = render(tp["workshop-manual"], {**base, "workshop": data["workshop"]})
    for k, body in docs.items():
        open(os.path.join(out_dir, f"{k}.html"), "w", encoding="utf-8").write(head + body + foot)
    json.dump(data, open(os.path.join(out_dir, "feedback.json"), "w", encoding="utf-8"), indent=1, ensure_ascii=False)
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("playwright not installed, HTML written only"); return
    with sync_playwright() as pw:
        br = pw.chromium.launch(); pg = br.new_page()
        for k in docs:
            pg.goto("file://" + os.path.abspath(os.path.join(out_dir, f"{k}.html"))); pg.wait_for_timeout(800)
            land = k == "certificates"
            pg.pdf(path=os.path.join(out_dir, f"{k}.pdf"), width="210mm", height="148mm" if land else "297mm", print_background=True,
                   margin=dict(top="0", bottom="0", left="0", right="0"))
            if png:
                pg.set_viewport_size(dict(width=794, height=560 if land else 1123))
                for i, el in enumerate(pg.query_selector_all("section.page")):
                    el.screenshot(path=os.path.join(out_dir, f"{k}-{i + 1:02d}.png"))
        br.close()


if __name__ == "__main__":
    ai, tj, out = sys.argv[1:4]
    data = build(ai, tj)
    render_all(data, out, png="--png" in sys.argv)
    print("participants:", len(data["participants"]), "| statuses:", {s: sum(p["status"] == s for p in data["participants"]) for s in ("active", "partial", "new")})
