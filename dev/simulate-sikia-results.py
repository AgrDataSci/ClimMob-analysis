"""Add simulated Sikia Analytics results to the simulated maize fixture.

Follows the structure of the payload ClimMob receives from Sikia
(summary, mentions table, topic tree). All phrases are invented.
Run once:  python3 dev/simulate-sikia-results.py
"""
import json, random, datetime

random.seed(11)
FIX = "tests/testdata/simulated-maize/data.json"
d = json.load(open(FIX))
items = sorted({e["alias_name"] for c in d["combination"] for e in c["elements"]})[:6]

# label, type, positive phrases, negative phrases, base rate per response, positive share
LABELS = [
    ("early maturity", "trait", ["ready before the rains stopped", "matured in three months", "harvested earliest of the three"], ["took too long to mature", "still green when the others were dry"], 0.45, 0.75),
    ("drought tolerance", "climate_event", ["survived the dry spell in February", "kept green with little rain"], ["wilted when the rains failed", "could not handle the dry weeks"], 0.40, 0.55),
    ("fall armyworm damage", "pest_disease", ["few worms on this one"], ["armyworm ate the leaves", "lost many plants to armyworm", "had to spray twice"], 0.50, 0.15),
    ("cob size", "trait", ["big cobs with full rows", "cobs filled to the tip"], ["small cobs", "many empty tips"], 0.45, 0.65),
    ("taste", "quality", ["sweet when roasted", "children liked the ugali from it", "soft and tasty"], ["bland taste", "hard to chew"], 0.35, 0.80),
    ("lodging", "trait", ["stood well in the wind"], ["fell over after the storm", "stems too weak"], 0.25, 0.35),
    ("seed cost", "economics", ["worth the price"], ["seed too expensive for next season", "cannot afford to buy again"], 0.20, 0.25),
    ("storage weevils", "pest_disease", ["stored for months without weevils"], ["weevils came within weeks"], 0.20, 0.55),
    ("market price", "economics", ["sold at a good price", "buyers asked for this one"], ["buyers did not want the colour"], 0.20, 0.70),
]
EMO = {"positive": ["happy", "proud", "relieved", "satisfied"], "negative": ["frustrated", "worried", "disappointed"]}
# variety-specific shifts of the positive share
SHIFT = {items[0]: {"early maturity": 0.2, "cob size": -0.2}, items[1]: {"fall armyworm damage": -0.1, "taste": 0.1},
         items[2]: {"drought tolerance": 0.3, "cob size": 0.15}, items[3]: {"lodging": -0.3, "early maturity": -0.2},
         items[4]: {"taste": -0.4, "market price": -0.3}, items[5]: {"storage weevils": 0.3}}

mentions = []
n_resp = 84
for r in range(n_resp):
    var = random.choice(items)
    gender = random.choice(["female", "male"])
    k = 0
    for lab, typ, pos, neg, rate, pshare in LABELS:
        if random.random() > rate: continue
        p = min(0.95, max(0.05, pshare + SHIFT.get(var, {}).get(lab, 0)))
        polarity = "positive" if random.random() < p else "negative"
        if random.random() < 0.08: polarity = "neutral"
        phrase = random.choice(pos if polarity != "negative" else neg)
        strength = round(random.uniform(0.5, 0.95), 2); conf = round(random.uniform(0.6, 0.98), 2)
        mentions.append(dict(mention_id=f"row-{r}-{k:04d}", response_id=f"row-{r}", target_entity_id=f"VAR_{items.index(var) + 1}",
                             target_entity_label=var, entity_type=typ, canonical_label=lab, normalized_label=lab,
                             phrase_en=phrase, phrase_sw=phrase, evidence_en=f"{phrase} with {var}", evidence_sw=f"{phrase} with {var}",
                             extraction_type="explicit_trait" if typ == "trait" else "explicit",
                             sentiment_polarity=polarity, sentiment_strength=0.0 if polarity == "neutral" else strength,
                             sentiment_confidence=conf, emotion_primary=None if polarity == "neutral" else random.choice(EMO[polarity]),
                             emotion_secondary=None, respondent_gender=gender, evidence_grounded=True))
        k += 1

labels = []
for lab, *_ in LABELS:
    m = [x for x in mentions if x["canonical_label"] == lab]
    pos = sum(x["sentiment_polarity"] == "positive" for x in m); neg = sum(x["sentiment_polarity"] == "negative" for x in m)
    labels.append(dict(canonical_label=lab, count=len(m), positive=pos, neutral=len(m) - pos - neg, negative=neg,
                       sentiment_score_sum=round(sum((1 if x["sentiment_polarity"] == "positive" else -1 if x["sentiment_polarity"] == "negative" else 0) * x["sentiment_strength"] for x in m), 1)))
labels.sort(key=lambda x: -x["count"])
lab_names = [x["canonical_label"] for x in labels]
matrix, counts = [], []
for v in items:
    row_m, row_c = [], []
    for lab in lab_names:
        m = [x for x in mentions if x["canonical_label"] == lab and x["target_entity_label"] == v]
        pos = sum(x["sentiment_polarity"] == "positive" for x in m); neg = sum(x["sentiment_polarity"] == "negative" for x in m)
        row_c.append(len(m)); row_m.append(round((pos - neg) / len(m), 2) if m else 0)
    matrix.append(row_m); counts.append(row_c)

summary = dict(filters=dict(include_inferred_traits=True, top_varieties=20, top_labels=25, min_sentiment_confidence=0.0, min_phrase_total=5),
               built_at="2026-10-20T09:12:00+00:00", updated_at="2026-10-20T09:12:00+00:00", summary_version="v1",
               gender_label_counts=[], labels=labels,
               variety_x_label=dict(varieties=items, labels=lab_names, matrix=matrix, counts=counts),
               run_id="simulated-run", project_id="simulated-project",
               totals=dict(with_gender=n_resp, responses=n_resp, varieties=len(items), mentions=len(mentions), labels=len(labels)),
               gender_phrase_log_odds=[], source_envelope_etag=None, dataset_id="simulated-dataset")

def leaf(i, label, kws, phr, n):
    return dict(topic_id=f"topic-{i:03d}", topic_index=i, label=label, keywords=kws, member_count=n, representative_phrases=phr)
cnt = {x["canonical_label"]: x["count"] for x in labels}
tree = dict(categories=[
    dict(topic_id="topic-cat-000", label="Growing season", keywords=["maturity", "rain", "dry spell", "lodging"],
         member_count=cnt["early maturity"] + cnt["drought tolerance"] + cnt["lodging"],
         leaves=[leaf(0, "Early maturity and escape from drought", ["ready", "three months", "dry spell"], ["ready before the rains stopped", "kept green with little rain", "harvested earliest of the three"], cnt["early maturity"] + cnt["drought tolerance"]),
                 leaf(1, "Standing in wind", ["wind", "fell", "stems"], ["stood well in the wind", "fell over after the storm"], cnt["lodging"])]),
    dict(topic_id="topic-cat-001", label="Pests in field and store", keywords=["armyworm", "spray", "weevils"],
         member_count=cnt["fall armyworm damage"] + cnt["storage weevils"],
         leaves=[leaf(2, "Fall armyworm and spraying", ["armyworm", "spray", "leaves"], ["armyworm ate the leaves", "had to spray twice"], cnt["fall armyworm damage"]),
                 leaf(3, "Weevils in storage", ["weevils", "store", "months"], ["stored for months without weevils", "weevils came within weeks"], cnt["storage weevils"])]),
    dict(topic_id="topic-cat-002", label="Harvest, food and money", keywords=["cobs", "taste", "price", "seed cost"],
         member_count=cnt["cob size"] + cnt["taste"] + cnt["seed cost"] + cnt["market price"],
         leaves=[leaf(4, "Cob size and filling", ["cobs", "rows", "tips"], ["big cobs with full rows", "many empty tips"], cnt["cob size"]),
                 leaf(5, "Eating quality", ["sweet", "ugali", "chew"], ["sweet when roasted", "children liked the ugali from it"], cnt["taste"]),
                 leaf(6, "Seed cost and market", ["price", "buyers", "expensive"], ["sold at a good price", "seed too expensive for next season"], cnt["seed cost"] + cnt["market price"])])],
    noise=None, totals=dict(categories=3, leaves=7))

d["SIKIA_results"] = [dict(model_name="entity_extraction", output_type="summary", json_result=summary),
                      dict(model_name="entity_extraction", output_type="table", json_result=mentions),
                      dict(model_name="entity_extraction", output_type="topic_tree", json_result=tree)]
json.dump(d, open(FIX, "w"), ensure_ascii=False)
print(len(mentions), "mentions,", len(labels), "labels,", len(items), "varieties")
