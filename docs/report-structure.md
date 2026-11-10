# Structure of the main report

What `report/mainreport.Rmd` prints, in order, and the pipeline step (`R/90_pipeline.R`) that feeds each part. Tables and figures are numbered in the order they appear, so moving one changes the numbers after it. Everything that is not needed for a decision goes to the appendix, which keeps the main body readable for trials with many traits, groups or enumerators.

| Section | Content | Step and object |
|---|---|---|
| Summary | Registered and valid answers, best items, items better than the check, dropped traits, what participants talked about (Sikia) with one quote, pointer to group results | `monitoring`, `rank_dat`, `release$summary`, `sikia$headline` |
| Introduction, What to expect | Fixed texts | i18n only |
| Trial monitoring | Attrition text and **figure**; answer quality **table**; one sentence pointing to the enumerator table in the appendix | `monitoring$attrition`, `attrition_plot`, `answer_quality` |
| Location of the trial plots | Map, sized from the country shape (`trial_map$aspect`) | `trial_map` |
| Who answered the latest data collection | Participation map | `participation_map` |
| Climate | Temperature and rainfall indices | `agroclimate` |
| How many answers are needed | Data value curve | `data_value` |
| Methods | Fixed texts and formulas | i18n, `PL_tree$tree_formula` |
| Design and connectivity | Participants, options, traits (list in appendix); network **figure**; items **table** | `overview_and_summaries` |
| Performance of the options | ANOVA **table**; favourability **table** for the reference trait and yield only (all traits in appendix); Kendall **figure**; worth map; reliability; log-worth of the reference trait | `PL_models` |
| Why they preferred it | Praise and complaints per item from the open answers, when Sikia linked mentions to items | `sikia$item_words`, `release$summary` |
| Differences between groups | Eligibility, balanced ranking, heterogeneity test, agreement, worth map by group | `group_analysis` |
| Covariates (tree) | Node table, tree figure, regret table | `PL_tree` |
| Measured traits | One subsection per trait, means against the check, heritability, box plot | `metric` |
| Release summary | One **table**, trait lists without the moment when unambiguous | `release` |
| What participants said in their own words | Sentiment chart, item by topic heatmap, quotes, topic tree | `sikia` |
| References | | |
| Appendix | Monitoring tables (attrition, forms by data collector); traits analysed; favourability by trait; Kendall table; outliers in measured traits; errors | as above |

## Rules

- A table goes to the appendix when it grows with the number of traits, enumerators or participants, unless it is the one the reader needs to act on.
- Captions come from `report/i18n/report-text.csv`. A table moved to the appendix keeps its caption key; only the text that pointed to it changes ("Table {k}" becomes "in the appendix").
- `kable_tr()` sets column widths from the content. Pass `widths` only when the automatic weights do not fit.
- Maps use `fig.width = map_w` and `fig.height = map_h`, computed in the setup chunk from `trial_map$aspect`.
- Colours come from `pal_diverging()`, `pal_sequential()`, `pal_qualitative()` and `pal_one()` (see `R/01_utils.R`).
