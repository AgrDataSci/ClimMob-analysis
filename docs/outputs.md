# Outputs of the ClimMob analysis

All files are written to the `OutputPath` sent by ClimMob, or to `settings$output_path` in local runs. Names in **bold** are part of the contract with the ClimMob server and must not change.

## Reports

| File | Content |
|---|---|
| **`climmob_main_report.<Format>`** | Main report in the language of the request. If it cannot be produced, a short report with the errors is written under the same name. |
| **`participant-report/participant_report_package_<i>.<Format>`** | One file per participant when `InfoSheets` is TRUE, with the feedback sheet and the certificate of participation. `<i>` is the row of the participant in the data. |
| `participant-report/feedback-sheets.pdf` | All feedback sheets, one A4 page per participant, for printing. |
| `participant-report/certificates.pdf` | All certificates of participation. |
| `participant-report/community-results.pdf` and `.png` | Results of the whole trial on one page, for group meetings and for participants who did not send their answers. |
| `participant-report/png/feedback_<id>.png`, `certificate_<id>.png` | The same pages as images, to send by phone. `<id>` is the package id. |

## Extra outputs, `extra-outputs/`

Figures are vector PDF files. Tables are CSV files in UTF-8.

| File | Content |
|---|---|
| **`trial-map.pdf`** | Location of the trial plots. |
| **`<k>-logworth.pdf`** | Log-worth of the items for trait number k, relative to the checks. |
| **`kendall-tau.pdf`** | Kendall tau between the reference trait and the other traits. |
| **`worth-map.pdf`** | Log-worth of every item in every trait. |
| **`reliability<k>.pdf`**, **`reliability-data.csv`** | Probability of outperforming check number k. The table has a column `check`. |
| **`netwinning-data.csv`** | Favourability of every item in every trait (see below). |
| **`plackettluce-tree.pdf`** | Plackett-Luce tree, when covariates split the data. |
| **`weekly-precipitation-indices.csv/.pdf`**, **`weekly-temperature-indices.csv/.pdf`** | Weekly climate indices during the season. |
| **`possible-outliers-quantitative-data.csv`** | Values of measured traits outside 1.5 times the interquartile range. |
| `monitoring-attrition.csv/.pdf` | Participants who answered each data collection moment. |
| `monitoring-answer-quality.csv` | Answers of every trait by type (valid, not answered, not observed, tie, same best and worst, invalid). |
| `monitoring-enumerators.csv` | Forms submitted by each data collector. |
| `variety-release-summary.csv` | One row per item with the evidence against the first check. |
| `variety-release-by-trait.csv` | Every item against every check, trait by trait. |
| `groups-*.csv`, `groups-worth-map.pdf` | Results by group when `Split` is used. |
| `measured-traits-means.csv`, `measured-traits-heritability.csv`, `measured-traits-quality.csv`, `measured-*-boxplot.pdf` | Analysis of measured traits. |
| `ai-ready/` | Tables and a guide to write material for participants with an AI assistant. |

## Column definitions

### netwinning-data.csv

| Column | Meaning |
|---|---|
| `trait` | Trait and data collection moment. |
| `item` | Tested item. |
| `n` | Number of rankings that included the item. |
| `top`, `bottom` | Probability (%) that the item is ranked first or last in the sets where it was tested, from the Plackett-Luce model. |
| `net_winning` | `top` minus `bottom`, the favourability score. |

### variety-release-summary.csv

| Column | Meaning |
|---|---|
| `item` | Tested item. |
| `n_tested` | Number of participants who tested it. |
| `prob_beats_check` | Probability that the item beats the check in a pairwise comparison, for the reference trait (0 to 1). |
| `p_value` | Two-sided test of the difference with the check, for the reference trait. |
| `favourability` | Favourability score for the reference trait (see above). |
| `better_than_check`, `worse_than_check` | Traits where the item was better or worse than the check (p < 0.05). |
| `yield_mean`, `yield_percent_of_check` | Mean of the first measured yield trait adjusted for the farm, and its percentage of the check. Only when yield was measured. |
| `item_is_check` | TRUE for the check. |

### variety-release-by-trait.csv

| Column | Meaning |
|---|---|
| `trait`, `check`, `item` | Trait, check and item compared. |
| `logworth_vs_check` | Log-worth of the item minus the log-worth of the check. |
| `se` | Standard error of the difference, from quasi variances. |
| `prob_beats_check` | Probability that the item beats the check, `plogis(logworth_vs_check)`. |
| `p_value` | Two-sided z test. |
| `result` | `better`, `worse` or `similar` at p < 0.05, `check` for the check itself. |

### measured-traits-means.csv

| Column | Meaning |
|---|---|
| `trait`, `moment` | Trait and data collection moment. |
| `method` | `lmer`, mixed model with farm as random effect, or `lm`, farm as fixed effect when the mixed model cannot be fitted. |
| `genotype` | Item. |
| `mean`, `se`, `lower`, `upper` | Mean adjusted for the farm, standard error and 95% confidence interval. |
| `diff_vs_check`, `percent_of_check`, `p_vs_check` | Difference with the check, percentage of the check and p-value. |
| `n` | Number of plots used. |
| `group` | Items sharing a letter are not different at p = 0.05. |

`measured-traits-heritability.csv` holds the genetic, farm and residual variances of the model with random genotype, and the broad-sense heritability by the ad hoc method of Holland et al. (2003).

### groups-*.csv

| File | Columns |
|---|---|
| `groups-eligibility.csv` | `group`, `n_valid` (valid answers for the reference trait), `share` among participants with a group, `eligible`, `reason` (`ok`, `too_few`, `no_label`). |
| `groups-test-by-trait.csv` | `trait`, likelihood ratio `statistic`, `df`, `p_value` of the difference between groups, `note`. |
| `groups-balanced-ranking.csv` | Worth (%) and rank of each item for all participants pooled, and with every group given the same weight. |
| `groups-agreement-within.csv` | Mean Kendall tau between each participant and the ranking of their group. |
| `groups-agreement-between.csv` | Kendall tau between the rankings of two groups. |

### ai-ready/

| File | Columns |
|---|---|
| `trial-summary.csv` | `project`, `crop`, `language`, `option_label`, `participant_label`, `participants_registered`, `participants_reference_trait`, `items_tested`, `items`, `check_items`, `reference_trait`, `traits`, `traits_dropped`, `report_date`. |
| `results-by-trait.csv` | `trait`, `is_reference_trait`, `item`, `worth_percent` (share of 100 among all items), `rank`, `n_answers`. |
| `item-vs-check.csv` | As `variety-release-by-trait.csv`, for the first check. |
| `participants.csv` | `participant_id`, `name`, `location`, `item_A`, `item_B`, `item_C`, `answered_reference_trait`, `first`, `second`, `third` (option letters of the own ranking of the reference trait), `points_A`, `points_B`, `points_C` (chance that each of the three options wins against the other two, from all participants, summing to 100). |
| `participant-answers.csv` | `participant_id`, `trait`, `is_reference_trait`, `answered`, `best_option`, `best_item`, `worst_option`, `worst_item`. |
