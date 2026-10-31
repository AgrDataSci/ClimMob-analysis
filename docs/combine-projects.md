# Combining ClimMob projects

ClimMob reports analyse one project at a time. To analyse several projects together, for example the same varieties tested in two seasons or in two countries, use `run-local/combine-projects.R`. This page explains each step.

## Why projects need harmonising

Every project names its questions after the template used to build it, and every data collection moment has its own code. The same question "which option had the best yield" can be `ASS3f2a..._char_yield_pos` in one project and `ASS91bc..._char_yieldperplant_pos` in another, asked at "Harvest" in one and at "Final evaluation" in the other. The analysis needs one column per question, so these must be matched by a person who knows the trials.

## Step 1. Catalog each project

`project_catalog(cmlist)` lists every question of a project and its role.

| type | how it is recognised | role |
|---|---|---|
| `ranking` | fields `char_<code>_pos` and `char_<code>_neg` | `pos` (best), `neg` (worst) |
| `quantitative` | numeric fields ending in `_a`, `_b`, `_c` | one value per option |
| `local` | fields `perf_<code>_1/2/3`, comparison with the local item | `1`, `2`, `3` |
| `covariate` | other numeric or single-choice fields, without system fields | `value` |

`build_pars_from_cmlist(cmlist)` turns the catalog of one project into the parameters that ClimMob would send with a report request. This is enough to analyse one project.

## Step 2. Review the harmonisation table

`suggest_harmonization()` puts the catalogs together and proposes a common code for each question (`trait`), for each data collection moment (`moment`, with its name in `moment_label`) and a label. Common synonyms are merged, for example `overallperf` and `overallchar` become `overall`. Only gender, age and education are included among the covariates by default.

Write the table to a CSV file and review it in a spreadsheet.

- Give the same `trait` to the same question in every project, and different codes to different questions.
- Give the same `moment` to data collection moments that should be analysed together. A trait is analysed once per moment.
- Set `include` to FALSE for anything you do not want in the analysis.
- Edit `label` and `moment_label` to change the names shown in the report.

## Step 3. Combine and analyse

`combine_projects(projects, harmonization, item_map)` builds one data set with columns named `<moment>_<trait>_<role>` for traits and measurements and `cov_<trait>` for covariates. Participant ids become `<project>-<package id>` so they stay unique. `item_map` is optional and gives the same name to the same item across projects.

`pars_from_harmonization()` builds the parameters for the combined data. It adds the project as a covariate, so the Plackett-Luce tree can show differences between projects.

Then run the analysis as for one project.

```r
res = run_climmob_analysis(combined, pars, settings)
write_climmob_outputs(res, settings)
```

## Checks before trusting the results

- The items must be connected. Projects that share no item cannot be ranked together. The network chart in the report shows this.
- Look at `res$monitoring$answer_quality` for traits with many invalid answers in one project only, which often means a question was asked differently.
- Use the project as a split (`settings$groups = "project_code"`) to see whether the projects agree.
