
<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3984711.svg)](https://doi.org/10.5281/zenodo.3976631) [![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![License](https://img.shields.io/badge/license%20-%20CC%20BY%20SA%204.0%20-%20%233182bd)](https://creativecommons.org/licenses/by-sa/4.0/)

<!-- badges: end -->

# Workflow for automated analysis and report of decentralized experiments with the tricot approach

## Overview

This repository contains the code that analyses tricot data and writes the reports of the [ClimMob](https://climmob.net/) platform. Read the latest changes [here](NEWS.md).

## How ClimMob calls the workflow

The server runs

```
Rscript ClimMob.R <parameters.json> <data.json>
```

The first file holds the report settings and the variables to analyse. The second holds the trial data. The workflow writes to the `OutputPath` given in the parameters

| Output | Content |
|---|---|
| `climmob_main_report.<Format>` | the main report, always written (a short error report if the main one fails) |
| `participant-report/participant_report_package_<i>.<Format>` | one info sheet per participant, when `InfoSheets` is TRUE |
| `extra-outputs/` | figures as vector PDF and tables as CSV |

## Repository organisation

```
ClimMob.R          entry point called by the server, reads the arguments and calls run_climmob_from_files()
load.R             load_climmob() sources R/ and attaches the packages
R/
  00_packages.R    the only place where packages are attached
  01_utils.R       small helpers (base R)
  02_runner.R      run_step(), runs each step, logs errors, skips dependent steps
  10_read_inputs.R reads and decodes the two JSON files
  20_groups.R      groups (segments) of participants
  21_prepare_rankings.R      cleans the tricot answers, builds rank_dat
  22_prepare_quantitative.R  quantitative traits
  30_overview.R    tables and charts of the trial overview
  31_map_and_climate.R       coordinates, map, agroclimatic indices
  40_plackett_luce.R         models, log-worth, worth map, ANOVA, reliability, Kendall tau
  41_plackett_luce_tree.R    covariates and Plackett-Luce trees
  42_quantitative_summaries.R
  43_participant_report_data.R
  50_stats_helpers.R         pseudo rankings, network, multiple comparisons, log-worth plot
  60_render_reports.R        renders the Rmd templates in a temporary folder
  61_extra_outputs.R         writes extra-outputs/
  90_pipeline.R              run_climmob_from_files(), run_climmob_analysis(), write_climmob_outputs()
report/            Rmd templates, Word styles, bibliography and multilanguage text
run-local/         analyse one or more ClimMob projects in your own R session
dev/               package installation, debugging guide, metadata helpers
tests/             unit tests (testthat)
```

The analysis is a list of named steps in `run_climmob_analysis()`. Each step is a plain function that takes data and returns a result, so any step can be called on its own from the console.

## Debugging

Every run prints a log with one line per step, for example

```
[ok]      rank_dat (0.2s)
[ok]      models (1.4s)
[FAILED]  kendall (0.1s) subscript out of bounds
[skipped] trial_map (0s) skipped because 'coords' failed
```

A failed step no longer removes the other results from the report, and steps that depend on it are skipped instead of running on placeholder data. The summary at the end lists the R calls that led to each error.

To reproduce a report on your machine

1. Run with a debug folder, `CLIMMOB_DEBUG_DIR=/tmp/cm-debug Rscript ClimMob.R pars.json data.json`. The folder receives a copy of both inputs, `state.rds` with every result and one `failed-<step>.rds` per failed step.
2. Open `dev/debug-a-failed-run.R`. It reruns the analysis in your session and shows how to step into the exact failing call with `debugonce()`.

## Running locally

`run-local/run-local-analysis.R` fetches projects with ClimMobTools, harmonises them and calls `run_climmob_analysis()` and `write_climmob_outputs()`. The result `res` holds every intermediate object (`res$rank_dat`, `res$PL_models`, `res$PL_tree` and so on).

## Setup and tests

```
Rscript dev/install-packages.R
Rscript tests/testthat.R
```

The tests cover reading the request, cleaning and organising the data and the step runner. They use base R only and run in a few seconds.

## Meta

-   Please [click here to report issues or bugs](https://github.com/agrdatasci/ClimMob-analysis/issues)
-   License: [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/)
-   The [Tricot user guide](https://hdl.handle.net/10568/109942) presents the experimental approach used by the ClimMob platform
-   The [ClimMob](https://climmob.net/) platform is a free online software for experimental trials with the tricot citizen-science approach
