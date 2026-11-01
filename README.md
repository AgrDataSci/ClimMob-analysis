
<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3984711.svg)](https://doi.org/10.5281/zenodo.3976631) [![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/AgrDataSci/ClimMob-analysis/blob/master/LICENSE)

<!-- badges: end -->

# Workflow for automated analysis and report of decentralized experiments with the tricot approach

## Overview

This repository contains the code that analyses tricot data and writes the reports of the [ClimMob](https://climmob.net/) platform. It also lets anyone analyse one or several ClimMob projects on their own computer. Read the latest changes [here](NEWS.md).

The reports serve three uses.

- **Monitoring a trial.** Participation at each data collection moment, attrition, answers that can be used and data collectors. Available as soon as participants are registered.
- **Results and variety release.** Plackett-Luce analysis of the rankings, analysis by group of participants with balance checks, measured traits such as yield, and a summary of every item against the check.
- **Feedback to participants.** A one-page feedback sheet, a certificate of participation and a community result, plus tables ready for AI assistants.

Reports are written in English, Spanish, French, Portuguese, Swahili or Norwegian.

## How ClimMob calls the workflow

The server runs

```
Rscript ClimMob.R <parameters.json> <data.json>
```

The first file holds the report settings and the variables to analyse. The second holds the trial data. The workflow writes to the `OutputPath` given in the parameters.

| Output | Content |
|---|---|
| `climmob_main_report.<Format>` | the main report, always written (a short error report if the main one fails) |
| `participant-report/participant_report_package_<i>.<Format>` | feedback sheet and certificate for each participant, when `InfoSheets` is TRUE |
| `extra-outputs/` | figures as vector PDF and tables as CSV |

Every file and column is described in [docs/outputs.md](docs/outputs.md).

## Repository organisation

```
ClimMob.R          entry point called by the server
load.R             load_climmob() sources R/ and attaches the packages
R/
  00_packages.R    the only place where packages are attached
  01_utils.R       small helpers (base R)
  02_runner.R      run_step(), runs each step, logs errors, skips dependent steps
  03_i18n.R        languages, tr() and the text files in report/i18n
  10_read_inputs.R reads and decodes the two JSON files
  20-23            groups, rankings, measured traits and monitoring data
  30-31            overview, map and climate
  40-41            Plackett-Luce models and trees
  44_group_analysis.R        results by group with balance checks
  45_metric_traits.R         measured traits, mixed models and heritability
  46_release_summary.R       evidence for variety release
  47_feedback_sheets.R       feedback sheets, certificates and community results
  48_ai_ready_export.R       tables for AI assistants
  50_stats_helpers.R         pseudo rankings, network, multiple comparisons, log-worth plot
  60-62            rendering the reports and writing the extra outputs
  70_combine_projects.R      parameters from ClimMob data and combining projects
  90_pipeline.R              run_climmob_from_files(), run_climmob_analysis(), write_climmob_outputs()
report/            report templates, Word styles, bibliography, i18n/ (all report text) and ai/
run-local/         analyse one project, combine projects, examples
docs/              outputs and combining projects
dev/               installation, debugging, releases, metadata helpers
tests/             unit tests and a simulated project for the full workflow
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

A failed step does not remove the other results from the report, and steps that depend on it are skipped instead of running on placeholder data. The summary at the end lists the R calls that led to each error.

To reproduce a report on your machine, run with a debug folder, `CLIMMOB_DEBUG_DIR=/tmp/cm-debug Rscript ClimMob.R pars.json data.json`, then follow `dev/debug-a-failed-run.R`.

## Running locally and combining projects

- `run-local/analyse-one-project.R` fetches a project with ClimMobTools, builds the analysis parameters from the project with `build_pars_from_cmlist()` and writes the same outputs as ClimMob.
- `run-local/combine-projects.R` combines several projects, also when they were built from different templates. See [docs/combine-projects.md](docs/combine-projects.md).
- `run-local/examples/` holds worked examples.

## Working with AI assistants

- `AGENTS.md` tells an AI agent how to use this repository, and `llms.txt` lists the files it should read.
- Every report writes `extra-outputs/ai-ready/` with the results as plain tables and a guide to write feedback for participants. Share that folder with an AI assistant to draft letters, flyers or summaries in any language.

## Translations

All report text is in `report/i18n/`. See [report/i18n/README.md](report/i18n/README.md) to correct a translation or add a language.

## Setup, tests and releases

```
Rscript dev/install-packages.R
Rscript tests/testthat.R
Rscript dev/release.R 4.0.0
```

The tests run on every push (`.github/workflows/tests.yml`), including the whole workflow on a simulated project. `dev/release.R` checks `NEWS.md`, updates `CITATION.cff` and runs the tests. Pushing the version tag publishes the GitHub release, which Zenodo archives.

## Meta

-   Please [click here to report issues or bugs](https://github.com/agrdatasci/ClimMob-analysis/issues)
-   License: [MIT](https://github.com/AgrDataSci/ClimMob-analysis/blob/master/LICENSE). Code, report templates and documentation are all under MIT
-   The [Tricot user guide](https://hdl.handle.net/10568/109942) presents the experimental approach used by the ClimMob platform
-   The [ClimMob](https://climmob.net/) platform is a free online software for experimental trials with the tricot citizen-science approach
