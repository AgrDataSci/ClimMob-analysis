# Instructions for AI agents

This repository holds the R workflow that analyses tricot trials and writes the reports of the [ClimMob](https://climmob.net/) platform. People share it with an AI assistant for three kinds of work. Find which one the user wants before you start.

1. **Analyse their own ClimMob data**, one project or several projects together, on their computer.
2. **Turn results into material for people**, such as feedback for farmers, flyers or summaries, from the files in `extra-outputs/ai-ready/`.
3. **Change the code**, for example a fix, a new analysis or a new language.

## 1. Analysing ClimMob data

- One project. Follow `run-local/analyse-one-project.R`. `build_pars_from_cmlist()` reads the traits, measured traits and covariates from the project, so the user does not need the ClimMob request.
- Several projects, including projects built from different templates. Follow `run-local/combine-projects.R` and `docs/combine-projects.md`. Always write the harmonisation table, show the user what it proposes, and ask them to confirm which questions and data collection moments are the same before combining. Do not guess that two questions are the same because their names look alike.
- The user needs R 4.1 or newer and the packages in `R/00_packages.R` (`Rscript dev/install-packages.R`). They also need their ClimMob API key. Ask them to store it in the environment variable `CLIMMOB_KEY` and never write it in a script or a message.
- Run `run_climmob_analysis()` first and show `print_run_summary(res$log)`. Explain any failed step in plain words before writing outputs.
- Every output is described in `docs/outputs.md`. Use it to explain results. Do not invent columns or results.

## 2. Material for participants and partners

The folder `extra-outputs/ai-ready/` of each report holds plain tables and a `README.md` with rules for writing feedback, based on the field-tested design of tricot feedback sheets. Follow that README. The key points:

- thank participants first and keep one page per person;
- use the option letters (A, B, C) with the item names;
- report only what is in the tables, and say that the tested items may not be released;
- treat names and locations as personal data.

## 3. Changing the code

- Read `README.md` for the layout. The analysis is a list of named steps in `R/90_pipeline.R`, each a plain function in `R/`.
- Keep the contract with ClimMob. The server calls `Rscript ClimMob.R <parameters.json> <data.json>` and expects `climmob_main_report.<Format>`, `participant-report/participant_report_package_<i>.<Format>` and the files in `extra-outputs/` listed in `docs/outputs.md`. Do not rename them.
- Put user-facing text in `report/i18n/*.csv` and use `tr("key", name = value)` in code and templates. Never hard-code report text in one language. Add the English text, and the other languages when you can, otherwise leave them empty (they fall back to English).
- Code style. Use `=` for assignment, not `<-`. Use base R where possible. New functions get a short roxygen-style comment with an example.
- Add or update tests in `tests/testthat/` and run `Rscript tests/testthat.R`. The test `test-pipeline-fixture.R` runs the whole workflow on simulated data in `tests/testdata/simulated-maize`.
- Never commit real trial data. Tricot data hold names, phone numbers and locations of participants, and some projects belong to partners. Use the simulated fixture or simulate new data.
- Record changes in `NEWS.md`. Releases are made with `dev/release.R` (see `README.md`).

## Where to find more

- Methods and evidence on tricot: the knowledge base at https://github.com/AgrDataSci/tricot-knowledge-base (read its `AGENTS.md`).
- R packages used: ClimMobTools (data access), gosset and PlackettLuce (ranking analysis), climatrends (climate indices).
- Report issues at https://github.com/AgrDataSci/ClimMob-analysis/issues.
