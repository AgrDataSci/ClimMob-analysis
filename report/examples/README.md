# Example reports

Rendered examples of the ClimMob report, to show partners what the workflow produces without running it. All of them come from the simulated maize project in `tests/testdata/simulated-maize/`, so they hold no real participants.

| File | What it shows |
|---|---|
| `climmob-report-simulated-maize-en.pdf` | The full main report in English, as rendered by the CI job of this repository. |
| `sikia-section-simulated-maize.pdf` | The section "What participants said in their own words", built from the simulated Sikia Analytics results in the fixture. |

The CI job renders the report in Spanish, English and Norwegian on every pull request and publishes the outputs to the branch `ci-report-output`, together with the participant sheets and the files in `extra-outputs/`. That branch is always the most recent example. The PDFs here are updated by hand at releases.

## Sikia Analytics

ClimMob sends the results of Sikia Analytics in the data file under `SIKIA_results`, with three outputs: a summary of the topics mentioned and their sentiment, the table of mentions with the words behind each one, and a topic tree. The report shows them in one section. The step is `sikia` in `R/49_sikia.R`, the texts are in `report/i18n/report-text.csv` (keys `sikia_*`), and the extra outputs are listed in `docs/outputs.md`. The simulated Sikia results of the fixture were made with `dev/simulate-sikia-results.py`.
