# Example reports

Rendered examples of the ClimMob report, to show partners what the workflow produces without running it. All of them come from the simulated maize project in `tests/testdata/simulated-maize/`, so they hold no real participants.

| File | What it shows |
|---|---|
| `climmob-report-simulated-maize-en.pdf` | The full main report in English, as rendered by the CI job of this repository. The Word original of the same render is `climmob-report-simulated-maize-en.docx`. |
| `sikia-section-simulated-maize.pdf` | The pages that use the simulated Sikia Analytics results: the headline in the Summary, "Why they preferred it" after the ranking, and the section "What participants said in their own words". |

The CI job renders the report in Spanish, English and Norwegian on every pull request and publishes the outputs to the branch `ci-report-output`, together with the participant sheets and the files in `extra-outputs/`. That branch is always the most recent example. The PDFs here are updated by hand at releases.

## Sikia Analytics

ClimMob sends the results of Sikia Analytics in the data file under `SIKIA_results`, with three outputs: a summary of the topics mentioned and their sentiment, the table of mentions with the words behind each one, and a topic tree. The report uses them in three places. The Summary opens with what participants talked about most and one quote. After the ranking of the reference trait, "Why they preferred it" lists the praise and complaints behind every item. A later section shows the sentiment chart, the item by topic heatmap, quotes and the topic tree. The step is `sikia` in `R/49_sikia.R`, the texts are in `report/i18n/report-text.csv` (keys `sikia_*`), and the extra outputs are listed in `docs/outputs.md`. The simulated Sikia results of the fixture were made with `dev/simulate-sikia-results.py`.
