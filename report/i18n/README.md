# Translations

All text of the reports is in two files.

- `report-text.csv`, the main report, its tables and the chart labels
- `feedback-text.csv`, the feedback sheet and the certificate for participants

Each row is one piece of text. The column `key` is the name used in the code and must not change. The other columns are the languages: `en` English, `es` Spanish, `fr` French, `pt` Portuguese, `sw` Swahili and `no` Norwegian (Bokmål).

## Editing a translation

1. Open the file in a spreadsheet program as UTF-8 CSV, or in a text editor.
2. Change the text in the column of your language.
3. Keep words in curly brackets, such as `{options}` or `{n}`, exactly as they are. They are replaced by values from the trial. You can move them within the sentence or leave out one you do not need, but do not add new ones.
4. Keep references in square brackets, such as `[@Plackett1975]`, as they are.
5. Save as CSV in UTF-8 with all fields in quotes.

An empty cell falls back to English.

## Adding a language

1. Add a column named with the two-letter language code (ISO 639-1), for example `rw` for Kinyarwanda.
2. Add the code to `climmob_languages` in `R/03_i18n.R`.
3. Fill in the texts, starting with `feedback-text.csv`, which farmers read.
4. Run `Rscript tests/testthat.R` to check the placeholders.

ClimMob sends the language of the report in the request field `Languaje`.

## Review status

The English text is the reference. The Spanish, French and Portuguese texts were written alongside it. The Swahili and Norwegian texts need review by native speakers before wide use, in particular the feedback sheet that farmers read.
