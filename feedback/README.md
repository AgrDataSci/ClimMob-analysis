# Feedback templates

Ready-made material to give results back to the people who took part in a tricot trial. The templates take the tables that ClimMob-analysis writes to `extra-outputs/ai-ready/`, plus a short `trial.json` with what those tables do not hold, and produce print-ready PDFs. They were developed and field tested on the INNOFORAGE forage trials in Norway (2025 to 2026) and follow the design rules of the tricot feedback studies (Schumann et al. 2024, Müller et al. 2025).

| Product | File | For whom | Pages |
|---|---|---|---|
| Participant sheet | `templates/participant-sheet.html` | each participant, personal | 1 per participant |
| Items page | `templates/items-page.html` | each participant, optional page 2 | 1 per participant |
| Community flyer | `templates/community-flyer.html` | everyone, village meeting, non-responders | 2 |
| Certificate | `templates/certificate.html` | each participant, A5 landscape | 1 per participant |
| Workshop guide | `templates/workshop-manual.html` | the facilitator of the feedback meeting | 1 |

Rendered examples are in `example/out/` (simulated cassava trial, invented names).

## How it works

```
extra-outputs/ai-ready/*.csv  +  trial.json
            │
            ▼   tools/build_feedback.py   (or tools/render.R from feedback.json)
       feedback.json  ──►  templates/*.html + theme.css  ──►  HTML  ──►  PDF, PNG
```

1. Run the ClimMob analysis. It writes `extra-outputs/ai-ready/` with `trial-summary.csv`, `results-by-trait.csv`, `participants.csv` and `participant-answers.csv`.
2. Write `trial.json`. Copy `example/trial.json` and fill it in. The checklist below says what goes in it.
3. Build.

   ```
   python tools/build_feedback.py <ai-ready folder> trial.json <output folder> --png
   ```

   or, with R,

   ```
   Rscript tools/render.R feedback.json <output folder>
   ```

   The Python script needs `playwright` (`pip install playwright && playwright install chromium`) for PDFs. Without it, it still writes the HTML files, which open in any browser and print to PDF from there. The R script needs `jsonlite`, `whisker` and `pagedown` with Chrome installed.

## Checklist, what the trial manager must provide

The tables give the results. Everything that concerns people, places and the project comes from `trial.json`. Nothing is invented.

Required

- `name`, `year`, `season_label` of the trial, in the language of the material.
- `language`, one of the files in `text/` (`en`, `nb`). Add a language by copying `text/en.json` and translating the values.
- `contact`, one line with a person, an organisation and an email or phone that participants may use.
- `partners`, one line with the organisations behind the trial.

Needed for the QR codes

- `website`, the registration site. It appears on the flyer with a QR code.
- `qr_url_template` or a `qr_url` column in `participants.csv`. The template may use `{package}` and `{participant_id}`. Each participant gets a QR code to their own registration link, so they never have to type a code.

Needed for the certificates

- `signatories`, one or two people with `name`, `role` and `org`.
- `cert_place_date`, for example "Umudike, 15 November 2025".

Optional

- `moment_labels`, a short description of each data collection moment, shown under the season timeline. Moments are read from the bracket in trait names, "Yield [1st cut]".
- `optional_moments`, moments that are not expected from everyone, shown dashed.
- `items_table` and `items_note`, a table about the tested items (composition, origin, release year, uses). Adds the items page to every participant sheet, with the participant's own three columns highlighted.
- `flyer`, the texts of the community flyer: `headline`, `lead`, `kpis`, `callout_title`, `callout`, `basis_note`, `page2_title`, `page2_intro`, `audience` (`all`, the default, thanks everyone; `non-responders` asks for the missing answers), optional `band_title` and `band` to override the bottom band, and `map` (path to a PNG of the trial map, for example the participation map from the ClimMob report).
- `workshop`, to replace the default facilitator guide in `text/workshop-default.<lang>.json`.
- `text`, overrides for any string in `text/<lang>.json`, for example to say "mixture" instead of "option".
- `short_item_names`, shorter labels for the flyer table.

## Rules the templates follow

- Thanks first, results second. Every sheet opens with thanks and says how many registrations the person sent.
- Option letters A, B and C are shown with the item names.
- A participant who did not answer gets the same sheet, with "?" where their answer would be and the ranking of the others for their three options. No ranking is invented for them.
- The points of the three options are the shares of the Plackett-Luce worths of the reference trait, so the three share 100 points.
- "Most participants said" is the option among the participant's three with the highest worth for that trait. "agree" appears when it matches the participant's own answer.
- Every sheet says that results are the participants' own judgements, not measured yield, and that the tested items may not be released or available for some time.
- Names, locations and emails are personal data. They appear only on that person's sheet and certificate, never on the flyer. Mailing lists stay inside the team.
- Colours come from the five variables at the top of `templates/theme.css`. The layout prints well in black and white.

## Groups

When `participants.csv` has a `group` column (region, agroecology, gender) and `results-by-trait.csv` has the same column, every sheet compares the participant with their own group and the flyer shows one top 3 and one trait table per group. Without a `group` column in the results, the trial-wide results are used and no group is named.

## Files

```
feedback/
├── README.md                this file
├── AGENTS.md                protocol for AI assistants
├── templates/               Mustache HTML templates and theme.css
├── text/                    strings per language, default workshop guide
├── tools/
│   ├── build_feedback.py    ai-ready tables + trial.json -> feedback.json, HTML, PDF, PNG
│   ├── render.R             feedback.json -> HTML, PDF
│   └── mustache.py          small Mustache renderer used by build_feedback.py
└── example/
    ├── ai-ready/            simulated cassava trial in the ai-ready format
    ├── trial.json           filled-in example
    └── out/                 rendered PDFs, PNGs and feedback.json
```

## References

- Schumann C, Londoño JM, Müller A (2024). Farmer feedback in tricot, 2024 review. Alliance of Bioversity International and CIAT. https://hdl.handle.net/10568/169094
- Müller A, Schumann C, Londoño JM (2025). Farmer feedback in tricot. Redesigning the tricot feedback process and field validation 2024 to 2025. Alliance of Bioversity International and CIAT. https://hdl.handle.net/10568/179807
