# Feedback material, protocol for AI assistants

Read this when a user asks for feedback sheets, farmer reports, a flyer, certificates or a feedback workshop for a tricot trial. The templates in this folder are the starting point. Do not design a layout from scratch, and do not write participant text without the tables.

## 1. Start by showing what exists

Before asking anything, show the user the five products and what each is for, with the example renderings in `example/out/*.png` when you can display images. Say in one sentence that the material is built from the ClimMob analysis tables plus a short `trial.json`, and that nothing in it is invented.

## 2. Ask for what is missing, as one list

Check what the user already has, then give one point-by-point list of what you still need. Use this order and skip what is already there.

1. The `extra-outputs/ai-ready/` folder of the analysis (four CSV files). If they only have raw ClimMob data, run the analysis first (see the repository `AGENTS.md`, section 1).
2. Trial name, season label and year, in the language of the material.
3. Language. Available: English (`en`) and Norwegian (`nb`). Offer to add another by translating `text/en.json`, and ask the user to check the translation.
4. A contact line and the partner organisations.
5. The registration website and the link pattern for the per-package QR codes, or a column with one link per participant. Without this, say that the QR codes will point to the website only.
6. For certificates, one or two signatories with role and organisation, and the place and date.
7. Optional, and worth asking about once: a table describing the items (composition, release year, uses), short labels for each data collection moment, a trial map image for the flyer, a `group` column if the user wants results by region or another grouping.
8. Which products they want. Default to the participant sheets and the flyer. Offer the certificates and the workshop guide when a meeting with participants is planned.

Stop and wait for the answers. Do not fill gaps with plausible values. A missing contact or signatory stays blank and is reported as missing.

## 3. Build

1. Write `trial.json` from the answers. Start from `example/trial.json`.
2. Run `python tools/build_feedback.py <ai-ready> trial.json <out> --png`, or build `feedback.json` and run `Rscript tools/render.R` when the user works in R.
3. Open the PNG previews and look at them. Check at least one sheet of each kind: a participant who answered everything, one who answered part, one who answered nothing.

## 4. Check before handing over

- Counts. The number of participants on the flyer and in the sheets equals the rows of `participants.csv`. Say which number you used when the user's own count differs (withdrawn packages are a common reason).
- QR codes. Decode a few and confirm that each opens that participant's own link.
- Groups. When the user mentions regions or groups, confirm that `results-by-trait.csv` carries the group column. If it does not, the sheets compare with the whole trial and must not name a group.
- Claims. Every statement on the flyer must follow from the tables. Differences between groups or items are not stated as facts unless the user confirms they were tested. Write "most participants preferred", not "is the best".
- Expectations. The sheet says that items may not be released or available. Keep it unless the user says otherwise.
- Language. Short sentences, no jargon. Read one sheet aloud in your head as a farmer would.
- Privacy. Names, locations and emails appear only on personal sheets and certificates, never on the flyer or in anything shared outside the team. Mailing lists are internal files.

## 5. Hand over

Give the user the PDFs, one sheet per participant when they will be emailed, and tell them in a few lines what the numbers are based on, what you left blank and what they should verify (dates, names of signatories, the registration schedule). Offer the mailing script pattern from the INNOFORAGE case (one email per participant with their sheet and the flyer attached) only when they ask how to send.

## What not to do

- Do not run or copy the archived project code of other trials. Use these templates.
- Do not change `theme.css` colours or the layout for one trial unless the user asks for their own branding. Then change only the variables at the top of the file.
- Do not add traits, items, advice or seed sources that are not in the tables or given by the user.
- Do not put results into the certificate. It thanks, it does not rank.
