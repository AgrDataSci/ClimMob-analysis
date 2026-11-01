# Trial results ready for AI assistants

These files hold the results of one tricot trial analysed with the ClimMob workflow. Share this folder with an AI assistant and ask it to write feedback for participants, a flyer for a community meeting, or a short summary for partners.

## Files

| File | One row per | Use it for |
|---|---|---|
| `trial-summary.csv` | trial | crop, number of participants, items, checks, reference trait |
| `results-by-trait.csv` | item and trait | how all participants together ranked the items (`worth_percent`, `rank`) |
| `item-vs-check.csv` | item and trait | whether an item was better, similar or worse than the check (`result`, `prob_beats_check`) |
| `participants.csv` | participant | the three options each participant tested (`item_A`, `item_B`, `item_C`), the own ranking of the reference trait (`first`, `second`, `third` hold option letters) and the points of the three options in the ranking of everyone |
| `participant-answers.csv` | participant and trait | the best and worst option given by each participant, trait by trait |

Full column definitions are in `docs/outputs.md` of the ClimMob-analysis repository.

## Instructions for the AI assistant

Follow these rules when you write material for participants. They come from the design and field validation of tricot feedback sheets (Schumann et al. 2024, Müller et al. 2025).

1. Write in the language and register the user asks for. Keep sentences short and words simple. Many readers have little formal schooling.
2. Lead with thanks. Participants gave their time, land and knowledge. Say so before any result.
3. Use the option letters (A, B, C) together with the item names. Farmers remember the letters from the trial.
4. Show the participant's own ranking next to the ranking of everyone for the same three options. Use `first`, `second`, `third` and the `points_` columns of `participants.csv`. Never invent a ranking for someone who did not answer (`answered_reference_trait` is FALSE). Thank them and show the results of everyone instead.
5. Give one best and one worst mark per trait from `participant-answers.csv`. Do not add traits that are not in the file.
6. Report results of the whole trial from `results-by-trait.csv` and `item-vs-check.csv` only. Say "most participants preferred", not "is the best variety". Results come from farmers' rankings in their own fields, not from measured yields, unless the user provides measured data.
7. Manage expectations. Always state that the tested items may not be released or available for some time, unless the user tells you otherwise. Do not promise seed.
8. Add advice, contacts or seed sources only when the user provides them.
9. Keep one page per participant. Plan for black and white printing and for sharing on phones.
10. Treat names and locations as personal data. Use them only in material for that participant, and leave them out of anything shared more widely.

## Example requests

- "Write a one-page feedback sheet in Norwegian for each participant in `participants.csv`."
- "Make a flyer for a village meeting with the overall results, for participants who did not send their answers."
- "Summarise which items beat the check and on which traits, for the breeding team."

## References

- Schumann C, Londoño JM, Müller A (2024). Farmer feedback in tricot, 2024 review. Alliance of Bioversity International and CIAT. https://hdl.handle.net/10568/169094
- Müller A, Schumann C, Londoño JM (2025). Farmer feedback in tricot. Redesigning the tricot feedback process and field validation 2024 to 2025. Alliance of Bioversity International and CIAT. https://hdl.handle.net/10568/179807
