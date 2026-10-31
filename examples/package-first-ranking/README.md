# Package-first ranking example

**Status: unexecuted proposal, not production code.**

This example starts with the documented `gosset::rank_tricot2()` and `nicabean` interface instead of maintaining a custom tricot ranking parser. It uses `=` for assignment.

Run after installing `gosset` and `PlackettLuce`:

```sh
Rscript examples/package-first-ranking/ranking_with_gosset.R
```

Before scientific use, pin package versions, run R tests, inspect actual rankings, check comparison-network connectivity and model identifiability, evaluate uncertainty and pseudo-ranking sensitivity, and verify interpretation with a known reference result. Source `AgrDataSci/gosset/R/rank_tricot2.R` was inspected on 2026-10-31: it warns about missing and duplicate observations but is not a complete design validator.

The earlier handwritten rank-matrix example is superseded. This example does not modify ClimMob production reporting.
