ClimMob-analysis v4.0 (2026-10-31)
=========================

### Changes in structure

* The code is reorganised in `R/` by responsibility, replacing `modules/`. `ClimMob.R` is now a short entry point and the call from the server is unchanged.
* `run_step()` replaces the repeated `tryCatch()` blocks. Each step is logged with its status, time, warnings and the R calls that led to an error. Steps that depend on a failed step are skipped.
* The Plackett-Luce outputs (log-worth, worth map, ANOVA, net winning, reliability, Kendall tau) are separate steps, so one failure does not remove the others from the report.
* Trial coordinates are a separate step, so a failed map download no longer stops the agroclimatic analysis.
* Reports are rendered in a temporary copy of `report/`, with an explicit list of objects. Two reports can render at the same time on the server.
* If the main report fails to render, a short report with the errors is written under the same file name, so ClimMob always receives a report.
* Optional debug folder (`CLIMMOB_DEBUG_DIR`) with the inputs, all results and a reproducible function for each failed step.
* `run-local/` calls `run_climmob_analysis()` directly, replacing `source2()` and line numbers.
* Unit tests in `tests/`.
* Unused functions and old templates are removed.

### Bug fixes

* Plackett-Luce trees dropped every covariate when none had near zero variance (`Gdata[, -integer(0)]`), so trees could rarely be produced.
* Writing the agroclimatic CSVs used list names that did not exist and stopped the script before the outlier table was written.
* `force_pseudo_rank()` assigned a transposed matrix, scrambling the pseudo rankings. It now also uses a fixed seed so the same data gives the same report.
* The cap of eight groups was never applied (`isTRUE()` on a vector).
* Covariate type checks failed on objects with two classes, such as dates.
* Highly correlated covariates were both removed. One of each pair is now kept, and the check runs on numeric covariates as intended.
* Covariates with any missing descriptive field (for example `questionAsked`) were silently dropped.
* Covariate names with special characters broke the tree formula.
* `organize_quantitative_data()` read the global `cmdatajson` instead of its argument, and failed when no enumerator column existed.
* The cleaned ranking data was discarded. All steps now use the cleaned data.
* A trait whose columns could not be found stopped the whole analysis. It is now dropped with a message.
* An empty `Reference` gave no check item and broke the reliability section. The first item is used, as documented in v1.1.
* `reliability-data.csv` only had the last check. It now has all checks with a `check` column.
* A failed log-worth plot for the last trait stopped the whole Plackett-Luce step (subscript out of bounds).
* Projects without GPS no longer show a map error in the report appendix.
* Participant reports used the English "Not replied" label in all languages, and row numbers to find their text.
* `paste3()` added double spaces around the conjunction.

### Known behaviour kept on purpose

* Agroclimatic indices are computed but not yet used as covariates in the trees, as before.
* Partial ties are removed from the rankings, as before.
* The table of items by group is switched off, as before.


ClimMob-analysis v2.0 (2025-12-25)
=========================

### Improvements 

- Uses geodata to generate trial maps avoiding issues in importing maps from leaflet
- Updates the introduction text
- Introduces the table with probability of winning


ClimMob-analysis v3.0 (2024-01-25)
=========================

### Improvements 

* Adds analysis of variance for variety performance
* Adds pseudo ranking when network is poorly connected

### Bug fixes 

* Fixes changes in reference for the log-worth plot




ClimMob-analysis v2.1 (2022-12-15)
=========================

### Improvements

* Enables participant individual reports
* Fix small issues in the text in main report 

ClimMob-analysis v2 (2022-06-22)
=========================

### Improvements

* Implement modules of data handling and analysis to allow the generation of different reports
* Add a simple forward selection to choose the best covariates for the Plackett-Luce trees
* Optimize the main script to avoid mistakes in placing tables and indices
* Update the participant reports, now the report in rendered as one single file (two per participant) and uses new display items i) a vertical bar plot to illustrate a podium ii) a horizontal bar plot with the probabilities of winning for the varieties in the baseline trait


ClimMob-analysis v1.2 (2021-03-17)
=========================

### Improvements

* New criteria in filtering the trait entries (participants' responses): (1) having at least 5 valid entries (no NA's, both positive and negative answered), and (2) that all the technologies are tested at least twice per given trait. 
* If no trait passes these criteria, then a report with the message is generated.
* The main script is revised in its structure and documentation to increase readability and improve bug fixes. The new structure to organise the sections follows, first an overview of the ClimMob project. Then the analysis of the main trait, followed by the other traits independently. Sections are now organised by trait and not but the type of analysis.
* The main trait (reference trait) is selected based on the availability of the most common reference traits, first the algorithm looks for the overall performance (or overall preference), then yield, then taste. If none of these traits are found, the algorithm selects the last trait assessed in the project, as provided by the arguments in the file `data.json`.
* The algorithm deals trait/covariates assessed more than once if in different data collection moments. To avoid issues in factor levels, we add an additional string with sequential numbers per each duplicated trait/covariate. Check function `rename_duplicates()` [here](https://github.com/agrobioinfoservices/ClimMob-analysis/blob/41cbb89934407480272318b8b09fe28e3ab7e0a9/R/functions.R#L764).
* We added a new chart showing the number of valid answers received for each trait in the data collection moments. Even those removed for the analysis due to few data available.
* Table with number of technologies assessed by gender now is produced based on the aliases of the variable "REG_gender" which makes possible the inclusion of new aliases.
* Information on the data collection moment is provided and linked to their respective traits and covariates. This will help the reader to find out which trait/covariate belongs to the data collection moment, mostly when the trait/covariate is collected in more than one data collection moment.
* An alpha of 0.5 is used in the Plackett-Luce tree for the main trait. This is to enable the creation of trees even with a small sample size. The algorithm still prints the message if the tree has significant groups with an alpha of 0.1 (default for the analysis).
* Minor improvements in sorting the traits for the participants' reports and how traits and question asked are displayed.
* Analysis will handle ties (if any)

### BUG FIXES

* Indices and p.values were mistakenly printed in the text describing the table of "Summary of differences found in varieties by trait" saying, for example, that one trait had significant difference when it did not. The indices in the data.frame are taken correctly now. This error did not influences the readability of previous versions of the mentioned table, only the text that is printed before it. 



ClimMob-analysis v1.1-2 (2020-12-13)
=========================

### Improvements 

* Enable the production of reports when registration and data collection are performed at the same time (*e.g.* market tasting)
* Variables from multichoice questions in ODK survey are now decoded and displayed as factors.

### Changes in behaviour

* The native characteristic 'overall characteristic' from the ClimMob default library is not required. If not provided the algorithm will use the last characteristic submitted in the file with the parameters for analysis (info.json). The reported is adapted to accept this new behaviour.
* In the case of error in matching the strings of covariates from the parameters (data.json) and data (info.json) the algorithm will not break but rather impute the "intercept" covariate.
* The file "climmob.R" executes all the code required to produce the reports. It calls for the other files in "R/" when needed.
* The reference to the [Tricot user guide](https://hdl.handle.net/10568/109942) is added to the report.

### Bug fixes

* Fixes an issue in matching the names of explanatory variables by using the string `$` to indicate the end of the pattern. 


ClimMob-analysis v1.1-1 (2020-08-14)
=========================

### Improvements
* More informative error messages
* Enable the production of participants individual reports
* Many bug fixes

### Changes in behaviour
* Name the report without project 'code'. The convention is `paste0("climmob_main_report", ".", extension)`

ClimMob-analysis v1.1 (2020-08-08)
=========================

### Improvements
* Analitical process was moved to a single script "R/analysis_climmob.R"
* Dynamic statements for the report based on the in-putted data and results from analysis. This will also make less complicated to translate the report to other languages
* Code is wrapped in a `tryCatch()` to prevent crashes when a non-expected behaviour in the data occurs. When any error occurs during the analysis the script will return a "failed report" with a message asking the user to contact ClimMob supporting service.
* The process now produce the reports for projects with only one characteristic, the 'overall_performance', which is mandatory. If overall_performance is missing the script will return the "failed report".
* Rankings can be build with more than 3 items per participant using `do.call()`, which will build the rankings with `rank_tricot()` when 3 items or with `rank_numeric()` when 4+ items. Projects with 4 or more items (to be tested by each participant) can now be enabled in ClimMob.net

### Changes in behaviour
* New argument (arg[10]) is added for the reference item (from a discussion with IITA and Cornell). The argument can be `NULL` or `NA` (which will take the first item as reference) or a character string selected by the user which should match with one of the items tested in the project.
* Map is build with package `leaflet`
* New packages added `leaflet` and `multcompView`
* Coordinates in the report map (if any) are clustered in groups with a resolution of 0.05, to comply with the participant's privacy. 


ClimMob-analysis v0.9 (2020-06-04)
=========================
### Improvents
* New function to plot Plackett-Luce trees

### Changes in behaviour
* New R packages added "patchwork", "ggparty", "ggrepel"]
* New bib file for the main report


ClimMob-analysis v0.8 (2020-04-29)
=========================

### Improvements
* Add map with trial plots when lonlat is available.
* Add number of valid answers in each characteristic in the first table of Section 1.
* Remove legend of self-explained figures.
* Add Partial Least Squares biplot.
* Some analysis are still performed in the .Rmd file as written by Stats4Dev. I've started a migration to the 'R/analysis_climmob.R' file for clarity and to make easy future debugs.
* Setup covariates before PLT analysis. Round floating numbers to 3 decimals (eg. lonlat). And coerce characters into factors.
* Fix colunms with p.values and harmonize all related tables

### Changes in behaviour
* Packages 'mapview' and 'ggrepel' are added

### Bug fixes
* Fix index of Table 1
* Fix palette smoothig in Fig 3.2 (and the others related to other characteristics) to accept more than 9 items.


ClimMob-analysis v0.7 (2020-04-15)
=========================

### Improvements

* Add conditional checks when only 'overall characteristic' is available to produce the report  



ClimMob-analysis v0.6 (2020-03-19)
=========================

### Improvements

* Add a script to check for packages updates **R/check_packages.R**. 
* Figures uses new implementations of 'gosset' to abbreviate large item names. 
* Figures now has better resolution with `dpi` argument. The default value is set to dpi=200, but can be adjusted in the main script **ClimMob.R**.
* Text revision, and checks for figure and tables titles for consistency.
* Use numbers for references to make a cleaner document and improve readability.


ClimMob-analysis v0.5 (2020-03-12)
=========================

### Fixed issues

* Very small sample size in minsplit for `pltree()` particulary in 'toy' demonstration of package automation
* Table of gender disaggregation now works when only one gender are find accross the data


### Improvements

* Figure with items network is added
* References to R packages used for report production are added
* Figure labels are added bellow the figures instead of Figure titles

ClimMob-analysis v0.4 (2020-02-21)
=========================

First release of new report