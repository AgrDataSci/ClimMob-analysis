ClimMob-analysis (2020-04-29)
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


ClimMob-analysis (2020-04-15)
=========================

### Improvements

* Add conditional checks when only 'overall characteristic' is available to produce the report  



ClimMob-analysis (2020-03-19)
=========================

### Improvements

* Add a script to check for packages updates **R/check_packages.R**. 
* Figures uses new implementations of 'gosset' to abbreviate large item names. 
* Figures now has better resolution with `dpi` argument. The default value is set to dpi=200, but can be adjusted in the main script **ClimMob.R**.
* Text revision, and checks for figure and tables titles for consistency.
* Use numbers for references to make a cleaner document and improve readability.


ClimMob-analysis (2020-03-12)
=========================

### Fixed issues

* Very small sample size in minsplit for `pltree()` particulary in 'toy' demonstration of package automation
* Table of gender disaggregation now works when only one gender are find accross the data


### Improvements

* Figure with items network is added
* References to R packages used for report production are added
* Figure labels are added bellow the figures instead of Figure titles

ClimMob-analysis (2020-02-21)
=========================

First release of new report