
<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3976638.svg)](https://doi.org/10.5281/zenodo.3976638)
[![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# Workflow for data analysis and report with crowdsourced citizen science-generated data

## Overview

This repository contains the code used in the workflow to analyse data and create reports for the [ClimMob](https://climmob.net/blog/) platform. Read the latest enhancements in this algorithm [here](NEWS.md).

## Repository organisation

**ClimMob.R** is the main script in this repository. It calls for the other supporting files/scrips and generates the automated reports for the ClimMob project and the infosheets for participants. 

  - **R/** contains the supporting scripts used to organise the data and perform the analysis. They are called by ClimMob.R
  - **report/** with its subdirectories report/en and report/es contains the markdown templates to produce the reports in both English (en) and Spanish (es, not implemented yet). 



## Meta

  - Please [report any issues or bugs](https://github.com/agrobioinfoservices/ClimMob-analysis/issues).

  - License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
