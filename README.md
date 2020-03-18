
<!-- badges: start -->
[![license](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
[![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# *ClimMob*: Workflow for data analysis using the ClimMob platform

## Overview

This repository contains the code used in the workflow to analyse and create reports from the [ClimMob](https://climmob.net/blog/) platform. Read the latest enhancements in this algorithm [here](NEWS.md).

## Repository organisation

**ClimMob.R** is the main script in this repository. It calls for the other supporting files/scrips and generates the automated reports for the ClimMob project and the infosheets for participants. 

  - **R/** contains the supporting scripts used to organise the data and perform the analysis. They are called by ClimMob.R
  - **report/** with its subdirectories report/en and report/es contains the markdown templates to produce the reports in both English (en) and Spanish (es, not implemented yet). 
  - **dev/** is used for the development of the package and should not be used to call for analysis or report production.



## Meta

  - Please [report any issues or bugs](https://github.com/agrobioinfoservices/ClimMob-analysis/issues).

  - License: GPL-3.
