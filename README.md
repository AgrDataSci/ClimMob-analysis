<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3984711.svg)](https://doi.org/10.5281/zenodo.3976631)
[![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# Workflow for automated analysis and report of decentralized experimental data with the tricot approach

## Overview

This repository contains the code used in the workflow to analyse data and create reports for the [ClimMob](https://climmob.net/) platform. Read the latest enhancements in this algorithm [here](NEWS.md).

## Repository organisation

**ClimMob.R** is the main script in this repository. It performs the analysis, calls for the other supporting files and generates the automated reports for the ClimMob project and the individual reports for the participants in the project. 

  - **R/** contains the supporting scripts used to organise the data and perform the analysis. They are called by ClimMob.R
  - **report/** contains the markdown templates to produce the reports. 
  - **run-local** contains the script to prepare a set of ClimMob project(s) locally before sending it for analysis

## Meta

  - Please [report any issues or bugs](https://github.com/agrdatasci/ClimMob-analysis/issues)
  - License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
  - The [Tricot user guide](https://hdl.handle.net/10568/109942) shows how the experimental method inside the ClimMob platform
  - The [ClimMob](https://climmob.net/) platform is an free on-line software for experimental trials with the tricot citizen-science approach
  
