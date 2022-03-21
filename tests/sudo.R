tag <- "testdata1"

args <- c(paste0("tests/",tag,"/data.json"), # a json file with parameters for the analysis
          paste0("tests/",tag,"/info.json"), # a json file with the results
          paste0("dev/output/",tag,""), # the path where results will be written
          "FALSE", # logical, if infosheets should be written TRUE FALSE
          "en", # the language to write the report "en" for english and "es" for spanish
          "docx", # report file format it can be "docx", "pdf", and "html"
          "farmer", # how the report will refer to participants/farmers
          "genotype", # how the report will refer to tested technologies
          getwd(), # this is backward path
          "REG_gender1", # any group to do the analysis 
          NULL, # the reference item for the analysis
          NA, # minimum n of complete data required in a trait evaluation before it is excluded
          NA, # minimum n of items tested, e.g. that all items are tested at least twice
          NA, # minimum proportion of covariates compared to total valid n
          NA, 
          0.1, # significance level for the standard PL model
          0.5, # significance level for the tree
          100) # minimum n in each tree node

source("modules/helper_02_internal_functions.R")

checkfile <- readLines("ClimMob.R")
b <- which(grepl("# Arguments ####", checkfile)) + 3
e <- which(grepl("# End of analysis", checkfile))

# Run the workflow to analyse the data and produce the report 
source2("ClimMob.R", b, e)

# sudo Rscript ClimMob.R dev/data/Pot21A/data.json dev/data/Pot21A/info.json /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/dev/output/Pot21A/ FALSE en docx farmer variety /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/ Kirundo
# sudo Rscript ClimMob.R dev/data/21AIP/data.json dev/data/21AIP/info.json /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/dev/output/21AIP/ TRUE en docx farmer variety /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/ Kirundo
# sudo Rscript ClimMob.R dev/data/RTBKUM/data.json dev/data/RTBKUM/info.json /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/dev/output/RTBKUM/ FALSE en docx farmer variety /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/
# sudo Rscript ClimMob.R dev/data/OPP1114827/data.json dev/data/OPP1114827/info.json /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/dev/output/OPP1114827/ TRUE en docx farmer variety /Users/kauedesousa/OneDrive\ \-\ \Høgskolen\ \i\ \Innlandet/Rcode/ClimMob-analysis/
