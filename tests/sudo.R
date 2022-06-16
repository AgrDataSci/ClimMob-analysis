tag <- "testdata3/"
path <- paste0("tests/", tag)

args <- c(paste0(path, "data.json"),
          paste0(path, "info.json"),
          paste0("tests/output/",tag,""),
          "FALSE",
          "en",
          "docx",
          "farmer",
          "variety",
          paste0(getwd(), "/"),
          "gender",
          "3")

source("modules/01_functions.R")

checkfile <- readLines("ClimMob.R")
b <- which(grepl("# Arguments ####", checkfile)) + 3
e <- which(grepl("# End of analysis", checkfile))

# Run the workflow to analyse the data and produce the report 
source2("ClimMob.R", b, e)

# sudo Rscript ClimMob.R tests/testdata1/data.json tests/testdata1/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata1/ FALSE en docx farmer variety /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender1 Danila
# sudo Rscript ClimMob.R tests/testdata2/data.json tests/testdata2/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata2/ FALSE en docx farmer variety /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender1 Gloria
# sudo Rscript ClimMob.R tests/testdata3/data.json tests/testdata3/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata3/ FALSE en docx farmer variety /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender1 
# sudo Rscript ClimMob.R tests/testdata4/data.json tests/testdata4/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata4/ FALSE en docx farmer genotype /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender
# sudo Rscript ClimMob.R tests/testdata5/data.json tests/testdata5/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata5/ FALSE en docx farmer variety /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender
# sudo Rscript ClimMob.R tests/testdata6/data.json tests/testdata6/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata6/ FALSE en docx farmer variety /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender
# sudo Rscript ClimMob.R tests/testdata7/data.json tests/testdata7/info.json /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/tests/output/testdata7/ FALSE en docx farmer variety /Users/kauedesousa/Library/Mobile\ \Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/ gender

