# # ................................................................
# # ................................................................
# # Check for the latest released version of core packages
# # ................................................................
# # ................................................................

latest_version <- function(pkg, repo_path){
  
  desc <- readLines(repo_path)
  
  vers <- desc[grepl("Version", desc)]
  
  locvers <- as.character(packageVersion(pkg))
  
  latest <- grepl(locvers, vers)
  
  isTRUE(latest)
  
}

# check if library has the latest pkg version
latest <- latest_version("ClimMobTools",
                         "https://raw.githubusercontent.com/agrobioinfoservices/ClimMobTools/master/DESCRIPTION")
  
if (isFALSE(latest)) {
  
  remotes::install_github("agrobioinfoservices/ClimMobTools",
                           upgrade = "never")

}

latest <- latest_version("gosset",
                         "https://raw.githubusercontent.com/agrobioinfoservices/gosset/master/DESCRIPTION")
  
if (isFALSE(latest)) {
  remotes::install_github("agrobioinfoservices/gosset",
                           upgrade = "never")

}

pkg <- c("PlackettLuce", "partykit", "qvcalc", "psychotools", "jsonlite", 
         "multcompView", "knitr", "rmarkdown", "pls", "gtools", "ggplot2", 
         "igraph")

install.packages(pkg, type = "binary")

