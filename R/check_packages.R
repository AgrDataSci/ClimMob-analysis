# # ................................................................
# # ................................................................
# # Check for the latest released version of core packages
# # run this code each 15-days
# # ................................................................
# # ................................................................
install.packages("remotes")
library("remotes")

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
                          upgrade = "never",
                          force = TRUE)

}

latest <- latest_version("gosset",
                         "https://raw.githubusercontent.com/agrobioinfoservices/gosset/master/DESCRIPTION")
  
if (isFALSE(latest)) {
  remotes::install_github("agrobioinfoservices/gosset",
                          upgrade = "never", 
                          force = TRUE)

}

pkg <- c("PlackettLuce", "partykit", "qvcalc", "psychotools", "jsonlite", 
         "knitr", "rmarkdown", "pls", "gtools", "ggplot2", "leaflet",
         "igraph", "ggrepel", "patchwork", "ggparty", "multcompView",
         "png", "plotrix","gridExtra")


install.packages(pkg)

