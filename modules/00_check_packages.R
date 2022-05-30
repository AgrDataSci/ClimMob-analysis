# # ................................................................
# # ................................................................
# # This Module checks for the latest released version of 
# # core packages to run this workflow
# # ................................................................
# # ................................................................
pkg <- c("PlackettLuce","partykit","qvcalc","psychotools","jsonlite","knitr","rmarkdown",
         "pls","gtools","ggplot2","igraph","ggparty","patchwork","leaflet",
         "mapview","multcompView","png","plotrix","gridExtra","caret", "janitor",
         "ClimMobTools", "gosset", "nasapower", "climatrends")


install.packages(pkg)

webshot::install_phantomjs(force = TRUE)

remotes::install_github("AgrDataSci/ClimMobTools",
                        upgrade = "never",
                        force = TRUE)

remotes::install_github("AgrDataSci/gosset",
                        upgrade = "never", 
                        force = TRUE)

remotes::install_github("AgrDataSci/climatrends",
                        upgrade = "never", 
                        force = TRUE)

