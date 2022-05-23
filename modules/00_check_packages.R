# # ................................................................
# # ................................................................
# # This Module checks for the latest released version of 
# # core packages to run this workflow
# # ................................................................
# # ................................................................
pkg <- c("PlackettLuce","partykit","qvcalc","psychotools","jsonlite","knitr","rmarkdown",
         "pls","gtools","ggplot2","igraph","ggrepel","ggparty","patchwork","leaflet",
         "mapview","multcompView","png","plotrix","gridExtra","caret", "janitor",
         "GGally", "ClimMobTools", "gosset", "network")


install.packages(pkg)

webshot::install_phantomjs(force = TRUE)

remotes::install_github("AgrDataSci/ClimMobTools",
                        upgrade = "never",
                        force = TRUE)

remotes::install_github("AgrDataSci/gosset",
                        upgrade = "never", 
                        force = TRUE)

