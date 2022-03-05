# # ................................................................
# # ................................................................
# # Check for the latest released version of core packages
# # run this code each 15-days
# # ................................................................
# # ................................................................
pkg <- c("PlackettLuce","partykit","qvcalc","psychotools","jsonlite","knitr","rmarkdown",
         "pls","gtools","ggplot2","igraph","ggrepel","ggparty","patchwork","leaflet",
         "mapview","multcompView","png","plotrix","gridExtra","caret", "janitor",
         "GGally")


install.packages(pkg)

webshot::install_phantomjs(force = TRUE)

remotes::install_github("agrobioinfoservices/ClimMobTools",
                        upgrade = "never",
                        force = TRUE)

remotes::install_github("agrobioinfoservices/gosset",
                        upgrade = "never", 
                        force = TRUE)


