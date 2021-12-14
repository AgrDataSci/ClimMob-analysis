# # ................................................................
# # ................................................................
# # Check for the latest released version of core packages
# # run this code each 15-days
# # ................................................................
# # ................................................................

pkg <- c("remotes", "PlackettLuce", "partykit", "qvcalc", "psychotools", "jsonlite", 
         "knitr", "rmarkdown", "pls", "gtools", "ggplot2", "leaflet", "mapview",
         "igraph", "ggrepel", "patchwork", "ggparty", "multcompView",
         "png", "plotrix", "gridExtra", "webshot", "caret")


install.packages(pkg)

webshot::install_phantomjs(force = TRUE)

remotes::install_github("agrobioinfoservices/ClimMobTools",
                        upgrade = "never",
                        force = TRUE)

remotes::install_github("agrobioinfoservices/gosset",
                        upgrade = "never", 
                        force = TRUE)


