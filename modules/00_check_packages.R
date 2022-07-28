# # ................................................................
# # ................................................................
# # This Module checks for the latest released version of 
# # core packages to run this workflow
# # ................................................................
# # ................................................................
pkg <- c('caret', 'climatrends', 'ClimMobTools', 'ggparty', 'ggplot2', 'gosset', 
         'gridExtra', 'gtools', 'igraph', 'janitor', 'jsonlite', 'knitr', 'leaflet', 
         'mapview', 'multcompView', 'nasapower', 'partykit', 'patchwork', 
         'PlackettLuce', 'plotrix', 'pls', 'png', 'psychotools', 'qvcalc',
         'remotes', 'rmarkdown')


install.packages(pkg)

webshot::install_phantomjs(force = TRUE)

remotes::install_github("AgrDataSci/ClimMobTools",
                        upgrade = "never",
                        force = TRUE)

remotes::install_github("AgrDataSci/gosset",
                        upgrade = "never", 
                        force = TRUE)

# remotes::install_github("AgrDataSci/climatrends",
#                         upgrade = "never", 
#                         force = TRUE)


