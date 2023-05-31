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
         'remotes', 'rmarkdown', 'lubridate')

install.packages(pkg)

webshot::install_phantomjs(force = TRUE)
