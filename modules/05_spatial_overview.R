#' Produces a trial location map from GPS coordinates
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param output_path the working directory to write the leaflet map
#' @param backward_path an absolute filepath representing the current working directory
#' @examples 
#' modules = list.files("modules",
#' full.names = TRUE,
#' pattern = ".R")
#' modules = modules[-which(grepl("check_packages.R", modules))]
#' for (i in seq_along(modules)) {
#'   source(modules[i])
#' }
#' 
#' load("modules/example-data-structure.rda")
#' 
#' coords = get_testing_sites_map(cmdata, 
#'                                output_path = "tests/output/testdata12", 
#'                                backward_path = getwd())
#' 
#' coords$map
#' coords$coords
#' 
#' @export
get_testing_sites_map = function(cmdata, output_path, backward_path){
  
  # Check if lonlat is provided
  lon = grepl("geotrial_longitude", names(cmdata))
  lat = grepl("geotrial_latitude", names(cmdata))
  
  geoTRUE = all(any(lon), any(lat))
  
  if (isFALSE(geoTRUE)) {
    return(list(geoTRUE = FALSE))
  }
  
  # find the vector with most completeness 
  less_nas = lapply(cmdata[lon], function(x){
    sum(is.na(x))
  })
  
  lon = names(which.min(unlist(less_nas)))
  lat = gsub("_longitude", "_latitude", lon)
  
  lonlat = cmdata[, c(lon,lat)]
  
  lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
  
  lonlat[,1] = ifelse(lonlat[,1] > 180 | lonlat[,1] < -180, NA, lonlat[,1])
  
  lonlat[,2] = ifelse(lonlat[,2] > 70 | lonlat[,2] < -70, NA, lonlat[,2])
  
  nlonlat = dim(lonlat)[[1]]
  
  if (nlonlat > 0) {
    
    trial_map = plot_map(data = lonlat, 
                         make.clusters = FALSE,
                         xy = c(1, 2),
                         minimap = TRUE, 
                         map_provider = "OpenStreetMap.Mapnik")
    
    tempmap = paste0(getwd(), "/tempmap/")
    
    dir.create(tempmap, recursive = TRUE, showWarnings = FALSE)
    
    mapview::mapshot(trial_map, 
                     url = paste0(tempmap, "/trial_map.html"),
                     file = paste0(tempmap, "/trial_map.png"))
    
  }
  
  result = list(geoTRUE = TRUE, 
                mapDIR = tempmap,
                map_path = paste0(tempmap, "/trial_map.png"),
                map = trial_map,
                coords = lonlat)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_trial_map = list(geoTRUE = FALSE, 
                            map_path = "",
                            map = 0L,
                            coords = data.frame())

