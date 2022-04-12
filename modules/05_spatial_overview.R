# Check if lonlat is provided
lon <- grepl("_longitude", names(cmdata))
lat <- grepl("_latitude", names(cmdata))

geoTRUE <- all(any(lon), any(lat))

if (isTRUE(geoTRUE)) {
  
  # find the vector with most completeness 
  less_nas <- lapply(cmdata[lon], function(x){
    sum(is.na(x))
  })
  
  lon <- names(which.min(unlist(less_nas)))
  lat <- gsub("_longitude", "_latitude", lon)
  
  lonlat <- cmdata[,c(lon,lat)]
  
  lonlat <- na.omit(lonlat)
  
  nlonlat <- dim(lonlat)[[1]]
  
  if (nlonlat > 0){
    
    trial_map <- plot_map(lonlat, xy = c(1, 2), minimap = TRUE, 
                          map_provider = "OpenStreetMap.Mapnik")
    
    mapshot(trial_map, 
            url = paste0(outputpath, "/", projname, "_trial_map.html"),
            file = paste0(outputpath, "/", projname, "_trial_map.png"))
    
  }
  
  if (nlonlat == 0) {
    geoTRUE <- FALSE
  }
  
}

if (isFALSE(geoTRUE)) {
  
  trial_map_statement <- ""
  
}  

