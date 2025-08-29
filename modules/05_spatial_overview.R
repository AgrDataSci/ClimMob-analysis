#' Produces a trial location map from GPS coordinates
#' 
#' @param x an object of class CM_list
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
#'                                path = "tests/output/testdata16")
#' 
#' coords$map
#' coords$coords
#' 
#' @export
get_testing_sites_map = function(x, path = getwd()){
  
  # extract coordinates
  coords = ClimMobTools:::.get_trial_coordinates(x, return = c("coordinates"), dist = 0.000001)
  
  # extract country ISO2 code
  ctry = ClimMobTools:::.safe_extract(x, c("project", "project_cnty"), default = NA)
  
  # download GADM map using geodata
  adm = geodata::gadm(country = ctry, level = 1, path = path)
  
  adm = st_as_sf(adm)
  
  xy = st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)
  
  trial_map = 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = adm, fill = "#ffffe5", color = "black", alpha = 1) +
    ggplot2::geom_sf(data = xy, color = "blue", size = 1.5, shape = 21, fill = "red", stroke = 1) +
    ggplot2::theme_minimal() 
  
  result = list(geoTRUE = TRUE, 
                map = trial_map,
                coords = coords)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_trial_map = list(geoTRUE = FALSE, 
                            map = 0L,
                            coords = data.frame())

