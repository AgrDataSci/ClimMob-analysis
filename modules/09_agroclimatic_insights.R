#' Produces charts with agroclimatic indices
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param coords a data frame with coordinates 
#' @param ndays an integer for the number of days from planting date to be used
#' @param climate.date character specifying the source of climate data, 
#'  currently only NASAPOWER is available
#' @examples
#' modules = list.files("modules",
#'                      full.names = TRUE,
#'                      pattern = ".R")
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
#' coords$coords
#' 
#' ag = get_agroclimatic_data(cmdata, 
#'                            coords = coords$coords,
#'                            ndays = 60)
#' @export
get_agroclimatic_data = function(cmdata, 
                                 coords, 
                                 ndays = 60, 
                                 climate.data = "NASAPOWER"){
  
  ncoords = nrow(coords)
  
  noNAs = apply(coords, 1, function(x) sum(is.na(x)) == 0)
  
  # if no coords, then no agroclimate data can be collected 
  if (isTRUE(ncoords == 0)) {
    return(list(agroclimate = FALSE))
  }
  
  result = list(agroclimate = FALSE)
  
  dates = grep("_plantingdate", names(cmdata))
  
  date_used = "planting day"
  
  # if no planting date, then the registration date
  if (isTRUE(length(dates) == 0)) {
    date_used = "participant registration day"
    dates = grep("REG_clm_start", names(cmdata))
  }
  
  # if no date, then no agroclimate data can be collected 
  if (isTRUE(length(dates) == 0)) {
    return(list(agroclimate = FALSE))
  }
  
  dates = cmdata[dates[1]]
  
  dates = lubridate::as_date(dates[,1])
  
  rain = rainfall(coords[noNAs, ],
                  day.one = dates[noNAs],
                  span = ndays,
                  timeseries = TRUE,
                  intervals = 7)
  
  temp = temperature(coords[noNAs, ],
                     day.one = dates[noNAs],
                     span = ndays,
                     timeseries = TRUE,
                     intervals = 7)
  
  
  # plot rain indices
  sel = c("Rtotal", "SDII")
  
  rplot = rain[rain$index %in% sel, ]
  
  rain_plot1 = ggplot(rplot) +
    geom_line(aes(y = value, x = date, group = id)) +
    geom_smooth(aes(y = value, x = date)) +
    facet_wrap(~ index, scales = "free_y") +
    theme_bw() +
    labs(x = "", y = "Index (mm)") +
    theme(panel.grid = element_blank(),
          text = element_text(size = 16),
          title = element_text(size = 14),
          axis.title = element_text(size = 15))
  
  # rain plot 2
  sel = c("MLDS", "MLWS")
  
  rplot = rain[rain$index %in% sel, ]
  
  rain_plot2 = ggplot(rplot) +
    geom_line(aes(y = value, x = date, group = id)) +
    geom_smooth(aes(y = value, x = date)) +
    facet_wrap(~ index, scales = "free_y") +
    theme_bw() +
    labs(x = "Date", y = "Index (days)") +
    theme(panel.grid = element_blank(),
          text = element_text(size = 16),
          title = element_text(size = 14),
          axis.title = element_text(size = 15))
  
  
  rain_plot = rain_plot1 / rain_plot2
  
  # temperature
  sel = c("maxDT", "minDT", "maxNT", "minNT")

  rplot = temp[temp$index %in% sel, ]

  rplot$index = factor(rplot$index,
                       levels = c("maxDT", "minDT",
                                  "maxNT", "minNT"))

  temperature_plot = ggplot(rplot) +
    geom_line(aes(y = value, x = date, group = id)) +
    geom_smooth(aes(y = value, x = date)) +
    facet_wrap(~ index, ncol = 2) +
    theme_bw() +
    labs(x = "Date", y = "Index (°C)") +
    theme(panel.grid = element_blank(),
          text = element_text(size = 16),
          title = element_text(size = 14),
          axis.title = element_text(size = 15))
  
  
  # now get the temperature and rainfall for the whole period
  # to use in the PLtree module
  # get rainfall using nasapower
  rain1 = rainfall(coords[noNAs, ],
                   day.one = dates[noNAs],
                   span = ndays)
  
  # temperature data
  temp1 = temperature(coords[noNAs, ],
                      day.one = dates[noNAs],
                      span = ndays)
  
  result = list(agroclimate = TRUE, 
                rain_plot = rain_plot,
                temperature_plot = temperature_plot,
                rainfall_weekly = rain,
                temperature_weekly = temp,
                rainfall = rain1,
                temperature = temp1,
                dates = dates,
                window = c(min(dates, na.rm = TRUE),
                           max(dates, na.rm = TRUE) + ndays),
                noNAs = noNAs,
                date_used = date_used)
  
  return(result)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_agroclimate = list(agroclimate = FALSE)
