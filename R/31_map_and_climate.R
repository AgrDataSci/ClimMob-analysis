# ................................................................
# Trial coordinates, map and agroclimatic indices
#
# Coordinates are extracted in their own step so that a failure to
# download the country boundaries (map) does not stop the
# agroclimatic analysis, and the other way around.
# ................................................................

#' Get the trial coordinates from the ClimMob data
#'
#' @param x the CM_list sent by ClimMob, or a data.frame with
#'  columns ending in _longitude and _latitude (local runs)
get_trial_coordinates = function(x) {

  coords = ClimMobTools:::.get_trial_coordinates(x,
                                                 return = c("coordinates"),
                                                 dist = 0.000001)

  if (is.null(coords) || nrow(coords) == 0 || all(is.na(coords))) {
    stop("No GPS coordinates found in the trial data")
  }

  coords

}

#' Map of the trial locations
#'
#' @param coords output of get_trial_coordinates()
#' @param country ISO2 code of the country of the project
#' @param path folder where the country boundaries are downloaded,
#'  a temporary folder by default so nothing is written in the code folder
get_testing_sites_map = function(coords, country, path = tempdir()) {

  if (is.na(country) || !nzchar(country)) {
    stop("The project country is not known, cannot draw the map")
  }

  adm = geodata::gadm(country = country, level = 1, path = path)

  if (is.null(adm)) {
    stop("Could not download the administrative boundaries for ", country)
  }

  adm = sf::st_as_sf(adm)

  xy = sf::st_as_sf(stats::na.omit(coords),
                    coords = c("longitude", "latitude"),
                    crs = 4326)

  trial_map = ggplot() +
    geom_sf(data = adm, fill = "#ffffe5", color = "black", alpha = 1) +
    geom_sf(data = xy, color = "blue", size = 1.5, shape = 21, fill = "red", stroke = 1) +
    theme_minimal() +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "br", width_hint = 0.3)

  list(geoTRUE = TRUE,
       map = trial_map,
       coords = coords)

}

#' Placeholder used when the map could not be produced
error_data_trial_map = list(geoTRUE = FALSE,
                            map = NULL,
                            coords = data.frame())

#' Weekly agroclimatic indices during the season
#'
#' @param cmdata the trial data as data.frame
#' @param coords output of get_trial_coordinates()
#' @param ndays number of days from the planting date, see season_length()
get_agroclimatic_data = function(cmdata, coords, ndays = 60) {

  if (is.null(coords) || nrow(coords) == 0) {
    return(list(agroclimate = FALSE))
  }

  date_col = grep("_plantingdate", names(cmdata), value = TRUE)
  date_used = "planting day"

  # if no planting date, then the registration date
  if (length(date_col) == 0) {
    date_col = grep("REG_clm_start", names(cmdata), value = TRUE)
    date_used = "participant registration day"
  }

  if (length(date_col) == 0) {
    return(list(agroclimate = FALSE))
  }

  dates = lubridate::as_date(cmdata[[date_col[1]]])

  ok = stats::complete.cases(coords) & !is.na(dates)

  if (!any(ok)) {
    stop("No rows with both coordinates and ", date_used)
  }

  rain = rainfall(coords[ok, ], day.one = dates[ok], span = ndays,
                  timeseries = TRUE, intervals = 7)

  temp = temperature(coords[ok, ], day.one = dates[ok], span = ndays,
                     timeseries = TRUE, intervals = 7)

  climate_theme = theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 16),
          title = element_text(size = 14),
          axis.title = element_text(size = 15))

  index_plot = function(dat, sel, ylab, xlab = "", ...) {
    d = dat[dat$index %in% sel, ]
    d$index = factor(d$index, levels = sel)
    ggplot(d) +
      geom_line(aes(y = value, x = date, group = id)) +
      geom_smooth(aes(y = value, x = date)) +
      facet_wrap(~ index, ...) +
      labs(x = xlab, y = ylab) +
      climate_theme
  }

  rain_plot = index_plot(rain, c("Rtotal", "SDII"), tr("lab_index_mm"), scales = "free_y") /
    index_plot(rain, c("MLDS", "MLWS"), tr("lab_index_days"), xlab = tr("lab_date"), scales = "free_y")

  temperature_plot = index_plot(temp, c("maxDT", "minDT", "maxNT", "minNT"),
                                tr("lab_index_c"), xlab = tr("lab_date"), ncol = 2)

  # indices for the whole window, one row per participant, used as
  # covariates in the Plackett-Luce trees (issue #105)
  rain1 = as.data.frame(rainfall(coords[ok, ], day.one = dates[ok], span = ndays))
  temp1 = as.data.frame(temperature(coords[ok, ], day.one = dates[ok], span = ndays))

  covariates = climate_covariates(rain1, temp1, ok)

  list(agroclimate = TRUE,
       rain_plot = rain_plot,
       temperature_plot = temperature_plot,
       rainfall_weekly = rain,
       temperature_weekly = temp,
       rainfall = rain1,
       temperature = temp1,
       dates = dates,
       window = c(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE) + ndays),
       noNAs = ok,
       ndays = ndays,
       covariates = covariates,
       date_used = date_used)

}

#' Placeholder used when the agroclimatic data could not be produced
error_data_agroclimate = list(agroclimate = FALSE)

#' Length of the climate window, from the design of the project
#'
#' The window starts at planting and ends at the last data collection
#' moment with a ranking trait, as set by the project designer in
#' ClimMob. It is kept between min_days and max_days. Without this
#' information the default of 60 days is used (issue #105).
#'
#' @examples
#' season_length(list(traits = data.frame(assessmentDay = c(30, 85))))
season_length = function(pars, default = 60, min_days = 45, max_days = 180) {

  days = suppressWarnings(as.numeric(pars$traits$assessmentDay))
  days = days[!is.na(days) & days > 0]

  if (length(days) == 0) {
    return(default)
  }

  as.integer(min(max(max(days), min_days), max_days))

}

#' Climate indices used as covariates in the trees
#'
#' A small set of interpretable indices is kept to limit the number
#' of covariates tested in the forward selection.
#'
#' @param rain,temp indices from climatrends, one row per participant in ok
#' @param ok logical, the participants with coordinates and dates
#' @return a data.frame with one row per participant (NA when not available)
climate_covariates = function(rain, temp, ok,
                              keep = c("Rtotal", "MLDS", "MLWS", "Rx5day",
                                       "maxDT", "minNT", "DTR", "SU")) {

  x = cbind(rain, temp)
  x = x[, intersect(keep, names(x)), drop = FALSE]

  out = as.data.frame(matrix(NA_real_, nrow = length(ok), ncol = ncol(x),
                             dimnames = list(NULL, names(x))))

  out[ok, ] = x

  names(out) = paste0("climate_", names(out))

  out

}
