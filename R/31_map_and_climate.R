# ................................................................
# Trial coordinates, map and agroclimatic indices
#
# Coordinates are extracted in their own step so that a failure to
# download the country boundaries (map) does not stop the
# agroclimatic analysis, and the other way around.
# ................................................................

#' Projection of every map in the report
#'
#' Equal Earth (EPSG:8857), the equal-area world projection endorsed by the
#' UN General Assembly in September 2026 and the standard of the AgrDataSci
#' AI-ready suite. Areas keep their true relative size. Coordinates stay in
#' WGS84 (EPSG:4326) for every calculation; only the drawing is projected.
MAP_CRS = "EPSG:8857"

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
    geom_sf(data = adm, fill = "#f4f5f0", colour = "grey40", linewidth = 0.25) +
    geom_sf(data = xy, colour = "white", size = 2.2, shape = 21, fill = pal_one("dark"), stroke = 0.5) +
    coord_sf(crs = sf::st_crs(MAP_CRS)) +
    theme_minimal() +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "br", width_hint = 0.3)

  list(geoTRUE = TRUE,
       map = trial_map,
       aspect = map_aspect(adm),
       coords = coords,
       adm = adm)

}

#' Height to width ratio of a map, from the bounding box in the map CRS
#'
#' Used to size the figure in the report so a tall country is not
#' squeezed into a landscape frame.
#' @param x an sf object
#' @return a number between 0.5 and 1.6
map_aspect = function(x) {
  b = tryCatch(sf::st_bbox(sf::st_transform(x, sf::st_crs(MAP_CRS))), error = function(e) NULL)
  if (is.null(b)) return(1)
  a = unname((b["ymax"] - b["ymin"]) / (b["xmax"] - b["xmin"]))
  if (!is.finite(a)) 1 else min(1.6, max(0.5, a))
}

#' Placeholder used when the map could not be produced
error_data_trial_map = list(geoTRUE = FALSE,
                            map = NULL,
                            aspect = 1,
                            coords = data.frame(),
                            adm = NULL)

#' Map of who answered the latest data collection moment
#'
#' Points are filled when the participant answered the last moment
#' that has any answer, and open when the answer is still missing.
#' Teams use it to plan visits and reminders.
#'
#' @param coords output of get_trial_coordinates(), one row per participant
#' @param responses the logical matrix in get_monitoring()$responses,
#'  participants in rows and data collection moments in columns
#' @param adm optional sf object with administrative boundaries, the one
#'  downloaded for the trial map. Without it the points are drawn alone
#' @return a list with the map, the moment shown and the counts
#' @examples
#' xy = data.frame(longitude = c(10, 11, 12), latitude = c(60, 61, 62))
#' r = cbind(Registration = TRUE, Harvest = c(TRUE, FALSE, TRUE))
#' get_participation_map(xy, r)$n_answered
get_participation_map = function(coords, responses, adm = NULL) {

  if (is.null(responses) || ncol(responses) < 2) {
    stop("There is no data collection moment after the registration")
  }

  if (nrow(coords) != nrow(responses)) {
    stop("Coordinates (", nrow(coords), " rows) and answers (", nrow(responses),
         " rows) do not match")
  }

  n_answers = colSums(responses, na.rm = TRUE)
  after_reg = which(n_answers > 0 & seq_along(n_answers) > 1)

  if (length(after_reg) == 0) {
    stop("No participant has answered after the registration yet")
  }

  last = max(after_reg)

  lab = c(tr("lab_answered"), tr("lab_not_answered"))
  answered = responses[, last] %in% TRUE

  d = data.frame(longitude = coords$longitude,
                 latitude = coords$latitude,
                 status = factor(ifelse(answered, lab[1], lab[2]), levels = lab))
  d = d[stats::complete.cases(d[c("longitude", "latitude")]), , drop = FALSE]

  if (nrow(d) == 0) {
    stop("No GPS coordinates to draw the participation map")
  }

  p = ggplot()

  if (!is.null(adm)) {
    p = p + geom_sf(data = adm, fill = "#f4f5f0", colour = "grey60", linewidth = 0.2)
  }

  p = p +
    geom_point(data = d, aes(x = longitude, y = latitude, fill = status),
               shape = 21, size = 2.4, colour = pal_one("dark"), stroke = 0.8) +
    scale_fill_manual(values = c(pal_one("dark"), "white"), name = "", drop = FALSE) +
    theme_minimal() +
    theme(legend.position = "top",
          text = element_text(size = 13, colour = "grey20")) +
    labs(x = "", y = "") +
    # points are given in longitude and latitude, the map is drawn in Equal Earth
    coord_sf(crs = sf::st_crs(MAP_CRS), default_crs = sf::st_crs(4326))

  list(available = TRUE,
       map = p,
       moment = colnames(responses)[last],
       n_answered = sum(d$status == lab[1]),
       n_total = nrow(d))

}

#' Placeholder used when the participation map could not be produced
error_data_participation_map = list(available = FALSE, map = NULL, moment = NA,
                                    n_answered = NA, n_total = NA)

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

  rain = climate_by_region(rainfall, coords[ok, ], dates[ok], span = ndays,
                           timeseries = TRUE, intervals = 7)

  temp = climate_by_region(temperature, coords[ok, ], dates[ok], span = ndays,
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
      geom_line(aes(y = value, x = date, group = id), colour = "grey70", linewidth = 0.3) +
      geom_smooth(aes(y = value, x = date), colour = pal_one("dark"), fill = pal_one("light")) +
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
  rain1 = as.data.frame(climate_by_region(rainfall, coords[ok, ], dates[ok], span = ndays))
  temp1 = as.data.frame(climate_by_region(temperature, coords[ok, ], dates[ok], span = ndays))

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

#' Call a climatrends function by regions of at most two degrees
#'
#' NASA POWER serves regions of at most 5 x 5 degrees. climatrends groups
#' the points in regions of about three degrees and pads the box by two
#' degrees, so a trial that spans more than two degrees can ask for a box
#' that is too large and the request fails. Splitting the points here, in
#' groups of at most two degrees across, keeps every box within the limit.
#'
#' @param fun rainfall or temperature from climatrends
#' @param coords data.frame with longitude and latitude, complete cases
#' @param dates vector of Date, one per row of coords
#' @param timeseries passed on to fun, TRUE for the weekly series
#' @param ... other arguments of fun (span, intervals)
#' @return as fun, rows in the order of coords; the ids of the time
#'  series are the row numbers of coords
climate_by_region = function(fun, coords, dates, timeseries = FALSE, ...) {

  coords = as.data.frame(coords)

  regions = if (nrow(coords) > 1) {
    stats::cutree(stats::hclust(stats::dist(coords)), h = 2)
  } else {
    1L
  }

  parts = lapply(sort(unique(regions)), function(r) {
    i = which(regions == r)
    x = fun(coords[i, , drop = FALSE], day.one = dates[i],
            timeseries = timeseries, ...)
    if (isTRUE(timeseries)) {
      x$id = as.character(i[as.integer(x$id)])
    } else {
      x = cbind(.row = i, as.data.frame(x))
    }
    x
  })

  out = do.call(rbind, parts)

  if (isFALSE(timeseries)) {
    out = out[order(out$.row), setdiff(names(out), ".row"), drop = FALSE]
    rownames(out) = NULL
  }

  class(out) = union("clima_df", class(out))

  out

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
