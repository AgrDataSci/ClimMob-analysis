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
#' @param moments data.frame from collection_moments(), to mark the data
#'  collection moments in the season chart
#' @param group optional character vector with one group per row of cmdata
#'  (from build_groups()), to draw one line per group
get_agroclimatic_data = function(cmdata, coords, ndays = 60, moments = NULL,
                                 group = NULL) {

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

  grp = if (is.null(group) || length(group) != nrow(cmdata)) NULL else group[ok]

  xlab = tr("lab_days_from", date_used = tr(paste0("date_used_", gsub(" ", "_", date_used))))

  season = season_plot(rain, temp, dates[ok], ndays, moments = moments, group = grp, xlab = xlab)

  envelope = climate_envelope_plot(rain1, temp1, group = grp)

  list(agroclimate = TRUE,
       season_plot = season,
       season_groups = !is.null(grp) && length(unique(grp)) > 1 && length(unique(grp)) <= 8,
       season_summary = season_summary(rain1, temp1),
       envelope_plot = envelope,
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

#' The season in one chart
#'
#' Cumulative rainfall and the day and night temperatures along the
#' season, as the median across the trial plots with the 10th to 90th
#' percentile band, or one line per group. The data collection moments
#' are marked. The x axis counts days from planting, so plots planted on
#' different dates line up.
#'
#' @param rain,temp the weekly time series from climatrends, with columns
#'  id, date, index and value
#' @param planting Date vector, one per id (in the order of the ids)
#' @param ndays length of the window in days
#' @param moments data.frame with name and day, from collection_moments()
#' @param group optional character vector, one per id
#' @param xlab label of the x axis
#' @return a patchwork of two ggplots
season_plot = function(rain, temp, planting, ndays, moments = NULL, group = NULL,
                       xlab = "Days from planting") {

  weekly = function(x, index) {
    d = x[x$index == index, c("id", "date", "value")]
    d$id = as.integer(d$id)
    d = d[order(d$id, d$date), ]
    d$day = as.numeric(as.Date(d$date) - planting[d$id])
    d
  }

  r = weekly(rain, "Rtotal")
  r = do.call(rbind, lapply(split(r, r$id), function(x) {
    x$value = cumsum(x$value)
    x$day = x$day + 7
    rbind(data.frame(id = x$id[1], date = x$date[1], value = 0, day = 0), x)
  }))

  t_day = weekly(temp, "maxDT")
  t_night = weekly(temp, "minNT")
  t_day$day = t_day$day + 3.5
  t_night$day = t_night$day + 3.5

  use_groups = !is.null(group) && length(unique(group)) > 1 && length(unique(group)) <= 8

  summarise = function(d) {
    d$group = if (use_groups) group[d$id] else "all"
    out = stats::aggregate(value ~ day + group, data = d, FUN = function(v) {
      c(median = stats::median(v), lo = stats::quantile(v, 0.1), hi = stats::quantile(v, 0.9))
    })
    out = cbind(out[c("day", "group")], as.data.frame(out$value))
    names(out) = c("day", "group", "median", "lo", "hi")
    out
  }

  rs = summarise(r)
  ts = rbind(cbind(summarise(t_day), what = tr("lab_day_max")),
             cbind(summarise(t_night), what = tr("lab_night_min")))
  ts$what = factor(ts$what, levels = c(tr("lab_day_max"), tr("lab_night_min")))

  climate_theme = theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 14),
          legend.position = "top",
          legend.title = element_blank())

  # the data collection moments
  mk = NULL
  if (!is.null(moments) && nrow(moments) > 0) {
    mk = moments[!is.na(moments$day) & moments$day > 0 & moments$day <= ndays, c("name", "day")]
  }

  add_moments = function(p, ymax) {
    if (is.null(mk) || nrow(mk) == 0) return(p)
    p +
      geom_vline(data = mk, aes(xintercept = day), linetype = "dashed",
                 colour = "grey50", inherit.aes = FALSE) +
      geom_text(data = mk, aes(x = day, y = ymax, label = name), angle = 90,
                hjust = 1, vjust = -0.4, size = 3.5, colour = "grey30",
                inherit.aes = FALSE)
  }

  if (use_groups) {
    p1 = ggplot(rs, aes(x = day, y = median, colour = group)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = pal_qualitative(length(unique(rs$group))))
    p2 = ggplot(ts, aes(x = day, y = median, colour = group, linetype = what)) +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = pal_qualitative(length(unique(ts$group)))) +
      scale_linetype_manual(values = c("solid", "dashed"))
  } else {
    p1 = ggplot(rs, aes(x = day, y = median)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = pal_one("light"), alpha = 0.6) +
      geom_line(colour = pal_one("dark"), linewidth = 1.1)
    p2 = ggplot(ts, aes(x = day, y = median, colour = what, fill = what)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, colour = NA) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c(BREWER$BrBG[2], BREWER$BrBG[8])) +
      scale_fill_manual(values = c(BREWER$BrBG[2], BREWER$BrBG[8]))
  }

  p1 = add_moments(p1, max(rs$hi, rs$median) * 1.05) +
    labs(x = NULL, y = tr("lab_cum_rain")) +
    climate_theme

  p2 = add_moments(p2, max(ts$hi, ts$median) + 2) +
    labs(x = xlab, y = tr("lab_temperature_c")) +
    climate_theme

  p1 / p2 + plot_layout(heights = c(1.1, 1))

}

#' Numbers for the climate text, from the season indices
#'
#' @param rain1,temp1 indices for the whole window, one row per plot
#' @return a list with the median and the 10th and 90th percentiles of
#'  total rainfall, the median longest dry spell, and the median of the
#'  maximum day and minimum night temperature
season_summary = function(rain1, temp1) {
  q = function(v, p) round(stats::quantile(v, p, na.rm = TRUE))
  list(rain = q(rain1$Rtotal, 0.5),
       rain_lo = q(rain1$Rtotal, 0.1),
       rain_hi = q(rain1$Rtotal, 0.9),
       dry = q(rain1$MLDS, 0.5),
       tmax = round(stats::median(temp1$maxDT, na.rm = TRUE), 1),
       tmin = round(stats::median(temp1$minNT, na.rm = TRUE), 1))
}

#' The climate envelope of the trial
#'
#' Every plot as a point in the space of total rainfall and maximum day
#' temperature over the window, coloured by group when there are groups.
#'
#' @inheritParams season_summary
#' @param group optional character vector, one per plot
climate_envelope_plot = function(rain1, temp1, group = NULL) {

  d = data.frame(rain = rain1$Rtotal, tmax = temp1$maxDT)
  use_groups = !is.null(group) && length(unique(group)) > 1 && length(unique(group)) <= 8

  p = ggplot(d, aes(x = rain, y = tmax))

  if (use_groups) {
    d$group = group
    p = ggplot(d, aes(x = rain, y = tmax, fill = group)) +
      geom_point(shape = 21, size = 2.6, colour = "grey30", alpha = 0.8) +
      scale_fill_manual(values = pal_qualitative(length(unique(group))), name = NULL)
  } else {
    p = p + geom_point(shape = 21, size = 2.6, colour = "grey30", fill = pal_one("mid"), alpha = 0.8)
  }

  p +
    labs(x = tr("lab_total_rain"), y = tr("lab_max_day_temp")) +
    theme_bw() +
    theme(panel.grid = element_blank(), text = element_text(size = 14),
          legend.position = "top")

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
