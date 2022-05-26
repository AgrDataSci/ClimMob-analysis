#' This module produces charts with temperature and rainfall indices
#' 
#' @param cmdata a data frame with the ClimMob data
get_agroclimatic_data <- function(cmdata){
  
  result <- list(agroclimate = FALSE)
  
  # look for the first and last dates in the dataset
  dates <- which(grepl("clm_start", names(cmdata)))
  
  # only run if more than one data collection moment
  if (length(dates) > 1) {
  
    dates <- as.Date(unlist(cmdata[dates]))
    
    dates <- c(dates[which.min(dates)], dates[which.max(dates)])
      
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
      
      d <- lonlat
      names(d) <- c("lon","lat")
      d[1:2] <- lapply(d[1:2], as.numeric)
      
      # make clusters 
      h <- stats::dist(d)
      
      h <- stats::hclust(h)
      
      h <- stats::cutree(h, h = 0.05)
      
      # split the d by each defined cluster
      d <- split(d, h)
      
      # and take the mean 
      d <- lapply(d, function(x) {
        colMeans(x)
      })
      
      # back to data frame
      d <- do.call("rbind", d)
      
      d <- as.data.frame(d)
      
      names(d) <- c("lon","lat")
      
      # get rainfall using nasapower
      rain <- rainfall(d, 
                       day.one = dates[1], 
                       last.day = dates[2],
                       timeseries = TRUE, 
                       intervals = 7)
      
      # temperature data
      temp <- temperature(d,
                          day.one = dates[1],
                          last.day = dates[2],
                          timeseries = TRUE, 
                          intervals = 7)
      
      
      # plot rain indices
      sel <- c("Rtotal", "SDII")
      
      rplot <- rain[rain$index %in% sel, ]
      
      rain_plot1 <- ggplot(rplot) +
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
      sel <- c("MLDS", "MLWS")
      
      rplot <- rain[rain$index %in% sel, ]
      
      rain_plot2 <- ggplot(rplot) +
        geom_line(aes(y = value, x = date, group = id)) +
        geom_smooth(aes(y = value, x = date)) +
        facet_wrap(~ index, scales = "free_y") +
        theme_bw() +
        labs(x = "Date", y = "Index (days)") +
        theme(panel.grid = element_blank(),
              text = element_text(size = 16),
              title = element_text(size = 14),
              axis.title = element_text(size = 15))
      
      
      rain_plot <- rain_plot1 / rain_plot2
      
      
      # now the temperature indices 
      sel <- c("maxDT", "minDT", "maxNT", "minNT")
      
      rplot <- temp[temp$index %in% sel, ]
      
      rplot$index <- factor(rplot$index, 
                            levels = c("maxDT", "minDT", "maxNT", "minNT"))
      
      temperature_plot <- ggplot(rplot) +
        geom_line(aes(y = value, x = date, group = id)) +
        geom_smooth(aes(y = value, x = date)) +
        facet_wrap(~ index, ncol = 2) +
        theme_bw() +
        labs(x = "Date", y = "Index (°C)") +
        theme(panel.grid = element_blank(),
              text = element_text(size = 16),
              title = element_text(size = 14),
              axis.title = element_text(size = 15))
      
      result <- list(agroclimate = TRUE, 
                     rain_plot = rain_plot,
                     temperature_plot = temperature_plot,
                     dates = as.vector(as.character(dates)))
      
      
      
    }
    
  }
  
  return(result)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_agroclimate <- list(agroclimate = FALSE, 
                               rain_plot = 0,
                               temperature_plot = 0,
                               dates = c("", ""))


