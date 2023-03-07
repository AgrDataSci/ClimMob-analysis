#' This module produces charts with temperature and rainfall indices
#' 
#' @param cmdata a data frame with the ClimMob data
get_agroclimatic_data = function(cmdata){
  
  result = list(agroclimate = FALSE)
  
  # look for the first and last dates in the dataset
  dates = sum(grepl("_start", names(cmdata)))
  
  # only run if more than one data collection moment
  if (isTRUE(dates > 1)) {
    
    # the first data is the registration of participants
    dates1 = which(grepl("REG_clm_start|registration_survey_start|planting_date", names(cmdata)))
    dates1 = as.character(cmdata[, dates1])
    dates1 = as.Date(dates1, origin = "1979-01-01")
    dates1 = dates1[which.min(dates1)]
    
    # then the last data record
    dates2 = which(grepl("_start", names(cmdata)))
    dates2 = as.character(cmdata[, dates2])
    dates2 = as.Date(unlist(cmdata[dates2]))
    dates2 = dates2[which.max(dates2)]
    
    dates = c(dates1, dates2)
    
    # Check if lonlat is provided
    lon = grepl("_longitude", names(cmdata))
    lat = grepl("_latitude", names(cmdata))
    
    geoTRUE = all(any(lon), any(lat))
    
    if (isTRUE(geoTRUE)) {
      
      # find the vector with most completeness 
      less_nas = lapply(cmdata[lon], function(x){
        sum(is.na(x))
      })
      
      lon = names(which.min(unlist(less_nas)))
      lat = gsub("_longitude", "_latitude", lon)
      
      lonlat = cmdata[,c(lon,lat)]
      
      lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
      
      lonlat[,1] = ifelse(lonlat[,1] > 180 | lonlat[,1] < -180, NA, lonlat[,1])
      
      lonlat[,2] = ifelse(lonlat[,2] > 70 | lonlat[,2] < -70, NA, lonlat[,2])
      
      keep_lonlat = apply(lonlat, 1, function(x){sum(is.na(x))}) == 0
      
      lonlat = lonlat[keep_lonlat, ]
      
      d = lonlat
      names(d) = c("lon","lat")
      
      # make clusters
      h = stats::dist(d)
      
      h = stats::hclust(h)
      
      h = stats::cutree(h, h = 0.05)
      
      # split the d by each defined cluster
      d = split(d, h)
      
      # and take the mean
      d = lapply(d, function(x) {
        colMeans(x)
      })
      
      # back to data frame
      d = do.call("rbind", d)
      
      d = as.data.frame(d)
      
      names(d) = c("lon","lat")
      
      dates = climatrends:::.coerce2Date(dates)
      
      days = dates[1]:dates[2]
      
      years = ceiling(length(days)/360)
      
      # TO DO !!!!
      # find an elegant way!!!
      if (isTRUE(years > 1)) {
        ny = rep(1:years, each = 360, length.out = length(days))
        begin = integer()
        end = integer()
        for (i in seq_len(years)){
          begin = c(begin, min(days[ny == c(1:years)[i]]))
          end = c(end, max(days[ny == c(1:years)[i]]))
        }
        begin = climatrends:::.coerce2Date(begin)
        end = climatrends:::.coerce2Date(end)
      }else{
        begin = dates[1]
        end = dates[2]
      }
      
      
      # get rainfall using nasapower
      # use the clustered points to save time
      rain = data.frame()
      temp = data.frame()
      
      for(i in seq_len(years)){
        
        r_i = rainfall(d, 
                       day.one = begin[i], 
                       last.day = end[i],
                       timeseries = TRUE, 
                       intervals = 7)
        
        rain = rbind(rain, r_i)
        
        # temperature data
        # use the clustered points to save time
        t_i = temperature(d,
                          day.one = begin[i],
                          last.day = end[i],
                          timeseries = TRUE, 
                          intervals = 7)
        
        temp = rbind(temp, t_i)
        
      }
      
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
      dates2 = dates
      # BUG FIX nasapower is not downloading data for more than a year
      # if more than a year than take the first year
      if (isTRUE(dates[2] - dates[1] > 362)) {
        dates[2] = dates[1] + 362
      }
      
      # if less than 2 days from TODAYS Sys.Date() then reduce the window
      if (isTRUE(Sys.Date() - dates[2] < 2)) {
        dates[2] = Sys.Date() - 2
      }
      
      rain1 = rainfall(lonlat, 
                       day.one = dates[1], 
                       last.day = dates[2])
      
      # temperature data
      temp1 = temperature(lonlat,
                          day.one = dates[1],
                          last.day = dates[2])
      
      rain_final = as.data.frame(matrix(NA, 
                                        ncol = ncol(rain1),
                                        nrow = nrow(cmdata)))
      
      rain_final[keep_lonlat, ] = rain1
      names(rain_final) = names(rain1)
      # TO IMPROVE! Replace NA's with zeros
      rain_final[is.na(rain_final)] = 0
      
      temp_final = as.data.frame(matrix(NA, 
                                        ncol = ncol(temp1),
                                        nrow = nrow(cmdata)))
      
      temp_final[keep_lonlat, ] = temp1
      names(temp_final) = names(temp1)
      # TO IMPROVE! Replace NA's with zeros
      temp_final[is.na(temp_final)] = 0
      
      result = list(agroclimate = TRUE, 
                    rain_plot = rain_plot,
                    temperature_plot = temperature_plot,
                    dates = as.vector(as.character(dates2)),
                    rainfall_season = rain,
                    temperature_season = temp,
                    keep_lonlat = keep_lonlat,
                    rainfall = rain_final,
                    temperature = temp_final)
      
      
    }
    
  }
  
  return(result)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_agroclimate = list(agroclimate = FALSE, 
                              rain_plot = 0,
                              temperature_plot = 0,
                              dates = c("", ""),
                              rainfall_season = data.frame(),
                              temperature_season = data.frame(),
                              rainfall = data.frame(),
                              temperature = data.frame())


