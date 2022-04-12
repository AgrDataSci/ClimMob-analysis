
lonlat <- cmdata[,c(lon, lat)]

lonlat[1:2] <- lapply(lonlat[1:2], as.numeric)

sum(is.na(lonlat))

pdates <- cmdata[,which(grepl("plantingdate", names(cmdata)))]

pdates <- as.Date(pdates)

sum(as.integer(format(pdates, "%Y")) < 2021)

pdates <- ifelse(as.integer(format(pdates, "%Y")) < 2021, pdates + 365, pdates)

pdates <- as.Date(pdates, origin = "1970-01-01")

boxplot(pdates)

rain1 <- rainfall(lonlat, day.one = pdates, span = 60, timeseries = TRUE, intervals = 7)

temp1 <- temperature(lonlat, day.one = pdates, span = 60, timeseries = TRUE, intervals = 7)



unique(rain1$index)

sel <- c("Rtotal", "SDII")

rplot <- rain1[rain1$index %in% sel, ]

ggplot(rplot) +
  geom_line(aes(y = value, x = date, group = id)) +
  geom_smooth(aes(y = value, x = date)) +
  facet_grid(~ index) +
  theme_bw() +
  labs(x = "Date", y = "Index")


unique(temp1$index)

sel <- c("T10p", "T90p")

rplot <- temp1[temp1$index %in% sel, ]

ggplot(rplot) +
  geom_line(aes(y = value, x = date, group = id)) +
  geom_smooth(aes(y = value, x = date)) +
  facet_grid(~ index) +
  theme_bw() +
  labs(x = "Date", y = "Index")




