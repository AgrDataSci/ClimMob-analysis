# This module organize the quantitative data
quanti_traits <- pars[["linear"]]

# rename traits to avoid duplicated strings, in case the same 
# trait is tested in different data collection moments 
quanti_traits$codeQst <- rename_duplicates(quanti_traits$codeQst)
quanti_traits$name    <- rename_duplicates(quanti_traits$name, sep = " ")

# run over traits and get it in long format (unlist)
quanti <- data.frame(id = as.vector(unlist(cmdata[, "id" ])) ,
                     items = as.vector(unlist(cmdata[, itemnames])))

for(i in seq_along(quanti_traits$codeQst)){
  
  strings_i <- as.vector(unlist(quanti_traits[i, c("nameString1",
                                                   "nameString2", 
                                                   "nameString3")]))
  index_i <- as.vector(sapply(strings_i,  function(x){
    which(grepl(x, names(cmdata)))
  }))
  
  values_i <- as.numeric(unlist(cmdata[,index_i]))
  
  quanti <- cbind(quanti, values_i)
  
}

# put colnames
names(quanti)[-c(1:2)] <- quanti_traits$codeQst

# identify outliers
outliers <- data.frame()

for(i in seq_along(quanti_traits$codeQst)){
  
  index_i <- quanti_traits$codeQst[i]
  
  out_values_i <- unique(boxplot.stats(quanti[,index_i])$out)
  
  out_i <- quanti[,index_i] %in% out_values_i
  
  quanti[out_i, index_i] <- NA
  
  outliers_i <- quanti[out_i, c("id", "items")]
  
  if (nrow(outliers_i) == 0) next
  
  outliers_i$values <- quanti[out_i, index_i]
  
  outliers_i$trait <- quanti_traits$name[i]
  
  outliers_i$assessment <- quanti_traits$assessmentName[i]
  
  outliers <- rbind(outliers, outliers_i)
  
}

rownames(outliers) <- NULL


# make box plots 
densitplots <- list()

for (i in seq_along(quanti_traits$codeQst)){
  
  ggdat_i <- quanti[,c("items", quanti_traits$codeQst[i])]
  
  names(ggdat_i) <- c("items", "values")
  
  b_i <- ggplot(ggdat_i, aes(x = values, group = items, fill = items)) + 
    geom_density(adjust=1.5, alpha=.4) +
    theme_bw() + 
    facet_wrap(~ items) +
    labs(x = Option, y = quanti_traits$name[i]) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, 
                                     vjust = 1, 
                                     hjust = 1))
  
  b_i
  
  densitplots[[i]] <- b_i
}



quanti_data <- list(densityplots = densitplots,
                    outliers = outliers, 
                    mod = NULL)

rm(densityplots, mod, outliers)

           