# This module produces the summary tables and trial 
# overview, with information of traits assessed 
# frequency of participation and technologies 
# evaluated by participants

#...........................................................
# This is the fist table in Section 1
# it shows the traits that were assessed in the project 
# and the number of answers used in the analysis
tbl_section1 <- lapply(trait_list, function(x){
  data.frame(name = x$name,
             collect = ClimMobTools:::.title_case(x$assessment),
             quest = x$question,
             n = sum(x$keep))
})

tbl_section1 <- do.call(rbind, tbl_section1)  

# rename columns in the original table
names(tbl_section1) <- c("Trait", "Data collection moment", 
                         "Question asked", "Number of valid answers")

#...........................................................
# Number of items tested
# This table show the frequencies where items were tested
# and how these frequencies were tested among groups, if any
itemtable <- cmdata[, grepl("package_item", names(cmdata))]

itemtable <- data.frame(table(unlist(itemtable)))

itemtable$x <- with(itemtable,
                    round((Freq / nranker * 100), 1))

itemtable$x <- with(itemtable,
                    paste0(x, "%"))

names(itemtable) <- c(Option, "Freq", "Relative freq")

# check if any group is provided so it can be added to the itemtable
if (length(groups) > 0) {
  
  x <- unlist(cmdata[, grepl("package_item", names(cmdata))])
  
  ngroups <- table(cmdata$group)
  
  grouptbl <- c()
  
  for (i in seq_along(groups)) {
    grouptbl <- cbind(grouptbl, 
                      tapply(rep(cmdata$group, ncomp), x, function(x) {
                        sum(x == groups[i], na.rm = TRUE)
                      }))
  }
  
  grouptbl <- as.data.frame(grouptbl)
  
  names(grouptbl) <- paste0(groups, " (n=", ngroups, ")")
  
  itemtable <- cbind(itemtable, grouptbl)
  
  rm(x)
  
}

itemtable$Abbreviation <- gosset:::.reduce(as.character(itemtable[, Option]))

itemtable <- itemtable[union(c(Option, "Abbreviation"), names(itemtable))]

rownames(itemtable) <- 1:nrow(itemtable)

#...........................................................
# Participation during trial (response rate)
# it is a plot showing the rate of response in each 
# data collection moment, it takes the larger N response
# for each data collection moment
participation <- data.frame(n = nrow(cmdata),
                            n_tot = nrow(cmdata),
                            group = "Whole group",
                            dc = "Registration")

for(i in seq_along(trait_list)) {
  
  part <- data.frame(n = sum(trait_list[[i]]$keep),
                     n_tot = nrow(cmdata),
                     group = "Whole group",
                     dc = trait_list[[i]]$assessment)
  
  participation <- rbind(participation, part)
  
}

if (length(groups) > 0) {
  
  participation2 <- data.frame(n = as.vector(table(cmdata$group)),
                               n_tot = as.vector(table(cmdata$group)),
                               group = names(table(cmdata$group)),
                               dc = "Registration")
  
  for(i in seq_along(trait_list)) {
    
    n <- table(cmdata[trait_list[[i]]$keep, "group"])
    
    p <- data.frame(group = groups,
                    dc = trait_list[[i]]$assessment)
    
    part <- data.frame(n = as.vector(n),
                       n_tot = as.vector(table(cmdata$group)),
                       group = names(n))
    
    part <- merge(p, part, by = "group", all.x = TRUE)
    
    participation2 <- rbind(participation2, part)
    
  }
  
  participation <- rbind(participation, participation2)
  
}

# get the highest value in each data collection moment
participation <- split(participation, paste0(participation$dc, participation$group))

participation <- lapply(participation, function(x){
  i <- which.max(x$n)
  x[i, ]
})

participation <- do.call(rbind, participation)

# transform into proportion to make it easier to visualize
participation$value_perc <- participation$n / participation$n_tot

participation$dc <- ClimMobTools:::.title_case(participation$dc)

participation$dc <- factor(participation$dc, levels = c("Registration", 
                                                        ClimMobTools:::.title_case(unique(trait$assessmentName))))

participation$group <- factor(participation$group, levels = c("Whole group",
                                                              unique(groups)))

partiplot <- 
  ggplot(participation, aes(x = dc, y = value_perc, 
                            group = group, color = group)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(values = col_pallet(length(unique(participation$group))), 
                      name = "") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 10, color = "grey20"),
        axis.text = element_text(size = 10, color = "grey20"),
        axis.title = element_text(size = 10, color = "grey20")) +
  labs(x = "Trial stage", y = "Rate of response")


