# ................................................................
# ................................................................
# Analyse the performance of agricultural technologies from 
# crowdsourcing trials using Plackett-Luce model
# ................................................................
# ................................................................

# Organise the rankings and check for missing data ####
trait <- pars$chars$char
trait_full <- pars$chars$char_full
overallVSlocal <- length(pars$perf) > 0

# check if overall is the first component
# it should always be the first as sorted in ClimMobTools:::.decode_pars()
of <- which(grepl("overall", trait))[1] ==  1
if (isFALSE(of)) {
  
  warning("Overall comparison is missing\n")

}

trait_list <- list()

# run over traits to filter NAs and prepare for PlackettLuce rankings
for(i in seq_along(trait)){
  result <- list()
  # get the question full name
  trait_i <- as.character(pars$chars[i, paste0("quest_", seq_len(nquest))])
  # look for it with cmdata
  for(j in seq_along(trait_i)) {
    trait_i[j] <- names(cmdata[which(grepl(trait_i[j], names(cmdata)))])
  }
  
  # check for NAs in this trait
  keep <- apply(cmdata[trait_i], 1, is.na)
  keep <- as.vector(colSums(keep) == 0)
  
  # check if proportion of missing data is larger than the threshold
  dropit <- (sum(keep) / nranker) < missper
  
  # if larger than it will be dropped
  if (dropit) next
  
  # add comparison with local item
  if (i == 1 & overallVSlocal) {
    
    ovsl <- as.vector(pars$perf[, paste0("quest_", seq_len(pars$perf$n_quest))])
    
    for (k in seq_along(ovsl)) {
      
      ovsl[k] <- names(cmdata[which(grepl(ovsl[k], names(cmdata)))])
    
    }
    
    ovsl <- as.character(ovsl)
    
    keep2 <- apply(cmdata[ovsl], 1, is.na)
    
    keep2 <- as.vector(colSums(keep2) == 0)
    
    keep2 <- keep & keep2
    
    dropit2 <- (sum(keep2) / nranker) < missper
    
    if (isTRUE(dropit2)) {
      overallVSlocal <- FALSE
      next
    }
    
    result[["keep2"]] <- keep2
    
    result[["ovsl"]] <- ovsl
    
  }
  
  result[["keep"]] <- keep
  result[["input"]] <- trait_i
  result[["fullname"]] <- trait_full[i]
  
  trait_list[[trait[i]]] <- result
  
}

trait <- names(trait_list)

# Now look for the explanatory variables
# if no variable provided than add a pseudo variable that will be used in pltree
# this is to fit the model with the intercept only
expvar_list <- list()
if (any(expvar == "xinterceptx")) {
  cmdata$xinterceptx <- rep(0, nranker)
  expvar_list[["expvar"]] <- "xinterceptx"
  expvar_list[["expvar_full"]] <- "Intercept"
  expvar_list[["keep"]] <- rep(TRUE, nranker)
}

if (all(expvar != "xinterceptx")) {
  
  # check for the full names
  for(i in seq_along(expvar)){
    expvar[i] <- names(cmdata[which(grepl(expvar[i], names(cmdata)))])
  }
  
  # check for missing data
  keep <- NULL
  for(i in seq_along(expvar)){
    
    k <- !is.na(cmdata[, expvar[i]])
    keep <- cbind(keep, k)
  }
  
  # find those that are bellow the threshold of missexp
  dropit <- (colSums(keep) / nranker) < missexp
  
  # drop those bellow threshold
  keep <- as.data.frame(keep[, !dropit])
  
  # create a single vector that will be used to filter cmdata
  keep <- rowSums(keep)
  keep <- keep == max(keep)
  
  expvar <- expvar[!dropit]
  expvar_full <- expvar_full[!dropit]
  expvar_dropped <- expvar_full[dropit]
  
  # if no explanatory variable left out put a pseudo variable
  if(length(expvar) == 0) {
    cmdata$xinterceptx <- rep(0, nranker)
    expvar_list[["expvar"]] <- "xinterceptx"
    expvar_list[["expvar_full"]] <- "Intercept"
    expvar_list[["keep"]] <- rep(TRUE, nranker)
  }else{
    expvar_list[["expvar"]] <- expvar
    expvar_list[["expvar_full"]] <- expvar_full
    expvar_list[["keep"]] <- keep
  }
  
}

# .......................................................
# .......................................................
# Make map ####

# Check if lonlat is provided
lon <- grepl("_lon", names(cmdata))
lat <- grepl("_lat", names(cmdata))

geoTRUE <- any(lon) & any(lat)

if (isTRUE(geoTRUE)) {
  lon <- which(lon)[1]
  lat <- which(lat)[1]
  
  lonlat <- cmdata[,c(lon,lat)]
  
  lonlat <- na.omit(lonlat)
  
  nlonlat <- dim(lonlat)[[1]]
  
  if (nlonlat > 0){
    
    names(lonlat) <- c("lon","lat")
    
    trial_map <- plot_map(lonlat, c("lon","lat"),
                          map.types = c("CartoDB.Positron", "OpenTopoMap"))
    
    mapshot(trial_map, 
            url = paste0(getwd(), "/", pathname, projname, "_trial_map.html"),
            file = paste0(getwd(), "/", pathname, projname, "_trial_map.png"))
    
    trial_map_statement <- c("The map below shows the distribution of the trials in this project.")
                             
  
  }
  
}

if (isFALSE(geoTRUE)) {
  
  trial_map_statement <- ""
  
}


# .......................................................
# .......................................................
# Make Table 1 ####
# Create a table with frequencies where each item was evaluated #
itemdata <- cmdata[, grepl("package_item", names(cmdata))]

itemtable <- data.frame(table(unlist(itemdata)))

itemtable$x <- with(itemtable,
                    round((Freq / nranker * 100), 1))

itemtable$x <- with(itemtable,
                    paste0(x, "%"))

names(itemtable) <- c(Option, "Freq", "Relative freq")

# check if gender is provided so it can be added to the itemtable
gender <- any(grepl("REG_gender", names(cmdata)))

if (isTRUE(gender)) {
  dt <- unlist(itemdata)
  
  gender_i <- which(grepl("REG_gender", names(cmdata)))
  gender_i <- cmdata[, gender_i]
  
  nMan <- sum(gender_i == "Man", na.rm = TRUE)
  nWom <- sum(gender_i == "Woman", na.rm = TRUE)
  
  dt <- cbind(tapply(rep(gender_i, ncomp), dt, function(x) sum(x == "Man", na.rm = TRUE)), 
              tapply(rep(gender_i, ncomp), dt, function(x) sum(x == "Woman", na.rm = TRUE))) 
  
  itemtable$m <- dt[, 1]
  
  itemtable$w <- dt[, 2]
  
  names(itemtable)[4:5] <- paste0(c("Man (n=","Woman (n="), c(nMan, nWom), ")")
  
  rm(dt)
  
}

itemtable$Abbreviation <- gosset:::.reduce(as.character(itemtable[,Option]))

itemtable <- itemtable[union(c(Option, "Abbreviation"), names(itemtable))]

# .......................................................
# .......................................................
# Favourability Analysis Table ####
# do this for overall performance

# find the index for overall evaluation
overall <- trait_list[[1]]

if (isTRUE(overallVSlocal)) {
  if (ncomp == 3) {
    keep <- overall$keep2 & overall$keep
    
    R <- rank_tricot(cmdata[keep, ],
                     items = itemnames,
                     input = overall$input,
                     additional.rank = cmdata[keep, overall$ovsl])
  }
  
  if (ncomp > 3) {
    keep <- overall$keep
    
    R <- rank_numeric(cmdata[keep, ],
                      items = itemnames,
                      input = overall$input)
  }
}

if (isFALSE(overallVSlocal)) {
  keep <- overall$keep
  
  if (ncomp == 3){
    R <- rank_tricot(cmdata[keep, ],
                     items = itemnames,
                     input = overall$input)
  }
  
  if (ncomp > 3){
    R <- rank_numeric(cmdata[keep, ],
                      items = itemnames,
                      input = overall$input)
  }
  
}

net <- network(R)

fav1 <- summarise_favourite(R) 

fav2 <- fav1

fav2$best <- paste0(round(fav2$best, 1), "%")
fav2$worst <- paste0(round(fav2$worst, 1), "%")
fav2$fav_score <- round(fav2$fav_score, 1)

fav2 <- fav2[,-which(grepl("wins", names(fav2)))]

names(fav2) <- c(Option,"N","Top Ranked",
                 "Bottom Ranked", "Net Favourability Score")

# Contest Plots
cont1 <- summarise_dominance(R)

cont2 <- summarise_victories(R)

# .......................................................
# .......................................................
# Trait concordance ####
# this assess how the other traits agreed with the overall preference
# build rankings for the other characteristics
other_traits <- trait[-1]

other_traits_full <- trait_full[-1]

if (length(other_traits) > 0) {
  
  # filter cmdata so it matches the dims in all traits
  keep <- NULL
  for(i in seq_along(trait_list)) {
    keep <- cbind(keep, trait_list[[i]]$keep)
  }
  keep <- rowSums(keep)
  keep <- keep == length(trait_list)
  
  if (ncomp == 3) {
    compare_to <- rank_tricot(cmdata[keep, ], 
                              items = itemnames,
                              input = overall$input)
  }
  
  if (ncomp > 3) {
    compare_to <- rank_numeric(cmdata[keep, ], 
                               items = itemnames,
                               input = overall$input)
  }

  
  compare_with <- list()
  if (ncomp == 3) {
    for (i in seq_along(other_traits)) {
      
      ot <- trait_list[[other_traits[i]]]
      
      otr <- rank_tricot(cmdata[keep, ],
                         items = itemnames,
                         input = ot$input)  
      compare_with[[i]] <- otr
    }
  }
  
  if (ncomp > 3) {
    for (i in seq_along(other_traits)) {
      
      ot <- trait_list[[other_traits[i]]]
      
      otr <- rank_numeric(cmdata[keep, ],
                          items = itemnames,
                          input = ot$input)  
      compare_with[[i]] <- otr
    }
  }
  

  agreement <- summarise_agreement(compare_to, 
                                   compare_with, 
                                   labels = other_traits_full)
  
  strongest_link <- c(agreement[[which.max(agreement$kendall), "labels"]],
                      round(max(agreement$kendall), 0))
  
  
  weakest_link   <- c(agreement[[which.min(agreement$kendall), "labels"]],
                      round(min(agreement$kendall), 0))
  
  
  agreement_table <- agreement
  
  agreement_table[,c(2:4)] <- lapply(agreement_table[,c(2:4)], function(x){
    x <- round(x, 1)
    x <- paste0(x,"%")
  })
  
  names(agreement_table) <- c(Option, 
                              "Complete Ranking Agreement",
                              "Agreement with Overall Best", 
                              "Agreement with Overall Worst")

  
} 

if (length(other_traits) == 0) {
  strongest_link <- character()
  weakest_link <- character()
  agreement_table <- data.frame(Option = "Only Overall Performance was used",
                                X = "",
                                Y = "",
                                Z = "")
  names(agreement_table)<-c(" ","","  ","   ")
  agreement <- data.frame(labels = "",
                          kendall = 0,
                          first = 0,
                          last = 0)
}

# .......................................................
# .......................................................
# PlackettLuce Model ####
mod_overall <- PlackettLuce(R)

model_summaries <- multcompPL(mod_overall, adjust = ci_adjust)

fullanova <- anova.PL(mod_overall)

worthscaled <- rev(sort(exp(coef(mod_overall)) / sum(exp(coef(mod_overall)))))

worthscaled <- data.frame(label = factor(names(worthscaled),
                                         (names(worthscaled))),
                          worth = worthscaled,
                          prob = paste0(round(worthscaled * 100, 1), "%"), 
                          check.names = FALSE)

names(worthscaled) <- c(Option, c("Worth","Win probability"))

# Run over the other traits
mods <- list()
summaries <- list()
worths <- list()
anovas <- list()
contests_t <- list()

for (i in seq_along(other_traits)){
  ot <- other_traits[i]
  ot <- trait_list[[ot]]
  
  if (isTRUE(overallVSlocal)) {
    if (ncomp == 3) {
      keep <- overall$keep2 & ot$keep
      
      Rot <- rank_tricot(cmdata[keep, ],
                         items = itemnames,
                         input = ot$input,
                         additional.rank = cmdata[keep, overall$ovsl])
    }
    
    if (ncomp > 3) {
      keep <- overall$keep2 & ot$keep
      
      Rot <- rank_numeric(cmdata[keep, ],
                          items = itemnames,
                          input = ot$input)
    }
  }
  
  if (isFALSE(overallVSlocal)) {
    keep <- ot$keep
    if (ncomp == 3){
      Rot <- rank_tricot(data  = cmdata[keep, ],
                         items = itemnames,
                         input = ot$input) 
    }
    
    if (ncomp > 3) {
      Rot <- rank_numeric(data  = cmdata[keep, ],
                          items = itemnames,
                          input = ot$input) 
    }
    
  }
  
 
  
  contests <- list()
  contests[[1]] <- summarise_dominance(Rot)
  contests[[2]] <- summarise_victories(Rot)
  
  contests_t[[i]] <- contests
  
  mod_t <- PlackettLuce(Rot)
  
  mods[[i]] <- mod_t
  
  summaries[[i]] <- multcompPL(mod_t, adjust = ci_adjust)
  
  anovas[[i]] <- anova.PL(mod_t)
  
  worths_i <- rev(sort(coef(mod_t, log = FALSE)))
  
  worths_i <- data.frame(label = factor(names(worths_i),
                                        (names(worths_i))),
                         worth = worths_i,
                         prob = paste0(round(worths_i * 100, 1), "%"), 
                         check.names = FALSE)
  
  names(worths_i) <- c(Option, c("Worth","Win probability"))
  
  worths[[i]] <- worths_i
}

# .......................................................
# .......................................................
# PlackettLuce combining traits together ####
coefs <- qvcalc(mod_overall)[[2]]$estimate

for(i in seq_along(other_traits)){
  coefs <- cbind(coefs, 
                 scale(qvcalc(mods[[i]])[[2]]$estimate))
}

coefs <- as.data.frame(coefs)
# add item names as rows
rownames(coefs) <- rownames(qvcalc(mod_overall)[[2]])
# set col names with title case traits
otf <- gsub(" ","_", other_traits_full)
names(coefs) <- c("Overall", ClimMobTools:::.title_case(otf))

# .......................................................
# .......................................................
# Compute partial least squares ####
arrows <- data.frame()
scores <- data.frame()

if (length(other_traits) > 0) {
  fml <- paste("Overall ~ ", paste(ClimMobTools:::.title_case(otf), collapse = " + "))
  fml <- as.formula(fml)
  
  m2 <- plsr(fml,
             data = coefs,
             validation = "LOO", 
             jackknife = TRUE)
  
  if (ncol(m2$projection) > 1 ) {
    arrows <- data.frame((m2$projection)[,1:2],
                         trait = otf,
                         x0 = 0, 
                         y0 = 0, 
                         stringsAsFactors = FALSE)
    scores <- data.frame((m2$scores)[,1:2],
                         var = rownames(m2$scores))
  }
  
  
  yve <- drop(R2(m2, 
                 estimate = "train",
                 intercept = FALSE)$val)
  
  adjCV <- m2$validation$adj
  
  nc <- which(adjCV == min(adjCV))
}

if (dim(arrows)[[1]] > 0) {
  
  pls_plot <- ggplot(data = arrows,
                     aes(y = Comp.2, 
                         x = Comp.1, 
                         label = trait, 
                         yend = y0, 
                         xend = x0)) +
    geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) +
    geom_segment(col = "red", arrow = arrow(length = unit(0.5, "cm"), ends = "first" )) +
    geom_abline(linetype = 2, col = "gray50", slope = (yve[2] - yve[1]) / yve[1], intercept = 0) +
    geom_text_repel(fontface = 2, size = 4, col = "red") +
    geom_text(data = scores, 
              aes(y = Comp.2,
                  x = Comp.1, 
                  label = var), 
              inherit.aes = FALSE, fontface = 2, size = 3) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    labs(x = paste0("PC1 ", round(yve[1] * 100, 2), "%"),
         y = paste0("PC2 ", round((yve[2]-yve[1]) * 100, 2),"%"))
  
  pls_summary_line <- paste("The first two components recombining the specific characteristics",
  "are able to explain", paste0(round(yve[2] * 100, 2), "%"), "of the variability in the overall performance.",
  "The dashed line represents the overall performance, with an increase in performance as the x and y increase.",
  "The", option, "positioned close to the dashed line will be performing equally across all characteristics;",
  "the", option, "positioned further away from the dashed line, on either side, will have varying performance",
  "in different characteristics. Better performance in characteristics will correspond with arrows pointing",
  "in the direction away from the dashed line and worse performance in characteristics directed on the opposite side.") 
}

if (dim(arrows)[[1]] == 0) {
  pls_summary_line<-"However, partial least squares analysis was not possible with this project data."
}

# .......................................................
# .......................................................
# Analysis with explanatory variables ####
if (isTRUE(overallVSlocal)) {
  if (ncomp == 3) {
    keep <- overall$keep2 & expvar_list$keep
    
    G <- rank_tricot(cmdata[keep, ],
                     items = itemnames,
                     input = overall$input,
                     additional.rank = cmdata[keep, overall$ovsl], 
                     group = TRUE)
  }
  
  if (ncomp > 3) {
    keep <- overall$keep2 & expvar_list$keep
    
    G <- rank_numeric(cmdata[keep, ],
                      items = itemnames,
                      input = overall$input,
                      group = TRUE)
  }
  
} 

if (isFALSE(overallVSlocal)) {
  keep <- overall$keep & expvar_list$keep
  if (ncomp == 3) {
    G <- rank_tricot(cmdata[keep, ],
                     items = itemnames,
                     input = overall$input, 
                     group = TRUE)
  }
  
  if (ncomp > 3) {
    G <- rank_numeric(cmdata[keep, ],
                      items = itemnames,
                      input = overall$input, 
                      group = TRUE)
  }
  
}

# data frame of explanatory variables
Gdata <- as.data.frame(cmdata[keep, expvar_list$expvar], stringsAsFactors = TRUE)
nvar <- length(expvar_list$expvar)
# setup the variables
Gdata[1:nvar] <- lapply(Gdata[1:nvar], function(x){
  if(is.numeric(x)) {
    x <- round(x, 3)
  }
  
  if(is.character(x)) {
    x <- as.factor(x)
  }
  return(x)
})

names(Gdata) <- expvar_list$expvar_full
Gdata <- cbind(G, Gdata)

tree_f <- pltree(G ~ .,
                 data = Gdata, 
                 minsize = minsplit,
                 alpha = sig_level)


# if the tree has splits, extract coeffs from nodes
if (length(tree_f) > 1) { 

  node_ids <- nodeids(tree_f,terminal = TRUE)
  
  coefs_t <- NULL
  for(i in seq_along(node_ids)) {
    
    coef_i <- data.frame(node = node_ids[i],
                         rule = partykit:::.list.rules.party(tree_f, node_ids[i]),
                         multcompPL(tree_f[[ node_ids[i] ]]$node$info$object),
                         n = tree_f[[ node_ids[i] ]]$node$info$nobs,
                         stringsAsFactors = FALSE)
    
    coefs_t <- rbind(coefs_t, coef_i)
    
  }
  
  coefs_t$Label <- paste("Node", coefs_t$node, ":", coefs_t$rule,"\n","n=",coefs_t$n)
  
  coefs_t <- split(coefs_t, coefs_t$node)
  
  coefs_t <- lapply(coefs_t, function(x){
    x$m <- mean(x$estimate)
    x$ctd <- x$estimate - x$m
    x
  })
  
  coefs_t <- do.call(rbind, coefs_t)

  rules <- unique(coefs_t$rule)
  best_tree <- NULL
  for(i in seq_along(rules)){
    
    tmp <- subset(coefs_t, rule==rules[i])
    
    best_tree <- rbind(best_tree,
                       c(tmp$n[1], 
                         paste(tmp$term[grepl("a", tmp$.group)], collapse=", "),
                         paste(rev(tmp$term[grepl(tmp$.group[nrow(tmp)], tmp$.group)]), collapse=", ")))
  }
  
  node_summary <- data.frame(rules, 
                             best_tree, 
                             stringsAsFactors = FALSE)
  
  names(node_summary) <- c("Split","Number of Respondents","Best Ranked","Worst Ranked")

}


outtabs <- NULL
for(j in seq_along(tree_f)){
  
  zzz <- nodeapply(tree_f, j, function(n){
    info_node(n)$test
    })[[1]]
  
  if (length(zzz) > 0) {
    x <- data.frame(Node = j,
                    t(nodeapply(tree_f, j, function(n){
                      info_node(n)$test
                    })[[1]]))
      
    x$p.value <- round(x$p.value, 3)
    
    outtabs[[j]] <- x
    
    outtabs[[j]]$p <- format.pval(outtabs[[j]]$p.value)
  }
  
  else{
    outtabs[[j]] <- data.frame(Node = j, 
                               Message = "No further splits possible", 
                               p.value = NA,
                               stringsAsFactors = FALSE)
  }
}

# Built table with p-values per node
pval_nodes <- data.frame()

for (i in seq_along(outtabs)) {
  if (ncol(outtabs[[i]]) > 3) {
    ot <- data.frame(Covariate = rownames(outtabs[[i]]),
                     outtabs[[i]])
    pval_nodes <- rbind(pval_nodes, ot)
  }
}

if (dim(pval_nodes)[[2]] > 0) {
  
  pval_nodes <- pval_nodes[,c("Covariate", "Node", "p")]
  
  row.names(pval_nodes) <- NULL
  
  # put bold in significant values 
  putbold <- which(pval_nodes$p <= sig_level)
  
  for(i in seq_along(putbold)) {
    index_i <- putbold[i]
    
    pval_nodes[index_i, ] <- paste0("**", pval_nodes[index_i, ], "**")
    
  }
  
  nodemessage <- ""
  
}

if (dim(pval_nodes)[[2]] == 0) {
  nodemessage <- "No covariates were able to be assessed statistically."
}

# ....................................................................
# ....................................................................
# Build headline summaries ####

siglist <- NULL
for(i in seq_along(outtabs)){
  if (dim(outtabs[[i]])[[2]] > 3) {
    siglist <- c(siglist,
                 rownames(outtabs[[i]])[outtabs[[i]]$p.value < sig_level])
  }
}

siglist <- unique(siglist)

ps <- fullanova[2, 5]
if(ps < 0.05){
  bests <- paste(model_summaries$term[grep("a", model_summaries$.group)], 
                 collapse =", ")
  
  worsts <-paste(rev(model_summaries$term[grep(model_summaries$.group[nrow(model_summaries)],
                                              model_summaries$.group)]),
                 collapse=", ")
}else{
  bests <- worsts <- "No significant differences"
}

if (length(other_traits) > 0) {
  for(i in 1:length(anovas)){
    
    ps <- c(ps, anovas[[i]][2,5])
    
    if(ps[i] < sig_level){
      bests<-c(bests,paste(summaries[[i]]$term[grep("a",summaries[[i]]$.group)],collapse=", "))
      worsts<-c(worsts,paste(rev(summaries[[i]]$term[grep(summaries[[i]]$.group[nrow(summaries[[i]])],
                                                          summaries[[i]]$.group)]),collapse=", "))
    }else{
      bests<-c(bests,"No significant differences")  
      worsts<-c(worsts,"No significant differences")  
    }
  }
  
  ptab <- data.frame(Ranking = c("Overall", other_traits_full),
                     "Best Ranked" = bests,
                     "Worst Ranked" = worsts,
                     p.value = ps,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
  
} 

if (length(other_traits) == 0) {
  ptab <- data.frame(Ranking = "Overall",
                     "Best Ranked" = bests,
                     "Worst Ranked" = worsts,
                     p.value = ps,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
}


ptab$sig <- stars.pval(ptab$p.value)

ptab$p.value <- round(ptab$p.value, 5)


outtabs[[1]]$p.value <- as.numeric(outtabs[[1]]$p.value)

uni_sum <- outtabs[[1]]
uni_sum$Variable <- rownames(outtabs[[1]])
uni_sum$p <- paste(format.pval(outtabs[[1]]$p.value),stars.pval(outtabs[[1]]$p.value))
rownames(uni_sum) <- NULL


# define height of plots based on items
favplot_h <- nitems * 0.4
contest_h <- nitems * 0.4 * 2
agreem_h <- ntrait * 0.9
multcomp_h <- nitems * 0.6 
