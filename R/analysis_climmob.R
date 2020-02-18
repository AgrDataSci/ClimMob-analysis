# ................................................................
# ................................................................
# Analyse the performance of agricultural technologies from 
# crowdsourcing trials using Plackett-Luce model
# ................................................................
# ................................................................
# Kaue de Sousa 
# Updated 17Feb2020

# .......................................................
# .......................................................
# Organise the rankings and check for missing data ####
trait <- pars$chars$char
trait_full <- pars$chars$char_full
overallVSlocal <- !is.null(pars$perf)

# check if overall is the first component
# it should be the first as sorted in ClimMobTools:::.decode_pars()
of <- which(grepl("overall", trait))[1] ==  1
if(!of) {
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
    
    dropit2 <- (sum(keep2) / nranker) < missper
    
    if (dropit2) next
    
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
if (any(expvar == "xinterceptx")) {
  cmdata$xinterceptx <- rep(0, nranker)
}

if (all(expvar != "xinterceptx")) {
  
  # check for the full names
  for(i in seq_along(expvar)) {
    expvar[i] <- names(cmdata[which(grepl(expvar[i], names(cmdata)))])
  }
  
  # check for missing data
  # here we prioritize the rankings, so if a explanatory variable is 
  # missing in any point it is dropped entirely
  drop <- apply(cmdata[expvar], 2, function(x) any(is.na(x)))

  expvar <- expvar[!drop]
  
  # if no explanatory variable put a pseudo variable
  if(length(expvar) == 0) {
    expvar <- "xinterceptx"
    cmdata$xinterceptx <- rep(0, nranker)
  }
  
}

# .......................................................
# .......................................................
# This is Table 01 #
# Create a table with frequencies where each item was evaluated
itemdata <- cmdata[, grepl("package_item", names(cmdata))]

itemtable <- data.frame(table(unlist(itemdata)))

itemtable$x <- with(itemtable,
                    round((Freq / nranker * 100), 1))

itemtable$x <- with(itemtable,
                    paste0(x, "%"))

names(itemtable) <- c(Option, "Freq", "Relative freq")

# check if gender is provided so it can be added to the itemtable
gender <- which(grepl("gender", names(cmdata)))

if (length(gender) == 1) {
  
  dt <- unlist(itemdata)
  
  nMan <- sum(gender == "Man", na.rm = TRUE)
  nWom <- sum(gender == "Woman", na.rm = TRUE)
  
  dt <- suppressWarnings(cbind(dt, rep(gender, ncomp)))
  
  dt <- table(dt[, 1], dt[, 2])
  
  itemtable$m <- dt[, "Man"]
  
  itemtable$w <- dt[, "Woman"]
  
  names(itemtable)[4:5] <- paste0(c("Man (n=","Woman (n="), c(nMan, nWom), ")")
  
  rm(dt)
    
}

# .......................................................
# .......................................................
# Favourability Analysis Table ####
# do this for overall performance

# find the index for overall evaluation
overall <- trait_list[[1]]

fav1 <- summarise_favourite(data  = cmdata[overall$keep, ],
                            items = itemnames,
                            input = overall$input) 

fav2 <- fav1

fav2$best <- paste0(round(fav2$best, 1), "%")
fav2$worst <- paste0(round(fav2$worst, 1), "%")
fav2$wins <- paste0(round(fav2$wins * 100, 1), "%")
fav2$fav_score <- round(fav2$fav_score, 1)

names(fav2) <- c("Variety","N","Top Ranked","Bottom Ranked",
                 "Contests Won","Net Favourability Score")

# Contest Plots
R <- rank_tricot(data  = cmdata[overall$keep, ],
                 items = itemnames,
                 input = overall$input) 

cont1 <- summarise_dominance(R)

cont2 <- summarise_victories(R)

# .......................................................
# .......................................................
# Trait concordance ####
# this assess how the other traits agreed with the overall preference
# build rankings for the other characteristics
other_traits <- trait[-1]
if (length(other_traits) > 0) {
  
  # filter cmdata so it matches the dims in all traits
  keep <- NULL
  for(i in seq_along(trait_list)) {
    keep <- cbind(keep, trait_list[[i]]$keep)
  }
  keep <- rowSums(keep)
  keep <- keep == length(trait_list)
  
  compare_to <- rank_tricot(cmdata[keep, ], 
                            items = itemnames,
                            input = overall$input)
  
  compare_with <- list()
  for (i in seq_along(other_traits)) {
    
    ot <- trait_list[[other_traits[i]]]
  
    otr <- rank_tricot(cmdata[keep, ],
                       items = itemnames,
                       input = ot$input)  
  
    compare_with[[i]] <- otr
      
  }
  

  agreement <- summarise_agreement(compare_to, 
                                   compare_with, 
                                   labels = other_traits)
  
  strongest_link <- agreement[[which.max(agreement$kendall), "labels"]]
  
  weakest_link   <- agreement[[which.min(agreement$kendall), "labels"]]
  
  agreement_table <- agreement[, c("labels", "first", "last", "kendall")]
  
  names(agreement_table) <- c(Option, 
                              "Agreement with Overall Best", 
                              "Agreement with Overall Worst",
                              "Complete Ranking Agreement")

  
}

# .......................................................
# .......................................................
# PlackettLuce Model ####

# overall model
if (overallVSlocal) {
  
}

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
  
  Rot <- rank_tricot(data  = cmdata[ot$keep, ],
                     items = itemnames,
                     input = ot$input) 
  
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
names(coefs) <- c("Overall", ClimMobTools:::.title_case(other_traits))

# compute partial least squares
fml <- paste("Overall ~ ", paste(ClimMobTools:::.title_case(other_traits), collapse = " + "))
fml <- as.formula(fml)

m2 <- plsr(fml,
           data = coefs,
           validation = "LOO", 
           jackknife = TRUE)

arrows <- NULL
scores <- NULL
if (ncol(m2$projection) > 1 ) {
  arrows <- data.frame((m2$projection)[,1:2],
                       trait = other_traits,
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

# .......................................................
# .......................................................
# Analysis with explanatory variables ####
# first for overall
G <- group(R, 1:sum(overall$keep))
Gdata <- as.data.frame(cmdata[overall$keep, expvar], stringsAsFactors = FALSE)
names(Gdata) <- expvar
Gdata <- cbind(G, Gdata)

tree_f <- pltree(G ~ .,
                 data = Gdata, 
                 minsize = minsplit, 
                 alpha = sig_level)


if(length(tree_f)>1){
  
  coefs_t<-map_df(nodeids(tree_f,terminal = TRUE),
                  function(x)data.frame(node=x,
                                        rule=partykit:::.list.rules.party(tree_f, x),
                                        multcompPL(tree_f[[ x ]]$node$info$object)))
  
  
  ns<-map_df(nodeids(tree_f,terminal = TRUE),
             function(x)data.frame(node=x,
                                   rule=partykit:::.list.rules.party(tree_f, x),
                                   n=tree_f[[ x ]]$node$info$nobs))
  
  coefs_t<-inner_join(coefs_t,ns)
  
  coefs_t$Label<-paste("Node",coefs_t$node,":",coefs_t$rule,"\n","n=",coefs_t$n)
  
  
  coefs_t<-coefs_t %>% mutate(term=reorder(term,estimate,mean)) %>%
    group_by(node) %>% mutate(m=mean(estimate),ctd=estimate-m) %>%data.frame()
  
  rules=unique(coefs_t$rule)
  best_tree<-NULL
  
  for(i in 1:length(rules)){
    tmp<-subset(coefs_t,rule==rules[i])
    best_tree<-rbind(best_tree,c(tmp$n[1],paste(tmp$term[grep("a",tmp$.group)],collapse=", "),
                                 paste(rev(tmp$term[grep(tmp$.group[nrow(tmp)],tmp$.group)]),collapse=", ")))
    
    
  }
  node_summary<-data.frame(rules,best_tree)
  colnames(node_summary)<-c("Subgroup","Number of Respondents","Best Ranked Varieties","Worst Ranked Varieties")
  
}    


outtabs<-NULL
for(j in 1:length(tree_f)){
  xxx<-nodeapply(tree_f,j,function(n) info_node(n)$test)[[1]]
  if(length(xxx)>0){
    outtabs[[j]]<-data.frame(Node=j,t(nodeapply(tree_f,j,function(n) info_node(n)$test)[[1]]))
    outtabs[[j]]$p<-format.pval(outtabs[[j]]$p.value)
  }
  else{
    outtabs[[j]]<-data.frame(Node=j,Message="No further splits possible",p.value=NA)
  }
}


##Build headline summaries

siglist<-NULL
for(i in 1:length(outtabs)){
  if(ncol(outtabs[[i]])>3){
    siglist<-c(siglist,rownames(outtabs[[i]])[outtabs[[i]]$p.value<0.05])
  }
}
siglist<-unique(siglist)

ps<-fullanova[2,5]
if(ps<0.05){
  bests<-paste(model_summaries$term[grep("a",model_summaries$.group)],collapse=", ")
  worsts<-paste(rev(model_summaries$term[grep(model_summaries$.group[nrow(model_summaries)],
                                              model_summaries$.group)]),collapse=", ")
}else{
  bests<-worsts<-"No significant differences"
}

for(i in 1:length(anovas)){
  if(class(mods[[i]])=="PlackettLuce"){
    ps<-c(ps,anovas[[i]][2,5])
    if(ps[i]<0.05){
      bests<-c(bests,paste(summaries[[i]]$term[grep("a",summaries[[i]]$.group)],collapse=", "))
      worsts<-c(worsts,paste(rev(summaries[[i]]$term[grep(summaries[[i]]$.group[nrow(summaries[[i]])],
                                                          summaries[[i]]$.group)]),collapse=", "))
    }
    
    else{
      bests<-c(bests,"No significant differences")  
      worsts<-c(worsts,"No significant differences")  
    }
  }
  else{
    ps<-c(ps,NA)
    bests<-c(bests,NA)
    worsts<-c(worsts,NA)
  }
}
ptab<-data.frame(Ranking=c("Overall",trait_short),
                 p.value=ps,"Best Ranked"=bests,
                 "Worst Ranked"=worsts,check.names = FALSE)

ptab$p.value<-paste(format.pval(ptab$p.value),stars.pval(ptab$p.value))


outtabs[[1]]$p.value<-as.numeric(outtabs[[1]]$p.value)

uni_sum<-outtabs[[1]]
uni_sum$Variable<-rownames(outtabs[[1]])
uni_sum$p<-paste(format.pval(outtabs[[1]]$p.value),stars.pval(outtabs[[1]]$p.value))
rownames(uni_sum)<-NULL
