#' Summarise the results for the participant reports
#' 
#' Process the data for the summaries for participants
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
#' @param fullpath character with the backward path to read the file
#' 
get_participant_report = function(cmdata, rank_dat, fullpath, language) {
  
  itemnames = rank_dat$technologies_index
  
  trait_list = rank_dat$trait_list
  
  reference_trait = rank_dat$reference_trait_index
  
  other_traits_list = trait_list[-reference_trait]
  
  nranker = rank_dat$nranker
  
  nitems = length(rank_dat$technologies)
  
  items = rank_dat$technologies
  
  ncomp = length(itemnames)
  
  nothertraits = length(trait_list) - 1 
  
  reference = rank_dat$reference_tech
  
  # Make the overview ####
  # table with the worth parameters from overall performance
  # number of times each item was tested
  # and how many times it was ranked first or last
  
  # make the rank without the Local item
  overall = trait_list[[reference_trait]]
  
  # take the question asked
  question_asked = overall$question
  
  # the vector to filter the data
  keep = overall$keep
  
  # multilanguage text
  reporttext = read.csv(paste0(fullpath,
                                "/report/participant_report_multilanguage_text.csv"))
  
  
  # pick the vector with the language
  if (isTRUE(language %in% names(reporttext))) {
    reporttext = reporttext[, c(1, match(language, names(reporttext)))]
  } else {
    reporttext = reporttext[, c(1:2)]
  }
  
  # replace fields with info from ClimMob
  notreplied = reporttext[match("notreplied", reporttext[,1]), 2]
  ranker     = reporttext[match("partictag", reporttext[,1]), 2]
  rankers    = reporttext[match("partictag2", reporttext[,1]), 2]
  option     = reporttext[match("techtag", reporttext[,1]), 2]
  Option     = ClimMobTools:::.title_case(option)
  options    = reporttext[match("techtag2", reporttext[,1]), 2]
  
  reporttext[,2] = gsub("r rankers", rankers, reporttext[,2])
  reporttext[,2] = gsub("r nranker", nranker, reporttext[,2])
  reporttext[,2] = gsub("r question_asked", question_asked, reporttext[,2])
  reporttext[,2] = gsub("r nitems", nitems, reporttext[,2])
  reporttext[,2] = gsub("r options", options, reporttext[,2])
  reporttext[,2] = gsub("r nothertraits", nothertraits, reporttext[,2])
  reporttext[,2] = gsub("r ncomp", ncomp, reporttext[,2])
  
  # list of arguments for the function that will be used to
  # create the rankings
  a = list(cmdata[keep, ],
            items = itemnames,
            input = overall$strings,
            full.output = TRUE)
  
  R = do.call("rankTricot", args = a)
  
  mod_overall = PlackettLuce(R[["PLranking"]])
  
  order_items = coef(mod_overall, ref = reference, log = FALSE)
  
  # do this to remove ties
  order_items = order_items[names(order_items) %in% items]
  
  rank_items = gosset:::.rank_decimal(order_items)$rank
  
  order_items = names(order_items)
  
  freq_items = table(unlist(cmdata[itemnames]))
  
  ordering = R[["myrank"]]
  
  first_items = table(ordering[,1])
  last_items  = table(ordering[,3])
  
  infotable = data.frame(item = order_items,
                          rank = rank_items,
                          freq = as.vector(freq_items[order_items]),
                          first = as.vector(first_items[order_items]),
                          last = as.vector(last_items[order_items]))
  
  infotable[is.na(infotable)] = 0
  
  infotable = infotable[order(infotable$rank), ]
  
  # ................................................................
  # ................................................................
  # Get the info from the participants ####
  names(cmdata)[names(cmdata) == "package_farmername"] = "package_participant_name"
  sel = c("id", "package_participant_name", paste0("package_item_", LETTERS[1:ncomp]))
  partitable = cmdata[, sel]
  
  names(partitable) = gsub("package_|farmer|participant_", "", names(partitable))
  
  # empty matrix to expand values from ord so it can fit partitable
  # in case of missing data when participants did not replied the reference trait
  x = matrix(NA,
              ncol = ncomp,
              nrow = length(cmdata$id),
              dimnames = list(seq_along(cmdata$id), paste0("Position", 1:ncomp)))
  
  partitable = cbind(partitable, x)
  
  partitable[keep, paste0("Position", 1:ncomp)] = ordering
  
  # fill NAs with "Not replied" in the first case and then with an empty character
  partitable$Position1[is.na(partitable$Position1)] = notreplied
  partitable[is.na(partitable)] = ""
  
  # ................................................................
  # ................................................................
  # If any other trait, do the same ####
  otrp = list()
  
  if(isTRUE(nothertraits > 0)){
    
    otr = list()
    
    otrnames = lapply(other_traits_list, function(x){
      paste0(x$name, " [", x$assessment, "]") 
    })
    
    otrnames = as.vector(unlist(otrnames))
    
    for(i in seq_along(other_traits_list)){
      
      ot = other_traits_list[[i]]
      
      a = list(cmdata[ot$keep, ],
                items = itemnames,
                input = ot$strings,
                full.output = TRUE)
      
      R = do.call("rankTricot", args = a)[["myrank"]]
      
      # expand the rankings (in rows) so it can fit with the full
      # information to include those participants who did not replied the
      # question
      Rexp = matrix(NA,
                     nrow = nrow(partitable),
                     ncol = ncomp,
                     dimnames = list(partitable$id,
                                     paste0("Position", 1:ncomp)))
      
      Rexp[ot$keep, ] = R
      
      R = Rexp
      
      R[is.na(R[, 1]), 1] = notreplied
      
      R[is.na(R)] = ""
      
      otr[[i]] = R
      
    }
    
    # now put all together by participants ids
    otrp = list()
    for(i in  seq_along(partitable$id)){
      
      x = NULL
      
      # combine (by rows) the response for the participant i
      # across all the j other traits
      for(j in seq_along(other_traits_list)){
        
        x = rbind(x, otr[[j]][i, ])
        
      }
      
      # add the question that was made
      x = cbind(Trait = otrnames,
                 x)
      
      x = data.frame(Trait = rep(x[,1], 3),
                 Tech = as.vector(unlist(x[,-1])),
                 Rank = rep(1:3, each = nrow(x)))
      
      x$Tech[x$Tech == ""] = NA
      
      x$Tech[x$Tech == "Not replied"] = NA
      
      x = na.omit(x)
      
      x$Rank = factor(x$Rank)
      x$Trait = factor(x$Trait, levels = rev(otrnames))
      
      # make a plot
      if (nrow(x) > 1) {
        x = ggplot(x, 
               aes(x = Tech, 
                   y = Trait, 
                   fill = Rank)) + 
          geom_tile() +
          scale_fill_manual(values = c("#FFD700", "#C0C0C0", "#cd7f32")) +
          theme_minimal() +  
          labs(x = "", y = "", fill = "Rank")
        
        otrp[[i]] = x
        
      } else {
        otrp[[i]] = 0L
      }
      
      
      
    }
    
  }
  
  # use the coefficients from the overall model and plot it as bar plot
  # to show the overall evaluation for all the farmers 
  pover = coef(mod_overall, log = FALSE)
  pover = sort(pover)
  pover = data.frame(items = factor(names(pover), levels = names(pover)),
                      pw = as.vector(pover))
  
  poverp = ggplot(pover, aes(x = pw,
                              y = items,
                              fill = pw)) +
    geom_bar(stat = "identity",
             position = "dodge",
             show.legend = FALSE) +
    labs(x = "", y = "") +
    theme(element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 5),
          panel.background = element_blank())
  
  # make a template of ggplot to assemble a podium
  podium = data.frame(label = factor(c("1st", "2nd", "3rd"), 
                                     levels = c("2nd", "1st", "3rd")),
                       values = (3:1))
  
  ggpodium <-
    ggplot(data = podium,
           aes(y = values, x = label, fill = label)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    labs(x = "",
         y = "") +
    scale_fill_manual(values = c("#C0C0C0", "#FFD700", "#cd7f32")) +
    theme(element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          panel.background = element_blank())
  
  
  output = list(podium = ggpodium,
                 poverp = poverp,
                 partitable = partitable,
                 reporttext = reporttext,
                 other_traits_table = otrp)
  
  return(output)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_participant_report = list(podium = 0L,
                                 poverp = 0L, 
                                 partitable = list(),
                                 reporttext = list(),
                                 other_traits_table = list())




