#'Get colour pallet
#' @param x an integer
#' @examples 
#' col_pallet(15)
col_pallet <- function(x, ...) {
  
  p <- c('#d73027','#4575b4', '#f46d43','#74add1', 
         '#fdae61','#abd9e9', '#fee090', '#762a83',
         '#a6dba0','#9970ab','#5aae61', '#c2a5cf', 
         '#1b7837','#ffffe5','#fff7bc','#fee391',
         '#fec44f','#fe9929','#ec7014','#cc4c02',
         '#993404','#662506')
  
  v <- p[1:x]
  
  return(v)
  
}

#' Plot map using leaflet
#' @param data a data frame
#' @param xy index of data for the longitude and latitude coordinates (in that order)
#' @param make.clusters logical, if TRUE coordinates are aggregated by a defined cluster size
#' @param cut.tree numeric, to define the cluster size when make.clusters = TRUE
#' @param map_provider the name of the provider (see http://leaflet-extras.github.io/leaflet-providers/preview/ 
#'        and https://github.com/leaflet-extras/leaflet-providers)
#' @param minimap logical, TRUE to add the minimap
#' @param minimap_position the position of the mini map 
#' @examples
#' lonlat <- data.frame(lon = c(15.6, 16.7, 15.55, 15.551),
#'                      lat = c(65.8, 66.3, 66.25, 66.251))
#' 
#' p <- plot_map(lonlat, xy = c(1,2), cut.tree = 0.05)
#' @noRd
plot_map <- function(data, 
                     xy = NULL,
                     make.clusters = TRUE,
                     cut.tree = 0.05,
                     map_provider = "Esri.WorldImagery",
                     minimap = TRUE,
                     minimap_position = "bottomright", 
                     ...){
  
  d <- data[, xy]
  
  # coerce to numeric
  d[1:2] <- lapply(d[1:2], as.numeric)
  
  # remove NAs
  d <- stats::na.omit(d)
  
  nd <- dim(d)[[1]]
  
  if (isTRUE(nd == 0)) {
    stop("No remaining coordinates to plot. ",
         "Please check for NAs or if the values can be coerced to numeric. \n")
  }
  
  names(d) <- c("lon","lat")
  
  if (isTRUE(make.clusters)) {
    # to ensure the privacy of participants location
    # we can put the lonlat info into clusters of 0.5 resolution
    h <- stats::dist(d)
    
    h <- stats::hclust(h)
    
    h <- stats::cutree(h, h = cut.tree)
    
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
    
  }
  
  
  map <- leaflet::leaflet(data = d, 
                          options = leaflet::leafletOptions(maxZoom = 17))
  
  map <- leaflet::fitBounds(map = map, 
                            lng1 = min(d$lon) - 0.25,
                            lat1 = min(d$lat) - 0.25,
                            lng2 = max(d$lon) + 0.25, 
                            lat2 = max(d$lat) + 0.25)
  
  map <- leaflet::addProviderTiles(map = map, 
                                   provider =  map_provider, 
                                   options = leaflet::providerTileOptions(maxNativeZoom = 17))
  
  map <- leaflet::addMarkers(map)
  
  if (isTRUE(minimap)) {
    
    map <- leaflet::addMiniMap(map = map, position = minimap_position, 
                               width = 100, height = 100)
    
  }
  
  map$x$options = list("zoomControl" = FALSE)
  
  return(map)
  
}

#' Analysis of variance in a PlackettLuce model 
#' 
#' @param model an object of class PlackettLuce
#' @return an ANOVA table
#' @examples 
#' library("PlackettLuce")
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 1, 3,
#'               1, 2, 3, 0,
#'               2, 1, 3, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' 
#' mod <- PlackettLuce(R)
#' 
#' anova.PL(mod)
#' @noRd
anova.PL <- function(model){
  if(class(model)!="PlackettLuce"){
    stop("Model type is not Plackett-Luce")
  }
  
  LLs <- round(c(model$null.loglik, model$loglik), 3)
  dfs <- c(model$df.null, model$df.residual)
  df_diff <- (-1) * diff(dfs)
  df_LL <- round((-1) * diff(LLs), 3)
  p <- round(1 - pchisq(-2 * df_LL, df_diff), 4)
  stars <- gtools::stars.pval(p)
  
  x <- data.frame(Model = c("NULL", deparse(substitute(model))),
                  "logLik" = LLs,
                  DF = dfs,
                  "Statistic" = c(NA, -2 * df_LL),
                  "Pr(>Chisq)" = c(NA, p),
                  " " = c("", stars),
                  check.names = FALSE,
                  stringsAsFactors = FALSE)
  
  return(x)
  
}


#' Visualise network
#' @param object an object of class rankings
#' @param ... additional arguments passed to igraph methods
#' @return an igraph plot
#' @examples 
#' library("PlackettLuce")
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 1, 3,
#'               1, 2, 3, 0,
#'               2, 1, 3, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' 
#' R <- as.rankings(R)
#' 
#' network(R)
#' @noRd
network <- function(object, ...) {
  
  if (class(object) == "grouped_rankings") {
    object <- as.rankings(object)
  }
  
  R <- object 
  
  adj <- PlackettLuce::adjacency(R)
  
  net <- network::network(adj)
  
  GGally::ggnet2(net, 
                 label = TRUE,
                 label.color = "grey20",
                 arrow.size = 8, 
                 edge.color = "grey50",
                 arrow.gap = 0.03, 
                 color = col_pallet(ncol(object)),
                 legend.size = 12)
  
}

#' Rename duplicates
#' 
#' Look for duplicated values in a vector and rename them,
#'  an additional string is added to avoid duplicate and
#'  get unique values with the same vector length
#' 
#' @param x a vector to check and rename duplicated values
#' @param rename.with choose between numbers and letters
#' @examples
#' 
#' v <- c("Pear", "Apple", "Pear", "Orange", "Apple", "Apple")
#' 
#' rename_duplicates(v)
#' @noRd
rename_duplicates <- function(x, rename.with = "numbers", sep = "") {
  
  dups <- duplicated(x)
  dups <- unique(x[dups])
  
  for(i in seq_along(dups)) {
    
    dups_i <- x == dups[i]
    
    index <- seq_len(sum(dups_i))
    
    if (rename.with == "letters") {
      index <- letters[index]
    }
    
    x[dups_i] <- paste(x[dups_i],  index, sep = sep)
    
  }
  
  return(x)
  
}

#' Validate the class of objects generated in the tryCatch(s)
#' @noRd
any_error <- function(x){
  isTRUE("error" %in% class(x))
}

#' Runs specific lines of the code
#' @param file the path of R script 
#' @param start an integer for the index of first line to read and run
#' @param end an integer fot the index of last line to read and run
#' @noRd
source2 <- function(file, start, end, ...) {
  
  file.lines <- scan(file, 
                     what = character(), 
                     skip = start-1, 
                     nlines = end-start+1, 
                     sep = '\n')
  
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  
  source(textConnection(file.lines.collapsed), ...)
  
}


#' Get table with estimates out of a PLADMM
#' @param object an object of class PLADMM
#' @examples 
#' library("PlackettLuce")
#' library("prefmod")
#' data(salad)
#' 
#' features <- data.frame(salad = LETTERS[1:4],
#'                        acetic = c(0.5, 0.5, 1, 0),
#'                        gluconic = c(0, 10, 0, 10))
#' 
#' mod <- pladmm(salad, ~ acetic + gluconic,
#'               data = features,
#'               rho = 8)
#' 
#' pladmm_coeffs(mod)
#' @noRd
pladmm_coeffs <- function(object, ...) {
  coefs <- coef(object)
  coefficients <- matrix(NA, nrow = length(coefs), ncol = 4L, 
                         dimnames = list(names(coefs), c("Estimate", "Std. Error", 
                                                         "z value", "Pr(>|z|)")))
  coefficients[, 1L] <- coefs
  se <- sqrt(diag(vcov(object)))
  coefficients[names(se), 2L] <- se
  coefficients[, 3L] <- coefficients[, 1L]/coefficients[, 2L]
  coefficients[, 4L] <- 2L * pnorm(-abs(coefficients[, 3L]))
  
  coefficients <- as.data.frame(coefficients)
  
  coefficients[, 5] <- stars.pval(coefficients[, 4])
  
  coefficients[, 4] <- formatC(coefficients[, 4], format = "e", digits = 2)
  
  coefficients[, 6] <- rownames(coefficients)
  
  rownames(coefficients) <- 1:nrow(coefficients)
  
  coefficients <- coefficients[,c(6, 1:5)]
  
  names(coefficients)[c(1, 6)] <- ""
  
  coefficients
  
}

#' Paste strings and put a final conjunction in the string
#' @param a vector with characters
#' @param lan the language, choose between c("en", "pt", "fr", "es", "sw", "no")
#' @param ... additional passed to methods
#' @examples 
#' paste3(LETTERS[1:5])
#' 
#' paste3(LETTERS[1])
#' 
#' paste3(LETTERS[1:2], lan = "fr")
#' @noRd
paste3 <- function(x, lan = "en", ...) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 1) {
    return(x)
  }
  
  idiom <- c("en", "pt", "fr", "es", "sw", "no")
  conj <- c(" and ", " e ", " et ", " y ", " na ", " og ")
  
  index <- which(idiom %in% lan)
  
  if (length(index) == 0) {
    index <- 1
  }
  
  conj <- conj[index]
  
  x1 <- x[1:length(x)-1]
  x2 <- x[length(x)]
  
  x1 <- paste(x1, collapse = ", ")
  
  result <- paste(x1, conj, x2)
  
  return(result)

}


#' Decode arguments from ClimMob3
#' 
#' @param x a list of arguments given by ClimMob
#' @return a list with data frames for:
#'  traits: characteristics to be analysed 
#'  tricotVSlocal: the comparison between tested items and the local item
#'  covariates: the explanatory variables
#'  linear: strings for the variables to be used in linear regression
#' @noRd
decode_pars <- function(x) {
  
  traits <- x[["Characteristics"]]
  tricotVSlocal  <- x[["Performance"]]
  covariates  <- x[["Explanatory"]]
  linear <- x[["linearRegression"]]
  
  result <- list()
  
  if (length(traits) > 0) {
    
    questions <- lapply(traits$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst <- dim(questions)[[2]]
    
    questions$name <- traits$name
    
    questions$codeQst <- traits$codeQst
    
    
    questionAsked <- do.call(rbind, traits$questionAsked)
    questionAsked <- as.data.frame(questionAsked, stringsAsFactors = FALSE)
    names(questionAsked) <- paste0("questionAsked", seq_len(questions$nQst[1]))
    
    questions <- cbind(questions, questionAsked)
    
    if (!all(is.na(traits$code))) {
      
      questions$assessmentId <- traits$code$ass_cod
      
      questions$assessmentName <- traits$code$ass_desc
      
      questions$assessmentDay <- traits$code$ass_days  
    }
    
    if (all(is.na(traits$code))) {
      
      questions$assessmentId <- "0000000000"
      
      questions$assessmentName <- "Data collection"
      
      questions$assessmentDay <- 0
      
    }
    
    questions$traitOrder  <- rep("otherTraits", length(questions$codeQst))
    
    # try to find something related to performance, than yield and lastly taste
    # this is going to be the reference trait for the main analysis in the report
    traits <- questions$codeQst
    tr <- tolower(traits)
    
    if (any(grepl("overallperf", tr))) {
      i <- which(grepl("overallperf", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    if (any(grepl("overall", tr))) {
      i <- which(grepl("overall", tr))
      i <- i[length(i)]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    if (any(grepl("performance", tr))) {
      i <- which(grepl("performance", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    if (any(grepl("yield", tr))) {
      i <- which(grepl("yield", tr))[1]
      questions$traitOrder[i] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    if (sum(grepl("performance|yield|overall|overallperf", tolower(tr))) == 0) {
      questions$traitOrder[length(questions$codeQst)] <- "referenceTrait"
      tr <- toupper(tr)
    }
    
    rownames(questions) <- 1:nrow(questions)
    
    result[["traits"]] <- questions
    
  }else{
    
    result[["traits"]] <- character(0L)
    
  }
  
  if (length(tricotVSlocal) > 0) {
    questions <- lapply(tricotVSlocal$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst <- dim(questions)[[2]]
    
    questions$name <- tricotVSlocal$name
    
    questions$codeQst <- tricotVSlocal$codeQst
    
    result[["tricotVSlocal"]] <- questions
    
  }else{
    
    result[["tricotVSlocal"]] <- character(0L)
    
  }
  
  if (length(covariates) > 0) {
    
    covar               <- covariates[,c("codeQst", "id")]
    covar$nameString    <- covariates$vars
    covar$name          <- covariates$name 
    covar$questionAsked <- covariates$questionAsked
    
    if (all(is.na(covariates$code))) {
      
      covar$assessmentId <- "000000000000"
      covar$assessmentName <- "Registration"
      covariates <- covar
      
    }else{
      
      covariates <- covariates$code[,c("ass_cod","ass_desc")]
      names(covariates) <- c("assessmentId", "assessmentName")
      covariates$assessmentId[is.na(covariates$assessmentId)] <- "000000000000"
      covariates$assessmentName[is.na(covariates$assessmentName)] <- "Registration"
      covariates <- cbind(covariates, covar)
      
    }
    
    result[["covariates"]] <- covariates
    
  }else{
    
    result[["covariates"]] <- character(0L)
    
  }
  
  if (length(linear) > 0) {
    
    questions <- lapply(linear$vars, function(y) {
      unlist(y)
    })
    
    questions <- do.call(rbind, questions)
    questions <- as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) <- paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst <- dim(questions)[[2]]
    
    questions$name <- linear$name
    
    questions$codeQst <- linear$codeQst
    
    
    questionAsked <- do.call(rbind, linear$questionAsked)
    questionAsked <- as.data.frame(questionAsked, stringsAsFactors = FALSE)
    names(questionAsked) <- paste0("questionAsked", seq_len(questions$nQst[1]))
    
    questions <- cbind(questions, questionAsked)
    
    if (!all(is.na(linear$code))) {
      
      questions$assessmentId <- linear$code$ass_cod
      
      questions$assessmentName <- linear$code$ass_desc
      
      questions$assessmentDay <- linear$code$ass_days  
    }
    
    if (all(is.na(linear$code))) {
      
      questions$assessmentId <- "0000000000"
      
      questions$assessmentName <- "Data collection"
      
      questions$assessmentDay <- 0
      
    }
    
    rownames(questions) <- 1:nrow(questions)
    
    result[["linear"]] <- questions
    
  }else{
    
    result[["linear"]] <- character(0L)
    
  }
  
  return(result)
  
}


