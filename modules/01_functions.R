# # ................................................................
# # ................................................................
# # This Module load the internal functions used in this workflow
# # ................................................................
# # ................................................................

#'Get colour pallet
#' @param x an integer
#' @examples 
#' col_pallet(3)
col_pallet = function(x, ...) {
  
  p = c('#d73027','#4575b4', '#f46d43','#74add1', 
         '#fdae61','#abd9e9', '#fee090', '#762a83',
         '#a6dba0','#9970ab','#5aae61', '#c2a5cf', 
         '#1b7837','#ffffe5','#fff7bc','#fee391',
         '#fec44f','#fe9929','#ec7014','#cc4c02',
         '#993404','#662506')
  
  v = p[1:x]
  
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
#' lonlat = data.frame(lon = c(15.6, 16.7, 15.55, 15.551),
#'                      lat = c(65.8, 66.3, 66.25, 66.251))
#' 
#' p = plot_map(lonlat, xy = c(1,2), cut.tree = 0.05)
#' @noRd
plot_map = function(data, 
                     xy = NULL,
                     make.clusters = TRUE,
                     cut.tree = 0.05,
                     map_provider = "Esri.WorldImagery",
                     minimap = TRUE,
                     minimap_position = "bottomright", 
                     ...){
  
  d = data[, xy]
  
  # coerce to numeric
  d[1:2] = lapply(d[1:2], as.numeric)
  
  # remove NAs
  d = stats::na.omit(d)
  
  nd = dim(d)[[1]]
  
  if (isTRUE(nd == 0)) {
    stop("No coordinates to plot. ",
         "Please check for NAs or if the values can be coerced to numeric. \n")
  }
  
  names(d) = c("lon","lat")
  
  if (isTRUE(make.clusters)) {
    # to ensure the privacy of participants location
    # we can put the lonlat info into clusters of 0.5 resolution
    h = stats::dist(d)
    
    h = stats::hclust(h)
    
    h = stats::cutree(h, h = cut.tree)
    
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
    
  }
  
  
  map = leaflet::leaflet(data = d, 
                          options = leaflet::leafletOptions(maxZoom = 17))
  
  map = leaflet::fitBounds(map = map, 
                            lng1 = min(d$lon) - 0.25,
                            lat1 = min(d$lat) - 0.25,
                            lng2 = max(d$lon) + 0.25, 
                            lat2 = max(d$lat) + 0.25)
  
  map = leaflet::addProviderTiles(map = map, 
                                   provider =  map_provider, 
                                   options = leaflet::providerTileOptions(maxNativeZoom = 17))
  
  map = leaflet::addCircleMarkers(map, 
                                   color = "#b30000", 
                                   radius = 6,
                                   stroke = FALSE,
                                   fillOpacity = 0.5)
  
  if (isTRUE(minimap)) {
    
    map = leaflet::addMiniMap(map = map, 
                               position = minimap_position, 
                               width = 100,
                               height = 100)
    
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
#' R = matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 1, 3,
#'               1, 2, 3, 0,
#'               2, 1, 3, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' 
#' mod = PlackettLuce(R)
#' 
#' anova.PL(mod)
#' 
#' @noRd
anova.PL = function(model){
  if(class(model)!="PlackettLuce"){
    stop("Model type is not Plackett-Luce")
  }
  
  LLs = round(c(model$null.loglik, model$loglik), 3)
  dfs = c(model$df.null, model$df.residual)
  df_diff = (-1) * diff(dfs)
  df_LL = round((-1) * diff(LLs), 3)
  p = round(1 - pchisq(-2 * df_LL, df_diff), 4)
  stars = gtools::stars.pval(p)
  
  x = data.frame(Model = c("NULL", deparse(substitute(model))),
                  "logLik" = LLs,
                  DF = dfs,
                  "Statistic" = c(NA, -2 * df_LL),
                  "Pr(>Chisq)" = c(NA, p),
                  " " = c("", stars),
                  check.names = FALSE,
                  stringsAsFactors = FALSE)
  
  return(x)
  
}

# function from https://github.com/EllaKaye/BradleyTerryScalable
# which unfortunately was removed from CRAN
#' @rdname btdata
btdata = function(x, return_graph = FALSE) {
  
  # if x is a table, convert it to a matrix
  if (is.table(x)) {
    attr(x, "class") = NULL
    attr(x, "call") = NULL
  }
  
  # if x is a df
  if (is.data.frame(x)) {
    if (!(ncol(x) %in% 3:4 )) stop("If x is a dataframe, it must have 3 or 4 columns.")
    wins = pairs_to_matrix(x)
    g = igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  # if x is a graph
  else if (igraph::is.igraph(x)) {
    if(!igraph::is.directed(x))  stop("If x is a graph, it must be a directed igraph object")
    
    # check for names
    if(!is.null(igraph::V(x)$name)) {
      
      arg = deparse(substitute(x))
      
      if(anyDuplicated(igraph::V(x)$name) > 0) stop(paste0("If x is a graph, vertex names must be unique. Consider fixing with V(", arg, ")$name = make.names(V(", arg, ")$name, unique = TRUE)"))
    }
    
    wins = graph_to_matrix(x)
    g = x
  }
  
  else if ((methods::is(x, "Matrix") | is.matrix(x) )) {
    
    # check dimensions/content
    if (dim(x)[1] != dim(x)[2]) stop("If x is a matrix or table, it must be a square")
    if(is.matrix(x)) {if (!is.numeric(x)) stop("If x is a matrix or table, all elements must be numeric")}
    if(methods::is(x, "Matrix")) {if (!is.numeric(as.vector(x))) stop("If x is a matrix or table, all elements must be numeric")}
    if (any(x < 0)) stop("If x is a matrix or table, all elements must be non-negative")
    if(!identical(rownames(x), colnames(x))) stop("If x is a matrix or table, rownames and colnames of x should be the same")
    if (anyDuplicated(rownames(x)) > 0) {
      
      arg = deparse(substitute(x))
      stop("If x is a matrix or table with row- and column names, these must be unique. Consider fixing with rownames(", arg, ") = colnames(", arg, ") = make.names(rownames(", arg, "), unique = TRUE)")
    }
    
    # ensure wins is a dgCMatrix
    if (is.matrix(x)) wins = Matrix::Matrix(x, sparse = TRUE)
    else wins = x
    if (class(wins) != "dgCMatrix") wins = methods::as(wins, "dgCMatrix")
    g = igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  else stop("x must be a 3 or 4 column dataframe, a directed igraph object, or square matrix or contingency table.")
  
  
  ## get components
  comp = igraph::components(g, mode = "strong")
  components = igraph::groups(comp)
  
  # name the rows and columns of the wins matrix, if NULL
  if (is.null(unlist(dimnames(wins)))) {
    K = nrow(wins)
    dimnames(wins) = list(1:K, 1:K)
  }
  
  # return
  result = list(wins = wins, components = components)
  if (return_graph) result$graph = g
  class(result) = c("btdata", "list")
  result
}

summary.btdata = function(object, ...){
  if (!inherits(object, "btdata")) stop("object should be a 'btdata' object")
  K = nrow(object$wins)
  num_comps = length(object$components)
  connected = num_comps == 1
  components_greater_than_one = Filter(function(x) length(x) > 1, object$components)
  my_tab = table(sapply(object$components, length))
  my_df = as.data.frame(my_tab)
  
  colnames(my_df) = c("Component size", "Freq")
  
  density = Matrix::mean(object$wins != 0)
  
  cat("Number of items:", K, "\n")
  cat("Density of wins matrix:", density, "\n")
  cat("Fully-connected:", connected, "\n")
  
  
  if (num_comps > 1) {
    cat("Number of fully-connected components:", num_comps, "\n")
    cat("Summary of fully-connected components: \n")
    print(my_df)
  }
}


#' Visualise network
#' @param object an object of class rankings
#' @param ... additional arguments passed to igraph methods
#' @return an igraph plot
#' @examples 
#' library("PlackettLuce")
#' R = matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 1, 3,
#'               1, 2, 3, 0,
#'               2, 1, 3, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' 
#' R = as.rankings(R)
#' 
#' network(R)
#' @noRd
network = function(object, ...) {
  
  if (class(object) == "grouped_rankings") {
    object = as.rankings(object)
  }
  
  R = object 
  
  adj = PlackettLuce::adjacency(R)
  
  adj = as.vector(adj)
  
  adj = t(matrix(adj, nrow = ncol(R), ncol = ncol(R)))
  
  dimnames(adj) = list(dimnames(R)[[2]], dimnames(R)[[2]])
  
  adj = btdata(adj, return_graph = TRUE)
  
  netw = adj$graph
  
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
#' v = c("Pear", "Apple", "Pear", "Orange", "Apple", "Apple")
#' 
#' rename_duplicates(v)
#' @noRd
rename_duplicates = function(x, rename.with = "numbers", sep = "") {
  
  dups = duplicated(x)
  dups = unique(x[dups])
  
  for(i in seq_along(dups)) {
    
    dups_i = x == dups[i]
    
    index = seq_len(sum(dups_i))
    
    if (rename.with == "letters") {
      index = letters[index]
    }
    
    x[dups_i] = paste(x[dups_i],  index, sep = sep)
    
  }
  
  return(x)
  
}

#' Validate the class of objects generated in the tryCatch(s)
#' @noRd
any_error = function(x){
  isTRUE("error" %in% class(x))
}

#' Runs specific lines of the code
#' @param file the path of R script 
#' @param start an integer for the index of first line to read and run
#' @param end an integer fot the index of last line to read and run
#' @noRd
source2 = function(file, start, end, ...) {
  
  file.lines = scan(file, 
                     what = character(), 
                     skip = start-1, 
                     nlines = end-start+1, 
                     sep = '\n')
  
  file.lines.collapsed = paste(file.lines, collapse='\n')
  
  source(textConnection(file.lines.collapsed), ...)
  
}


#' Get table with estimates out of a PLADMM
#' @param object an object of class PLADMM
#' @examples 
#' library("PlackettLuce")
#' library("prefmod")
#' data(salad)
#' 
#' features = data.frame(salad = LETTERS[1:4],
#'                        acetic = c(0.5, 0.5, 1, 0),
#'                        gluconic = c(0, 10, 0, 10))
#' 
#' mod = pladmm(salad, ~ acetic + gluconic,
#'               data = features,
#'               rho = 8)
#' 
#' pladmm_coeffs(mod)
#' @noRd
pladmm_coeffs = function(object, ...) {
  coefs = coef(object)
  coefficients = matrix(NA, nrow = length(coefs), ncol = 4L, 
                         dimnames = list(names(coefs), c("Estimate", "Std. Error", 
                                                         "z value", "Pr(>|z|)")))
  coefficients[, 1L] = coefs
  se = sqrt(diag(vcov(object)))
  coefficients[names(se), 2L] = se
  coefficients[, 3L] = coefficients[, 1L]/coefficients[, 2L]
  coefficients[, 4L] = 2L * pnorm(-abs(coefficients[, 3L]))
  
  coefficients = as.data.frame(coefficients)
  
  coefficients[, 5] = stars.pval(coefficients[, 4])
  
  coefficients[, 4] = formatC(coefficients[, 4], format = "e", digits = 2)
  
  coefficients[, 6] = rownames(coefficients)
  
  rownames(coefficients) = 1:nrow(coefficients)
  
  coefficients = coefficients[,c(6, 1:5)]
  
  names(coefficients)[c(1, 6)] = ""
  
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
paste3 = function(x, lan = "en", ...) {
  
  x = x[!is.na(x)]
  
  if (length(x) == 1) {
    return(x)
  }
  
  idiom = c("en", "pt", "fr", "es", "sw", "no")
  conj = c(" and ", " e ", " et ", " y ", " na ", " og ")
  
  index = which(idiom %in% lan)
  
  if (length(index) == 0) {
    index = 1
  }
  
  conj = conj[index]
  
  x1 = x[1:length(x)-1]
  x2 = x[length(x)]
  
  x1 = paste(x1, collapse = ", ")
  
  result = paste(x1, conj, x2)
  
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
#' @examples
#' library("jsonlite")
#' x = jsonlite::fromJSON("tests/testdata1/data.json")
#' 
#' pars = decode_pars(x)
#' 
#' names(pars)
#' 
#' pars
#' @noRd
decode_pars = function(x) {
  
  traits = x[["Characteristics"]]
  tricotVSlocal  = x[["Performance"]]
  covariates  = x[["Explanatory"]]
  linear = x[["linearRegression"]]
  
  result = list()
  
  if (length(traits) > 0) {
    
    questions = lapply(traits$vars, function(y) {
      unlist(y)
    })
    
    questions = do.call(rbind, questions)
    questions = as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) = paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst = dim(questions)[[2]]
    
    questions$name = traits$name
    
    questions$codeQst = traits$codeQst
    
    
    questionAsked = do.call(rbind, traits$questionAsked)
    questionAsked = as.data.frame(questionAsked, stringsAsFactors = FALSE)
    names(questionAsked) = paste0("questionAsked", seq_len(questions$nQst[1]))
    
    questions = cbind(questions, questionAsked)
    
    if (!all(is.na(traits$code))) {
      
      questions$assessmentId = traits$code$ass_cod
      
      questions$assessmentName = traits$code$ass_desc
      
      questions$assessmentDay = traits$code$ass_days  
    }
    
    if (all(is.na(traits$code))) {
      
      questions$assessmentId = "0000000000"
      
      questions$assessmentName = "Data collection"
      
      questions$assessmentDay = 0
      
    }
    
    questions$traitOrder  = rep("otherTraits", length(questions$codeQst))
    
    # try to find something related to performance, than yield and lastly taste
    # this is going to be the reference trait for the main analysis in the report
    traits = questions$codeQst
    tr = tolower(traits)
    
    if (any(grepl("overallperf", tr))) {
      i = which(grepl("overallperf", tr))[1]
      questions$traitOrder[i] = "referenceTrait"
      tr = toupper(tr)
    }
    
    if (any(grepl("overall", tr))) {
      i = which(grepl("overall", tr))
      i = i[length(i)]
      questions$traitOrder[i] = "referenceTrait"
      tr = toupper(tr)
    }
    
    if (any(grepl("performance", tr))) {
      i = which(grepl("performance", tr))[1]
      questions$traitOrder[i] = "referenceTrait"
      tr = toupper(tr)
    }
    
    if (any(grepl("yield", tr))) {
      i = which(grepl("yield", tr))[1]
      questions$traitOrder[i] = "referenceTrait"
      tr = toupper(tr)
    }
    
    if (any(grepl("market", tr))) {
      i = which(grepl("market", tr))[1]
      questions$traitOrder[i] = "referenceTrait"
      tr = toupper(tr)
    }
    
    if (sum(grepl("performance|yield|overall|overallperf|market", tolower(tr))) == 0) {
      questions$traitOrder[length(questions$codeQst)] = "referenceTrait"
      tr = toupper(tr)
    }
    
    rownames(questions) = 1:nrow(questions)
    
    questions$assessmentName = gsub("[[:punct:]]", "", questions$assessmentName)
    
    result[["traits"]] = questions
    
  }else{
    
    result[["traits"]] = character(0L)
    
  }
  
  if (length(tricotVSlocal) > 0) {
    questions = lapply(tricotVSlocal$vars, function(y) {
      unlist(y)
    })
    
    questions = do.call(rbind, questions)
    questions = as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) = paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst = dim(questions)[[2]]
    
    questions$name = tricotVSlocal$name
    
    questions$codeQst = tricotVSlocal$codeQst
    
    result[["tricotVSlocal"]] = questions
    
  }else{
    
    result[["tricotVSlocal"]] = character(0L)
    
  }
  
  if (length(covariates) > 0) {
    
    covar               = covariates[,c("codeQst", "id")]
    covar$nameString    = covariates$vars
    covar$name          = covariates$name 
    covar$questionAsked = covariates$questionAsked
    
    if (all(is.na(covariates$code))) {
      
      covar$assessmentId = "000000000000"
      covar$assessmentName = "Registration"
      covariates = covar
      
    }else{
      
      covariates = covariates$code[,c("ass_cod","ass_desc")]
      names(covariates) = c("assessmentId", "assessmentName")
      covariates$assessmentId[is.na(covariates$assessmentId)] = "000000000000"
      covariates$assessmentName[is.na(covariates$assessmentName)] = "Registration"
      covariates = cbind(covariates, covar)
      
    }
    
    covariates$assessmentName = gsub("[[:punct:]]", "", covariates$assessmentName)
    
    result[["covariates"]] = covariates
    
  }else{
    
    result[["covariates"]] = character(0L)
    
  }
  
  if (length(linear) > 0) {
    
    questions = lapply(linear$vars, function(y) {
      unlist(y)
    })
    
    questions = do.call(rbind, questions)
    questions = as.data.frame(questions, stringsAsFactors = FALSE)
    names(questions) = paste0("nameString", seq_len(dim(questions)[[2]]))
    
    questions$nQst = dim(questions)[[2]]
    
    questions$name = linear$name
    
    questions$codeQst = linear$codeQst
    
    
    questionAsked = do.call(rbind, linear$questionAsked)
    questionAsked = as.data.frame(questionAsked, stringsAsFactors = FALSE)
    names(questionAsked) = paste0("questionAsked", seq_len(questions$nQst[1]))
    
    questions = cbind(questions, questionAsked)
    
    if (!all(is.na(linear$code))) {
      
      questions$assessmentId = linear$code$ass_cod
      
      questions$assessmentName = linear$code$ass_desc
      
      questions$assessmentDay = linear$code$ass_days  
    }
    
    if (all(is.na(linear$code))) {
      
      questions$assessmentId = "0000000000"
      
      questions$assessmentName = "Data collection"
      
      questions$assessmentDay = 0
      
    }
    
    questions$assessmentName = gsub("[[:punct:]]", "", questions$assessmentName)
    
    rownames(questions) = 1:nrow(questions)
    
    result[["linear"]] = questions
    
  }else{
    
    result[["linear"]] = character(0L)
    
  }
  
  return(result)
  
}

#' @rdname multcompPL
#' @export
multcompPL = function(mod, items = NULL, threshold = 0.05, adjust = "none", ...){
  
  #get estimates with quasi-SEs
  qv1 = qvcalc::qvcalc(mod, ...)$qvframe
  
  #reduce frame to only selected items if not all comparisons are desired
  if (!is.null(items)) {
    qv1 = subset(qv1, rownames(qv1) %in% items)
    # give error if less than 2 items can be identified
    if (nrow(qv1) < 3) {
      stop("Less than 2 items selected")
    }
  }
  
  #set up matrices for all differences and pooled errors
  diffs = mat.or.vec(nrow(qv1),nrow(qv1))
  ses = mat.or.vec(nrow(qv1),nrow(qv1))
  
  for(i in 1:nrow(qv1)){
    for(j in 1:nrow(qv1)){
      #get differences and pooled ses
      diffs[i,j] = qv1$estimate[i] - qv1$estimate[j]
      ses[i,j] = sqrt(qv1$quasiVar[i] + qv1$quasiVar[j])
    }
  }
  
  #calculate z scores
  z = diffs/ses
  #TO DO: What DF to use to use here? Is it just the resid DF?
  p = 2 * (1 - stats::pt(abs(z), mod$df.residual))
  
  #adjust p-value if you want to adjust. make sure to only take each p once for adjustment
  p[upper.tri(p)] = stats::p.adjust(p[upper.tri(p)], method = adjust)
  
  #make sure lower triangular is mirror of upper
  p[lower.tri(p)] = t(p)[lower.tri(p)]
  
  #set rownames
  rownames(p) = colnames(p) = rownames(qv1)
  
  #re-order qv output to ensure letters are produced in a sensible order
  qv1$items = stats::reorder(factor(rownames(qv1)), qv1$estimate, mean)
  qv1 = qv1[order(qv1$estimate, decreasing = TRUE), ]
  
  #get mean seperation letter groupings
  args = list(formula = estimate ~ items, 
               x = p, 
               data = qv1,
               compare = "<",
               threshold =  threshold,
               Letters = letters,
               reversed = FALSE)
  
  let = do.call("multcompLetters2", args)
  
  qv1$group = let$Letters
  
  qv1 = qv1[, union("items", names(qv1))]
  
  row.names(qv1) = seq_along(qv1$group)
  
  class(qv1) = union("multcompPL", class(qv1))
  
  return(qv1)
  
}

#' Plot log-worth
#' @param x a multicomp dataframe
#' @param ci.level the confidence interval level
#' @param multcomp logical to add group letters 
#' @param levels an optional vector with factor levels to plot
plot_logworth = function(x, ci.level = 0.95, ref = NULL, multcomp = TRUE, levels = NULL, ...) {
  
  frame = data.frame()

  for (i in seq_along(ref)) {
    fi = qvcalc(x, ref = ref[i], ...)$qvframe
    fi$ref = ref[i]
    fi$items = rownames(fi)
    frame = rbind(frame, fi)
  }
  
  if (is.null(levels)) {
    levels = unique(frame$items)
  }
  
  items = factor(frame$items, levels = levels)
  
  est = frame$estimate
  
  se = frame$quasiSE
  
  tops = est + stats::qnorm(1-(1 - ci.level) / 2) * se
  
  tails = est - stats::qnorm(1-(1 - ci.level) / 2) * se
  
  range = max(tops) - min(tails)
  
  pdat = data.frame(est, se, items, 
                    ref = frame$ref, 
                    tops, tails)
  
  if (isTRUE(multcomp)) {
    lettersdat = multcompPL(mod = x, ...)
    lettersdat = lettersdat[, c("items", "group")]
    pdat = merge(pdat, lettersdat, by = "items")
  } 
  
  if (!isTRUE(multcomp)) {
    pdat$group = ""
  }
  
  pdat$items = factor(pdat$items, levels = levels)
  
  p = ggplot(data = pdat,
              aes(x = items, 
                  y = est,
                  ymax = tops,
                  ymin = tails, 
                  label = group)) +
    geom_hline(yintercept = 0, 
               colour = "#E5E7E9", size = 0.8) +
    geom_point() +
    geom_errorbar(width = 0.1) +
    geom_text(vjust = 1.2, hjust = 1.2) +
    theme_bw() +
    facet_wrap(~ ref, strip.position = "bottom") +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,
                                     size = 10, color = "grey20"),
          axis.text.y = element_text(size = 10, color = "grey20"),
          text = element_text(color = "grey20"),
          legend.position = "bottom",
          legend.title = element_blank(),
          strip.background.x = element_blank(),
          strip.placement = "outside") +
    labs(x = "", y = "Log-worth")
  
  p
  
  return(p)
  
}

#' Pluralize
#' @param x a character
pluralize = function(x, p = "s") {
  
  pl = matrix(c("variety","varieties",
                 "variedad","variedades",
                 "opcion", "opciones",
                 "technology", "technologies"), 
               nrow = 4, ncol = 2, byrow = TRUE)
  
  is_here = x %in% pl[,1]
  if (isTRUE(is_here)) {
    x = pl[pl[,1] %in% x, 2]
  }
  
  if (isFALSE(is_here)) {
    x = paste0(x, p)
  }
  
  return(x)
  
}


#' Put sentences in title case
#' @param x a character
#' @examples 
#' title_case("the apple tree")
title_case = function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
       x, 
       perl = TRUE)
}


#' Abbreviate long sentences 
#' @param x a vector with characters
reduce = function(x, nchars = 8, ...){
  
  reduc = nchar(x) > nchars
  
  abb = abbreviate(x[reduc], ...)
  
  x[reduc] = abb
  
  return(x)
}

#' Forward selection with PL trees
#' @param data data.frame with ranking data and covariates
#' @param ... additional arguments passed to methods 
forward_selection = function(data, ...) {
  
  Gdata = data
  var_keep = character(0L)
  best = TRUE
  counter = 1
  exp_var = names(Gdata)[-1]
  
  cat("Selecting the best covariate for Plackett-Luce trees \n")
  
  while (best) {
    
    fs = length(exp_var)
    models = data.frame()
    
    for(i in seq_len(fs)){
      
      t_i = try(pltree(as.formula(paste0("G ~ ", paste(c(var_keep, exp_var[i]), collapse = " + "))),
                       data = Gdata,
                       ...), silent = TRUE)
      
      if (isFALSE("try-error" %in% class(t_i))) {
        validations = data.frame(nnodes = length(nodeids(t_i, terminal = TRUE)),
                                 AIC = AIC(t_i),
                                 noerror = TRUE)
      }else{
        validations = data.frame(nnodes = NA,
                                 AIC = NA,
                                 noerror = FALSE)
      }
      
      models = rbind(models, validations)
      
    }
    
    counter = counter + 1
    
    if (length(exp_var) == 0) {
      best = FALSE
    }
    
    if (best) {
      # update vector with covariates to keep only those with no error
      # and those with no split
      exp_var = exp_var[models$noerror & models$nnodes > 1]
      # also take out from the models data frame
      models = models[models$noerror == TRUE & models$nnodes > 1, ]
      # find the index for the best model, the one with lowest AIC
      index_bext = which.min(models$AIC)
      # and the best model
      best_model = exp_var[index_bext]
      exp_var = exp_var[-index_bext]
      var_keep = c(var_keep, best_model)
    }
    
    if (length(exp_var) == 0) {
      best = FALSE
    }
    
  }
  
  if (length(var_keep) > 0) {
    treeformula = paste0("G ~ ", paste(c(var_keep), collapse = " + "))
  }
  
  if (length(var_keep) == 0) {
    treeformula = "G ~ 1"
  }
  
  return(treeformula)
  
}




