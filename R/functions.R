###Functions for Climmob Reporting Analysis


#' Validate the class of objects generated in the tryCatch(s)
any_error <- function(x){
  isTRUE("error" %in% class(x))
}

#' Runs specific lines of the code
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
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
  
  map <- leaflet::fitBounds(map = map, lng1 = min(d$lon)-0.25, lat1 = min(d$lat)-0.25,
                            lng2 = max(d$lon)+0.25, lat2 = max(d$lat)+0.25)
  
  map <- leaflet::addProviderTiles(map = map, 
                                   provider =  map_provider, 
                                   options = leaflet::providerTileOptions(maxNativeZoom = 17))
  
  #map <- leaflet::addCircleMarkers(map = map)
  
  map <- leaflet::addMarkers(map)
  
  if (isTRUE(minimap)) {
    
    map <- leaflet::addMiniMap(map = map, position = minimap_position, 
                               width = 100, height = 100)
    
  }
  
  map$x$options = list("zoomControl" = FALSE)
  
  return(map)
  
}

scale01 <- function(x) (x-min(x))/(max(x)-min(x))

byfac<-function(model,split){
  
  split<-as.factor(split)
  out<-NULL
  for(i in 1:nlevels(split)){
    mod_t<-update(mod1,rankings=R[split==levels(split)[i],])
    
    tmp<-data.frame(var=rownames(qvcalc(mod_t)$qvframe),split=levels(split)[i],qvcalc(mod_t)$qvframe)
    tmp$estimate_adj<-tmp$estimate-mean(tmp$estimate)
    
    
    out<-rbind(out,tmp)
  }
  out
  
  ggplot(data=out,aes(y=estimate_adj,x=var,ymax=estimate_adj+qnorm(0.92)*quasiSE,
                      ymin=estimate_adj-qnorm(0.92)*quasiSE,col=split))+
    geom_errorbar(width=0.2,position = position_dodge(width=0.25))+
    geom_point(position = position_dodge(width=0.25))
}


win_plot<-function(x){
  p1<- ggplot(data=x,aes(y=wins,fill=wins,x=var))+
    geom_bar(stat="identity",col="black")+
    coord_flip()+
    scale_y_continuous(breaks=seq(0,1,by=0.1),labels=scales::percent)+
    scale_fill_gradient2(low="red",mid="white",high="forestgreen",limits=c(0,1),midpoint=0.5)
  
  return(p1)
}

anova.PL <- function(model){
  if(class(model)!="PlackettLuce"){
    stop("Model type is not Plackett-Luce")
  }
  LLs <- c(model$null.loglik, model$loglik)
  dfs <- c(model$df.null, model$df.residual)
  df_diff <- (-1) * diff(dfs)
  df_LL <- (-1) * diff(LLs)
  p <- 1 - pchisq(-2 * df_LL, df_diff)
  
  
  x <- data.frame(model = c("NULL", deparse(substitute(model))),
                  "logLikelihood" = LLs,
                  DF=dfs,
                  "Statistic" = c(NA, -2 * df_LL),
                  "Pr(>Chisq)" = c(NA, p),
                  check.names = FALSE,
                  stringsAsFactors = FALSE)
  return(x)
}


node_terminal1<-
  function (mobobj, id = TRUE, worth = TRUE, names = TRUE, abbreviate = TRUE, 
            index = TRUE, ref = TRUE, col = "black", refcol = "lightgray", 
            bg = "white", cex = 0.5, pch = 19, xscale = NULL, yscale = NULL, 
            ylines = 1.5) 
  {
    node <- nodeids(mobobj, terminal = FALSE)
    cf <- psychotree:::apply_to_models(mobobj, node, FUN = function(z) if (worth) 
      worth(z)
      else coef(z, all = FALSE, ref = TRUE))
    
    
    cf <- do.call("rbind", cf)
    rownames(cf) <- node
    cf<-cf[,order(colSums(cf))]
    
    mod <- psychotree:::apply_to_models(mobobj, node = 1L, FUN = NULL, drop = TRUE)
    if (!worth) {
      if (is.character(ref) | is.numeric(ref)) {
        reflab <- ref
        ref <- TRUE
      }
      else {
        reflab <- mod$ref
      }
      if (is.character(reflab)) 
        reflab <- match(reflab, if (!is.null(mod$labels)) 
          mod$labels
          else colnames(cf))
      cf <- cf - cf[, reflab]
    }
    if (worth) {
      cf_ref <- 1/ncol(cf)
    }
    else {
      cf_ref <- 0
    }
    if (is.character(names)) {
      colnames(cf) <- names
      
      names <- TRUE
    }
    if (is.logical(abbreviate)) {
      nlab <- max(nchar(colnames(cf)))
      abbreviate <- if (abbreviate) 
        as.numeric(cut(nlab, c(-Inf, 1.5, 4.5, 7.5, Inf)))
      else nlab
    }
    colnames(cf) <- abbreviate(colnames(cf), abbreviate)
    if (index) {
      x <- 1:NCOL(cf)
      if (is.null(xscale)) 
        xscale <- range(x) + c(-0.1, 0.1) * diff(range(x))
    }
    else {
      x <- rep(0, length(cf))
      if (is.null(xscale)) 
        xscale <- c(-1, 1)
    }
    if (is.null(yscale)) 
      yscale <- range(cf) + c(-0.1, 0.1) * diff(range(cf))
    rval <- function(node) {
      idn <- id_node(node)
      cfi <- cf[idn, ]
      top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3, 
                                              widths = unit(c(ylines, 1, 1), c("lines", "null", 
                                                                               "lines")), heights = unit(c(1, 1), c("lines", 
                                                                                                                    "null"))), width = unit(1, "npc"), 
                         height = unit(1, "npc") - unit(2, "lines"), 
                         name = paste("node_btplot", idn, sep = ""))
      pushViewport(top_vp)
      grid.rect(gp = gpar(fill = bg, col = 0))
      top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
      pushViewport(top)
      mainlab <- paste(ifelse(id, paste("Node", idn, 
                                        "(n = "), ""), info_node(node)$nobs, 
                       ifelse(id, ")", ""), sep = "")
      grid.text(mainlab)
      popViewport()
      plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = 2, 
                           xscale = xscale, yscale = yscale, name = paste("node_btplot", 
                                                                          idn, "plot", sep = ""))
      pushViewport(plot_vpi)
      grid.lines(xscale, c(cf_ref, cf_ref), gp = gpar(col = refcol), 
                 default.units = "native")
      if (index) {
        grid.lines(x, cfi, gp = gpar(col = col, lty = 2), 
                   default.units = "native")
        grid.points(x, cfi, gp = gpar(col = col, cex = cex), 
                    pch = pch, default.units = "native")
        grid.xaxis(at = x,edits = gEdit(gPath="labels", rot=90,cex=0.4),  label = if (names) 
          names(cfi)
          else x)
      }
      else {
        if (names) 
          grid.text(names(cfi), x = x, y = cfi, default.units = "native")
        else grid.points(x, cfi, gp = gpar(col = col, cex = cex), 
                         pch = pch, default.units = "native")
      }
      grid.yaxis(at = c(ceiling(yscale[1] * 100)/100, floor(yscale[2] * 
                                                              100)/100))
      grid.rect(gp = gpar(fill = "transparent"))
      upViewport(2)
    }
    return(rval)
    
  }

draw.emojis <- function(x,y,type="happy",radius=0.3, color="grey", border="black", thickness=1.5){
  draw.circle(x,y,radius,nv=100,border=color,col=color,lty=1,density=NULL,angle=45,lwd=thickness/1.5)
  segments(x0=x+radius/5, x1=x+radius/5, y0=y+radius/2.5, y1=y+radius/5, lwd = thickness*1.5, col=border)
  segments(x0=x-radius/5, x1=x-radius/5, y0=y+radius/2.5, y1=y+radius/5, lwd = thickness*1.5, col=border)
  if(type=="happy") draw.arc(x,y,radius=radius/2, deg1=200, deg2=340, col=border, lwd=thickness/1.2)
  if(type=="sad") draw.arc(x,y-radius/1.5,radius=radius/2, deg1=20, deg2=160, col=border, lwd=thickness/1.2)
  if(type=="neutral") segments(x0=x-radius/4, x1=x+radius/4, y0=y-radius/3, y1=y-radius/3, lwd = thickness, col=border)
}
draw.emojis <- Vectorize(draw.emojis)

#' Visualise network
#' @param object an object of class rankings
#' @param ... additional arguments passed to igraph methods
#' @return an igraph plot
network <- function(object, ...) {
  
  R <- object 
  
  adj <- adjacency(R)
  
  adj <- as.vector(adj)
  
  adj <- t(matrix(adj, nrow = ncol(R), ncol = ncol(R)))
  
  dimnames(adj) <- list(dimnames(R)[[2]], dimnames(R)[[2]])
  
  adj <- btdata(adj, return_graph = TRUE)
  
  netw <- adj$graph
  
}


#' Coearce rankings and explatory variables in a readable file for Cortana
#' @param x a rankings object
#' @param y a data.frame with explanatory variables
#' @return a 'cortana' object
ranking4cortana <- function(x, y) {
  L <- c(letters, LETTERS)
  
  
  ranking <- apply(y, 1, function(w)
  {
    if (length(unique(w)) == length(w))
    {
      prefString <- paste(L[order(w)], collapse = ">")
    } else {
      prefString <- NULL
      nbr <- Inf
      sapply(order(w), function(i)
      {
        #if () {
        if(w[i]>nbr & !is.na(w[i])){
          prefString <<- paste(prefString, ">", L[i], sep="") 
        } else {
          prefString <<- paste(prefString, L[i], sep="")
        }
        #}
        
        nbr <<- w[i]
      })
    }
    prefString
  })
  
  X <- cbind(x,ranking)
  
  return(X)
}


# function from https://github.com/EllaKaye/BradleyTerryScalable
# which unfortunately was removed from CRAN
# Converts a graph representation of wins into a square matrix.
graph_to_matrix <- function(g) {
  
  # check that graph is a directed igraph object
  if(!igraph::is.igraph(g))  stop("g must be a directed igraph object")
  if(!igraph::is.directed(g))  stop("g must be a directed igraph object")
  
  # check names
  if(!is.null(igraph::V(g)$name)) {
    
    arg <- deparse(substitute(g))
    
    if(anyDuplicated(igraph::V(g)$name) > 0) stop(paste0("Vertex names must be unique. Consider fixing with V(", arg, ")$name <- make.names(V(", arg, ")$name, unique = TRUE)"))
  }
  
  if (igraph::is.weighted(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight", names = TRUE)
  else W <- igraph::as_adjacency_matrix(g, sparse = TRUE, names = TRUE)
  
  return(W)
  
}

# function from https://github.com/EllaKaye/BradleyTerryScalable
# which unfortunately was removed from CRAN
# Converts a data frame of paired results into a square matrix.
pairs_to_matrix <- function(df) {
  # Check for Matrix.utils
  if (!requireNamespace("Matrix.utils", quietly = TRUE)) {
    stop("The package Matrix.utils is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Check for stringr
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The package stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # check if data frame
  if(!(is.data.frame(df))) stop ("Argument must be a data frame")
  
  # ensure df is a data.frame (rather than tbl_df or tbl)
  class(df) <- "data.frame"
  
  # check number of columns
  if (!(ncol(df) %in% 3:4 )) stop("Argument must be a data frame with three or four columns")
  
  # get base data
  items <- sort(base::union(df[[1]], df[[2]]))
  n <- length(items)
  
  # get formula for dMcast
  f <- stats::as.formula(paste(names(df)[1:2], collapse= " ~ "))
  
  # convert names to factors
  if(!is.factor(df[,1])) {
    df[,1] <- factor(df[,1])
  }
  
  if(!is.factor(df[,2])) {
    df[,2] <- factor(df[,2])
  }
  
  # create empty mat if all zeros in column 3
  if(all(df[,3] == 0)) {
    mat <- Matrix::Matrix(0, n, n, sparse = TRUE)
  }
  
  # create matrix with wins from column 3
  else {
    # create cross-tabs matrix (not square)
    mat <- Matrix.utils::dMcast(df, f, value.var = names(df)[3], as.factors = TRUE)
    
    # fix colnames
    colnames(mat) <- stringr::str_replace(colnames(mat), names(df)[2], "")
    
    # remove zeros, if any, taking care with dimnames
    summary_mat <- Matrix::summary(mat)
    x <- NULL # hack to avoid CRAN note
    if (any(summary_mat[,3] == 0)) {
      summary_mat <- dplyr::filter(summary_mat, x != 0)
      
      mat_rownames <- rownames(mat)
      mat_colnames <- colnames(mat)
      
      new_mat_rownames <- mat_rownames[sort(unique(summary_mat[,1]))]
      new_mat_colnames <- mat_colnames[sort(unique(summary_mat[,2]))]
      
      mat <- Matrix::sparseMatrix(i = summary_mat[,1], j = summary_mat[,2], x = summary_mat[,3])
      
      nonzero_rows <- which(Matrix::rowSums(mat) != 0)
      nonzero_cols <- which(Matrix::colSums(mat) != 0)
      
      mat <- mat[nonzero_rows, nonzero_cols, drop = FALSE]
      dimnames(mat) <- list(new_mat_rownames, new_mat_colnames)
    }
    
    
    # add in zeros for missing rows
    if (nrow(mat) < n) {
      new_rows <- Matrix::Matrix(0, n - nrow(mat), ncol(mat),
                                 dimnames = list(base::setdiff(items, rownames(mat)), colnames(mat)))
      mat <- rbind(mat, new_rows)
    }
    
    # add in zeros for missing columns
    if (ncol(mat) < n) {
      new_cols <- Matrix::Matrix(0, n, n - ncol(mat),
                                 dimnames = list(rownames(mat), base::setdiff(items, colnames(mat))))
      mat <- cbind(mat, new_cols)
    }
    
    # get rows and columns in same, sorted order and return
    mat <- mat[items,]
    mat <- mat[, rownames(mat)]
  }
  
  # repeat above steps if in 4-column format (for item2 beating item1)
  # as long as col 4 isn't all zeros
  if (ncol(df) == 4) fourth_all_zero <- all(df[,4] == 0)
  else fourth_all_zero <- TRUE
  
  if (ncol(df) == 4 & !fourth_all_zero) {
    f2 <- stats::as.formula(paste(names(df)[2:1], collapse= " ~ "))
    mat2 <- Matrix.utils::dMcast(df, f2, value.var = names(df)[4], as.factors = TRUE)
    colnames(mat2) <- stringr::str_replace(colnames(mat2), names(df)[1], "")
    
    
    # remove zeros, if any, taking care with dimnames
    summary_mat2 <- Matrix::summary(mat2)
    if (any(summary_mat2[,3] == 0)) {
      summary_mat2 <- dplyr::filter(summary_mat2, x != 0)
      
      mat2_rownames <- rownames(mat2)
      mat2_colnames <- colnames(mat2)
      
      new_mat2_rownames <- mat2_rownames[sort(unique(summary_mat2[,1]))]
      new_mat2_colnames <- mat2_colnames[sort(unique(summary_mat2[,2]))]
      
      mat2 <- Matrix::sparseMatrix(i = summary_mat2[,1], j = summary_mat2[,2], x = summary_mat2[,3])
      
      nonzero_rows2 <- which(Matrix::rowSums(mat2) != 0)
      nonzero_cols2 <- which(Matrix::colSums(mat2) != 0)
      
      mat2 <- mat2[nonzero_rows2, nonzero_cols2, drop = FALSE]
      dimnames(mat2) <- list(new_mat2_rownames, new_mat2_colnames)
    }
    
    # add in zeros for missing rows
    if (nrow(mat2) < n) {
      new_rows2 <- Matrix::Matrix(0, n - nrow(mat2), ncol(mat2),
                                  dimnames = list(base::setdiff(items, rownames(mat2)), colnames(mat2)))
      mat2 <- rbind(mat2, new_rows2)
    }
    
    # add in zeros for missing columns
    if (ncol(mat2) < n) {
      new_cols2 <- Matrix::Matrix(0, n, n - ncol(mat2),
                                  dimnames = list(rownames(mat2), base::setdiff(items, colnames(mat2))))
      mat2 <- cbind(mat2, new_cols2)
    }
    
    # get rows and columns in same, sorted order and return
    mat2 <- mat2[items,]
    mat2 <- mat2[, rownames(mat2)]
    
    # add the result to mat
    mat <- mat + mat2
  }
  
  if(!is.null(colnames(df)[1]) & !is.null(colnames(df)[2])) names(dimnames(mat)) <- colnames(df)[1:2]
  
  return(mat)
}


# function from https://github.com/EllaKaye/BradleyTerryScalable
# which unfortunately was removed from CRAN
#' Create a btdata object
#'
#' Creates a btdata object, primarily for use in the \link{btfit} function.
#'
#' The \code{x} argument to \code{btdata} can be one of four types:
#'
#' \itemize{
#'
#' \item{A matrix (either a base \code{matrix}) or a class from the \code{Matrix} package), dimension \eqn{K} by \eqn{K}, where \eqn{K} is the number of items. The \emph{i,j}-th element is \eqn{w_{ij}}, the number of times item \eqn{i} has beaten item \eqn{j}. Ties can be accounted for by assigning half a win (i.e. 0.5) to each item.}
#' \item{A contingency table of class \code{table}, similar to the matrix described in the above point.}
#' \item{An \code{igraph}, representing the \emph{comparison graph}, with the \eqn{K} items as nodes. For the edges:
#' \itemize{
#' \item{If the graph is unweighted, a directed edge from node \eqn{i} to node \eqn{j} for every time item \eqn{i} has beaten item \eqn{j}}
#' \item{If the graph is weighted, then one edge from node \eqn{i} to node \eqn{j} if item \eqn{i} has beaten item \eqn{j} at least once, with the weight attribute of that edge set to the number of times \eqn{i} has beaten \eqn{j}.}
#' }}
#' \item{
#' If \code{x} is a data frame, it must have three or four columns:
#' \itemize{
#' \item{3-column data frame}{The first column contains the name of the winning item, the second column contains the name of the losing item and the third columns contains the number of times that the winner has beaten the loser. Multiple entries for the same pair of items are handled correctly. If \code{x} is a three-column dataframe, but the third column gives a code for who won, rather than a count, see \code{\link{codes_to_counts}}.}
#' \item{4-column data frame}{The first column contains the name of item 1, the second column contains the name of item 2, the third column contains the number of times that item 1 has beaten item 2 and the fourth column contains the number of times item 2 has beaten item 1. Multiple entries for the same pair of items are handled correctly. This kind of data frame is also the output of \code{\link{codes_to_counts}}.}
#' \item{In either of these cases, the data can be aggregated, or there can be one row per comparison.}
#' \item{Ties can be accounted for by assigning half a win (i.e. 0.5) to each item.}
#' }
#' }
#'
#' }
#'
#' \code{summary.btdata} shows the number of items, the density of the \code{wins} matrix and whether the underlying comparison graph is fully connected. If it is not fully connected, \code{summary.btdata} will additional show the number of fully-connected components and a table giving the frequency of components of different sizes. For more details on the comparison graph, and how its structure affects how the Bradley-Terry model is fitted, see \code{\link{btfit}} and the vignette: \url{https://ellakaye.github.io/BradleyTerryScalable/articles/BradleyTerryScalable.html}.
#'
#' @param x The data, which is either a three- or four-column data frame, a directed igraph object, a square matrix or a square contingency table. See Details.
#' @param return_graph Logical. If TRUE, an igraph object representing the comparison graph will be returned.
#' @return An object of class "btdata", which is a list containing:
#' \item{wins}{A \eqn{K} by \eqn{K} square matrix, where \eqn{K} is the total number of players. The \eqn{i,j}-th element is \eqn{w_{ij}}, the number of times item \eqn{i} has beaten item \eqn{j}. If the items in \code{x} are unnamed, the wins matrix will be assigned row and column names 1:K.}
#' \item{components}{A list of the fully-connected components.}
#' \item{graph}{The comparison graph of the data (if return_graph = TRUE). See Details.}
#' @seealso \code{\link{codes_to_counts}} \code{\link{select_components}}
#' @author Ella Kaye
#' @examples
#' citations_btdata <- btdata(BradleyTerryScalable::citations)
#' summary(citations_btdata)
#' toy_df_4col <- codes_to_counts(BradleyTerryScalable::toy_data, c("W1", "W2", "D"))
#' toy_btdata <- btdata(toy_df_4col)
#' summary(toy_btdata)
#' @export
btdata <- function(x, return_graph = FALSE) {
  
  # if x is a table, convert it to a matrix
  if (is.table(x)) {
    attr(x, "class") <- NULL
    attr(x, "call") <- NULL
  }
  
  # if x is a df
  if (is.data.frame(x)) {
    if (!(ncol(x) %in% 3:4 )) stop("If x is a dataframe, it must have 3 or 4 columns.")
    wins <- pairs_to_matrix(x)
    g <- igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  # if x is a graph
  else if (igraph::is.igraph(x)) {
    if(!igraph::is.directed(x))  stop("If x is a graph, it must be a directed igraph object")
    
    # check for names
    if(!is.null(igraph::V(x)$name)) {
      
      arg <- deparse(substitute(x))
      
      if(anyDuplicated(igraph::V(x)$name) > 0) stop(paste0("If x is a graph, vertex names must be unique. Consider fixing with V(", arg, ")$name <- make.names(V(", arg, ")$name, unique = TRUE)"))
    }
    
    wins <- graph_to_matrix(x)
    g <- x
  }
  
  else if ((methods::is(x, "Matrix") | is.matrix(x) )) {
    
    # check dimensions/content
    if (dim(x)[1] != dim(x)[2]) stop("If x is a matrix or table, it must be a square")
    if(is.matrix(x)) {if (!is.numeric(x)) stop("If x is a matrix or table, all elements must be numeric")}
    if(methods::is(x, "Matrix")) {if (!is.numeric(as.vector(x))) stop("If x is a matrix or table, all elements must be numeric")}
    if (any(x < 0)) stop("If x is a matrix or table, all elements must be non-negative")
    if(!identical(rownames(x), colnames(x))) stop("If x is a matrix or table, rownames and colnames of x should be the same")
    if (anyDuplicated(rownames(x)) > 0) {
      
      arg <- deparse(substitute(x))
      stop("If x is a matrix or table with row- and column names, these must be unique. Consider fixing with rownames(", arg, ") <- colnames(", arg, ") <- make.names(rownames(", arg, "), unique = TRUE)")
    }
    
    # ensure wins is a dgCMatrix
    if (is.matrix(x)) wins <- Matrix::Matrix(x, sparse = TRUE)
    else wins <- x
    if (class(wins) != "dgCMatrix") wins <- methods::as(wins, "dgCMatrix")
    g <- igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  else stop("x must be a 3 or 4 column dataframe, a directed igraph object, or square matrix or contingency table.")
  
  
  ## get components
  comp <- igraph::components(g, mode = "strong")
  components <- igraph::groups(comp)
  
  # name the rows and columns of the wins matrix, if NULL
  if (is.null(unlist(dimnames(wins)))) {
    K <- nrow(wins)
    dimnames(wins) <- list(1:K, 1:K)
  }
  
  # return
  result <- list(wins = wins, components = components)
  if (return_graph) result$graph <- g
  class(result) <- c("btdata", "list")
  result
}

# function from https://github.com/EllaKaye/BradleyTerryScalable
# which unfortunately was removed from CRAN
#' @rdname btdata
#' @param object An object of class "btdata", typically the result \code{ob} of \code{ob <- btdata(..)}.
#' @param ... Other arguments
#' @export
summary.btdata <- function(object, ...){
  if (!inherits(object, "btdata")) stop("object should be a 'btdata' object")
  K <- nrow(object$wins)
  num_comps <- length(object$components)
  connected <- num_comps == 1
  components_greater_than_one <- Filter(function(x) length(x) > 1, object$components)
  my_tab <- table(sapply(object$components, length))
  my_df <- as.data.frame(my_tab)
  
  colnames(my_df) <- c("Component size", "Freq")
  
  density <- Matrix::mean(object$wins != 0)
  
  cat("Number of items:", K, "\n")
  cat("Density of wins matrix:", density, "\n")
  cat("Fully-connected:", connected, "\n")
  
  
  if (num_comps > 1) {
    cat("Number of fully-connected components:", num_comps, "\n")
    cat("Summary of fully-connected components: \n")
    print(my_df)
  }
}

# Plot worth bar
# @param object a data.frame with worth parameters
# @param value an integer for index in object for the column with values to plot
# @param group an integer for index in object to the colunm with values to group with
plot_worth_bar <- function(object, value, group, palette = NULL){
  
  if(is.null(palette)) {
    palette <- grDevices::colorRampPalette(c("#FFFF80", "#38E009","#1A93AB", "#0C1078"))
  }
  
  object <- object[,c(group, value)]
  names(object) <- c("group", "value")
  
  nr <- dim(object)[[1]]
  
  object$group <- as.character(object$group)
  
  object$group <- gosset:::.reduce(object$group)
  
  object <- object[rev(order(object$value)), ] 
  
  object$value <- round(object$value * 100, 0)
  
  # get order of players based on their performance
  player_levels <- rev(gosset:::.player_order(object, "group", "value"))
  
  object$group <- factor(object$group, levels = player_levels)
  
  value <- object$value
  group <- object$group
  
  maxv <- round(max(value) + 10, -1)
  
  ggplot2::ggplot(data = object, 
                  ggplot2::aes(x = value,
                               y = "", 
                               fill = group)) +
    ggplot2::geom_bar(stat = "identity", 
                      position = "dodge",
                      show.legend = FALSE,
                      width = 1, 
                      color = "#ffffff") + 
    scale_fill_manual(values = palette(nr)) + 
    ggplot2::scale_x_continuous(labels = paste0(seq(0, maxv, by = 10), "%"),
                                breaks = seq(0, maxv, by = 10),
                                limits = c(0, maxv)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
                   legend.text = element_text(size = 9),
                   panel.grid.major = element_blank(),
                   axis.text.x = element_text(color = "#000000")) +
    ggplot2::labs(y = "",
                  x = "") + 
    ggplot2::geom_text(aes(label = group), 
                       position = position_dodge(width = 1), hjust = -.1)
  
}


# Plot coefficient estimates
plot_coef <- function(object, ...) {
  
  ggplot(data = object, 
         aes(x = term, 
             y = ctd,
             ymax = ctd + 1.40 * quasiSE,
             ymin = ctd - 1.40 * quasiSE,
             col = Label)) +
    geom_point(position = position_dodge(width = 0.3), size = 1) +
    geom_errorbar(position = position_dodge(width = 0.3), width = 0) +
    coord_flip() +
    scale_color_brewer(palette = "Set1", name = "") +
    geom_text(aes(label= .group),
              size = 2,
              fontface = 1,
              nudge_x = rep(c(-0.3, 0.5), each = nlevels(object$term))) +
    labs(y = "", 
         x = "") + 
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 7, colour = "black"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 9, colour = "black"),
          axis.text.y = element_text(size = 9, colour = "black"))
  
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
#' 
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

