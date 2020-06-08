###Functions for Climmob Reporting Analysis

##List of functions
##Updated 01 May 2019
##
##scale01() : simple scaling function
##
##byfac() : produces PlackettLuce estimates and errorbars to compare between different levels of factor
##
##favourability() : calculates total wins, losses and favourability scores
##
##favourability_plot() : plot of favourability scores
##
##win_plot() : plot of win %
##
##multcompPL() : does mean seperation from PlackettLuce object with CLD grouping
##
##plot.multcompPL() : plots errorbars, confidence intervals and CLD letters
##
##concordance() : determines level of agreement between list of traits with the overall trait
##
##contests() : produce pairwise contest matrix between all varieties
##
##anova.PL() : ANOVA table from LRT test of PL Model
##
##draw.emojis() : draws emojis for the feedback forms
##
##get_ranking() : part of the favourability function, slightly modified to include second best as well


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

multcompPL<-function(mod,terms=NULL,threshold=0.05,Letters=letters,adjust="none"){
  
  require(qvcalc)
  require(multcompView)
  
  #get estimates with quasi-SEs
  qv1<-qvcalc(mod)$qvframe
  
  #reduce frame to only selected terms if not all comparisons are desired
  if(is.null(terms)==FALSE){
    qv1<-subset(qv1,rownames(qv1)%in%terms)
    #give error if less than 2 terms can be identified
    if(nrow(qv1)<3){
      stop("Less than 2 terms selected")
    }
  }
  
  #set up matrices for all differences and pooled errors
  diffs<-mat.or.vec(nrow(qv1),nrow(qv1))
  ses<-mat.or.vec(nrow(qv1),nrow(qv1))
  
  for(i in 1:nrow(qv1)){
    for(j in 1:nrow(qv1)){
      
      #get differences and pooled ses
      diffs[i,j]<-qv1$estimate[i]-qv1$estimate[j]
      ses[i,j]<-sqrt(qv1$quasiVar[i]+qv1$quasiVar[j])
    }
  }
  
  #calculate z scores
  z<-diffs/ses
  #TO DO: What DF to use to use here? Is it just the resid DF?
  p<-2*(1-pt(abs(z),mod$df.residual))
  
  #adjust p-value if you want to adjust. make sure to only take each p once for adjustment
  p[upper.tri(p)]<-p.adjust(p[upper.tri(p)],method = adjust)
  
  #make sure lower triangular is mirror of upper
  p[lower.tri(p)] = t(p)[lower.tri(p)]
  
  #set rownames
  rownames(p)<-colnames(p)<-rownames(qv1)
  
  #re-order qv output to ensure letters are produced in a sensible order
  qv1$term<-reorder(factor(rownames(qv1)),qv1$estimate,mean)
  qv1<-qv1[order(qv1$estimate,decreasing = TRUE),]
  
  #get mean seperation letter groupings
  qv1$.group<-multcompLetters2(estimate ~ term, p, qv1,
                               compare="<",
                               threshold=threshold,
                               Letters=Letters,
                               reversed = FALSE)$`Letters`
  return(qv1)
  
}

# simple ggplot function to plot output from multcompPL with error bars
plot_multcompPL <- function(object, term, estimate, quasiSE, group, level = 0.95, xlab = "", ylab = "", ...){
  
  object <- object[,c(term, estimate, quasiSE, group)]
  names(object) <- c("x","y","qse","g")
  
  object$x <- gosset:::.reduce(as.character(object$x), ...)
  
  ggplot(data = object,
              aes(x = x, 
                  y = y,
                  label = g, 
                  ymax = y + stats::qnorm(1-(1-level)/2) * qse,
                  ymin = y - stats::qnorm(1-(1-level)/2) * qse)) +
    geom_point() +
    geom_errorbar(width = 0.1) +
    coord_flip() +
    geom_text(vjust = 1.2) +
    xlab(xlab) + 
    ylab(ylab) +
    theme_bw()

}

anova.PL<-function(model){
  if(class(model)!="PlackettLuce"){
    stop("Model type is not Plackett Luce")
  }
  LLs<-c(model$null.loglik,model$loglik)
  dfs<-c(model$df.null,model$df.residual)
  df_diff<-(-1)*diff(dfs)
  df_LL<-(-1)*diff(LLs)
  p=1-pchisq(-2*df_LL,df_diff)
  
  
  x<-data.frame(model=c("NULL",deparse(substitute(model))),
                "logLikelihood"=LLs,
                DF=dfs,"Statistic"=c(NA,-2*df_LL),
                "Pr(>Chisq)"=c(NA,p),check.names = FALSE,stringsAsFactors = FALSE)
  return(x)
}



map.f <-  function(
    data = data4,
    lon = "lon", 
    lat = "lat",
    remove_outliers = TRUE,
    number_of_clusters = 6,
    cluster_method = "complete",
    max_cluster_dist = 250,
    min_cluster_pert = 0.05, 
    padding = NULL,
    map_provider = "Esri.WorldImagery",
    minimap_position = "bottomright"
  ) {
    
    ## Take input df and create a data frame with only lon & lat
    ## Making coding easier and removing unnecessary columns
    
    df <- 
      data.frame(
        lon = data[, lon],
        lat = data[, lat],
        stringsAsFactors = FALSE
      )
    
    ## Checks if there are any NAs and removes them
    
    if(
      any(is.na(df))
    ){
      warning("Data contains ", sum(is.na(df[,"lon"])|is.na(df[,"lat"])) ,
              " missing or invalid points which have been removed \n")
      df <- na.omit(df)
    }    
    
    
    ## Checks if lon & lat are  are numeric, converts if neccesary
    ## and supresses warnings. May introduce NAs.
    
    if(
      !(is.numeric(df$lon))|!(is.numeric(df$lat))
    ){
      df[, "lon"] <- suppressWarnings(as.numeric(df[, "lon"]))
      df[, "lat"] <- suppressWarnings(as.numeric(df[, "lat"]))
    }
    
    ## Ensure data lon & lat are each within [-180, 180] and 
    ## data frame does not include the point (0,0).
    ## If not, then points are converted to NAs
    
    
    if (
      any(
        df <= -180 | df >= 180 | (df$lon == 0 & df$lat == 0)
      )
    ){
      
      df$lon[which(!between(df$lon,-180, 180))] <- NA 
      df$lat[which(!between(df$lat,-180, 180))] <- NA
      df[which(df$lon == 0 & df$lat == 0),] <- NA
    }
    
    ## Checks if there are any NAs and removes them
    
    if(
      any(is.na(df))
    ){
      warning("Data contains ", sum(is.na(df[,"lon"])|is.na(df[,"lat"])) ,
              " missing or invalid points which have been removed \n")
      df <- na.omit(df)
    }    
    
    
    
    ## Outliers are defined as points that are part of cluster that are not too 
    ## small or are not too far away from other clusters.
    ## This section will calculate clusters, test if these clusters meet the size 
    ## and distance requirements and remove any that do not meet this criteria
    ## If the number_of_clusters == 0 or remove_outliers is FALSE then the code
    ## will not run.
    
    if(remove_outliers){
      
      ## Create clusters; 
      df <- 
        df %>% 
        ## Remove duplicate lon & lat temporarily
        distinct(lon, lat, .keep_all = FALSE) %>% 
        ## Create a distance matrix of all points,
        dist() %>% 
        ## Calculate clustering information of the points with hclust
        ## using the algo specified by cluster_methods
        hclust(method = "single") %>% 
        ## Return cluster group for each point, grouping into the number of clusters 
        ## specified by number_of_points 
        cutree(h = max_cluster_dist/110) %>%
        ## Add clusters group column to the distinct lon & lat points
        {
          bind_cols(
            distinct(df, lon, lat, .keep_all = FALSE), 
            cluster = .)
        } %>%  
        ## Rejoin to main dataset
        right_join(df, by = c("lon", "lat")) 
      
      # 
      # ## Create list of clusters to include
      # 
      #     tmp <- 
      #       df %>%
      # ## Calculate centre for each cluster using mean lon and mean lat
      #       group_by(cluster) %>% 
      #       summarise(mean(lon), mean(lat)) %>% 
      #       ungroup() %>% 
      # ## Calculate distance between each cluster centre as a distance matrix
      #       dist() %>% 
      # ## Crude conversion from lon & lat to km
      #       multiply_by(110) %>% 
      # ## Set all distances between the cluster and itself to NAs
      #       as.matrix() %>% 
      #       `diag<-`(NA) %>% 
      # ## Find the minimum distance to another cluster for each cluster
      #       apply(1, min, na.rm = TRUE) %>% 
      # ## Test if mimimum distance of each cluster to the nearest cluster is below the  
      # ## the maximum boundary set by max_cluster_dist
      #       t() %>% 
      #       is_less_than(max_cluster_dist) %>% 
      # ## Return the list of all clusters the are considered valid
      #       which()
      
      tmp <-
        df %>% 
        ## Calculate percentage of points in each cluster
        group_by(cluster) %>% 
        summarise(pert = n()) %>% 
        mutate(pert = pert / sum(pert))  %>%
        ## Keep those that are larger than the minimum cluster size
        filter(pert > min_cluster_pert) %>%
        ## Combine the list of cluster with enough points with the clusters that are not
        ## too far from each other
        use_series("cluster") 
      #    plot(df$lon, df$lat, col = df$cluster)
      
      
      ## If clusters are to be removed, then create a warning message saying how many
      ## points are considered outliers
      
      if(NROW(df[!(df$cluster %in% tmp), ]) != 0){
        warning(NROW(df[!(df$cluster %in% tmp), ]),
                " records are considered outliers and have been removed \n")
      }
      
      # Subset dataset to only those that are considered vaild clusters
      df <-
        df[df$cluster %in% tmp, ]
    }
    
    ## Find the maximum distance between my points
    
    lon_dif <- dist(df$lon)
    lat_dif <- dist(df$lat)
    max_dif <- 
      max(
        c(max(lon_dif),
          max(lat_dif)
        ))
    
    ## Create the map
    
    #  return(list(df = df, map = map))
    map <-
      ## Supress messages and create base layer 
      suppressMessages(
        df %>% 
          leaflet(options = leafletOptions(maxZoom = 17)) %>% 
          ## Set rectangular view of the final map using the min and max of lon & lat
          ## padding option does not work
          fitBounds(
            lng1 = min(df$lon), lat1 = min(df$lat), 
            lng2 = max(df$lon), lat2 = max(df$lat),
            options = list(padding = padding)
          ) %>%
          ## Define the base map texture using map_provider
          addProviderTiles(map_provider, options = providerTileOptions(maxNativeZoom=17))  %>% 
          ## Add clusters markers (calculated seperate to above process)
          #addAwesomeMarkers(icon = icons, clusterOptions = markerClusterOptions()) %>% 
          ## Add point markers
          addCircleMarkers(
            radius = 4, 
            #        fillColor = "midnight blue", 
            opacity = 0.5, 
            fillOpacity = 0.5,
            fillColor = "black",
            color = "white",
            clusterOptions = markerClusterOptions()
          ) %>%
          ## Add minimap to final map, position based on minimap_position
          addMiniMap(position = minimap_position)
      )
    
    map_interval <-
      10^round(log10(max_dif))/2
    
    map_interval <-
      ifelse(round(max_dif/map_interval) == 1, 10^round(log10(max_dif))/5, 10^round(log10(max_dif))/2)
    
    
    if(abs(map_interval)<0.01){
      lon_line <-
        data.frame(
          lon = c(median(df$lon)-0.03, median(df$lon)+0.03),
          lat = c(median(df$lat))
        )
      
      lat_line <-
        data.frame(
          lon = c(median(df$lon)),
          lat = c(median(df$lat)-0.03, median(df$lat)+0.03)
        )
      
      map <-
        map %>% 
        addPolylines(data = lon_line, lng = ~lon, lat = ~lat, color = "#000", opacity = 1, weight = 3) %>% 
        addPolylines(data = lat_line, lng = ~lon, lat = ~lat, color = "#000", opacity = 1, weight = 3) %>%
        addControl(html = paste(round(median(df$lon), digits = 5), round(median(df$lat), digits = 5), sep = ", "), position = "topleft")
    } else {
      
      xticks <-
        seq(
          floor(map$x$fitBounds[[2]]/map_interval)*map_interval,
          ceiling(map$x$fitBounds[[4]]/map_interval)*map_interval,
          by=map_interval
        )
      
      yticks <-
        seq(
          floor(map$x$fitBounds[[1]]/map_interval)*map_interval,
          ceiling(map$x$fitBounds[[3]]/map_interval)*map_interval,
          by = map_interval
        )
      
      map <- 
        map %>%
        addGraticule(interval = map_interval) %>% 
        addLabelOnlyMarkers(lng=xticks-map_interval/20,lat=max(df$lat,na.rm=T),label=as.character(xticks), 
                            labelOptions = labelOptions(noHide = T, direction = 'right', textOnly = T,style = list(
                              "color" = "white",
                              "font-style" = "bold",
                              "font-size" = "12px"
                            ))) %>%
        addLabelOnlyMarkers(lng=min(df$lon,na.rm=T),lat=yticks,label=as.character(yticks), 
                            labelOptions = labelOptions(noHide = T, direction = 'right', textOnly = T,style = list(
                              "color" = "white",
                              "font-style" = "bold",
                              "font-size" = "12px"
                            )))
    }
    
    map$x$options = list("zoomControl" = FALSE)
    
    return(map = map)
    
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
draw.emojis<-Vectorize(draw.emojis)

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



#' Plot map using mapview
#' @param data a data frame
#' @param coords index of data for the lonlat coordinates
#' @param add any additional index for colunms in data to add to the map
plot_map <- function(data, coords = NULL, add = NULL, ...) {
  
  lonlat <- data[, c(coords, add)]
  
  lonlat <- stats::na.omit(lonlat)
  
  lonlat <- suppressWarnings(
    sf::st_as_sf(lonlat, coords = c("lon","lat"), crs = 4326)
  )
  
  suppressWarnings(
    mapview::mapview(lonlat, ...)
  )
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

# Check if library has the latest version
.latest_version <- function(pkg, repo_path){
  
  desc <- readLines(repo_path)
  
  vers <- desc[grepl("Version", desc)]
  
  locvers <- as.character(packageVersion(pkg))
  
  latest <- grepl(locvers, vers)
  
  isTRUE(latest)
  
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



#' Plot nodes from recursive partitioning trees
#'
#' Make a ggplot2 chart from model-based recursive partitioning trees with quasi-variance
#'
#' @param object an object of class modelparty
#' @param add.letters optional
#' @param ... additional arguments passed to ggplot2
#' @return a list of plots with probabilities of winning and 
#' intervals based on quasi-standard errors
#' @seealso \code{\link[qvcalc]{qvcalc}} \code{\link[ggplot2]{ggplot}}
#' @examples
#' 
#' library("psychotree")
#' library("ggplot2")
#' library("ggparty")
#' library("patchwork")
#' 
#' 
#' ## Germany's Next Topmodel 2007 data
#' data("Topmodel2007", package = "psychotree")
#' 
#' ## BT tree
#' tm_tree <- bttree(preference ~ ., data = Topmodel2007, minsize = 5, alpha = 0.1)
#' 
#' plot_tree(tm_tree)
#' 
#' @importFrom partykit nodeids
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc
#' @importFrom ggparty ggparty, geom_edge, geom_edge_label, geom_node_label
#' @importFrom ggplot2 ggplot aes geom_vline geom_point geom_errorbar scale_x_continuous 
#' theme_bw labs theme element_text element_blank element_rect element_line
#' @export
plot_tree <- function(object, add.letters = FALSE, ...){
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
  dots <- list(...)
  
  font.size <- dots[["font.size"]]
  
  # get node information
  nodes <- list()
  for (i in seq_along(node_id)) {
    nodes[[i]] <- object[[ node_id[i] ]]$node$info$object
  }
  
  # get number of observers in each node
  nobs <- list()
  for (i in seq_along(node_id)) {
    nobs[[i]] <- as.integer(object[[ node_id[i] ]]$node$info$nobs) 
  }
  
  # get item parameters from model
  coeffs <- lapply(nodes, psychotools::itempar)
  
  # get estimates from item parameters using qvcalc
  coeffs <- lapply(coeffs, qvcalc::qvcalc)
  
  # extract data frames with estimates
  coeffs <- lapply(coeffs, function(X){
    df <- X[]$qvframe }
  )
  
  # get item names
  items <- rownames(coeffs[[1]])
  
  # Add limits in error bars and item names
  coeffs <- lapply(coeffs, function(X){
    X <- within(X, {
      bmin <- X$estimate-(X$quasiSE)
      bmax <- X$estimate+(X$quasiSE)
      items <- items
    })
    
    X$bmax <- ifelse(X$bmax > 1, 0.991, X$bmax)
    
    X$bmin <- ifelse(X$bmin < 0, 0.001, X$bmin)
    return(X)
  })
  
  # Add node information and number of observations
  for (i in seq_along(node_id)) {
    coeffs[[i]] <- within(coeffs[[i]], {
      nobs <- nobs[[i]]
      node <- node_id[i]}
    )
  }
  
  coeffs <- do.call("rbind", coeffs)
  
  if (isTRUE(add.letters)){
    groups <- try(lapply(nodes, function(x){
      x <- multcompPL(x)
      x[sort(items), ".group"]
    }), silent = TRUE)
    groups <- unlist(groups)
    if (grepl("Error",groups[[1]])){
      message("Unable to get letters for the plotting object.",
              " The issue has likely occurred in qvcalc::qvcalc() \n")
      groups <- ""
    }
    coeffs <- cbind(coeffs, groups = groups)
  }else{
    coeffs$groups <- ""
  }
  
  node_lev <- unique(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"))
  
  coeffs$id <- coeffs$node
  
  coeffs$node <- factor(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"),
                         levels = node_lev)
   
  coeffs$items <- factor(coeffs$items, levels = sort(items))
  
  # get the tree structure
  if(length(node_id) > 1){
  tree <- 
    ggparty::ggparty(object, terminal_space = 0) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label() +
    ggplot2::theme(legend.position = "none") +
    ggparty::geom_node_label(line_list = list(
      aes(label = splitvar),
      aes(label = paste("p =",
                        formatC(p.value,
                                format = "e",
                                digits = 1))),
      aes(label = ""),
      aes(label = id)),
      line_gpar = list(list(size = 12),
                       list(size = 8),
                       list(size = 8),
                       list(size = 8,
                            col = "black",
                            fontface = "bold",
                            alignment = "center")
    ),
                    ids = "inner") +
    coord_cartesian(ylim = c(0.1, 1.1))
  }
  
  # Get max and min values for the x axis in the plot
  xmax <- round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  xmin <- round(min(coeffs$bmin, na.rm = TRUE) - 0.01, digits = 4)
  xbreaks <- round(c(mean(c(0, xmax)), xmax), 2)
  #xbreaks <- seq(0, (xmax*100), by = 2)/100
  xbreaks <- c(0, xbreaks)
  xlabs <- as.character(xbreaks)
  #xlabs <- gsub("0[.]",".", as.character(xbreaks))
  
  
  # Check font size for axis X and Y, and plot title
  s.axis <- 11

  
  p <- 
    ggplot2::ggplot(coeffs, ggplot2::aes(x = estimate, y = items)) +
    ggplot2::geom_vline(xintercept = 1/length(items), 
                        colour = "#E5E7E9", size = 0.8) +
    geom_text(aes(label = groups),
              size = 2.5,
              nudge_y = 0.25) +
    ggplot2::geom_point(pch = 21, size = 2, 
                        fill = "black",colour = "black") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin,
                                         xmax = bmax),
                            colour="black", height = 0.1) +
    ggplot2::scale_x_continuous(limits = c(0, xmax),
                                breaks = xbreaks,
                                labels = xlabs) +
    facet_grid(. ~ node) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = s.axis, angle = 0,
                                                       hjust = 0.5, vjust = 1, 
                                                       face = "plain",
                                                       colour = "black"),
                   axis.text.y = ggplot2::element_text(size = s.axis, angle = 0,
                                                       hjust = 1, vjust = 0.5, 
                                                       face = "plain",
                                                       colour = "black"),
                   text = element_text(size = 10),
                   strip.background = element_blank(),
                   plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", size = 1),
                   axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),
                   axis.ticks.length = grid::unit(0.3, "cm"))
  
  if(length(node_id) > 1){
    require("patchwork")
    p <- (tree / p)
  }
  return(p)
}

