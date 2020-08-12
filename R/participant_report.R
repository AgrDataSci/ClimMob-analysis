# ................................................................
# ................................................................
# Summarise the results for the farmers reports
# ................................................................
# ................................................................
# ................................................................
# ................................................................

# Make the overview ####
# table with the worth parameters from overall performance
# number of times each item was tested
# and how many times it was ranked first or last

# check if items have more than 20 characters and make it shorter
cmdata[, itemnames] <- lapply(cmdata[, itemnames], function(x){
  abbreviate(x, minlength = 20)
})

# make the rank without the Local item
overall <- trait_list[[1]]
keep <- overall$keep

# list of arguments for the function that will be used to 
# create the rankings
a <- list(cmdata[keep, ],
          items = itemnames,
          input = overall$input)

R <- do.call(rankwith, args = a)

order_items <- coef(PlackettLuce(R), ref = reference)

rank_items <- gosset:::.rank_decimal(order_items)$rank

order_items <- names(order_items)

freq_items <- table(unlist(itemdata))

R <- R[1:nrow(R),, as.rankings = FALSE]
R[R == 0] <- NA

first <- apply(R, 1, function(x){
  i <- which.min(x)
  names(x)[i]
}) 

last <- apply(R, 1, function(x){
  i <- which.max(x)
  names(x)[i]
}) 

first_items <- table(first)
last_items  <- table(last)

infotable <- data.frame(item = order_items,
                        rank = rank_items,
                        freq = as.vector(freq_items[order_items]),
                        first = as.vector(first_items[order_items]),
                        last = as.vector(last_items[order_items]))

infotable[is.na(infotable)] <- 0

infotable <- infotable[order(infotable$rank), ]

# ................................................................
# ................................................................
# Get the info from the participants ####
sel <- c("id", "package_farmername", paste0("package_item_", LETTERS[1:ncomp]))
partitable <- cmdata[keep, sel]

names(partitable) <- gsub("package_|farmer", "", names(partitable))

# now get the items that where ranked from first to last by 
# each participant
ord <- t(apply(R, 1, function(x){
  x <- na.omit(x)
  x <- sort(x)
  names(x)
}))

ord <- as.data.frame(ord)

names(ord) <- paste0("Position", 1:ncomp)

partitable <- cbind(partitable, ord)

# ................................................................
# ................................................................
# If any other trait, do the same ####
if(isTRUE(nothertraits > 0)){
  otr <- list()
  for(i in seq_along(other_traits_list)){
    ot <- other_traits_list[[i]]
    
    a <- list(cmdata[ot$keep, ],
              items = itemnames,
              input = ot$input)
    
    R <- do.call(rankwith, args = a)
    
    R <- R[1:nrow(R),, as.rankings = FALSE]
    
    R[R == 0] <- NA
    
    R <- t(apply(R, 1, function(x){
      x <- na.omit(x)
      x <- sort(x)
      names(x)
    }))
    
    dimnames(R)[[1]] <- partitable$id
    
    # expand the rankings (in rows) so it can fit with the full
    # information to include those participants who did not replied the
    # question 
    Rexp <- matrix(NA, 
                   nrow = nrow(partitable), 
                   ncol = ncomp, 
                   dimnames = list(partitable$id, 
                                   paste0("Position", 1:ncomp)))
    
    Rexp[dimnames(R)[[1]], ] <- R
    
    R <- Rexp
    
    R[is.na(R[, 1]), 1] <- "Not replied"
    
    R[is.na(R)] <- ""
    
    otr[[i]] <- R
    
  }
  
  # now put all together by participants ids
  otrp <- list()
  for(i in  seq_along(partitable$id)){
    
    x <- NULL
    
    for(j in seq_along(other_traits_list)){
      
      x <- rbind(x, otr[[j]][i, ])
      
    }
    
    x <- rbind(ord[i, ], x)
    
    # add the question that was made
    x <- cbind(Question = tbl_section1$Question, x)
    
    # change names of order based on the number of comparisons 
    # used in the trail
    if (isTRUE(ncomp == 3)){
      names(x)[-1] <- c("First","Middle","Last")
    }
    
    if (isTRUE(ncomp > 3)) {
      names(x)[-1] <- paste("Position", 1:ncomp)
    }
    
    otrp[[i]] <- x
    
  }
  
}

# ................................................................
# ................................................................
# define colours, line widths, styles etc. - you can safely play with all of it
grey_col <- colors()[230]
grey_col1 <- colors()[235]
grey_col2 <- colors()[240]
grey_col3 <- colors()[245]
grey_dark <- colors()[190]
grey_dark2 <- colors()[205]
grey_dark2 <- grey_dark
text_colors <- c(red = "#B2182B",
                 grey = colors()[200],
                 blue = "#2166AC")
emoji_colors <- c(red = "#e7b9bf", grey = grey_col2, blue = "#bcd1e6")
text_colors <- c(red = colors()[205],
                 grey = colors()[195],
                 blue = "#2166AC")
emoji_colors <- c(red = grey_col2, grey = grey_col2, blue = "#bcd1e6")
text_colors <- c(red = colors()[195],
                 grey = grey_dark,
                 blue = "black")
emoji_colors <- c(red = grey_col2, grey = grey_col1, blue = grey_col)
emoji_colors <- c(red = grey_col, grey = grey_col, blue = grey_col)
textsize_1 <- 0.6
textsize_2 <- 1.1
textsize_3 <- 1.3
arrowsize <- 3
arrowlength <- 0.07
boxwidth <- 1
emojiradius <- 0.4
color4you <- TRUE
transparency <- 1
smiley_thickness <- 1


# ................................................................
# ................................................................
# some position parameters to play with the style of the form

# vertical parameters
n_toplines <- 7 #How many lines in the top box
yspace_top <- 0.6 #line height in the top box
yspace_box <- 3 #space between top box and title
yspace_title <- 4 #space between title and ranking info
yspace_header <- 1.5 #space between header and ranking info (for the item sheet)
yspace <- 1 #line height in the ranking info
htitle <- 3.5 #height of the title box

# modify parameters to deal with high number of items
if (isTRUE(nitems > 23)) {
  yspace <- 0.5
  textsize_2 = 0.8
  emojiradius = 0.3
  
  if (isTRUE(nitems > 43)) {
    yspace <- 0.3
    textsize_2 = 0.5
    emojiradius = 0.2
    if (isTRUE(nitems > 73)) {
      yspace <- 0.15
      textsize_2 = 0.27
      textsize_1 = 0.4
    }
  }
}


# horizontal parameters
xstart <- 0
xend <- 25
xtop <- 0.5
xtitle <- c(12.5, 12.5)
xsmileys <- 4.8
xarrowVtext <- 2.2
xarrowV <- 5.5
xranking <- 7
xarrowH <- c(15, 17)
xsmiley <- 18
xranking2 <- 18.8
charperunit <- (xend - xstart) / (70)
xinfo <- 10 


# ................................................................
# ................................................................
# function to calculate the position of the items/varieties in relation to the title
make_y_bloc <- function(nb_items){
  return(c(title = 0,
           header = -yspace_title + yspace_header, 
           -yspace_title - (0:(nb_items - 1)) * yspace))
}

# ................................................................
# ................................................................
# preparing the design of the form

# vertical positioning
# positions of the lines in the box
ytop <- -(1:n_toplines) * yspace_top 
# position of the box top and bottom
ytopbox <-rev(range(ytop)) + c(yspace_top, -yspace_top) 
# position of the items/varieties in the ranking
y <- ytopbox[2] - yspace_box + make_y_bloc(nitems) 
# position of the items in the info list for items sheet
y2 <- -yspace_box + make_y_bloc(nrow(infotable)) 
# nb of characters of each item - used to calculate the 
# approximate width of the ranker's items arrows
global_width <- nchar(as.character(items))

# colors of the items/varieties
global_ranking_colors <- rgb(colorRamp(text_colors)(rev(scale01(infotable$rank))), 
                             max = 255)

# ................................................................
# ................................................................
# create a black arrow, saved as external file
png(paste0(fullpath, "/", pathname, "participant_report/png/mask.png"))
ytmp1 <- max(y[length(y)],-35)
ytmp2 <- y[3]
grid.polygon(
  c(-.3, .3, .3, .5, 0,-.5,-.3),
  c(
    ytmp1,
    ytmp1,
    ytmp2 - yspace / 2,
    ytmp2 - yspace / 2 * 1.2,
    ytmp2,
    ytmp2 - yspace / 2 * 1.2,
    ytmp2 - yspace / 2
  ),
  gp = gpar(fill = "black"),
  def = "native",
  vp = viewport(xs = c(-.5, .5), ys = c(ytmp1, ytmp2))
)
dev.off()

# ................................................................
# ................................................................
# read back in the arrow as colour matrix
m <- readPNG(paste0(fullpath, "/", pathname, "participant_report/png/mask.png"), native=FALSE)
mask <- matrix(rgb(m[,,1],m[,,2],m[,,3]),
               nrow=nrow(m))
rmat <- matrix(grey(seq(0,1,length=nrow(m))),
               nrow=nrow(m), 
               ncol=ncol(m))
rmat[mask == "#FFFFFF"] <- NA

# ................................................................
# ................................................................
# make all the personalized results ####
# png files, by looping over all the ids
for(i in seq_along(partitable$id)){
  
  # name of ranker
  name <- partitable$name[i]

  
  # items ranked by the ranker, ordered
  your_ranking <- as.vector(unlist(partitable[i, paste0("Position", 1:ncomp)]))
  your_ranking <- your_ranking[1:3]
  
  # ranker parameters
  # position of the ranker's items
  y_yours <- y[-(1:2)][match(your_ranking, infotable$item)] 
  #approximate width of the ranker's items - for the sizes of the arrows
  width_yours <- global_width[match(your_ranking, infotable$item)] 
  
  # make the result png file
  pngpath1 <- paste0(fullpath, "/", pathname, "participant_report/png/", 
                       partitable$id[i], ".png")
  
  png(pngpath1, width= 21, height= 29, units="cm", res=300)
  par(mai=c(0,0,0,0), omi=c(.8,.5,.8,.5))
  plot.new()
  plot.window(ylim=c(-35,0), xlim=c(xstart, xend))
  
  # top_box
  rect(xleft=xstart, 
       ybottom=ytopbox[2], 
       xright = xend, 
       ytop = ytopbox[1], 
       col=NA, 
       border = grey_col,
       lty = 2, lwd=boxwidth)
  
  text(x=xtop, 
       y=ytop[1], 
       paste0(name, "'s Certificate of Participation in a ClimMob Trial"), 
       cex=textsize_2, 
       adj=c(0, NA))
  
  text(x=xtop,y=ytop[3], 
       paste(nranker, rankers, "participated in this trail."), 
       cex=textsize_1, 
       adj=c(0, NA))
  
  text(x=xtop,
       y=ytop[4], 
       paste("Each of these", rankers, "evaluated", ncomp, 
             options, "out of", nitems, options,
             "of the experiment."),
       cex=textsize_1, 
       adj=c(0, NA))
  
  text(x=xtop, 
       y=ytop[5], 
       paste0("You ranked these ", ncomp, " ", options,": ", 
              paste(sort(as.character(your_ranking)), collapse=", ")),
       cex=textsize_1, 
       adj=c(0, NA))
  
  text(x=xtop,
       y=ytop[7], 
       "Thank you for your participation!", 
       font=2, 
       cex=textsize_1, 
       adj=c(0, NA))
  
  # title
  rect(
    xleft = xstart,
    ybottom = y[1] - htitle * 1 / 2,
    xright = xend,
    ytop = y[1] + htitle / 2,
    col = grey_dark,
    border = NA
  )
  
  text(
    xtitle[2],
    y["title"] + textsize_3 * .8,
    paste(nranker, rankers,
          "(including you) were asked to rank the", ncomp, 
          options, "they got, to answer the question:"),
    adj = c(0.5, NA),
    font = 1,
    cex = textsize_1,
    col = "white"
  )
  
  text(xtitle[2], 
       y["title"], 
       paste("Overall, which", option, 
             "you prefer most?"), 
       adj=c(0.5,NA), 
       font=2, 
       cex=textsize_3, 
       col="white")
  
  
  text(xtitle[2], 
       y["title"]-textsize_3*.8, 
       "Adding up all the responses resulted in the following ranking:", 
       adj=c(0.5,NA), 
       font=1, 
       cex=textsize_1, 
       col="white")
  
  # global ranking left text
  text(xarrowVtext,
       y[3]+.6, 
       "most often favourite", 
       adj=c(.5,NA), 
       cex=textsize_1*1.2, font=2)
  
  text(xarrowVtext,y[3], 
       paste(infotable[1, "freq"], rankers, "ranked this", option), 
       adj=c(.5,NA), 
       cex=textsize_1*.8)
  
  text(xarrowVtext,
       y[3]-.4, 
       paste(infotable[1, "first"], rankers, "ranked it as the best"),
       adj=c(.5,NA), 
       cex=textsize_1*.8)
  
  text(xarrowVtext,
       y[length(y)]+.6, 
       "least often favourite", 
       adj=c(.5,NA), 
       cex=textsize_1*1.2, 
       col=grey_dark2,
       font=2)
  
  text(xarrowVtext,
       y[length(y)],
       paste(infotable[nrow(infotable), "freq"], rankers, "ranked this", option), 
       adj=c(.5,NA), 
       cex=textsize_1*.8, 
       col=grey_dark2)
  
  text(xarrowVtext, 
       y[length(y)]-.4, 
       paste(infotable[nrow(infotable), "first"], rankers, "ranked it as the top one"), 
       adj=c(.5,NA), 
       cex=textsize_1*.8, 
       col=grey_dark2)
  
  # global ranking arrow
  rasterImage(rmat,
              xarrowV+.5, 
              y[length(y)]+1,
              xarrowV-.5, y[3]-1)
  
  draw.emojis(xsmileys+c(0.3,.8,1,1.5),
              y[3]+c(-.1,.4,-.3,.2),
              radius=emojiradius*0.8,
              type="happy",
              border=grey_dark, 
              thickness=smiley_thickness, 
              col=emoji_colors["blue"])
  
  
  draw.emojis(xsmileys+c(0.3,.8,1,1.5),
              y[length(y)]+c(-.1,.4,-.3,.2), 
              radius=emojiradius*0.8,
              type="sad",
              border=grey_dark2, 
              thickness=smiley_thickness, 
              col=emoji_colors["red"])
  
  # global ranking
  text(xranking,y[-(1:2)], infotable$item, 
       adj=c(0,NA), 
       col=global_ranking_colors, 
       cex=textsize_2)
  
  # ranker's own ranking
  arrows(rep(xarrowH[2],3), 
         y0 = y_yours, 
         x1=xranking+width_yours*charperunit*(textsize_2)+1, 
         y1=y_yours,  
         lwd=arrowsize,
         length=arrowlength,
         col = if(color4you) rev(emoji_colors) else grey_col2)
  
  draw.emojis(xsmiley,y_yours, 
              radius=emojiradius, 
              type=c("happy", "neutral", "sad"),
              col= if(color4you) rev(emoji_colors) else emoji_colors["grey"], 
              border=grey_dark, 
              thickness=smiley_thickness)
  
  text(xranking2,
       y_yours,
       paste("You ranked this",
             option, c("as the top one", "second", "third")), 
       adj=c(0,NA), 
       cex=textsize_1, 
       col=if(color4you) rev(text_colors) else text_colors["grey"])
  
  dev.off()
  
  if(isTRUE(nothertraits > 0)){
  # make the result png file
  pngpath2 <- paste0(fullpath, "/", pathname, "participant_report/png/", 
                     partitable$id[i], "page2.png")
  
  png(pngpath2, width= 21, height= 29, units="cm", res=300)
  par(mai=c(0,0,0,0), omi=c(.8,.5,.8,.5))
  plot.new()
  plot.window(ylim=c(-35,0), xlim=c(xstart, xend))
  
  # top_box
  rect(xleft=xstart, 
       ybottom=ytopbox[2], 
       xright = xend, 
       ytop = ytopbox[1], 
       col=NA, 
       border = grey_col,
       lty = 2, lwd=boxwidth)
  
  text(x=xtop, 
       y=ytop[1], 
       paste("This is how you ranked the", 
             ncomp, options, "that you received,"), 
       cex=textsize_3, 
       adj=c(0, NA))
  
  text(x=xtop,y=ytop[3], 
       paste("based on the other", nothertraits, 
             "characteristics evaluated in this trial:"),
       cex=textsize_3, 
       adj=c(0, NA))
  
  grid.table(otrp[[i]], rows = NULL)
  
  dev.off()
  
  }
}

# create all the feedback reports, by looping over all the ranker.ids
for(i in seq_along(partitable$id)){
  # ranker description
  id_i <- partitable$id[i]
  name <- partitable$name[i]
  name <- gsub(" ", "_", tolower(name))
  
  ranker_description <- paste0(id_i, "_", name)
  
  rmarkdown::render(paste0(fullpath, "/report/", language, "/participant_report.Rmd"),
                    output_dir = paste0(pathname, "participant_report/"),
                    output_format = output_format,
                    output_file = paste0("participant_report_", ranker_description, ".",extension))
  
}

# # ................................................................
# # ................................................................
# # make the list of items with info png file
# png(paste0(fullpath, "/", pathname, "participant_report/png/items.png"),
#     width= 8.27, height= 11.7, units="in", res=300)
# par(mai=c(0,0,0,0), omi=c(.8,.5,.8,.5), lheight=.9)
# plot.new()
# plot.window(ylim=c(-35,0), xlim=c(xstart, xend))
# 
# # title
# rect(xleft=xstart, 
#      ybottom=y2["title"]-htitle*1/3, 
#      xright = xend, 
#      ytop = y2["title"]+htitle/3, 
#      col=grey_dark, 
#      border=NA)
# 
# text(xtitle[2], 
#      y2["title"], 
#      paste("Information about the", options),
#      font=2, 
#      cex=textsize_3, 
#      col="white")
# 
# #header
# text(xstart,
#      y2["header"], 
#      options, 
#      adj=c(0,NA), 
#      cex=textsize_3,  
#      font=2)
# 
# text(xinfo,y2["header"],
#      paste("Which of the", ncomp, options, "that you received", 
#            "had the best overall impression?"), 
#      adj=c(0,NA),
#      cex=textsize_3,  
#      font=2)
# 
# #List items
# text(xstart,
#      y2[-(1:2)], 
#      infotable$items, 
#      adj=c(0,NA), 
#      cex=textsize_1)
# 
# 
# #Items/varieties info/advice
# text(xinfo,y2[-(1:2)], "blab kakankan", adj=c(0,NA), cex=textsize_1)
# 
# dev.off()

