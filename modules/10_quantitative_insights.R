#' This module summarises the quantitative data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the quanti_dat object
#' 
#' @param quanti_dat a list with parameters
#' @examples 
#' modules = list.files("modules", full.names = TRUE,pattern = ".R")
#' 
#' modules = modules[-which(grepl("check_packages.R", modules))]
#' for (i in seq_along(modules)) {
#'   source(modules[i])
#' }
#' 
#' load("modules/example-data-structure.rda")
#' 
#' # ClimMob parameters
#' pars = decode_pars(cmpars)
#' 
#' quanti_dat =  organize_quantitative_data(cmdata,
#'                                          pars,
#'                                          groups = "gender1",
#'                                          id = "id",
#'                                          tech_index = paste0("package_item_", LETTERS[1:3]))
#' 
#' 
#' get_quantitative_summaries(quanti_dat)
#' @export
get_quantitative_summaries <- function(quanti_dat) {
  
  # make density plots with the data 
  densitplots <- list()
  
  # a list with file names to print charts
  density_file_names <- list()
  
  for (i in seq_along(quanti_dat$quanti_dat)){
    
    ggdat_i <- quanti_dat$quanti_dat[[i]]
    
    b_i <- ggplot(ggdat_i, aes(x = value, 
                               y = technology, 
                               color = "#006d2c")) +
      geom_boxplot(show.legend = FALSE,
                   outlier.colour = "#d95f02") +
      
      #geom_jitter(show.legend = FALSE, color = "#006d2c") +
      scale_color_manual(values = "#006d2c") +
      labs(title = paste(unique(ggdat_i$trait), unique(ggdat_i$data_collection), sep = " - "),
           x = "",
           y = "") +
      theme_bw() + 
      theme(panel.grid = element_blank(),
            text = element_text(size = 16),
            title = element_text(size = 12),
            axis.text.x = element_text(angle = 0,  hjust = 0.5))
    
    densitplots[[i]] <- b_i
    density_file_names[[i]] <- tolower(gsub(" ", "", 
                                            paste(unique(ggdat_i$trait), 
                                                  unique(ggdat_i$data_collection), 
                                                  sep = " - ")))
    
  }
  
  result <- list(density_plots = densitplots,
                 density_file_names = density_file_names)
  
  return(result)
  
}


# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_quantitative_traits <- list(density_plots = list(),
                                       density_file_names = list())



# 
# 
# # first for yield
# yield = unlist(lapply(quanti_dat$quanti_dat, function(x){
#   grepl("yieldperse", x$trait_code[1])
# }))
# 
# yield = quanti_dat$quanti_dat[yield]
# 
# dat = yield[[2]]
# 
# dat = split(dat, dat$technology)
# 
# unlist(lapply(dat, nrow))
# 
# dat = lapply(dat, function(x){
#   out = boxplot.stats(x[, "value"])$out
#   rmv = !x[, "value"] %in% out
#   x = x[rmv, ]
#   x
# })
# 
# dat = do.call("rbind", dat)
# 
# 
# ggplot(dat, aes(y = value, x = technology, color = technology)) +
# geom_boxplot(show.legend = FALSE) +
#      geom_jitter(show.legend = FALSE) +
#      scale_color_brewer(palette = "BrBG", name = "") +
#      labs(title = paste(unique(dat$trait), unique(dat$data_collection), sep = " - "),
#                   x = "",
#                    y = "") +
#      theme_bw() + 
#      theme(panel.grid = element_blank(),
#                      text = element_text(size = 16),
#                      title = element_text(size = 12),
#                      axis.text.x = element_text(angle = 45,  hjust = 1))
# 
# mod = lm(value ~ technology, data = dat)
# 
# summary(mod)
# 
# 
