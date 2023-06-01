#' This module summarises the quantitative data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the quanti_dat object
#' 
#' @param quanti_dat a list with parameters
get_quantitative_summaries <- function(quanti_dat) {
  
  # make density plots with the data 
  densitplots <- list()
  
  # a list with file names to print charts
  density_file_names <- list()
  
  for (i in seq_along(quanti_dat$quanti_dat)){
    
    ggdat_i <- quanti_dat$quanti_dat[[i]]
    
    b_i <- ggplot(ggdat_i, aes(y = value, x = technology, color = technology)) +
      geom_boxplot(show.legend = FALSE) +
      geom_jitter(show.legend = FALSE) +
      scale_color_brewer(palette = "BrBG", name = "") +
      labs(title = paste(unique(ggdat_i$trait), unique(ggdat_i$data_collection), sep = " - "),
           x = "",
           y = "") +
      theme_bw() + 
      theme(panel.grid = element_blank(),
            text = element_text(size = 16),
            title = element_text(size = 12),
            axis.text.x = element_text(angle = 45,  hjust = 1))
    
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


