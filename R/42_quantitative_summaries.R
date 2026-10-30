# ................................................................
# Summaries of the quantitative traits
# ................................................................

#' Boxplots of each quantitative trait by item
#'
#' @param quanti_dat output of organize_quantitative_data()
get_quantitative_summaries = function(quanti_dat) {

  density_plots = list()
  density_file_names = list()

  for (i in seq_along(quanti_dat$quanti_dat)) {

    d = quanti_dat$quanti_dat[[i]]
    title = paste(unique(d$trait), unique(d$data_collection), sep = " - ")

    density_plots[[i]] =
      ggplot(d, aes(x = value, y = technology, color = "#006d2c")) +
      geom_boxplot(show.legend = FALSE, outlier.colour = "#d95f02") +
      scale_color_manual(values = "#006d2c") +
      labs(title = title, x = "", y = "") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            text = element_text(size = 16),
            title = element_text(size = 12),
            axis.text.x = element_text(angle = 0, hjust = 0.5))

    density_file_names[[i]] = tolower(gsub(" ", "", title))

  }

  list(density_plots = density_plots,
       density_file_names = density_file_names)

}

#' Placeholder used when the summaries could not be produced
error_data_quantitative_traits = list(density_plots = list(),
                                      density_file_names = list())
