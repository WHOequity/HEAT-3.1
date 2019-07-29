# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

plotSummaryBar_explore_ggplot <- function(plotData, session) {
  # Plot 3: Barchart for a single country (Summary data)
  
  plot_data <- plotData
  
  summary_measure <- plot_data$measure[1]
  measure_name <- gsub("\\s\\(.*\\)$", "", translate(plot_data$measure[1], isolate(input$lang)))
  
  log_scale <- plot_data$logscale[1]
  
  data_years <- paste(sort(unique(plot_data$year)), collapse = ", ")
  data_sources <- paste(sort(unique(plot_data$source)), collapse = " & ")
  
  isolate({
    xaxis_title <- input$xaxis_title2 %||% ""
    
    yaxis_title <- if (input$yaxis_title2 == "") measure_name else input$yaxis_title2
    yaxis_min <- input$axis_limitsmin2
    yaxis_max <- input$axis_limitsmax2
    
    lang <- input$lang
  })
  
  plot_title <- formatLabels(
    .rdata[["plotSummary_explore_title"]],
    .rdata[["numTitleChars"]]
  )
  
  num_years <- n_distinct(plot_data$year)
  bin_width <- if (num_years > 2) 0.75 else 0.35
  error_width <- if (num_years > 2) 0.5 else 0.25

  # Make sure that the data frame includes all the possible bars by adding
  # missing data across the factor levels
  plot_data <- plot_data %>% 
    mutate(
      dimension = map_chr(dimension, translate, lang),
      measure_name = map_chr(measure, translate, lang)
    ) %>% 
    select(
      year, indic_name, indic, dimension, inequal, se.lowerci, se.upperci
    )
  
  if (HEATversion == "whodata") {
    plot_data <- mutate(
      plot_data, 
      indic_name = map_chr(indic, translate, lang),
      indic_title = indic_name
    )
  }
  
  # probably I need this but not clear why
  all_possibles <- expand.grid(
    year = unique(plot_data$year), 
    indic = unique(plot_data$indic),
    indic_name = unique(plot_data$indic_name),
    dimension = unique(plot_data$dimension), 
    stringsAsFactors = FALSE
  )
  
  plot_data <- left_join(
    all_possibles,
    plot_data,
    by = c("year", "indic", "indic_name", "dimension")
  )
  plot_data$year <- as.factor(plot_data$year)
  
  if (!is.null(input$long_names2) && input$long_names2 == FALSE) {
    indic_type <- "indic"
  } else {
    indic_type <- "indic_name"
  }
  
  form <- paste(indic_type, " ~ dimension")
  
  ci_type <- c("se.lowerci", "se.upperci") 
  
  scale_colors <- setNames(
    .rdata[["pal_single"]][c("red", "blue", "green", "purple", "orange")],
    sort(unique(plot_data[[indic_type]]))
  )
  
  p <- ggplot(
    data = plot_data, 
    mapping = aes_string(
      x = "year",
      weight = "inequal", 
      fill = indic_type,
      ymin = ci_type[1],
      ymax = ci_type[2]
    )
  ) + 
    geom_text(
      data = plot_data, 
      aes_string(
        x = "year", 
        y = "inequal",
        fill = indic_type, 
        label = "format(round(inequal, 1), nsmall = 1)",
        ymin = ci_type[1], 
        ymax = ci_type[2]
      ), 
      position = position_dodge(width = bin_width), 
      vjust = -0.75,
      size = 3, 
      color = "grey50",
      alpha = !input$summary_error_bars
    ) +
    geom_bar(
      mapping = aes(
        y = inequal
      ), 
      position = position_dodge(), 
      stat = "identity", 
      color = "white", 
      size = 1, 
      width = bin_width
    ) +
    geom_errorbar(
      color = "grey50",  
      position = position_dodge(0.9), 
      width = 0.25, 
      alpha = input$summary_error_bars
    ) +
    scale_fill_manual(
      values = scale_colors
    ) +
    labs(
      x = xaxis_title, 
      y = yaxis_title, 
      title = plot_title
    ) +
    guides(
      colour = FALSE, 
      fill = FALSE
    ) + 
    facet_grid(form, labeller = splitLabels) +
    heat_theme()
  
  if (yaxis_min != "" || yaxis_max != "") {
    p <- adjust_axes(
      p, 
      x = FALSE,
      y = TRUE, 
      Yaxismin = yaxis_min,
      Yaxismax = yaxis_max
    )
  }
  
  if (log_scale == 1) {
    if (any(c(yaxis_min, yaxis_max) <= 0)) {
      p <- p + coord_cartesian()
    }
    
    scale_breaks <- c(1, unique(round(pretty(c(plot_data$se.lowerci, plot_data$se.upperci), n = 5), 1)))
    p <- p + 
      scale_y_log10(breaks = scale_breaks, labels = scale_breaks) +
      ylab(
        label = gsub(")", ", axis log-scaled)", yaxis_title, fixed = TRUE)
      )
  }
  
  
  return(p)
}
