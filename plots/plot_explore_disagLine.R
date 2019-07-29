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


plotDisagLine_explore_ggplot <- function(plotData) {
  # Plot 2: Horizontal line chart for a single country (Disaggregation of data)

  # git681, flash
  isolate({
    xaxis_title <- input$xaxis_title1
    xaxis_min <- input$axis_limitsmin1
    xaxis_max <- input$axis_limitsmax1
    
    yaxis_title <- input$yaxis_title1
  })

  plot_title <- formatLabels(
    .rdata[["plotDisag_explore_title"]],
    .rdata[["numTitleChars"]]
  )
  
  lang <- isolate(input$lang)
  
  plot_data <- plotData %>% 
    mutate(
      subgroup = as.character(subgroup),
      subgroup = ifelse(dimension == "Subnational region", dimension, subgroup),
      country = map_chr(country, translate, lang),
    )
  
  if (HEATversion == "whodata") {
    plot_data <- mutate(
      plot_data,
      subgroup = map_chr(subgroup, translate, lang),

      indic_name = map_chr(indic, translate, lang),
      dimension = map_chr(dimension, translate, lang)
    )
  }
  
  data_total <- plot_data %>% 
    mutate(
      subgroup = if_else(dimension %in% .rdata[["geo_dimension"]], dimension, subgroup)
    )
  
  scale_colors <- setNames(data_total$colors, data_total$subgroup)
  scale_shapes <- setNames(data_total$shapes, data_total$subgroup)
  scale_breaks <- data_total$subgroup
  scale_labels <- data_total$subgroup
  scale_alpha <- if_else(data_total$dimension %in% .rdata[["geo_dimension"]], 0.4, 0.75)
  
  p <- ggplot(
    data = data_total, 
    mapping = aes(
      x = estimate, 
      y = as.factor(year)
    )
  ) +
    geom_line() +
    geom_point(
      mapping = aes(
        fill = subgroup,
        shape = subgroup,
        alpha = subgroup
      ), 
      color = "grey80",
      size = 4
    ) +
    scale_shape_manual(
      values = scale_shapes,
      breaks = scale_breaks,
      name = "", 
      labels = scale_labels
    ) +
    scale_fill_manual(
      breaks = scale_breaks, 
      values = scale_colors,
      name = "",
      labels = scale_labels
    ) +
    scale_alpha_manual(
      values = scale_alpha,
      guide = "none"
    ) 
  
  if (!is.null(input$long_names1) && input$long_names1 == FALSE) {
    form <- "indic ~ dimension"
  } else {
    form <- "indic_name ~ dimension"
  }
  
  p <- p +
    facet_grid(
      facets = form, 
      labeller = splitLabels
    ) +
    labs(x = xaxis_title, y = yaxis_title, title = plot_title) +
    heat_theme() +
    guides(
      shape = guide_legend(
        ncol = 5,
        byrow = TRUE
      )
    )
  
  if (xaxis_min != "" || xaxis_max != "") {
    p <- adjust_axes(p, Xaxismin = xaxis_min, Xaxismax = xaxis_max)
  }
  
  return(p)
}
