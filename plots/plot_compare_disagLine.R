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

plotDisagLine_compare_ggplot <- function(plotData, session) {
  #  Plot 5: Horizontal line chart for benchmark countries (Disaggregation of data)
  
  isolate({
    xaxis_title <- input$xaxis_title3 %||% ""
    xaxis_min <- input$axis_limitsmin3
    xaxis_max <- input$axis_limitsmax3
    
    yaxis_title <- input$yaxis_title3 %||% ""
    
    lang <- input$lang
  })
  
  plot_title <- formatLabels(
    .rdata[["plotDisag_compare_title"]],
    .rdata[["numTitleChars"]]
  )
  
  plot_data <- plotData
  
  plot_data <- plot_data %>% 
    mutate(
      subgroup = as.character(subgroup),
      subgroup = ifelse(dimension == "Subnational region", "Subnational region", subgroup),
      dimension = map_chr(dimension, translate, lang)
    )
  
  if (HEATversion == "whodata") {
    plot_data <- mutate(
      plot_data,
      subgroup = map_chr(subgroup, translate, lang),
      indic_name = map_chr(indic, translate, lang),
      country = map_chr(country, translate, lang)
    )
  }
  
  plot_data <- plot_data %>% 
    mutate(
      country = paste0(country, " (", source, " ", year, ")"),
    ) %>% 
    dplyr::arrange(
      anchor, desc(country)
    ) %>% 
    ungroup() %>% 
    mutate(
      country = factor(country, levels = unique(country))
    )
  
  data_info <- plot_data %>% 
    select(subgroup, colors, shapes) %>% 
    distinct()
  
  indictype <- "indic_name"
  p <- ggplot(plot_data, aes(estimate, country)) + 
    geom_line()
  
  
  if (plot_data$dimension[1] %in% .rdata[["geo_dimension"]]) {
    p <- p + 
      geom_point(
        mapping = aes(
          fill = subgroup
        ), 
        size = 4,
        alpha = 0.5, 
        color = data_info$colors[1]
      ) +
      scale_fill_manual(
        breaks = as.character(data_info$subgroup[1]), 
        values = data_info$colors,
        name = "",
        labels = plot_data$dimension[1]
      )
  } else {
    p <- p + 
      geom_point(
        mapping = aes(
          fill = subgroup, 
          shape = subgroup, 
          alpha = subgroup
        ), 
        size = 4, 
        color = "grey"
      )
    
    p <- p + 
      scale_shape_manual(
        breaks = data_info$subgroup, 
        values = data_info$shapes,
        name = "", 
        labels = data_info$subgroup
      ) +
      scale_fill_manual(
        breaks = data_info$subgroup, 
        values = data_info$colors,
        name = "",
        labels = data_info$subgroup
      ) +
      scale_alpha_manual(
        values = rep(0.75, nrow(plot_data)), 
        guide = "none"
      ) +
      guides(
        shape = guide_legend(byrow = TRUE)
      ) 
  } 
  
  p <- p +
    labs(x = xaxis_title, y = yaxis_title, title = plot_title) + 
    facet_grid(paste0(indictype, " ~ ."), labeller = splitLabelsWide) + 
    heat_theme()
  
  if (xaxis_min != "" || xaxis_max != "") {
    p <- adjust_axes(p, Xaxismin = xaxis_min, Xaxismax = xaxis_max)
  }
  
  return(p)
}


