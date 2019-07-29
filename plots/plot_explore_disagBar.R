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


plotDisagBar_explore_ggplot <- function(plotData, session) {
  # Plot 1: Barchart for a single country (Disaggregation of data)

    # this is here to trigger a change if the title changes
  if (is.null(input$disag_error_bars)) {
    return()
  }
  
  isolate({
    xaxis_title <- input$xaxis_title1 %||% ""
    
    yaxis_title <- input$yaxis_title1 %||% ""
    yaxis_min <- input$axis_limitsmin1
    yaxis_max <- input$axis_limitsmax1
    
    lang <- input$lang
    error_bars <- input[["disag_error_bars"]]
  })

  plot_title <- formatLabels(
    .rdata[["plotDisag_explore_title"]],
    .rdata[["numTitleChars"]],
    fixed = FALSE
  )
  
  add_text_labels <- !error_bars &&
    n_distinct(plotData$dimension) == 1 && 
    n_distinct(plotData$indic) == 1 &&
    n_distinct(plotData$subgroup) <= 7
  
  plot_data <- plotData %>%
    ungroup() %>% 
    mutate(
      subgroup = as.character(subgroup),
      subgroup = if_else(dimension %in% c(.rdata[["geo_dimension"]], "Subnational region"), dimension, subgroup)
    )
  
  if (HEATversion == "whodata") {
    plot_data <- mutate(
      plot_data,
      subgroup = map_chr(subgroup, translate, lang),
      country = map_chr(country, translate, lang),
      indic_name = map_chr(indic, translate, lang),
      dimension = map_chr(dimension, translate, lang)
    )
  }
  
  plot_data <- plot_data %>% 
    arrange(
      dimension, year, subgroup, estimate
    ) %>% 
    group_by(
      dimension, year
    ) %>% 
    mutate(
      grouping = seq_len(n()),
      n_subgroups = n_distinct(subgroup)
    ) %>% 
    ungroup() 
  
  num_years <- n_distinct(plot_data$year)
  binwidth <- if (num_years > 2) 0.75 else 0.35
  textwidth <- if (num_years > 2) 0.9 else 0.35
  
  dodge <- position_dodge(width = 0.9)
  
  scale_colors <- setNames(plot_data$colors, plot_data$subgroup)
  scale_shapes <- setNames(plot_data$shapes, plot_data$subgroup)
  scale_breaks <- plot_data$subgroup
  scale_labels <- plot_data$subgroup
  
  p <- ggplot(plot_data) +
    geom_col(
      mapping = aes(
        x = as.factor(year),
        fill = subgroup,
        y = estimate,
        group = grouping
      ),
      position = dodge,
      width = binwidth,
      color = "white",
      size = 0.01
    ) + 
    list(
      if (add_text_labels) {
        geom_text(
          aes(
            x = as.factor(year),
            y = estimate,
            group = grouping,
            label = format(round(estimate, 1), nsmall = 1)
          ),
          position = dodge,
          vjust = -0.75,
          size = 3,
          color = "grey50"
        )
      }
    ) +
    geom_errorbar(
      mapping = aes(
        x = as.factor(year),
        # label = round(estimate),
        fill = subgroup,
        group = grouping,
        ymin = lower_95ci, 
        ymax = upper_95ci
      ), 
      color = "grey50",  
      position = dodge,
      width = 0.25, 
      alpha = input$disag_error_bars
    ) + 
    scale_fill_manual(
      breaks = scale_breaks, 
      values = scale_colors,
      name = "",
      labels = scale_labels
    ) +
    labs(x = xaxis_title, y = yaxis_title, title = plot_title)
  
  if (!is.null(input$long_names1) && input$long_names1 == FALSE) {
    frmula <- "indic ~ dimension"
  } else {
    frmula <- "indic_name ~ dimension"
  }
  
  p <- p +
    facet_grid(frmula, scale = "free_y",  labeller = splitLabels) +
    labs(x = xaxis_title, y = yaxis_title, title = plot_title) +
    guides(
      fill = guide_legend(
        ncol = 5,
        byrow = TRUE,
        override.aes = list(linetype = 0)
      ),
      size = FALSE
    ) +
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
  
  return(p)
}

