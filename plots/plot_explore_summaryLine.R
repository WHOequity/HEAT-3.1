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

plotSummaryLine_explore_ggplot <- function(plotData, session){
  #  Plot 4: Line chart for a single country (Disaggregation of data)
  
  plot_data <- plotData
  
  summary_measure <- plot_data$measure[1]
  measure_name <- gsub("\\s\\(.*\\)$", "", translate(plot_data$measure[1], isolate(input$lang)))
  
  log_scale <- plot_data$logscale[1]
  
  data_years <- paste(sort(unique(plot_data$year)), collapse = ", ")
  sources <- paste(sort(unique(plotData$source)), collapse = " & ")
  
  isolate({
    xaxis_title <- input$xaxis_title2 %||% ""
    
    yaxis_title <- input$yaxis_title2 %||% measure_name
    yaxis_min <- input$axis_limitsmin2
    yaxis_max <- input$axis_limitsmax2    
    
    lang <- input$lang
  })

  
  plot_title <- formatLabels(
    .rdata[["plotSummary_explore_title"]],
    .rdata[["numTitleChars"]]
  )
  
  plot_data <- mutate(
    plot_data, 
    year = as.integer(year)
  )
  
  if (HEATversion == "whodata") {
    plot_data <- mutate(
      plot_data,
      indic_name = map_chr(indic, translate, lang),
      indic_title = indic_name,
      dimension = map_chr(dimension, translate, lang)
    )
  }
  
  num_years <- length(unique(plot_data$year))
  error_width <- if (num_years > 2) 0.5 else 0.25
  
  if (num_years == 1) {
    tmp1 <- plot_data
    tmp1$year <- tmp1$year + 1
    tmp1$inequal <- NA
    tmp1$se.lowerci <-NA
    tmp1$se.upperci <- NA

    tmp2 <- plot_data
    tmp2$year <- tmp1$year - 2
    tmp2$inequal <- NA
    tmp2$se.lowerci <- NA
    tmp2$se.upperci <- NA

    plot_data <- rbind(plot_data, tmp1, tmp2)
  }
  
  ci_type <-  c("se.lowerci", "se.upperci")
  
  if (!is.null(input$long_names2) && input$long_names2 == FALSE) {
    indic_type <- "indic"
  } else {
    indic_type <- "indic_name"
  }
  
  form <- paste(indic_type, " ~ dimension")
  
  scale_colors <- setNames(
    .rdata[["pal_single"]][c("red", "blue", "green", "purple", "orange")],
    sort(unique(plot_data[[indic_type]]))
  )
  
  scale_shapes <- setNames(
    c(16, 15, 18, 17, 25),
    sort(unique(plot_data[[indic_type]]))
  )
  
  p <- ggplot(
    data = plot_data, 
    mapping = aes_string(
      x = "year",
      y = "inequal", 
      group = indic_type, 
      color = indic_type,
      shape = indic_type,
      fill = indic_type
    )
  ) +
    geom_line(size = 1) +
    geom_point(size = 4) +
    geom_errorbar(
      mapping = aes_string(
        ymin = ci_type[1], 
        ymax = ci_type[2]
      ),   
      width = error_width, 
      alpha = input$summary_error_bars
    ) +
    labs(
      x = xaxis_title,
      y = yaxis_title,
      title = plot_title
    ) +
    scale_x_continuous(
      breaks = sort(unique(plot_data$year))
    ) + 
    scale_color_manual(
      values = scale_colors
    ) + 
    scale_fill_manual(
      values = scale_colors
    ) + 
    scale_shape_manual(
      values = scale_shapes
    ) +
    guides(
      color = guide_legend(ncol = 1)
    ) +
    facet_grid(
      facets = dimension ~ .,
      labeller = labeller(
        dimension = function(labels) gsub(" ", "\n", labels, fixed = TRUE)
      )
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

  if (log_scale == 1) {
    if (any(c(yaxis_min, yaxis_max) <= 0)) {
      p <- p + coord_cartesian()
    }
    
    scale_breaks <- unique(c(1, round(pretty(c(plot_data$se.lowerci, plot_data$se.upperci), n = 5))))
    scale_breaks <- scale_breaks[scale_breaks > 0]
    
    p <- p + 
      geom_hline(
        yintercept = 1,
        alpha = 0
      ) + 
      scale_y_log10(
        breaks = scale_breaks,
        labels = scale_breaks
      ) +
      ylab(
        label = gsub(")", ", axis log-scaled)", yaxis_title)
      )
  }
  
  return(p)
}
