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

plotSummaryScatter_compare_ggplot <- function(plotData, session){
  #  Plot 6: Scatter plot showing benchmark countries National Average against inequality
  isolate({
    use_points <- input$disag_plot_summary_pts == "points"
    
    xaxis_title <- if (input$xaxis_title4 == "") translate("tooltip_setting_avg", input$lang) else input$axis_title4
    xaxis_min <- input$xaxis_limitsmin4
    xaxis_max <- input$xaxis_limitsmax4
    
    yaxis_title <- input$yaxis_title4 %||% measure_name
    yaxis_min <- input$yaxis_limitsmin4
    yaxis_max <- input$yaxis_limitsmax4    
    
    lang <- input$lang
  })
  
  plot_data <- plotData %>% 
    mutate(
      measure_name = map_chr(measure, translate, lang),
      country = map_chr(country, translate , lang)
    )
  
  sum_measure <- plot_data$measure[1]
  measure_name <- gsub("\\s\\(.*\\)", "", plot_data$measure_name[1])
  
  log_scale <- plot_data$logscale[1]
  
 
  
  if (all(plot_data$anchor != 0)) {
    # this dummy is added with NA data except for the benchmark
    # so that the legend and color-coding works in the plot
    
    anchor_dummy <- plot_data[1, ]
    anchor_dummy[, names(anchor_dummy) != "anchor"]  <- NA
    anchor_dummy$anchor <- 0
    plot_data <- rbind(plot_data, anchor_dummy)
  }
  
  plot_pch <- c(19, 19) # used to be c(19, 15)
  plot_palette <- c("#00616B", "#6B0A00")
  plot_title <- formatLabels(
    .rdata[["plotSummary_compare_title"]],
    .rdata[["numTitleChars"]]
  )
    
  plot_data <- plot_data[order(plot_data$anchor), ]
  
  p <- ggplot(plot_data, aes(x = estimate, y = inequal), color = plot_palette) +
    labs(x = xaxis_title, y = yaxis_title, title = plot_title) +
    heat_theme()
 
  if (use_points) {
    p <- p + 
      geom_point(
        mapping = aes(
          color = as.factor(anchor),
          shape = as.factor(anchor)
        ), 
        size = 4
      )
  } else {
    p <- p + 
      geom_text(
        mapping = aes(
          label = ccode, 
          color = as.factor(anchor)
        ), 
        hjust = 0.5, 
        vjust = 0.5, 
        size = 3.5,
        show.legend = FALSE
      ) +
      geom_line(
        mapping = aes(
          color = as.factor(anchor)
        ),
        size = 0,
        alpha = 0
      ) +
      guides(
        colour = guide_legend(
          override.aes = list(
            size = 2,
            alpha = 1
          )
        )
      )    
  }
  
  p <- p + 
    scale_shape_manual(
      name = "",
      breaks = c(0, 1),
      labels = c(
        translate("tooltip_benchmark_settings", lang), # "Benchmark settings", 
        plot_data$country[plot_data$anchor == 1][1]
      ),
      values = plot_pch
    ) +
    scale_colour_manual(
      name = "",
      breaks = c(0, 1),
      labels = c(
        translate("tooltip_benchmark_settings", lang), # "Benchmark settings",
        plot_data$country[plot_data$anchor == 1][1]
      ),
      values = plot_palette
    ) 
  
  # if the user makes changes to any of the axes
  if (any(c(xaxis_min, xaxis_max, yaxis_min, yaxis_max) != "")) {
    p <- adjust_axes(
      p,
      x = TRUE, 
      y = TRUE, 
      Xaxismin = xaxis_min,
      Xaxismax = xaxis_max,
      Yaxismin = yaxis_min, 
      Yaxismax = yaxis_max
    )
  }
  
  if (log_scale == 1) {
    if (any(c(yaxis_min, yaxis_max) <= 0)) {
      p <- p + coord_cartesian()
    }
    
    scale_breaks <- sort(unique(c(1, unique(round(pretty(c(plot_data$inequal), n = 5), 1)))))
    scale_breaks <- scale_breaks[scale_breaks != 0]
    
    p <- p + 
      geom_hline(yintercept = 1, alpha = 0) +
      scale_y_log10(
        breaks = scale_breaks, 
        labels = scale_breaks
      ) +
      ylab(
        label = gsub(")", ", axis log-scaled)", yaxis_title)
      )
  }
  
  p
}


