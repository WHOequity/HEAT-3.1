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


plotDetailBar_explore_ggplot <- function(plotData, sortBy = NULL, regs = NULL, 
                                         showAVG = TRUE, showMedian = TRUE, 
                                         addGroupNames = TRUE, indicSort = NULL, 
                                         ...) {
  # Plot 1: Barchart for a single country (Disaggregation of data)
  # this is here to trigger a change if the title changes
  
  isolate({
    lang <- input$lang
    
    xaxis_title <- input$xaxis_title_dtl %||% ""
    
    yaxis_title <- input$yaxis_title_dtl %||% ""
    yaxis_min <- input$axis_limitsmin_dtl
    yaxis_max <- input$axis_limitsmax_dtl
    
    long_names <- TRUE # input$long_names_dtl
    
    disag_plot_explore_dtl_sort <- input$disag_plot_explore_dtl_sort
    disag_plot_explore_dtl_showAVG <- input$disag_plot_explore_dtl_showAVG
    disag_plot_explore_dtl_subgroups <- input$disag_plot_explore_dtl_subgroups
    
    descending <- input$sortOrder_ind_dim == "Descending"
    sort_by_dim <- input$sortBy_ind_dim == "Dimension"
      
    disag_plot_explore_dtl_showMedian <- input$disag_plot_explore_dtl_showMedian
    disag_plot_explore_dtl_showNames <- input$disag_plot_explore_dtl_showNames
    disag_indicSort <- input$focus_indicator_explore_plotdtl[input$focus_indicator_explore_plotdtl %in% unique(plotData$indic)]
    indics <- input$focus_indicator_explore
  })

  plot_title <- .rdata[["plotDisag_explore_title_dtl"]]
  
  if (.rdata[["is_modal"]]) {
    sort_by <- disag_plot_explore_dtl_sort
    regs <- disag_plot_explore_dtl_subgroups
    show_avg <- disag_plot_explore_dtl_showAVG
    show_median <- disag_plot_explore_dtl_showMedian
    add_group_names <- disag_plot_explore_dtl_showNames
    indic_sort <- disag_indicSort
  } else {
    sort_by <- sortBy %||% ""
    show_avg <- showAVG
    show_median <- showMedian
    add_group_names <- addGroupNames
    indic_sort <- indicSort
  }
  
  plot_data <- plotData
  
  if (HEATversion == "whodata") {
    plot_data <- mutate(
      plot_data,
      indic_name = map_chr(indic, translate, lang),
      indic_title = indic_name,
      subgroup = map2_chr(dimension, as.character(subgroup), ~ {
        if (.x != "Subnational region") translate(.y, lang) else .y
      }),
    )
  }
  
  plot_data <- plot_data %>% 
    group_by(indic_name) %>% 
    mutate(
      median_estimate = round(median(estimate, na.rm = TRUE), 2),
      range_estimate = abs(diff(range(estimate))),
      country = map_chr(country, translate, lang),
      subgroup = as.character(subgroup),
      dimension = map_chr(dimension, translate, lang)
    ) %>% 
    ungroup()
  
  if (!is.null(long_names) && long_names == FALSE) {
    # form <- "dimension ~ indic"
    plot_data <- mutate(plot_data, indic_sort = factor(indic, levels = isolate(input$focus_indicator_explore_plotdtl)))
  } else {
    indic_levels <- plot_data %>%  
      filter(indic %in% isolate(input$focus_indicator_explore_plotdtl)) %>% 
      select(indic, indic_name) %>% 
      distinct() %>% 
      .[match(isolate(input$focus_indicator_explore_plotdtl), .$indic), ] %>% 
      pull(indic_name)
    
    plot_data <- mutate(plot_data, indic_sort = factor(indic_name, levels = indic_levels))
    # form <- "dimension ~ indic_name"    
  }
  
  form <- "dimension ~ indic_sort"
  
  line_data <- count(plot_data, indic_sort, median_estimate, range_estimate, national)
  
  plot_data$colors[as.character(plot_data$subgroup) %in% regs] <- "red"
  
  # sorting subgroups ----
  if (!sort_by_dim) {
    sort_indices <- plot_data %>% 
      pull(subgroup) %>% 
      as.factor() %>% 
      levels() %>% 
      map_dbl(~ plot_data %>% 
                filter(subgroup == .x, indic == !!sort_by) %>% 
                pull(estimate) %>% 
                ifelse(length(.) == 0, NA_real_, .)
      ) %>% 
      order(na.last = FALSE)
    
    plot_data <- mutate(
      plot_data, 
      subgroup = as.factor(subgroup),
      subgroup = factor(subgroup, rev(levels(subgroup)[sort_indices]))
    )
  } else {
    plot_data <- mutate(
      plot_data, 
      subgroup = as.factor(subgroup),
      subgroup = factor(subgroup, rev(levels(subgroup)))
    )
  }
  
  wrap_width <- switch(n_distinct(plot_data$indic), "1" = 40, "2" = 25, "3" = 15)
  
  if (line_data$n[1] == 1) {
    width <- 0.25
  } else {
    width <- 0.5
  }
  
  p <- ggplot(plot_data, aes(subgroup, estimate)) + 
    geom_bar(
      mapping = aes(
        fill = subgroup
      ),
      stat = "identity",
      width = width
    ) + 
    geom_text(
      data = plot_data,
      aes(
        # fill = subgroup,
        y = estimate,
        label = format(round(estimate, 1), nsmall = 1)
      ),
      hjust = -0.25,
      size = 2,
      color = "grey50"
    ) +
    scale_fill_manual(
      values = setNames(
        plot_data$colors,
        plot_data$subgroup
      ),
      guide = "none"
    ) +
    coord_flip() +
    scale_x_discrete(drop = FALSE) + 
    facet_grid(
      form, 
      # scales = "free_x", 
      labeller = labeller(
        indic_sort = label_wrap_gen(wrap_width),
        dimension = function(labels) gsub(" ", "\n", labels)
      ),
      as.table = FALSE
    ) + 
    labs(
      title = plot_title,
      x = yaxis_title,
      y = xaxis_title
      # y = gsub(" ", "\n", yaxis_title, fixed = TRUE)
    ) +
    heat_theme() +
    theme(
      panel.spacing.y = unit(2, "cm")
    )
  
  if (show_median) {
    p <- p + 
      geom_hline(
        data = line_data, 
        aes(yintercept = median_estimate), 
        size = 0.25, 
        color = "#ee7600"
      ) +    
      geom_text(
        data = line_data, 
        mapping = aes(
          y = median_estimate, 
          x = max(line_data$n), 
          label = paste0(
            "\n", translate("tooltip_median", isolate(input$lang)), ": ", # "Median: ", 
            median_estimate
          )
        ), 
        angle = -90,
        hjust = 0,
        vjust = -0.25,
        color = "#ee7600",
        lineheight = 0.85
      ) 
  }
  
  if (show_avg) {
    p <- p + 
      geom_hline(
        data = line_data, 
        mapping = aes(
          yintercept = national
        ), 
        size = 0.25,
        color = "grey7"
      ) +    
      geom_text(
        data = line_data, 
        mapping = aes(
          y = national,
          x = max(n),
          label = paste0(
            "\n", translate("tooltip_setting_avg", isolate(input$lang)), ": ", #Setting average: ",
            national
          )
        ), 
        angle = -90, 
        hjust = 0,
        vjust = -0.25,
        color = "grey7", 
        lineheight = 0.85
      )
  }
  
  # if (yaxis_min != "" || yaxis_max != "") {
  p_build <- ggplot_build(p)
  axis_range <- p_build$layout$panel_scales_y[[1]]$range$range

  yaxis_min <- {
    if (yaxis_min == "") {
      min(map_dbl(p_build$layout$panel_scales_y, ~ .$range$range[1]))
    } else {
      as.numeric(yaxis_min)
    }
  }
  
  yaxis_max <- {
    if (yaxis_max == "") {
      max(map_dbl(p_build$layout$panel_scales_y, ~ .$range$range[2]))
    } else {
      as.numeric(yaxis_max)
    }
  }
  
  if (yaxis_max >= (axis_range[2] * 0.95)) {
    yaxis_max <- yaxis_max + ceiling(yaxis_max * 0.09)
  }
  
  p <- p + coord_flip(ylim = c(yaxis_min, yaxis_max))
  # }
  
  return(p)
}

