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

#******************************************************************************
# Downloading explore
#******************************************************************************

get_plot_footnote <- function() {
  if (.rdata[["HEATversion"]] == "upload") {
    note <- translate("heat_plus_graph_footnote", isolate(input$lang))
  } else {
    note <- translate("heat_graph_footnote", isolate(input$lang))
  }
  
  note <- str_replace_all(note, fixed("<br>"), "\n")
  note <- strsplit(note, "\\\\n")[[1]]
  
  tableGrob(
    as.matrix(strwrap(note, 150)),
    theme = ttheme_minimal(
      core = list(
        padding = unit(c(0, 2), "mm"),
        fg_params = list(
          x = 0,
          hjust = 0,
          fontsize = 9,
          fontface = "italic",
          col = "grey50"
        )
      )
    )
  )
  
  # textGrob( 
  #   translate("footnoteplot", isolate(input$lang)),
  #   x = 0.05,
  #   y = 0.75,
  #   hjust = 0,
  #   gp = gpar(
  #     fontface = "italic", 
  #     fontsize = 9, 
  #     col = "grey50",
  #     lineheight = 0.9
  #   )
  # )
}

output$btnDownloadDisagPlotData_explore_component <- renderUI({
  if (is.null(input$focus_country_explore) || 
      trimws(input$focus_country_explore) == "") {
    return()
  }
  
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadDisagPlotData_explore", 
      label = translationOutput("data1", "download_data"),
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataDisagModal_explore"
    ),
    actionButton(
      inputId = "btnDownloadDisagPlot_explore", 
      label = translationOutput("graph1", "download_graph"), # "Download graph", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle`= "modal",
      `data-target`="#dataplotModal_explore"
    )
  )
})

output$btnDownloadDisagPlotData_explore_dtl <- renderUI({
  if (is.null(input$focus_country_explore) || 
      trimws(input$focus_country_explore) == "") {
    return()
  }
  
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadDisagPlotData_explore_dtl", 
      label = translationOutput("data2", "download_data"), # "Download data", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataDisagModal_explore_dtl"
    ),
    actionButton(
      inputId = "btnDownloadDisagPlot_explore_dtl", 
      label = translationOutput("graph2", "download_graph"), # "Download graph", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#dataplotModal_explore_dtl"
    )
  )
})


output$btnDownloadDisagPlotData_explore_map <- renderUI({
  if (is.null(input$focus_country_explore) || 
      trimws(input$focus_country_explore) == "") {
    return()
  }
  

  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadDisagPlotData_explore_map",
      label = translationOutput("data0", "download_data"), # "Download data",
      class = "btn-primary",
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataDisagModal_explore_map"
    )#,
    # actionButton(
    #   inputId = "btnDownloadDisagPlot_explore_map",
    #   label = translationOutput("graph2", "download_graph"), # "Download graph",
    #   class = "btn-primary",
    #   icon = icon("download"),
    #   `data-toggle` = "modal",
    #   `data-target` = "#dataplotModal_explore_map"
    # )
  )
})


output$btnDownloadSummaryPlotData_explore <- renderUI({
  if (is.null(input$focus_country_explore) || 
      trimws(input$focus_country_explore) == "") {
    return()
  }
                 
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadSummaryPlotData_explore", 
      label = translationOutput("data3", "download_data"), # "Download data", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataSummaryModal_explore"
    ),
    actionButton(
      inputId = "btnDownloadSummaryPlot_explore", 
      label = translationOutput("graph3", "download_graph"), # "Download graph",
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#summplotModal_explore"
    )
  )
})
  

output$btnDownloadDisagPlotData_compare <- renderUI({
  if (is.null(input$focus_country_compare) || 
     trimws(input$focus_country_compare) == "") {
    return()
  }
   
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadDisagPlotData_compare", 
      label = translationOutput("data4", "download_data"), # "Download data", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataDisagModal_compare"
    ),
    actionButton(
      inputId = "btnDownloadDisagPlot_compare", 
      label = translationOutput("graph4", "download_graph"), # "Download graph", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compplot1Modal_compare"
    )
  )
})
                                                 
output$btnDownloadSummaryPlotData_compare <- renderUI({
  if (is.null(input$focus_country_compare) || 
      trimws(input$focus_country_compare) == "") {
    return()
  }
  
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadSummaryPlotData_compare", 
      label = translationOutput("data5", "download_data"), # "Download data", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataSummaryModal_compare"
    ),
    actionButton(
      inputId = "btnDownloadSummaryPlot_compare", 
      label = translationOutput("graph5", "download_graph"), # "Download graph", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compplot2Modal_compare"
    )
  )
})

# ----------------------------------------
# --- Explore disaggregated data
# ----------------------------------------

output$btnDownloadDisagData_explore <- renderUI({
  if (is.null(input$focus_country_explore) || 
      trimws(input$focus_country_explore) == "") {
    return()
  }
  
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadDisagData_explore",
      label = translationOutput("data6", "download_data"), # "Download data", 
      class = "btn-primary", 
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#datatableModal_explore"
    )
  )
})

output$btnStartDownloadDisagData_explore <- downloadHandler(
  filename = function() {

    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("disaggregated_data", '.', input$filetype1, sep='')
  },
  
  content = function(file) {
    
    isolate({
      
      dat<-getDisagData(indicator=input$focus_indicator_explore, 
                        stratifier=input$focus_dimension_explore,  # in hetkdb.R
                        countries=input$focus_country_explore, 
                        years=input$focus_year_explore, 
                        mostrecent=input$mostrecent_explore,
                        datasource=input$focus_data_source_explore)
      
      lang <- input$lang
    })
    

    
    dat <- dat %>% 
      select(
        country, year, source, indic, indic_name, dimension,
        subgroup, estimate, se, lower_95ci, upper_95ci,
        pop, flag, national, iso3, maxoptimum, indicator_scale,
        rankable, order, reference_subgroup
      ) %>% 
      rename(
        indicator_abbr = indic,
        `ci_lb` = lower_95ci,
        `ci_ub` = upper_95ci,
        setting = country,
        setting_average = national,
        population = pop,
        indicator_name = indic_name,
        favourable_indicator = maxoptimum,
        ordered_dimension = rankable,
        subgroup_order = order
      )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        subgroup = map2_chr(dimension, as.character(subgroup), ~ {
          if (..1 == "Subnational region") ..2 else translate(..2, lang)
        }),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    dat$flag[is.na(dat$flag)]<-""
    
    
    
    
    sep <- switch(input$filetype1, "csv" = ",", "tsv" = "\t")
  
    write_table_wcitation(
      dat, file,  sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    ) 
  }
)




output$btnStartDownloadDisagPlot_explore  <- downloadHandler(
  filename = function() { 
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("disaggregated_data", '.', input$disagPlotType_explore, sep='')
  },
  content = function(file) {
    

    plottype <- isolate(input$disag_plot_type_explore)
    plotfunc <- get(sprintf("plotDisag%s_explore_ggplot", plottype))
    p <- plotfunc(.rdata[["focus_plot_data_disag_explore"]])
    
    save_plot_wcitation(file, p, get_plot_footnote()) # .rdata[["plot_footnote"]])
  }
)   



output$btnStartDownloadDisagPlot_explore_dtl  <- downloadHandler(
  filename = function() { 
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("disaggregated_data", '.', input$disagPlotType_explore_dtl, sep='')
  },
  content = function(file) {
    
    isolate({
      focus_indicator_explore_plotdtl<- input$focus_indicator_explore_plotdtl
      sortBy_var <- input$disag_plot_explore_dtl_sort
      disag_plot_explore_dtl_subgroups <- input$disag_plot_explore_dtl_subgroups
      disag_plot_explore_dtl_showAVG <- input$disag_plot_explore_dtl_showAVG
      disag_plot_explore_dtl_showMedian <- input$disag_plot_explore_dtl_showMedian
      disag_plot_mode_explore_dtl <- input$disag_plot_mode_explore_dtl
    })

    focus_indic <- focus_indicator_explore_plotdtl[focus_indicator_explore_plotdtl %in% unique(.rdata[['focus_plot_data_disag_dtl_explore']]$indic)]
    
    plot_data <- mutate(
      .rdata[["focus_plot_data_disag_dtl_explore"]],
      estimate = ifelse(estimate == -9999, NA, estimate)
    )
    
    p <- plotDetailBar_explore_ggplot(
      plot_data, #.rdata[['focus_plot_data_disag_dtl_explore']],
      sortBy = sortBy_var,
      regs = disag_plot_explore_dtl_subgroups,
      showAVG = disag_plot_explore_dtl_showAVG,
      showMedian = disag_plot_explore_dtl_showMedian,
      #addGroupNames = disag_plot_explore_dtl_showNames,
      indicSort = focus_indic
    )
    
    # p <-  .rdata[["disag_plot_explore_dtl"]]
    n_subgroups <- .rdata[["focus_plot_data_disag_dtl_explore"]] %>% 
      pull(subgroup) %>% 
      n_distinct()
    
    plot_height <- if (n_subgroups > 200) {
      260
    } else if (n_subgroups > 100) {
      160
    } else if (n_subgroups > 50) {
      48
    } else {
      24
    }
    
    plot_proportions <- if (n_subgroups > 100) {
      c(0.99, 0.01)
    } else if (n_subgroups > 50) {
      c(0.95, 0.05)
    } else {
      c(0.9, 0.1)
    }
    
    save_plot_wcitation(file, p, get_plot_footnote(), height = plot_height, heights = plot_proportions)
  }
) 




output$btnStartDownloadDisagPlotData_explore <- downloadHandler(
  filename = function() {
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("disaggregated_data", '.', input$filetype_explore_disag, sep='')
  },
  content = function(file) {
    
    isolate({
      
    dat<-getDisagData(indicator=input$focus_indicator_explore, 
                        stratifier=input$focus_dimension_explore,  # in hetkdb.R
                        countries=input$focus_country_explore, 
                        years=input$focus_year_explore, 
                        mostrecent=input$mostrecent_explore,
                        datasource=input$focus_data_source_explore)
    
    lang <- input$lang
    })
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                   pop, flag, national,iso3, maxoptimum,indicator_scale,
                  rankable, order, reference_subgroup) %>% 
      rename(indicator_abbr = indic,
             `ci_lb` = lower_95ci,
             `ci_ub` = upper_95ci,
            setting = country,
            setting_average = national,
             population = pop,
             indicator_name = indic_name,
             favourable_indicator = maxoptimum,
             ordered_dimension = rankable,
             subgroup_order = order
             )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        subgroup = as.character(subgroup),
        subgroup = map2_chr(dimension, subgroup, ~ {
          if (..1 == "Subnational region") ..2 else translate(..2, lang)
        }),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    dat$flag[is.na(dat$flag)]<-""
    
    
    
    
    sep <- switch(input$filetype_explore_disag, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file,  sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)



output$btnStartDownloadDisagPlotData_explore_dtl <- downloadHandler(
  filename = function() {
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("disaggregated_data", '.', input$filetype_explore_disag_dtl, sep='')
  },
  content = function(file) {
    isolate({
      
      dat<- getDisagData(indicator=input$focus_indicator_explore_plotdtl, 
                               stratifier=input$focus_dimension_explore_dtl,  # in hetkdb.R
                               countries=input$focus_country_explore, 
                               years=input$focus_year_explore_dtlmap, 
                               mostrecent=input$mostrecent_explore_dtlmap,
                               datasource=input$focus_data_source_explore_dtlmap)
      
      lang <- input$lang
    })
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                  pop, flag, national,iso3, maxoptimum,indicator_scale,
                  rankable, order, reference_subgroup) %>% 
      rename(indicator_abbr = indic,
             `ci_lb` = lower_95ci,
             `ci_ub` = upper_95ci,
             setting = country,
             setting_average = national,
             population = pop,
             indicator_name = indic_name,
             favourable_indicator = maxoptimum,
             ordered_dimension = rankable,
             subgroup_order = order
      )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        subgroup = map2_chr(dimension, as.character(subgroup), ~ {
          if (..1 == "Subnational region") ..2 else translate(..2, lang)
        }),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    dat$flag[is.na(dat$flag)] <- ""
    
    sep <- switch(input$filetype_explore_disag_dtl, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file,  sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)


output$btnStartDownloadDisagPlotData_explore_map <- downloadHandler(
  filename = function() {
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("disaggregated_data_map", '.', input$filetype_explore_disag_map, sep='')
  },
  content = function(file) {
    isolate({
     
      dat<- getDisagData(indicator=input$focus_indicator_explore_plotdtl, 
                         stratifier="Subnational region",  # in hetkdb.R
                         countries=input$focus_country_explore, 
                         years=input$focus_year_explore_dtlmap, 
                         mostrecent=input$mostrecent_explore_dtlmap,
                         datasource=input$focus_data_source_explore_dtlmap)
      
      lang <- input$lang
    })
    
    dat <- select(dat, country, year, source, indic, indic_name, dimension,
                  subgroup, estimate, se, lower_95ci, upper_95ci,
                  pop, flag, national,iso3, maxoptimum,indicator_scale,
                  rankable, order, reference_subgroup) %>% 
      rename(indicator_abbr = indic,
             `ci_lb` = lower_95ci,
             `ci_ub` = upper_95ci,
             setting = country,
             setting_average = national,
             population = pop,
             indicator_name = indic_name,
             favourable_indicator = maxoptimum,
             ordered_dimension = rankable,
             subgroup_order = order
      )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        subgroup = map2_chr(dimension, as.character(subgroup), ~ {
          if (..1 == "Subnational region") ..2 else translate(..2, lang)
        }),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    dat$flag[is.na(dat$flag)] <- ""
    
    sep <- switch(input$filetype_explore_disag_dtl, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file,  sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)


# ----------------------------------------
# --- Explore summary data
# ----------------------------------------

output$btnDownloadSummaryData_explore <- renderUI({
  if (is.null(input$focus_country_explore) || 
      trimws(input$focus_country_explore) == "") {
    return()
  }
  
  tags$div(
    class = "button-holder",
    actionButton(
      inputId = "btnDownloadSummaryData_explore", 
      label = translationOutput("data7", "download_data"), # "Download data", 
      class = "btn-primary", 
      icon=icon("download"),
      `data-toggle`="modal",
      `data-target`="#summtableModal_explore"
    )
  )
})

output$btnStartDownloadSummaryData_explore <- downloadHandler(
  filename = function() {
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("summary_measures", '.', input$filetype2, sep='')
  },
  content = function(file) {
    
    isolate({
     dat <- getInequalData(indicator=input$focus_indicator_explore,  
                           stratifier=input$focus_dimension_explore, 
                           countries= input$focus_country_explore, 
                           years=input$focus_year_explore, 
                           mostrecent=input$mostrecent_explore,
                           datasource=input$focus_data_source_explore,  
                           inequal_types=input$focus_inequal_type_explore_table,
                           multiplier1 = input$summultiplier1,
                           multiplier2 = input$summultiplier2)
     lang <- input$lang
})
    
    # This should definitely be cleaned up another time. But for now
    # originally we had an analytic and boot SE for all. But combined
    # into one variable. All but three of the measures use analytic
    # but three use bootstrap. So I'm getting around this here by
    # taking the lower and upper CI for three measures and putting
    # the values in boot where they should be
    
    dat[,c("boot.se", "boot.lowerci", "boot.upperci")] <- NA
    
    # dat$boot.lowerci[dat$measure%in%c("mdb", "mdm", "idis")] <-
    #   dat$se.lowerci[dat$measure%in%c("mdb", "mdm", "idis")]
    # 
    # dat$boot.upperci[dat$measure%in%c("mdb", "mdm", "idis")] <-
    #   dat$se.upperci[dat$measure%in%c("mdb", "mdm", "idis")]
    # 
    # dat[dat$measure%in%c("mdb", "mdm", "idis"),
    #       c("se.lowerci", "se.upperci")] <- NA

    
  
    dat <- dat %>% 
      select(
        country, year, source, indic, indic_name, dimension,
        measure, measure_name, inequal, se, se.lowerci, se.upperci,
        estimate, ccode
      ) %>% 
      mutate(
        measure_name = map_chr(measure, translate, lang)
      ) %>% 
      rename(
        indicator_abbr = indic,
        setting = country,
        `ci_lb` = se.lowerci,
        `ci_ub` = se.upperci,
        #`bootstrap_95ci_lb` = boot.lowerci,
        #`bootstrap_95ci_ub` = boot.upperci,
        setting_average = estimate,
        estimate = inequal,
        se = se,
        #bootstrap_se = boot.se,
        iso3 = ccode,
        indicator_name = indic_name,
        measure_abbr = measure
      )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    sep <- switch(input$filetype2, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file,  sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)






# ----------------------------------------
# --- Explore summary plot
# ----------------------------------------


# output$btnDownloadSummaryPlot_explore <- renderUI({
#   thePlot <- "xx" #theDataPlot()
#   if(is.null(thePlot)){
#     return()
#   } else {
#     list(br(),
#          actionButton("btnDownloadSummaryPlot_explore", "Download graph", class = "btn-primary"))
#   }
# })



output$btnStartDownloadSummaryPlot_explore  <- downloadHandler(
  filename = function() { 
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("summary_measures", '.', input$summaryPlotType_explore, sep='')
  },
  content = function(file) {

    plottype <- isolate(input$summary_plot_type_explore)
    plotfunc <- get(sprintf("plotSummary%s_explore_ggplot", plottype))
    p <- plotfunc(.rdata[["focus_plot_data_summary_explore"]])
    
    g <- grid.arrange(
      p, 
      get_plot_footnote(),
      # translate("footnoteplot", isolate(input$lang)), # .rdata[["plot_footnote"]], 
      ncol = 1,
      nrow = 2, 
      heights = c(0.9, 0.1)
    )
    
    ggsave(file, g, width = 24, height = 24, units = "cm")
  }
)   





output$btnStartDownloadSummaryPlotData_explore <- downloadHandler(
  filename = function() {
    #iso3 <- getISO3(input$focus_country_explore)
    #if(length(iso3) == 0) iso3 <- toupper(substring(input$focus_country_explore, 1,5))
    paste("summary_measures", '.', input$filetype_explore_summary, sep='')
  },
  content = function(file) {
    
    isolate({
      dat <- getInequalData(indicator=input$focus_indicator_explore,  
                            stratifier=input$focus_dimension_explore, 
                            countries= input$focus_country_explore, 
                            years=input$focus_year_explore, 
                            mostrecent=input$mostrecent_explore,
                            datasource=input$focus_data_source_explore,  
                            inequal_types=input$focus_inequal_type_explore_plot,
                            multiplier1 = input$summultiplier1,
                            multiplier2 = input$summultiplier2)
      
      lang <- input$lang
    })
    
    dat <- dat %>% 
      select(
        country, year, source, indic, indic_name, dimension,
        measure, measure_name, inequal, se, se.lowerci, se.upperci,
        estimate, ccode
      ) %>% 
      mutate(
        measure_name = map_chr(measure, translate, lang)
      ) %>% 
      rename(
        indicator_abbr = indic,
        setting = country,
        `ci_lb` = se.lowerci,
        `ci_ub` = se.upperci,
        #`bootstrap_95ci_lb` = boot.lowerci,
        #`bootstrap_95ci_ub` = boot.upperci,
        setting_average = estimate,
        estimate = inequal,
        se = se,
        #bootstrap_se = boot.se,
        iso3 = ccode,
        indicator_name = indic_name,
        measure_abbr = measure
      )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    sep <- switch(input$filetype_explore_summary, "csv" = ",", "tsv" = "\t")
    write_table_wcitation(
      dat, file,  sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)




#******************************************************************************
# Downloading compare
#******************************************************************************

# ----------------------------------------
# --- Compare summary data
# ----------------------------------------



output$btnDownloadDisagData_compare <- renderUI({
  if (is.null(input$focus_country_compare) || 
      trimws(input$focus_country_compare) == "") {
    return()
  }

  tags$div(
    class = "button-holder",
    actionButton(
      "btnDownloadDisagData_compare", 
      label = translationOutput("data8", "download_data"), # "Download data", 
      class = "btn-primary",  
      icon = icon("download"),
      `data-toggle` = "modal",
      `data-target` = "#compdataModal_compare"
    )
  )
})



output$btnStartDownloadDisagData_compare <- downloadHandler(
  filename = function() {

    paste("benchmark_disaggregated_data", '.', input$filetype_benchmark, sep='')
  },
  content = function(file) {
    
    # TODO make this a function
    isolate({
    anchordata<-getDisagData(indicator=input$focus_indicator_compare, 
                             stratifier=input$focus_dimension_compare,  # in hetkdb.R
                             countries=input$focus_country_compare, 
                             years=input$focus_year_compare, 
                             mostrecent=input$mostrecent_compare,
                             datasource=input$focus_data_source_compare)
    
    lang <- input$lang
    
    
    #if(is.null(anchordata) || nrow(anchordata)==0) return()
    
    if(nrow(anchordata)>0) anchordata$anchor <- 1
    
    
    benchmarkdata <- NULL
    
    if(!is.null(input$benchmark_countries)){
      benchmarkdata <- getDisagData(indicator = input$focus_indicator_compare, 
                                    stratifier = input$focus_dimension_compare, 
                                    countries = input$benchmark_countries, 
                                    years =  input$focus_year_compare, 
                                    mostrecent = input$mostrecent_compare,
                                    datasource = input$focus_data_source_compare,
                                    elasticity = input$benchmarkYears,
                                    anchor_country = input$focus_country_compare)
    }
    
    if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
      benchmarkdata$anchor <- 0
     dat <- rbind(anchordata, benchmarkdata) 
    }else{
      
      dat <- anchordata
    }
})
    
    
    dat <- dat %>% 
      select(
        country, year, source, indic, indic_name, dimension,
        subgroup, estimate, se, lower_95ci, upper_95ci,
        popshare, flag, national,iso3, maxoptimum,indicator_scale,
        rankable, order, reference_subgroup
      ) %>% 
      rename(
        indicator_abbr = indic,
        `ci_lb` = lower_95ci,
        `ci_ub` = upper_95ci,
        population_share = popshare,
        indicator_name = indic_name,
        setting = country,
        setting_average = national,
        indicator_name = indic_name,
        favourable_indicator = maxoptimum,
        ordered_dimension = rankable,
        subgroup_order = order
      )
    
    if (HEATversion == "whodata") {
      dat <- mutate(
        dat,
        setting = map_chr(setting, translate, lang),
        subgroup = map2_chr(dimension, as.character(subgroup), ~ {
          if (..1 == "Subnational region") ..2 else translate(..2, lang)
        }),
        dimension = map_chr(dimension, translate, lang),
        indicator_name = map_chr(indicator_abbr, translate, lang)
      )
    }
    
    dat$flag[is.na(dat$flag)] <- ""
    
    sep <- switch(input$filetype_benchmark, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file, sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)




output$btnStartDownloadDisagPlot_compare  <- downloadHandler(
  filename = function() { 
    paste("benchmark_disaggregated_data", '.', input$disagPlotType_compare, sep='')
  },
  content = function(file) {

    
    # Here we use a ggplot
    p <- plotDisagLine_compare_ggplot(.rdata[["focus_plot_data_disag_compare"]])
    

    # this adds a little more space to the right plot margin because it was coming
    # to the edge of the graph but only if the graph is not a rectGrob (blank plot)
    
    if(!any(class(p)=="rect")){
    p <- p + theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5), "cm"))
    }
    
    g <- grid.arrange(
      p, 
      get_plot_footnote(), 
      ncol = 1, 
      nrow = 2,
      heights = c(0.9, 0.1)
    )
    
    ggsave(file, g, width=24, height=24, units="cm")
  }
)   







output$btnStartDownloadDisagPlotData_compare <- downloadHandler(
  filename = function() {
    paste("benchmark_disaggregated_data", '.', input$filetype_benchmark_disag, sep='')
  },
  content = function(file) {
    dat <- anchordata<-getDisagData(indicator=input$focus_indicator_compare, 
                             stratifier=input$focus_dimension_compare,  # in hetkdb.R
                             countries=input$focus_country_compare, 
                             years=input$focus_year_compare, 
                             mostrecent=input$mostrecent_compare,
                             datasource=input$focus_data_source_compare)
    
    lang <- isolate(input$lang)
    
    
    #if(is.null(anchordata) || nrow(anchordata)==0) return()
    
    if(nrow(anchordata)>0) anchordata$anchor <- 1
    
    
    benchmarkdata <- NULL
    
    if(!is.null(input$benchmark_countries)){
      benchmarkdata <- getDisagData(indicator = input$focus_indicator_compare, 
                                    stratifier = input$focus_dimension_compare, 
                                    countries = input$benchmark_countries, 
                                    years =  input$focus_year_compare, 
                                    mostrecent = input$mostrecent_compare,
                                    datasource = input$focus_data_source_compare,
                                    elasticity = input$benchmarkYears,
                                    anchor_country = input$focus_country_compare)
    }
    
    if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
      benchmarkdata$anchor <- 0
      dat <- rbind(anchordata, benchmarkdata) 
    }else{
      
      dat <- anchordata
    }
    
    
    if(is.null(dat)){
      dat <- data.frame(
        setting = NA,
        `year` = NA,
        `source` = NA,
        `indicator_abbr` = NA,
        `indicator_name` = NA,
        dimension = NA,
        subgroup = NA,
        estimate = NA,
        se = NA,
        `ci_lb` = NA,
        `ci_ub` = NA,
        population_share = NA,
        flag = NA,
        setting_average = NA,
        iso3 = NA,
        favourable_indicator = NA,
        indicator_scale = NA,
        ordered_dimension = NA,
        subgroup_order = NA,
        reference_subgroup = NA
      )
      
      dat <- filter(dat, !is.na(indicator_abbr)) %>% 
        rename(`ci_lb` = X95ci_lb, `ci_ub` = X95ci_ub)
      
      
    } else {
      
      
      dat <- dat %>% 
        select(
          country, year, source, indic, indic_name, dimension,
          subgroup, estimate, se, lower_95ci, upper_95ci,
          popshare, flag, national,iso3, maxoptimum,indicator_scale,
          rankable, order, reference_subgroup
        ) %>% 
        rename(
          indicator_abbr = indic,
          `ci_lb` = lower_95ci,
          `ci_ub` = upper_95ci,
          population = popshare,
          indicator_name = indic_name,
          setting = country,
          setting_average = national,
          favourable_indicator = maxoptimum,
          ordered_dimension = rankable,
          subgroup_order = order
        )
      
      if (HEATversion == "whodata") {
        dat <- mutate(
          dat,
          setting = map_chr(setting, translate, lang),
          subgroup = map2_chr(dimension, as.character(subgroup), ~ {
            if (..1 == "Subnational region") ..2 else translate(..2, lang)
          }),
          dimension = map_chr(dimension, translate, lang),
          indicator_name = map_chr(indicator_abbr, translate, lang)
        )
      }
      
      dat$flag[is.na(dat$flag)] <- ""
      
    }
    
    sep <- switch(input$filetype_benchmark_disag, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file, sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)






output$btnStartDownloadSummaryPlot_compare  <- downloadHandler(
  filename = function() { 
    paste(
      "benchmark_summary_measures", '.', input$summaryPlotType_compare, 
      sep = ''
    )
  },
  content = function(file) {
    p <- plotSummaryScatter_compare_ggplot(.rdata[["focus_plot_data_summary_compare"]])
    
    if (!any(class(p) == "rect")) {
      p <- p + theme(plot.margin = unit(c(0.5, 2, 0.5, 0.5), "cm"))
    }
    
    g <- grid.arrange(
      p,
      get_plot_footnote(),
      ncol = 1,
      nrow = 2,
      heights = c(0.9, 0.1)
    )
  
    ggsave(file, g, width = 24, height = 24, units = "cm")
  }
)   






output$btnStartDownloadSummaryPlotData_compare <- downloadHandler(
  filename = function() {
    paste("benchmark_summary_measures", '.', input$filetype_benchmark_summary, sep='')
  },
  content = function(file) {
    isolate({
      anchordata <- getInequalData(indicator=input$focus_indicator_compare,  
                                   stratifier=input$focus_dimension_compare, 
                                   countries=input$focus_country_compare, 
                                   years=input$focus_year_compare, 
                                   mostrecent=input$mostrecent_compare,
                                   datasource=input$focus_data_source_compare,  
                                   inequal_types=input$focus_inequal_type_compare,
                                   elasticity = input$benchmarkYears,
                                   multiplier1 = input$summultiplier1,
                                   multiplier2 = input$summultiplier2)
      
      lang <- input$lang
      
      #if(is.null(anchordata) || nrow(anchordata)==0) return()
      if (nrow(anchordata) > 0) anchordata$anchor <- 1
      
      benchmarkdata <- NULL
      
      if (!is.null(input$benchmark_countries)) {
        benchmarkdata <- getInequalData(
          indicator=input$focus_indicator_compare,  
          stratifier=input$focus_dimension_compare, 
          countries=input$benchmark_countries, 
          years=input$focus_year_compare, 
          mostrecent=input$mostrecent_compare,
          datasource=input$focus_data_source_compare,  
          inequal_types=input$focus_inequal_type_compare,
          elasticity = input$benchmarkYears,
          multiplier1 = input$summultiplier1,
          multiplier2 = input$summultiplier2,
          anchorCountry = input$focus_country_compare)
      }
      
      if (!is.null(benchmarkdata) && nrow(benchmarkdata) != 0) {
        benchmarkdata$anchor <- 0
        dat <- rbind(anchordata, benchmarkdata) 
      } else {
        dat <- anchordata
      }
      
    })
    
    # This is added to return a blank plot
    if (is.null(dat)) {
      dat <- data.frame(
        country = NA,
        `year` = NA,
        `source` = NA,
        `indicator_abbr` = NA,
        `indicator_name` = NA,
        dimension = NA,
        measure_abbr = NA,
        measure_name = NA,
        estimate = NA,
        analytic_se = NA,
        `ci_lb` = NA,
        `ci_ub` = NA,
        #boostrap_se = NA,
        #bootstrap_95ci_lb = NA,
        #bootstrap_95ci_ub = NA,
        setting_average = NA,
        iso3 = NA
      )
      
      dat <- filter(dat, !is.na(indicator_abbr))
    } else {
      dat <- dat %>% 
        select(
          country, year, source, indic, indic_name, dimension,
          measure, measure_name, inequal, se, se.lowerci, se.upperci,
          estimate, ccode
        ) %>% 
        mutate(
          measure_name = map_chr(measure, translate, lang)
        ) %>% 
        rename(
          indicator_abbr = indic,
          `ci_lb` = se.lowerci,
          `ci_ub` = se.upperci,
          #`bootstrap_95ci_lb` = boot.lowerci,
          #`bootstrap_95ci_ub` = boot.upperci,
          setting_average = estimate,
          setting = country,
          estimate = inequal,
          se = se,
          #bootstrap_se = boot.se,
          iso3 = ccode,
          indicator_name = indic_name,
          measure_abbr = measure
        )
      
      if (HEATversion == "whodata") {
        dat <- mutate(
          dat,
          setting = map_chr(setting, translate, lang),
          dimension = map_chr(dimension, translate, lang),
          indicator_name = map_chr(indicator_abbr, translate, lang)
        )
      }
    }
    
    sep <- switch(input$filetype_benchmark_summary, "csv" = ",", "tsv" = "\t")
    
    write_table_wcitation(
      dat, file, sep, 
      if (.rdata[["HEATversion"]] == "upload") {
        translate("heat_plus_table_footnote", isolate(input$lang))
      } else {
        translate("heat_table_footnote", isolate(input$lang))
      }
    )
  }
)
