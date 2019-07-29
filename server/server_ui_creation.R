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
#******************************************************************************
# FUNCTIONS TO CREATE SELECTORS
#******************************************************************************
#******************************************************************************


focusCountry_selector <- function(id, multiple = FALSE){
  if (is.null(.rdata[['all_countries']])) {
    return()
  }
  
  lang <- input$lang

  countries <- .rdata[["all_countries"]]
  
  if (HEATversion == "whodata") {
    countries_labels <- vapply(
      .rdata[["all_countries"]],
      translate,
      character(1),
      lang = lang
    )
  } else {
    countries_labels <- countries
  }
  
  label <- translate("select", lang)
  
  selectInput(
    inputId = id, 
    label = label, # "Select setting (e.g. country, province, district)", 
    choices = setNames(countries, countries_labels), 
    multiple = multiple, 
    selected = .rdata[["focus_country"]] # selected
  )
}



focusIndicator_selector <- function(id, multiple = FALSE, core = FALSE,
                                    maxitems = 5, label = NULL) {
                                    # header = "Select health indicators") {

  if (is.null(.rdata[['focus_indicator']]) || 
      grepl("^\\s*$", .rdata[['focus_indicator']])) {
    indic <- "sba"
    choices <- "sba" #git842
  } else {
    indic <- .rdata[['focus_indicator']][1:maxitems]
    choices <- .rdata[['full_indicators']]
  }
  
  if (HEATversion == "whodata") {
    choices_labels <- vapply(choices, translate, character(1), lang = input$lang)
  } else {
    choices_labels <- names(choices)
  }
  
  # label <- translate("indicator", input$lang)
  
  # .rdata[["full_indicators"]] <<- setNames(choices, choices_labels)
  
  selectizeInput(
    inputId = id,
    label = label,
    choices  = setNames(choices, choices_labels),
    selected = indic,
    multiple = TRUE,
    options = list(maxItems = maxitems)
  )
}


focusInequalType_selector <- function(id, multiple = FALSE) {
  if (!inherits(.rdata[["maindata"]], "data.frame")) {
    choices <- NULL
    choices_labels <- NULL
    sel <- NULL
  } else {
    choices <- .rdata[["summary_measures_all"]]
    choices_labels <- vapply(choices, translate, character(1), input$lang)
    sel <- .rdata[["focus_inequal_type"]]
  }
  
  label <- translate("measure", input$lang)
  
  selectInput(
    inputId = id, 
    label = label, 
    choices = setNames(choices, choices_labels), 
    selected = sel, 
    multiple = multiple
  )
}


focusDimension_selector <- function(id, multiple = FALSE, maxitems = 5, 
                                    label = NULL) {
  if (multiple) {
    focus_dimen <- .rdata[["focus_dimension"]][1]
  } else {
    focus_dimen <- .rdata[["focus_dimension"]]
  }
  
  choices <- sort(unique(.rdata[["equity_dimensions"]]))
  
  if (length(choices)) {
    if (HEATversion == "whodata") {
      choices_labels <- vapply(choices, translate, character(1), input$lang)
    } else {
      choices_labels <- choices
    }
    
    names(choices) <- choices_labels
  }
  
  selectizeInput(
    inputId = id,
    label = label,
    choices = choices,
    selected = focus_dimen,
    options = list(maxItems = maxitems)
  )
}


#******************************************************************************
#******************************************************************************
# CREATE SELECTORS, SLIDERS AND BUTTONS ---- 
#******************************************************************************
#******************************************************************************


# ----- Country selector -----------------------------------------------

output$focus_country_explore_component <- renderUI({
  
  focusCountry_selector("focus_country_explore")
  
})

output$focus_country_compare <- renderUI({
  
  focusCountry_selector("focus_country_compare")
  
})

# ----- Year and source selector -----------------------------------------------


output$focus_source_year_explore <- renderUI({
  lang <- input$lang
  source_label <- translate("sources", lang)
  years_label <- translate("years", lang)
  recent_label <- translate("recent", lang)
  
  list(
    selectInput(
      inputId = "focus_data_source_explore", 
      label = source_label, # "Select data sources",
      choices = .rdata[['data_sources']], #c("All", "DHS", "MICS"),
      selected = .rdata[['focus_data_source']],
      multiple = TRUE
    ),
    tags$label(class = "control-label", years_label), # "Select years"),
    checkboxInput(
      inputId = "mostrecent_explore", 
      label = recent_label, # "Most recent year", 
      value = .rdata[['mostrecent']]
    ),
    
    conditionalPanel( 
      condition = "!input.mostrecent_explore",  
      selectInput(
        inputId = "focus_year_explore", 
        label = NULL, 
        choices = .rdata[['all_years']], 
        multiple = TRUE, 
        selected = .rdata[['focus_year']]
      )
    )
  )
})

output$focus_source_year_explore_map <- renderUI({
  lang <- input$lang
  source_label <- translate("sources", lang)
  years_label <- translate("years", lang)
  recent_label <- translate("recent", lang)
  
  list(
    # conditionalPanel(condition = "input.assessment_panel == 'datatable' | input.assessment_panel == 'dataplot'",
    selectInput(
      inputId = "focus_data_source_explore_map", 
      label = source_label, # "Select data sources",
      choices = .rdata[["data_sources"]], # c("All", "DHS", "MICS"),
      selected = .rdata[["focus_data_source"]],
      multiple = TRUE
    ),
    tags$label(class="control-label", years_label), # "Select year"),
    checkboxInput(
      inputId = "mostrecent_explore_map", 
      label = recent_label, # "Most recent year",
      .rdata[["mostrecent"]]
    ),
    
    conditionalPanel( 
      condition = "!input.mostrecent_explore_map",  
      
      selectInput(
        inputId = "focus_year_explore_map", 
        label = NULL, 
        choices = .rdata[["all_years"]], 
        multiple = FALSE, 
        selected = .rdata[["focus_year"]][1]
      )
    )
  )
})

output$focus_source_year_explore_dtlmap <- renderUI({

  lang <- input$lang
  source_label <- translate("sources", lang)
  years_label <- translate("explore_detail_select_year", lang)
  recent_label <- translate("recent", lang)
  
  list(
    #conditionalPanel(condition = "input.assessment_panel == 'datatable' | input.assessment_panel == 'dataplot'",
    selectInput(
      inputId = "focus_data_source_explore_dtlmap", 
      label = source_label,  # "Select data sources",
      choices = .rdata[["data_sources"]], # c("All", "DHS", "MICS"),
      selected = .rdata[["focus_data_source"]],
      multiple = TRUE
    ),
    tags$label(
      class = "control-label", 
      years_label # "Select year"
    ),
    checkboxInput(
      inputId = "mostrecent_explore_dtlmap", 
      label = recent_label, # 'Most recent year', 
      .rdata[["mostrecent"]]
    ),
    conditionalPanel( 
      condition = "!input.mostrecent_explore_dtlmap",  
      
      selectInput(
        inputId = "focus_year_explore_dtlmap", 
        label = NA, 
        choices = .rdata[["all_years"]], 
        multiple = FALSE, 
        selected = .rdata[["focus_year"]][1]
      )
    )
  )
})







# ----- Indicator selector -----------------------------------------------


output$focus_indicator_explore_component <- renderUI({
  focusIndicator_selector(
    "focus_indicator_explore", 
    multiple = TRUE, 
    core = FALSE,
    label = translate("explore_disag_select_indicator", input$lang)
  )
})
outputOptions(output, "focus_indicator_explore_component", suspendWhenHidden = FALSE)

output$focus_indicator_explore_map <- renderUI({
  focusIndicator_selector(
    id = "focus_indicator_explore_map",
    multiple = FALSE,
    core = FALSE, 
    maxitems = 1,
    label = translate("explore_map_select_indicator", input$lang)
  )
})
#outputOptions(output, "focus_indicator_explore_map", suspendWhenHidden = FALSE)

output$focus_indicator_explore_plotdtl_component <- renderUI({
  focusIndicator_selector(
    id = "focus_indicator_explore_plotdtl", 
    multiple = TRUE,
    core = FALSE, 
    maxitems = 3,
    label = translate("explore_detail_select_indicator", input$lang)
  )
})
outputOptions(output, "focus_indicator_explore_plotdtl_component", suspendWhenHidden = FALSE)


# ----- Dimension selector -----------------------------------------------

output$focus_dimension_explore_component <- renderUI({
  focusDimension_selector(
    id = "focus_dimension_explore", 
    multiple = TRUE, 
    maxitems = 5,
    label = translate("explore_disag_select_inequal", input$lang)
  )
})

output$focus_dimension_explore_dtl <- renderUI({
  focusDimension_selector(
    id = "focus_dimension_explore_dtl", 
    multiple = TRUE,
    maxitems = 1,
    label = translate("explore_detail_select_inequal", input$lang)
  )
})


# ----- Variable selector -----------------------------------------------



output$dataTableItems_explore <- renderUI({
  lang <- input$lang
  
  label <- translate("tablecontent", lang)
  
  choices <- vapply(
    .rdata[["all_table_variables"]]$table_vars,
    translate,
    character(1),
    lang = lang
  )
  
  list(
    selectInput(
      inputId = "dataTableItems",
      label = label, # "Select table content",
      choices = setNames(
        .rdata[["all_table_variables"]]$table_vars,
        choices
      ),
      selected = .rdata[["focus_table_variables"]],
      multiple = TRUE
    )
  )
})


# ----- Variable selector -----------------------------------------------


output$dataTableItemsSummary_explore <- renderUI({
  lang <- input$lang
  
  label <- translate("tablecontent", lang)
  
  choices <- vapply(
    .rdata[["all_table_variables_summary"]]$table_vars,
    translate,
    character(1),
    lang = lang
  )
  
  list(
    selectInput(
      inputId = "dataTableItemsSummary",
      label = label, # "Select table content",
      choices = setNames(
        .rdata[['all_table_variables_summary']]$table_vars,
        choices
      ),
      selected = .rdata[['focus_table_variables_summary']],
      multiple = TRUE
    )
  )
})


# ----- Plot type -----------------------------------------------

output$disag_plot_type_explore_component <- renderUI({
  lang <- input$lang
  
  label <- translate("graphtype", lang)
  choices <- setNames(
    c("Bar", "Line"),
    c(translate("bar", lang), translate("line", lang))
  )
  
  radioButtons(
    inputId = "disag_plot_type_explore",
    label = label, # "Select graph type",
    choices = choices,
    inline = TRUE,
    selected = isolate(input$disag_plot_type_explore) %||% "Line"
  )
})

output$disag_plot_type_compare <- renderUI({
  lang <- input$lang
  
  label <- translate("graphstyle", lang)
  choices <- setNames(
    c("points", "labels"),
    c(translate("points", lang), translate("labels", lang))
  )
  
  radioButtons(
    inputId = "disag_plot_summary_pts", 
    label = label, # "Select graph style",
    choices = choices,
    inline = TRUE,
    selected = isolate(input$disag_plot_summary_pts) %||% "points"
  )
})


# ----- Plot type -----------------------------------------------

output$disag_plot_mode_explore_dtl <- renderUI({
  label <- translate("graphmode", input$lang)
  
  radioButtons(
    inputId = "disag_plot_mode_explore_dtl", 
    label = label, # "Select graph mode",
    choices = c("Static" = "ggplot"), #,"Interactive" = "hc"),
    inline = TRUE,
    selected = "ggplot"
  )
})

# ----- Plot type -----------------------------------------------

output$disag_plot_mode_explore <- renderUI({
  
  radioButtons("disag_plot_mode_explore", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})


# ----- Disaggregated error bars -----------------------------------------------

output$disag_plot_error_bars <- renderUI({
  checkboxInput(
    inputId = "disag_error_bars",
    label = translate("95conf", input$lang),
    value = FALSE
  )
})


# ----- Summary Measure -----------------------------------------------

output$focus_summeasure_explore_summary_table <- renderUI({
  label <- translate("explore_table_select_measures", input$lang)
  
  choices <- .rdata[["summary_measures_all"]]
  choices_labels <- vapply(choices, translate, character(1), input$lang)
  
  selectInput(
    inputId = "focus_inequal_type_explore_table", 
    label = label, # "Select summary measures", 
    choices = setNames(choices, choices_labels),
    selected = .rdata[["focus_inequal_type"]], 
    multiple = TRUE
  )
})

# ----- Summary Measure -----------------------------------------------

output$focus_summeasure_explore_summary_plot <- renderUI({
  focusInequalType_selector("focus_inequal_type_explore_plot", multiple=FALSE)
})



# ----- Multiplier -----------------------------------------------

# No longer used
# output$summary_measures <- renderUI({
#   list(
#     tags$span(class="control-label", "Select estimate display"),
#     checkboxInput('summultiplier1', 'MLD and TI multiplied by 1000', TRUE),
#     checkboxInput('summultiplier2', 'RCI multiplied by 100', TRUE)#,
#     
#   )
# })


# ----- Download data-----------------------------------------------

output$downloadSummtable <- renderUI({ 
  theData <- datasetInequal()
  
  if(is.null(theData)){
    return()
  }
  if(nrow(theData)==0){
    return()
  } else {
    list(br(),
         actionButton("downloadSummtable", "Download data", class = "btn-primary"))
  }  
})



# ----- Summary plot type ---------------------------------------------

output$summary_plot_type_explore_component <- renderUI({
  lang <- input$lang
  
  radioButtons(
    inputId = "summary_plot_type_explore",
    label = translate("graphtype", lang),
    choices = setNames(
      c("Bar", "Line"),
      c(translate("bar", lang), translate("line", lang))
    ),
    inline = TRUE,
    selected = isolate(input$summary_plot_type_explore) %||% "Bar"
  )
})


output$summary_plot_mode_explore <- renderUI({
  lang <- input$lang
  
  radioButtons(
    inputId = "summary_plot_mode_explore",
    label = translate("graphmode", lang),
    choices = setNames(
      "ggplot", # "hc"),
      "Static"  # "Interactive")
    ),
    inline = TRUE,
    selected = "ggplot"
  )
})

# ----- Summary error bars ---------------------------------------------

output$summary_plot_error_bars <- renderUI({
  checkboxInput(
    inputId = "summary_error_bars",
    label = translate("95conf", input$lang),
    value = FALSE
  )
})

# ----- Summary error bars ---------------------------------------------

output$summary_plot_CI_type <- renderUI({
  radioButtons("summary_CI_type", NA,
               c("Analytic CI" = "analytic",
                 "Bootstrap CI" = "bootstrap"),
               inline=T,
               selected="analytic")
})



#******************************************************************************
# Compare inquality: sidepanel -----
#******************************************************************************



output$disag_plot_mode_compare <- renderUI({
  
  radioButtons("disag_plot_mode_compare", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})


output$summary_plot_mode_compare <- renderUI({
  
  radioButtons("summary_plot_mode_compare", "Select graph mode",
               c("Static" = "ggplot"), #,"Interactive" = "hc"),
               inline=T,
               selected="ggplot")
  
  
})






output$focus_indicator_compare <- renderUI({

  #indic<-ifelse(is.null(.rdata[['focus_indicator']]), focus_indicator, .rdata[['focus_indicator']])
  label <- translate("compare_disag_select_indicator", input$lang)
  
  choices <- .rdata[["full_indicators"]]
  
  if (length(choices)) {
    if (HEATversion == "whodata") {
      choices_labels <- vapply(choices, translate, character(1), input$lang)
    } else {
      choices_labels <- names(choices)
    }
    
    names(choices) <- choices_labels
  }
  
  selectInput(
    inputId = "focus_indicator_compare", 
    label = label, # "Select health indicator", 
    choices  = choices,
    multiple = FALSE, 
    selected = .rdata[["focus_indicator"]][1]
  )
})


output$focus_source_year_compare <- renderUI({
  lang <- input$lang
  label_sources <- translate("sources", lang)
  label_years <- translate("compare_disag_select_year", lang)
  label_recent <- translate("recent", lang)
  
  datsource <- .rdata[['data_sources']]
  focsource <- .rdata[['focus_data_source']]
  indx <- match(.rdata[['focus_data_source']], .rdata[['data_sources']])
  datsource <- c(datsource[indx], datsource[-indx])
  
  list(
    selectInput(
      inputId = "focus_data_source_compare", 
      label = label_sources, # "Select data sources",
      choices = datsource, # c("All", "DHS", "MICS"),
      selected = focsource,
      multiple = TRUE
    ),
    tags$label(class="control-label", label_years), # "Select year"),
    checkboxInput(
      inputId = "mostrecent_compare", 
      label = label_recent, # 'Most recent year',
      value = .rdata[['mostrecent']]
    ),
    
    conditionalPanel( 
      condition = "!input.mostrecent_compare",  
      selectInput(
        inputId = "focus_year_compare", 
        label = NULL, 
        choices = c(.rdata[['all_years']]), 
        multiple = FALSE, 
        selected = .rdata[['focus_year']][1]
      )
    )
  )
})



output$focus_summeasure_compare_summary <- renderUI({
  focusInequalType_selector("focus_inequal_type_compare", multiple = FALSE)
})


output$focus_dimension_compare <- renderUI({
  focus_dimen <- .rdata[["focus_dimension"]][1]
  
  label <- translate("compare_disag_select_inequal", input$lang)
  
  choices <- unique(.rdata[["equity_dimensions"]])

  if (length(choices)) {
    if (HEATversion == "whodata") {
      names(choices) <- vapply(choices, translate, character(1), input$lang)
    } else {
      choices <- choices
    }
  }
  
  selectInput(
    inputId = "focus_dimension_compare",
    label = label, # "Select inequality dimension",
    choices = choices,
    selected = focus_dimen,
    multiple = FALSE,
    selectize = TRUE
  )
})

output$benchmark_countries <- renderUI({
  countries <- .rdata[["benchmark_countries"]]
  focus <- .rdata[["focus_country"]]
  
  countries <- countries[!(countries %in% focus)]
  
  .rdata[["benchmark_countries"]] <<- countries
  
  label <- translate("comparison", input$lang)
  
  if (!is.null(countries)) {
    if (HEATversion == "whodata") {
      names(countries) <- vapply(countries, translate, character(1), input$lang)
    } else {
      names(countries) <- countries
    }
  }
  
  selectInput(
    inputId = "benchmark_countries", 
    label = label, # "Select comparison settings", 
    choices = countries,
    selected = countries,
    multiple = TRUE
  )
})

output$benchmarkWBgroup <- renderUI({
  label <- translate("income", input$lang)
  
  choices <- .rdata[["income_groups"]]
  
  if (length(choices)) {
    names(choices) <- vapply(choices, translate, character(1), input$lang)
  }
  
  selectInput(
    inputId = "benchmarkWBgroup", 
    label = label, # "Filter by country-income group",
    choices = choices,
    selected = .rdata[['focus_income_group']],
    multiple = TRUE
  )
})

output$benchmarkWHOregion <- renderUI({
  label <- translate("region", input$lang)
  
  choices <- .rdata[["who_regions"]]
  
  if (length(choices)) {
    names(choices) <- vapply(choices, translate, character(1), input$lang)
  }

  selectInput(
    inputId = "benchmarkWHOregion", 
    label = label, # "Filter by WHO Region",
    choices = choices,
    selected = .rdata[["focus_who_regions"]],
    multiple = TRUE
  )
})


output$benchmarkYears <- renderUI({
  lang <- input$lang
  label <- translate("years", lang)
  help_text <- translate("vary", lang)
  
  list(
    tagAppendAttributes(
      sliderInput(
        inputId = "benchmarkYears", 
        label = label, # 'Select years',
        min = 0, 
        max = 5, 
        value = 2, 
        step = 1,
        round = TRUE, 
        ticks = TRUE, 
        animate = FALSE
      ),
      class = "hide-small-ticks"
    ),
    helpText(
      help_text,
      # HTML("By how many years can the benchmark countries' data vary from the focus country's data?"),
      style = "color:#666666; font-size: 85%"
    )
  )
})

# output$disag_explore_map_container <- renderUI({
#   list(
#     div(id = "disag_explore_map_container")#,
#     #tags$footer(tags$script(src = "create_map.js"))
#   )
# 
# })
