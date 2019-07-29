output$longindicname_disag_explore_dtl <- renderUI({
  
  if (.rdata[['HEATversion']] == "upload") {
    li <- NULL
  } else {
    # li <- list(
    #   checkboxInput(
    #     inputId = "long_names_dtl",
    #     label = translate("longindicator", input$lang),
    #     # label = "Use long health indicator names",
    #     value = TRUE
    #   )
    # )
    li <- NULL
  }
  
  li
})

output$disag_plot_explore_dtl_btn <- renderUI({
  lang <- input$lang
  
  tags$div(
    class = "panel-group",
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading dtl-panel",
        tags$a(
          `data-toggle` = "collapse",
          `data-target` = "#collapse_explore_dtl_plot", 
          class = "collapsesectionhead collapsed cursor-pointer", 
          translate("graphoptions", lang)
        )
      ),
      tags$div(
        id = "collapse_explore_dtl_plot", 
        class = "panel-collapse collapse",
        tags$div(
          class = "panel-body",
          fluidRow(
            column(
              width = 7,
              class = "dtl-radio", 
              radioButtons(
                "sortBy_ind_dim", 
                label = translate("sortby", lang),
                choices = setNames(
                  c("Dimension", "Indicator"),
                  c(translate("inequaldim", lang), translate("healthind", lang))
                ),
                selected = "Dimension"
              )
            ),
            column(
              width = 5,
              radioButtons(
                inputId = "sortOrder_ind_dim", 
                label = translate("sortorder", lang),
                choices = setNames(
                  c("Ascending", "Descending"),
                  c(translate("ascend", lang), translate("descend", lang))
                ),
                selected = "Ascending"
              )
            )
          ),                             
          fluidRow(
            column(
              width = 12,
              class = "longerselect",
              conditionalPanel(
                "input.sortBy_ind_dim == 'Indicator'",
                selectizeInput(
                  inputId = "disag_plot_explore_dtl_sort", 
                  label = translate("selectind", lang),
                  choices = getFullIndic(.rdata[['focus_indicator']]),
                  selected = getFullIndic(.rdata[['focus_indicator']])
                )
              )
            )
          ),
          fluidRow(
            id = "bottom-dtl-row",
            column(
              width = 10,    
              checkboxInput(
                "disag_plot_explore_dtl_showAVG",
                label = translate("showaverage", lang),
                value = FALSE
              ),
              checkboxInput(
                "disag_plot_explore_dtl_showMedian",
                label = translate("showmedian", lang),
                value = TRUE
              )
              # checkboxInput("disag_plot_explore_dtl_showNames",
                   #               label = "Show subgroup names", value = TRUE)
            )
          ),
          fluidRow(
            column(
              width = 12,
              class = "longerselect", 
              selectInput(
                "disag_plot_explore_dtl_subgroups", 
                label = translate("subgroup", lang),
                choices = setNames(
                c("", .rdata[["focus_plotdtl_subgroups"]]),
                c(translate("choose", lang), .rdata[["focus_plotdtl_subgroups"]])
                ),
                multiple = TRUE
              )
            )
          ),
          #conditionalPanel(condition = "input.disag_plot_mode_explore_dtl == 'ggplot'",
          # uiOutput("disag_plot_dimensions_explore_dtl"),#),
          fluidRow(
            column(
              width = 12,
              tags$span(
                class = "control-label axis-range",
                translate("axisrange", lang)
              ),
              # conditionalPanel(
              #   "input.ai_plot_type == 'data_line'",
              #   textInputRow(inputId = "axis_limitsmin1", label = "Axis-min", value = NULL)
              # ),
              tags$div(
                class="axis-minmax",
                textInputRow(
                  inputId = "axis_limitsmin_dtl", 
                  label = translate("axismin", lang),
                  value = NULL
                ),
                textInputRow(
                  inputId = "axis_limitsmax_dtl",
                  label = translate("axismax", lang),
                  value = NULL
                )
              ),
              tags$span(
                class = "control-label graph-names",
                translate("graphnames", lang)
              ),
              uiOutput("longindicname_disag_explore_dtl"),
              tags$div(
                class = "axis-title-label",
                textInput(
                  inputId = "main_title_dtl",
                  label = translate("graphtitle", lang),
                  value = translate("titledisag", lang)
                ),
                textInput(
                  inputId = "xaxis_title_dtl", 
                  label = translate("axishtitle", lang),
                  value = ""
                ),
                textInput(
                  inputId = "yaxis_title_dtl", 
                  label = translate("axisvtitle", lang),
                  value = ""
                )
              )
            )
          )
        )
      )
    )
  )
})