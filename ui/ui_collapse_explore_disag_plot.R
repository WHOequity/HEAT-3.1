output$longindicname_disag_explore <- renderUI({
  if (.rdata[['HEATversion']] == "upload") {
    li <- NULL
  } else {
    # li <- list(
    #   checkboxInput(
    #     inputId = "long_names1",
    #     label = translate("longindicator", input$lang),
    #     value = TRUE
    #   )
    # )
    li <- NULL
  }
  
  li
})

output$ui_collapse_explore_disag_plot <- renderUI({
  lang <- input$lang
  
  tags$div(
    class = "panel-group",
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        tags$a(
          `data-toggle` = "collapse",
          `data-target` = "#collapse_explore_disag_plot",
          class = "collapsesectionhead collapsed cursor-pointer",
          translate("graphoptions", lang)
        )
      ),
      tags$div(
        id = "collapse_explore_disag_plot", 
        class = "panel-collapse collapse",
        tags$div(
          class = "panel-body",
          conditionalPanel(
            condition = "input.disag_plot_type_explore == 'Bar'", 
            uiOutput("disag_plot_error_bars")
          ),
          # conditionalPanel(condition = "input.disag_plot_mode_explore == 'ggplot'",
          # uiOutput("disag_plot_dimensions_explore"),
          tags$span(
            class = "control-label axis-range", 
            translate("axisrange", lang)
          ),
          # conditionalPanel("input.ai_plot_type == 'data_line'",
          #                  textInputRow(inputId="axis_limitsmin1", label="Axis-min", value = NULL)
          # ),
          tags$div(
            class = "axis-minmax",
            textInputRow(
              inputId = "axis_limitsmin1", 
              label = translate("axismin", lang),
              value = NULL
            ),
            textInputRow(
              inputId = "axis_limitsmax1",
              label = translate("axismax", lang),
              value = NULL
            )
          ),
          tags$span(
            class = "control-label graph-names",
            translate("graphnames", lang)
          ),
          uiOutput("longindicname_disag_explore"),
          tags$div(
            class="axis-title-label",
            textInput(
              inputId = "main_title1", 
              label = translate("graphtitle", lang),
              value = translate("titledisag", lang)
            ),
            textInput(
              inputId = "xaxis_title1", 
              label = translate("axishtitle", lang),
              value = ""
            ),
            textInput(
              inputId = "yaxis_title1", 
              label = translate("axisvtitle", lang),
              value = ""
            )
          )
        )
      )
    )
  )
})
