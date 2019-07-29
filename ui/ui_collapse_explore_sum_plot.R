output$longindicname_explore_sum <- renderUI({
  if (.rdata[['HEATversion']] == "upload") {
    li <- NULL
  } else {
    # li <- list(
    #   checkboxInput(
    #     inputId = "long_names2", 
    #     label = translate("longindicator", input$lang),
    #     value = TRUE
    #   )
    # )
    li <- NULL
  }
  
  li
})

output$ui_collapse_explore_sum_plot <- renderUI({
  lang <- input$lang
  
  tags$div(
    class="panel-group",
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        tags$a(
          `data-toggle` = "collapse", 
          `data-target` = "#collapse_explore_sum_plot", 
          class = "collapsesectionhead collapsed cursor-pointer", 
          translate("graphoptions", lang)
        ),
        tags$div( 
          id = "collapse_explore_sum_plot",
          class = "panel-collapse collapse",
          tags$div(
            class = "panel-body",
            uiOutput("summary_plot_error_bars"),
            # conditionalPanel(condition="input.summary_error_bars", uiOutput("summary_plot_CI_type")),
            # conditionalPanel(condition = "input.summary_plot_mode_explore == 'ggplot'",
            # uiOutput("summary_plot_dimensions_explore"),
            # checkboxInput(inputId='long_names2', label='Long health indicator names', value = FALSE),
            tags$span(
              class = "control-label axis-range",
              translate("axisrange", lang)
            ),
            # conditionalPanel("input.sumplot_type == 'data_line'",
            #                  textInputRow(inputId="axis_limitsmin2", label="Axis-min", value = NULL)
            # ),
            tags$div(
              class = "axis-minmax",
              textInputRow(
                inputId = "axis_limitsmin2", 
                label = translate("axismin", lang),
                value = NULL
              ),
              textInputRow(
                inputId = "axis_limitsmax2", 
                label = translate("axismax", lang),
                value = NULL
              )
            ),
            tags$span(
              class = "control-label graph-names",
              translate("graphnames", lang)
            ),
            uiOutput("longindicname_explore_sum"),
            tags$div(
              class = "axis-title-label",                
              textInput(
                inputId = "main_title2",
                label = translate("graphtitle", lang),
                value = ""
              ),
              textInput(
                inputId = "xaxis_title2",
                label = translate("axishtitle", lang),
                value = ""
              ),
              textInput(
                inputId = "yaxis_title2", 
                label = translate("axisvtitle", lang),
                value = ""
              )
            ) 
          )
        )
      )
    )
  )
})
  