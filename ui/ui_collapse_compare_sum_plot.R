output$ui_collapse_compare_sum_plot <- renderUI({
  lang <- input$lang
  
  tags$div(
    class = "panel-group",
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        tags$a(
          `data-toggle` = "collapse", 
          `data-target` = "#collapse_compare_sum_plot", 
          class = "collapsesectionhead collapsed cursor-pointer", 
          translate("graphoptions", lang)
        )
      ),
      tags$div(
        id = "collapse_compare_sum_plot", 
        class = "panel-collapse collapse",
        tags$div(
          class="panel-body",
          #conditionalPanel(condition = "input.summary_plot_mode_compare == 'ggplot'",
          # uiOutput("summary_plot_dimensions_compare"),
          tags$span(
            class = "control-label axis-range",
            translate("axisrange", lang)
          ),
          tags$div(
            class = "axis-minmax-dtl",
            textInputRow(
              inputId = "xaxis_limitsmin4", 
              label = translate("axishmin", lang),
              value = NULL
            ),
            textInputRow(
              inputId = "xaxis_limitsmax4",
              label = translate("axishmax", lang),
              value = NULL
            ),
            textInputRow(
              inputId = "yaxis_limitsmin4", 
              label = translate("axisvmin", lang),
              value = NULL
            ),
            textInputRow(
              inputId = "yaxis_limitsmax4",
              label = translate("axisvmax", lang),
              value = NULL
            )
          ),
          tags$span(
            class="control-label graph-names", 
            translate("graphnames", lang)
          ),
          tags$div(
            class = "axis-title-label",
            textInput(
              inputId = "main_title4", 
              label = translate("graphtitle", lang),
              value = ""
            ),
            textInput(
              inputId = "xaxis_title4", 
              label = translate("axishtitle", lang),
              # label = "X-axis label", 
              value = ""
            ),
            textInput(
              inputId = "yaxis_title4", 
              label = translate("axisvtitle", lang),
              # label = "Y-axis label", 
              value = ""
            )
          )
        )
      )
    )
  )
})
