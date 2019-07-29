output$ui_collapse_compare_disag_plot <- renderUI({
  lang <- input$lang
  
  tags$div(
    class="panel-group",
    tags$div(
      class="panel panel-default",
      tags$div(
        class="panel-heading",
        tags$a(
          `data-toggle` = "collapse", 
          `data-target` = "#collapse_compare_disag_plot", 
          class = "collapsesectionhead collapsed cursor-pointer",
          translate("graphoptions", lang)
        )
      ),
      tags$div( 
        id = "collapse_compare_disag_plot", 
        class = "panel-collapse collapse",
        tags$div(
          class = "panel-body",
          
          # These sliders were originally in server_logic but were not getting
          # loaded if the compare tab was clicked first. Seems that the split second
          # that the condition was not TRUE caused the sliders not to show up
          
          #conditionalPanel(condition = "input.disag_plot_mode_compare == 'ggplot'",
          # sliderInput(
          #   "plot_height3",
          #   translate("graphheight", lang),
          #   min = 200, 
          #   max = 1500, 
          #   value = 650, 
          #   step = 100,
          #   round = TRUE,
          #   ticks = TRUE,
          #   animate = FALSE
          # ),
          # sliderInput(
          #   "plot_width3",
          #   translate("graphwidth", lang),
          #   min = 200,
          #   max = 1500,
          #   value = 650,
          #   step = 100,
          #   round = T,
          #   ticks = TRUE,
          #   animate = FALSE
          # ),
          tags$span(
            class = "control-label axis-range",
            translate("axisrange", lang)
          ),
          tags$div(
            class="axis-minmax",
            textInputRow(
              inputId = "axis_limitsmin3", 
              label = translate("axismin", lang),
              value = NULL
            ),
            textInputRow(
              inputId = "axis_limitsmax3", 
              label = translate("axismax", lang),
              value = NULL
            )
          ),
          tags$span(
            class = "control-label graph-names", 
            translate("graphnames", lang)
          ),
          #checkboxInput(inputId='long_names3', label='Use long health indicator names', value = TRUE),
          tags$div(
            class = "axis-title-label",
            textInput(
              inputId = "main_title3", 
              label = translate("graphtitle", lang),
              value = translate("titlesummary", lang)
            ),
            textInput(
              inputId = "xaxis_title3", 
              label = translate("axishtitle", lang),
              value = ""
            ),
            textInput(
              inputId = "yaxis_title3",
              label = translate("axisvtitle", lang),
              value = ""
            )
          )
        )
      )
    )
  )
})
