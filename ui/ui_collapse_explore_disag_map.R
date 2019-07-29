output$longindicname_disag_explore_map <- renderUI({
  
  if(.rdata[['HEATversion']] == "upload"){
    li <- NULL
  }else{
    # li <- list(
    #   checkboxInput(inputId='long_names_map', label='Use long health indicator names', value = TRUE))
    li <- NULL
  }
  li
})



output$ui_collapse_explore_disag_map <- renderUI({
  
  tags$div(class="panel-group",
           tags$div(class="panel panel-default",
                    tags$div(class="panel-heading",
                             
                             tags$a(`data-toggle`="collapse", `data-target`="#collapse_explore_disag_map_plot", 
                                    class="collapsesectionhead collapsed cursor-pointer", translate("explore_map_graph_options", input$lang))
                    ),
                    tags$div( id="collapse_explore_disag_map_plot", class="panel-collapse collapse",
                              tags$div(class="panel-body",
                                       
                                       # conditionalPanel(condition="input.disag_plot_type_explore == 'Bar'", 
                                       #                  uiOutput("disag_plot_error_bars")),
                                       #conditionalPanel(condition = "input.disag_plot_mode_explore == 'ggplot'",
                                       #uiOutput("disag_plot_dimensions_explore"),#),
                                       
                                       
                                       
                                       #tags$span(class="control-label axis-range", "Select axis range"),
                                       #                        conditionalPanel("input.ai_plot_type == 'data_line'",
                                       #                                         textInputRow(inputId="axis_limitsmin1", label="Axis-min", value = NULL)
                                       #                        ),
                                       # tags$div(class="axis-minmax",
                                       #          textInputRow(inputId="axis_limitsmin_map", label="Axis minimum ", value = NULL),
                                       #          textInputRow(inputId="axis_limitsmax_map", label="Axis maximum", value = NULL)
                                       # ),
                                       tags$span(class="control-label graph-names", translate("graphnames", input$lang)),
                                       uiOutput("longindicname_disag_explore_map"),
                                       
                                       tags$div(class="axis-title-label",
                                                textInput(inputId = 'main_title_map', label = translate("graphtitle", input$lang), value = NULL)
                                                #textInput(inputId = 'sub_title_map', label = 'Subtitle', value = "")
                                                #textInput(inputId = 'yaxis_title_map', label = 'Vertical axis title', value = "")
                                       )
                              )
                    )
           )
  )
  
})