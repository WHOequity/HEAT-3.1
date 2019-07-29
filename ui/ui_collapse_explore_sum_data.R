output$ui_collapse_explore_sum_data <- renderUI({
  lang <- input$lang
  
  # list(
  # commented out per git 678
  # tags$div(class="panel-group",
  #          tags$div(class="panel panel-default",
  #                   tags$div(class="panel-heading",
  #                            
  #                            tags$a(`data-toggle`="collapse", href="#collapse_explore_sum_data1", class="collapsesectionhead", "Summary measure options")
  #                   ),
  #                   tags$div( id="collapse_explore_sum_data1", class="panel-collapse collapse",
  #                             tags$div(class="panel-body",
  #                                      uiOutput("summary_measures")
  #                             )
  #                   )
  #          )
  # ),
  
  tags$div(
    class = "panel-group",
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        tags$a(
          `data-toggle` = "collapse", 
          `data-target` = "#collapse_explore_sum_data2", 
          class = "collapsesectionhead collapsed cursor-pointer",
          translate("tableopts", lang)    
        )
      ),
      tags$div( 
        id = "collapse_explore_sum_data2", 
        class = "panel-collapse collapse",
        tags$div(
          class = "panel-body",
          uiOutput("dataTableItemsSummary_explore"),
          tagAppendAttributes(
            sliderInput(
              inputId = "sumsigfig2", 
              label = translate("explore_table_sig_figs", lang),
              min = 0, 
              max = 5, 
              value = 1,
              round = TRUE,
              width = "100%"
            ),
            class = "hide-small-ticks"
          )
        )
      )
    )
  )
})
