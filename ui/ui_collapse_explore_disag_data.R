output$ui_collapse_explore_disag_data <- renderUI({
  lang <- input$lang
  
  tags$div(
    class = "panel-group",
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        tags$a(
          `data-toggle` = "collapse", 
          `data-target` = "#collapse_explore_disag_data", 
          class = "collapsesectionhead collapsed cursor-pointer",
          translate("tableopts", lang)
        )
      ),
      tags$div(
        id = "collapse_explore_disag_data", 
        class = "panel-collapse collapse",
        tags$div(
          class = "panel-body",
          uiOutput("dataTableItems_explore"),
          tagAppendAttributes(
            sliderInput(
              inputId = "sumsigfig", 
              label = translate("decimals", lang),
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
