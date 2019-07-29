renderTranslation <- function(key) {
  renderText({
    # # req(input$lang)
    translate(key, input$lang)
  })  
}

doNotSuspend <- function(key) {
  outputOptions(output, key, suspendWhenHidden = FALSE)
}

# HEAT & HEAT Plus zoom modal ----
output$close_zoom_modal1 <- renderTranslation("download_close_label")
doNotSuspend("close_zoom_modal1")

output$close_zoom_modal2 <- renderTranslation("download_close_label")
doNotSuspend("close_zoom_modal2")

# HEAT & HEAT Plus downloads ----
for (i in 1:9) {
  output[[str_glue("download_data_title{i}")]] <- renderTranslation("download_data_title")
  
  output[[str_glue("download_data_msg{i}")]] <- renderTranslation("download_data_msg")
  
  output[[str_glue("download_data_close_msg{i}")]] <- renderTranslation("download_close_msg")
  
  output[[str_glue("download_data_label{i}")]] <- renderTranslation("download_data_label")
  
  output[[str_glue("download_format_csv{i}")]] <- renderTranslation("download_format_csv")
  
  output[[str_glue("download_format_tsv{i}")]] <- renderTranslation("download_format_tsv")

  output[[str_glue("download_data_button_label{i}")]] <- renderTranslation("download_button_label")
}

for (i in 1:5) {
  output[[str_glue("download_chart_title{i}")]] <- renderTranslation("download_chart_title")
  
  output[[str_glue("download_chart_msg{i}")]] <- renderTranslation("download_chart_msg")
  
  output[[str_glue("download_chart_close_msg{i}")]] <- renderTranslation("download_close_msg")
  
  output[[str_glue("download_chart_label{i}")]] <- renderTranslation("download_chart_label")
  
  output[[str_glue("download_chart_button_label{i}")]] <- renderTranslation("download_button_label")
}

# HEAT & HEAT Plus navbar ----
output$navbar_button_home <- renderTranslation("navbar_button_home")
doNotSuspend("navbar_button_home")

output$navbar_button_explore <- renderTranslation("navbar_button_explore")
doNotSuspend("navbar_button_explore")

output$navbar_button_compare <- renderTranslation("navbar_button_compare")
doNotSuspend("navbar_button_compare")

output$navbar_button_about <- renderTranslation("navbar_button_about")
doNotSuspend("navbar_button_about")

output$navbar_dropdown_manual <- renderTranslation("navbar_dropdown_manual")
doNotSuspend("navbar_dropdown_manual")

output$navbar_dropdown_notes <- renderTranslation("navbar_dropdown_notes")
doNotSuspend("navbar_dropdown_notes")

output$navbar_dropdown_compendium <- renderTranslation("navbar_dropdown_compendium")
doNotSuspend("navbar_dropdown_compendium")

output$navbar_dropdown_software <- renderTranslation("navbar_dropdown_software")
doNotSuspend("navbar_dropdown_software")

output$navbar_dropdown_versions <- renderTranslation("navbar_dropdown_versions")
doNotSuspend("navbar_dropdown_versions")

output$navbar_dropdown_license <- renderTranslation("navbar_dropdown_license")
doNotSuspend("navbar_dropdown_license")

output$navbar_dropdown_feedback <- renderTranslation("navbar_dropdown_feedback")
doNotSuspend("navbar_dropdown_feedback")

output$navbar_dropdown_acknowledgements <- renderTranslation("navbar_dropdown_acknowledgements")
doNotSuspend("navbar_dropdown_acknowledgements")

# HEAT landing page ----
if (HEATversion == "whodata") {
  output$navbar_whodata_title <- renderTranslation("navbar_whodata_title")
  
  output$heat_landing_title <- renderTranslation("heat_landing_title")
  doNotSuspend("heat_landing_title")
  
  output$heat_landing_acronym <- renderTranslation("heat_landing_acronym")
  doNotSuspend("heat_landing_acronym")
  
  output$heat_landing_subtitle <- renderTranslation("heat_landing_subtitle")
  doNotSuspend("heat_landing_subtitle")
  
  output$heat_landing_body <- renderTranslation("heat_landing_body")
  doNotSuspend("heat_landing_body")
  
  output$heat_landing_citation <- renderTranslation("heat_landing_citation")
  doNotSuspend("heat_landing_citation")
}

# HEAT Plus landing page ----
if (HEATversion == "upload") {
  output$navbar_plus_title <- renderTranslation("navbar_plus_title")
  
  output$heat_plus_landing_title <- renderTranslation("heat_plus_landing_title")  
  doNotSuspend("heat_plus_landing_title")
  
  output$heat_plus_landing_acronym <- renderTranslation("heat_plus_landing_acronym")
  doNotSuspend("heat_plus_landing_acronym")
  
  output$heat_plus_landing_subtitle <- renderTranslation("heat_plus_landing_subtitle")
  doNotSuspend("heat_plus_landing_subtitle")
  
  output$heat_plus_landing_body <- renderTranslation("heat_plus_landing_body")
  doNotSuspend("heat_plus_landing_body")
  
  output$heat_plus_landing_citation <- renderTranslation("heat_plus_landing_citation")
  doNotSuspend("heat_plus_landing_citation")
  
  output$heat_plus_landing_db_intro <- renderTranslation("heat_plus_landing_db_intro")
  doNotSuspend("heat_plus_landing_db_intro")
  
  output$heat_plus_landing_db_select <- renderTranslation("heat_plus_landing_db_select")
  doNotSuspend("heat_plus_landing_db_select")
  
  output$heat_plus_landing_db_upload <- renderTranslation("heat_plus_landing_db_upload")
  doNotSuspend("heat_plus_landing_db_upload")  
  
  output$heat_plus_landing_db_open <- renderTranslation("heat_plus_landing_db_open")
  doNotSuspend("heat_plus_landing_db_open")
  
  output$heat_plus_landing_db_file_links <- renderTranslation("heat_plus_landing_db_file_links")
  doNotSuspend("heat_plus_landing_db_file_links")

  output$heat_plus_landing_db_save <- renderTranslation("heat_plus_landing_db_save")  
  doNotSuspend("heat_plus_landing_db_save")
  
  output$heat_plus_landing_db_tooltip_format <- renderUI({
    # req(input$lang)
    
    tagList(
      tags$span(
        class = "glyphicon glyphicon-info-sign tooltip-upload",
        `data-toggle` = "tooltip",
        `data-original-title` = translate("heat_plus_landing_db_tooltip_format", input$lang) #"Databases require a specific format in order to be uploaded to HEAT Plus. The HEAT Plus template exemplifies the required structure, variables, order, etc. Please refer to the HEAT Plus user manual for further information on specific requirements and instructions on how to prepare databases for use in HEAT Plus."
      ),
      tags$script(HTML("$('#heat_plus_landing_db_tooltip_format [data-toggle=\"tooltip\"]').tooltip();"))
    )
  })
  doNotSuspend("heat_plus_landing_db_tooltip_format")
  
  output$heat_plus_landing_db_tooltip_name <- renderUI({
    # req(input$lang)
    
    tagList(
      tags$span(
        class = "glyphicon glyphicon-info-sign tooltip-upload",
        `data-toggle` = "tooltip",
        `data-original-title` = translate("heat_plus_landing_db_tooltip_name", input$lang)
      ),
      tags$script(HTML("$('#heat_plus_landing_db_tooltip_name [data-toggle=\"tooltip\"]').tooltip();"))
    )
  })
  doNotSuspend("heat_plus_landing_db_tooltip_name")
  
  output$heat_plus_landing_db_checking <- renderTranslation("heat_plus_landing_db_checking")
  doNotSuspend("heat_plus_landing_db_checking")
  
  output$heat_plus_landing_db_ready <- renderTranslation("heat_plus_landing_db_ready")
  doNotSuspend("heat_plus_landing_db_ready")
  
  output$heat_plus_landing_db_browse <- renderTranslation("heat_plus_landing_db_browse")
  doNotSuspend("heat_plus_landing_db_browse")
}

# HEAT & HEAT Plus control panel ----
output$control_panel_explore <- renderTranslation("control_panel_explore")
doNotSuspend("control_panel_explore")

output$control_panel_compare <- renderTranslation("control_panel_compare")
doNotSuspend("control_panel_compare")

output$benchmark <- renderText({
  # req(input$lang)
  translate("benchmark", input$lang)
})
outputOptions(output, "benchmark", suspendWhenHidden = FALSE)

output$disaggraphs1 <- renderText({
  # req(input$lang)
  translate("disaggraphs", input$lang)
})
outputOptions(output, "disaggraphs1", suspendWhenHidden = FALSE)

output$disaggraphs2 <- renderText({
  # req(input$lang)
  translate("disaggraphs", input$lang)
})
outputOptions(output, "disaggraphs2", suspendWhenHidden = FALSE)

output$disagdetail <- renderText({
  # req(input$lang)
  translate("disagdetail", input$lang)
})
outputOptions(output, "disagdetail", suspendWhenHidden = FALSE)

output$disagtables <- renderText({
  # req(input$lang)
  translate("disagtables", input$lang)
})
outputOptions(output, "disagtables", suspendWhenHidden = FALSE)

output$summarygraphs1 <- renderText({
  # req(input$lang)
  translate("summarygraphs", input$lang)
})
outputOptions(output, "summarygraphs1", suspendWhenHidden = FALSE)

output$summarygraphs2 <- renderText({
  # req(input$lang)
  translate("summarygraphs", input$lang)
})
outputOptions(output, "summarygraphs2", suspendWhenHidden = FALSE)

output$summarytables <- renderText({
  # req(input$lang)
  translate("summarytables", input$lang)
})
outputOptions(output, "summarytables", suspendWhenHidden = FALSE)

render_download_data <- renderText({
  # req(input$lang)
  translate("download_data", input$lang)
})

render_download_graph <- renderText({
  # req(input$lang)
  translate("download_graph", input$lang)
})

output$data1 <- render_download_data
outputOptions(output, "data1", suspendWhenHidden = FALSE)

output$data2 <- render_download_data
outputOptions(output, "data2", suspendWhenHidden = FALSE)

output$data0 <- render_download_data
outputOptions(output, "data0", suspendWhenHidden = FALSE)

output$data3 <- render_download_data
outputOptions(output, "data3", suspendWhenHidden = FALSE)

output$data4 <- render_download_data
outputOptions(output, "data4", suspendWhenHidden = FALSE)

output$data5 <- render_download_data
outputOptions(output, "data5", suspendWhenHidden = FALSE)

output$data6 <- render_download_data
outputOptions(output, "data6", suspendWhenHidden = FALSE)

output$data7 <- render_download_data
outputOptions(output, "data7", suspendWhenHidden = FALSE)

output$data8 <- render_download_data
outputOptions(output, "data8", suspendWhenHidden = FALSE)

output$graph1 <- render_download_graph
outputOptions(output, "graph1", suspendWhenHidden = FALSE)

output$graph2 <- render_download_graph
outputOptions(output, "graph2", suspendWhenHidden = FALSE)

output$graph3 <- render_download_graph
outputOptions(output, "graph3", suspendWhenHidden = FALSE)

output$graph4 <- render_download_graph
outputOptions(output, "graph4", suspendWhenHidden = FALSE)

output$graph5 <- render_download_graph
outputOptions(output, "graph5", suspendWhenHidden = FALSE)

output$graph6 <- render_download_graph
outputOptions(output, "graph6", suspendWhenHidden = FALSE)

# HEAT nav map tab ----
if (.rdata[["HEATversion"]] == "whodata") {
  output$disagmaps <- renderText({
    # req(input$lang)
    translate("disagmaps", input$lang)
  })
  outputOptions(output, "disagmaps", suspendWhenHidden = FALSE)
}

observeEvent(input$lang, {
  options(
    highcharter.lang = list(
      downloadJPEG = translate("download_menu_jpeg", input$lang),
      downloadPNG = translate("download_menu_png", input$lang),
      downloadSVG = translate("download_menu_svg", input$lang)
    )
  )
})
