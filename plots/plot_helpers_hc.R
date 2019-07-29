





options(highcharter.theme = hc_theme_smpl(
  legend = list(enabled = FALSE, align = "center"),
  title = list(style = list(fontWeight = "normal"),  align = "center"),
  yAxis = list(endOnTick = TRUE), #53
  tooltip = list(useHTML = TRUE),
  plotOptions = list(scatter = list(marker = list(symbol = "circle")),
                     line = list(marker = list(enabled = TRUE)),
                     column = list(marker = list(symbol = "square"))), #48
  subtitle = list(style = list(fontWeight = "normal"),  align = "center")
))

# # http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
# roundUp <- function(x, percent = 10, logscale = FALSE){
# 
#   if(x<0 & !(logscale & x<=1)){
#     x <- x-(x*(percent/100))
#   }
#   
#   if(x>0 & !(logscale & x<=1)){
#     x <- x+(x*(percent/100))
#   }
#   
#   if(logscale && x <= 1){
#     x <- x/2
#   }
#   
#   x
#   
# }
# 
# roundDown <- function(x, percent = 10, logscale = FALSE){
#   
#   if(x<0 & !(logscale & x<=1)){
#     x <- x+(x*(percent/100))
#   }
#   
#   if(x>0 & !(logscale & x<=1)){
#     x <- x-(x*(percent/100))
#   }
#   
# 
#   if(logscale && x <= 1){
#     x <- x/2
#   }
#   
#   x
#   
# }


newMinMax <- function(minval, maxval = NULL, thepercent = 10, logscale = FALSE){
  
  
  if(!is.null(maxval)){
    thediff <- minval - maxval
    if(thediff == 0){
      diffper <- abs(minval) * thepercent/100
    }else{
      diffper <- abs(thediff) * (thepercent/100)
    }
    
    newmin <- minval - diffper
    newmax <- maxval + diffper
  }else{
    
    newmin <- minval - (abs(minval) * thepercent/100)
    newmax <- NULL
  }
  
  
  c(min = newmin, max = newmax)
}



minmaxAllPly <- function(plotDataChart, plotData, confInt = FALSE, logscale = FALSE, forceMinMax = FALSE) {
  #browser()
  if(!confInt){ #git772
    minmaxAll <- plotData %>% 
      summarise(maxReal = max(value, na.rm = TRUE),
                minReal = min(value, na.rm = TRUE)) 
  }else{
    # group_by(indic_name, dimension) %>% 
    minmaxAll <- plotData %>% 
      summarise(maxReal = max(c(se.lowerci, se.upperci), na.rm = TRUE),
                minReal = min(c(se.lowerci, se.upperci), na.rm = TRUE)) 
  }
  
  newvals <- newMinMax(minmaxAll$minReal, minmaxAll$maxReal)
  
  minmaxAll <- mutate(minmaxAll, minAll = unname(newvals['min']), maxAll = unname(newvals['max']))
  
  
  
  if(forceMinMax){
    
    if(all(plotData$value>0) & !logscale) minmaxAll$minAll <- 0
    if(all(plotData$value<0) & !logscale) minmaxAll$maxAll <- 0
    if(all(plotData$value>1) & logscale) minmaxAll$minAll <- 1
    if(all(plotData$value<1) & logscale) minmaxAll$maxAll <- 1
  }
  
  
  plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, 
                             max = unname(minmaxAll$maxAll), min =unname(minmaxAll$minAll))
  
  if(logscale){
    #plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, tickInterval = 0.2)
  }
  
  
  plotDataChart
  
}


maxIndPly <- function(plotDataChart, plotData, confInt = FALSE){
  
  
  if(!confInt){ #git772
    maxInd <- plotData %>%
      group_by(indic_name) %>%
      summarise(maxReal = max(value, na.rm = TRUE)) %>%
      mutate(maxIndic = maxReal)
  }else{
    maxInd <- plotData %>%
      group_by(indic_name) %>%
      summarise(maxReal = max(c(lower_95ci, upper_95ci), na.rm = TRUE)) %>%
      mutate(maxIndic = maxReal)
  }
  
  
  newvals <- newMinMax(maxInd$maxIndic)
  maxInd$maxInd <- newvals['max']
  #git612
  #maxInd$maxIndic[grepl("%", maxInd$indic_name) & maxInd$maxReal>90] <- 100
  
  plotDataChartAux <- left_join(plotDataChart, maxInd, by = "indic_name")
  
  plotDataChartAux$chart <- map2(plotDataChartAux$chart, 
                                 plotDataChartAux$maxIndic, function(hc, mx){
                                   hc_yAxis(hc, max = mx)
                                 })
  
  #joinFlds <- "indic_name"
  #if("dimension"%in%names(plotDataChart)) joinFlds <- c(joinFlds, "dimension")
  
  joinFlds <- names(plotDataChart)[names(plotDataChart)%in%names(plotDataChartAux)]
  joinFlds <- joinFlds[joinFlds!="chart"]
  
  plotDataChart <- plotDataChart %>% 
    select(-chart) %>% 
    left_join(plotDataChartAux, by = joinFlds) %>% 
    select(-maxIndic)
  
  plotDataChart
  
  
}





addJustify <- function(hc) {
  # http://jsfiddle.net/zmktekak/14/
  hc %>% 
    hc_chart(
      animation = FALSE,
      events = list(
        load = JS("function () {justifyColumns(this);}"),
        redraw = JS("function () {justifyColumns(this);}")
      )
    ) %>% 
    hc_xAxis(
      cosshair = TRUE
    ) %>% 
    hc_plotOptions(
      series = list(
        animation = FALSE,
        events = list(
          show = JS("function () {justifyColumns(this.chart);}"),
          hide = JS("function () {justifyColumns(this.chart);}")
        )
      )
    )
}

getCats <- function(x) {
  
  if(is.factor(x)) {
    catgs <- levels(x)
  } else {
    catgs <- as.character(sort(unique(x)))
  }
  
  if(length(catgs) == 1)
    catgs <- list(catgs)
  
  
  catgs
  
}


observeEvent(input$nchart, {
  if (input$who_heat == "explore") { 
    toggleModal(session, "hc_model_explore", "open")
  } else if (input$who_heat == "compare") {
    toggleModal(session, "hc_model_compare", "open")
  }
})



# This is the modal 
# zoom explore ----
output$zoomhc_explore <- renderHighchart({
  
  
  .rdata[['is_modal']] <<- TRUE
  includelegend <- TRUE
  legend_orientation <- 'horizontal'
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore) &&
     input$assessment_panel == "dataplot"){
    focus_data <- .rdata[["focus_plot_data_disag_explore"]]
    whichPlot <- ifelse(input$disag_plot_type_explore == "Line", "plotDisagLine_explore_hc",
                        "plotDisagBar_explore_hc")
    
  }
  
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore)
     && input$assessment_panel == "dataplot_dtl"){
    
    focus_data <- .rdata[['focus_plot_data_disag_dtl_explore']]
    whichPlot <- "plotDetailBar_explore_hc"
    includelegend <- FALSE
    
  }
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore)
     && input$assessment_panel == "datamap"){
    
    focus_data <- .rdata[['focus_plot_data_disag_map_explore']]
    whichPlot <- "plotDetailMap_explore_hc"
    includelegend <- TRUE
    
  }
  
  
  if(!is.null(input$assessment_panel) && !is.null(input$disag_plot_type_explore)
     && input$assessment_panel == "sumplot"){
    
    focus_data <- .rdata[["focus_plot_data_summary_explore"]]
    whichPlot <- ifelse(input$summary_plot_type_explore == "Line", "plotSummaryLine_explore_hc",
                        "plotSummaryBar_explore_hc")
    
    if(whichPlot == "plotSummaryBar_explore_hc") includelegend <- FALSE
    if(whichPlot == "plotSummaryLine_explore_hc") legend_orientation <- 'vertical'
    
  }
  
  
  if(!is.null(input$inequaldisag)  && input$inequaldisag == "sumplot"){
    
    focus_data <- .rdata[["focus_plot_data_summary_explore"]]
    whichPlot <- ifelse(input$summary_plot_type_explore == "Line", "plotSummaryLine_explore_hc",
                        "plotSummaryBar_explore_hc")
    
  }
  
  
  # print(head(focus_data))
  fun_hc <- get(whichPlot)
  hc <- fun_hc(focus_data, nchart = floor(input$nchart))
  
  .rdata[['is_modal']] <<- FALSE
  
  hc <- hc %>%
    hc_legend(
      enabled = includelegend,
      layout = legend_orientation,
      # width = 10,
      # height = 10,
      symbolRadius = 0 #https://api.highcharts.com/highcharts/plotOptions.scatter.marker
    ) %>% 
    hc_plotOptions(
      series = list(
        events = list(legendItemClick = JS("function(){ return false;}"))
      )
    )
  
  hc
})



# zoom compare ----
output$zoomhc_compare <- renderHighchart({
  
  uselegend <- TRUE
  
  if(!is.null(input$comparison_panel)){
    
    focus_data_source <-ifelse(input$comparison_panel == "inequaldisag", 
                               "focus_plot_data_disag_compare",
                               "focus_plot_data_summary_compare")
    focus_data <- .rdata[[focus_data_source]]
    whichPlot <- ifelse(input$comparison_panel == "inequaldisag", "plotDisagLine_compare_hc",
                        "plotSummaryScatter_compare_hc")

    
  }
  
  
  
  fun_hc <- get(whichPlot)
  hc <- fun_hc(focus_data, nchart = floor(input$nchart))

  #git1115
  if (whichPlot == "plotSummaryScatter_compare_hc" && isolate(input$disag_plot_summary_pts) == "labels") {
    has_benchmarks <- length(isolate(input$benchmark_countries)) > 0
      
    hc$x$hc_opts$series[[1]]$showInLegend <- FALSE
    
    if (has_benchmarks) {
      hc$x$hc_opts$series[[2]]$showInLegend <- FALSE
    }
    
    series_names <- map_chr(hc$x$hc_opts$series, "name")
    series_benchmark <- translate("tooltip_benchmark_settings", isolate(input$lang))

    if (has_benchmarks) {
      hc <- hc %>% 
        hc_add_series(
          data = NULL,
          name = series_benchmark, # "Benchmark settings",
          showInLegend = TRUE,
          type = "scatter",
          marker = list(
            symbol = "circle",
            fillColor = "#235ba3",
            lineColor = "#235ba3",
            lineWidth = 1,
            radius = 3
          )
        )
    }
    
    hc <- hc %>%
      hc_add_series(
        data = NULL,
        name = series_names[series_names != series_benchmark], # hc$x$hc_opts$series[[2]]$name, 
        showInLegend = TRUE,
        type = "scatter",
        marker = list(
          symbol = "circle",
          fillColor = "#b54848",
          lineColor = "#b54848",
          lineWidth = 1,
          radius = 3
        )
      )
  }

  
  
  hc %>%
    hc_legend(
      enabled = uselegend
    ) %>% 
    hc_plotOptions(
      series = list(
        events = list(legendItemClick = JS("function(){ return false;}"))
      )
    )
  
})





#************************************************
# getGrid function for assembling plot HTML ----
#************************************************

# width_facet_p is the space devoted to the indicator title (converted to %)
# width_yaxis_p is the space devoted to the yaxis title
# back to other helpers ----
getGrid <- function(plotDataChart, title = "This is the main title", legend = TRUE,
                    titleY = NULL, minY = NULL, maxY = NULL, logY = FALSE, 
                    titleX = NULL, minX = NULL, maxX = NULL,
                    nchart = NULL, width_div = "10%",
                    height_px = 500, width_facet_p = 10/100, width_yaxis_px = 50,
                    indic_title = TRUE,
                    indic_title_var = "indic_name",
                    plot_type = "",
                    subgroup_count = 1, subtitle = NULL,
                    legend_layout = "horizontal") {
  #- dim01 dim02 ... space
  #- cht11 cht12 ... ind01
  #- cht21 cht22 ... ind02
  #- ..... ..... ... .....
  #- chtn1 chtn2 ... ind0n
  
  # Definitions: note the chart is a chart with no titles etc.
  
  # plotDataChart: here is an example with two indicators (one dimension)
  #  if you have multiple dimensions then there will be more rows
  #Groups: <by row>
  # A tibble: 2 x 4
  #   indic_name                              dimension    indic_title                             chart   
  # * <chr>                                   <chr>        <chr>                                   <list>  
  #   1 Adolescent fertility rate (per 1000 wo… Economic st… Adolescent fertility rate (per 1000 wo… <S3: hi…
  #   2 Births attended by skilled health pers… Economic st… Births attended by skilled health pers… <S3: hi…
  
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # This section for widths of things
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # This is hard-coded for the moment
  width_yaxis_p <- 6/100
  dims <- unique(plotDataChart$dimension)
  
  # git 1020, arghh, not ideal
  if(plot_type == "plotDetailMap_explore_hc")  dims <- plotDataChart$indic_name
  inds <- unique(plotDataChart$indic_name)
  height_px <- height_px + (100*(length(inds)-1))
  #plotDataChart$indic_title <- plotDataChart[[indic_title_var]]
  
  # Here we determine what the indicator title for
  # the end of the row should be (long or not long name)
  
  
  # ******** Width-related *********
  
  # yaxis_label(6%) plot() margin plot() gap indicators
  
  # for the margin on right of plots.
  width_margin <- 0.03
  extra_width <- width_margin * length(dims)
  
  
  # if there is a real title for the y-axis then add space on y-side
  width_yaxis_p <- ifelse(!is.null(titleY) && titleY != "", width_yaxis_p, 0)
  
  # This is the width of each facet that is converted to a
  # percent later. It is 100% - 10% (for the indicator title) - 
  # the yaxis title space (if used.)
  width_dim <- (1 - width_yaxis_p - extra_width - width_facet_p)/ length(dims)
  
  
  # This is the width of the plots plus the margin between them 
  # if more than one
  width_plots <- (width_dim * length(dims)) + width_margin * (length(dims)-1)
  
  # This is the width of the top and bottom titles which
  # is the total plot width plus the yaxis gap (if exists)
  width_titles <- width_plots + width_yaxis_p
  
  
  height_margin <- 0.03
  extra_height <- height_margin * (length(inds)-1)
  
  # This is the margin at top of the indicator titles
  ind_title_marg <- 0.20/length(inds) - extra_height
  if(ind_title_marg<0.01) ind_title_marg <- 0.03
  
  
  # This the background color for indicator title
  #bgindicCol <- ifelse(indic_title, "#e2dddd", "white")
  bgindicCol <- "white"
  
  
  
  # The height of the charts is 500px divided by number of
  # indicators
  height_px_per_chart <- (height_px - extra_height)/length(inds)
  
  
  # validate there are a squere number of charts
  # this is horrendous coding and can be cleaned with more time
  # but the indic_title piece is needed to allow long/non-long
  # indicators
  
  if("indic_title"%in%names(plotDataChart)){
    indic_titles <- distinct(plotDataChart, indic_name, indic_title)
    plotDataChart <- completeCombinations(plotDataChart)
    plotDataChart$indic_title <- NULL
    plotDataChart <- left_join(plotDataChart, indic_titles, by = "indic_name")
  }
  
  
  
  # Change the size, axes etc of each chart
  plotDataChart$chart <- map(plotDataChart$chart, hc_size, height = height_px_per_chart)
  
  
  # if(plot_type == "plotDetailBar_explore_hc" && subgroup_count > 20) {
  #   
  #   plotDataChart$chart <- map(plotDataChart$chart, hc_size, height = height_px + subgroup_count * 10)
  # }
  
  if(logY) {
    
    plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, type = "logarithmic")
    #plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, min = 0.01)
    # plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, 
    #                            max = log(plotDataChart$chart[[1]]$x$hc_opts$yAxis$max))
  }
  

  
  if(!is.null(minY) && minY != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, min = as.numeric(minY), endOnTick = FALSE, startOnTick = FALSE)
  }
  
  if(!is.null(maxY) && maxY != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_yAxis, max = as.numeric(maxY), endOnTick = FALSE, startOnTick = FALSE)
  }
  
  
  if(!is.null(minX) && minX != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_xAxis, min = as.numeric(minX), endOnTick = FALSE, startOnTick = FALSE)
  }
  
  if(!is.null(maxX) && maxX != "") {
    plotDataChart$chart <- map(plotDataChart$chart, hc_xAxis, max = as.numeric(maxX), endOnTick = FALSE, startOnTick = FALSE)
  }
  
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  #  Here we send to a modal
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  if(!is.null(nchart)) {
    
    tmpTitleX <- titleX
    tmpTitleY <- titleY
    
    # Not sure why there is an issue here?
    if (plot_type %in% c(
      "plotDisagLine_explore_hc", 
      "plotDisagLine_compare_hc", 
      "plotDetailBar_explore_hc")) {
      tmpTitleX <- titleY
      tmpTitleY <- titleX
    }
    
    # Create the various titles
    
    modal_chart <- plotDataChart[nchart,]
    if(plot_type%in%c("plotDisagLine_explore_hc", 
                      "plotDisagBar_explore_hc")){
      # mod_title <- paste0(
      #   translate(modal_chart$indic_name, lang),
      #   " by ", 
      #   tolower(translate(modal_chart$dimension, lang))
      # )
      mod_title <- paste(modal_chart$indic_name, translate("graphby", isolate(input$lang)), tolower(modal_chart$dimension))
      mod_subtitle <- .rdata[["plotDisag_explore_title_static"]]
    }
    
    
    # for detailed bar it's turned on its side so indicator is labeled
    # dimension
    if(plot_type%in%c("plotDetailBar_explore_hc")){
      mod_title <- paste(
        translate(modal_chart$dimension, input$lang),
        translate("graphby", isolate(input$lang)),
        tolower(modal_chart$indic_title)
      )
      mod_subtitle <- .rdata[["plotDisag_explore_title_dtl_static"]]
    }
    
    # See below
    # if(plot_type%in%c("plotDetailMap_explore_hc")){
    #   mod_title <- paste(.rdata[["plotDisag_explore_title_map_static"]], .rdata[["plotDisag_explore_subtitle_map_static"]])
    #   mod_subtitle <- ""
    # }
    
    # Here We change the titles for the modal
    if (plot_type %in% c("plotSummaryBar_explore_hc", "plotSummaryLine_explore_hc")) {
      if (plot_type == "plotSummaryBar_explore_hc") {
        mod_title <- paste0(
          modal_chart$indic_name,
          ": ",
          str_glue_data(
            list(dimension = tolower(modal_chart$dimension2)),
            translate(if (HEATversion == "whodata") "title_within_country" else "title_within_setting", input$lang)
          )
        )
      } else {
        mod_title <- str_glue_data(
          list(dimension = tolower(modal_chart$dimension2)),
          translate(if (HEATversion == "whodata") "title_within_country" else "title_within_setting", input$lang)
        )
      }
      
      mod_subtitle <- .rdata[["plotSummary_explore_title_static"]] 
    }
    
    if (plot_type %in% c("plotDisagLine_compare_hc")) {
      modifier <- tolower(
        translate(
          if (modal_chart$ncountry == 1) "Setting" else "Settings",
          isolate(input$lang)
        )
      )
      
      mod_title <- paste(
        modal_chart$indic_name, 
        translate("graphby", isolate(input$lang)),
        tolower(modal_chart$dimension2), 
        translate("graphin", isolate(input$lang)), 
        modal_chart$ncountry, 
        modifier
      )
      
      mod_subtitle <- ""
    }
    
    
    if (plot_type %in% c("plotSummaryScatter_compare_hc")) {
      val <- tolower(translate("Setting", isolate(input$lang)))
      modifier <- tolower(
        translate(
          if (modal_chart$ncountry == 1) "Setting" else "Settings",
          isolate(input$lang)
        )
      )
      
      mod_title <- paste(
        modal_chart$indic_name2,
        str_glue_data(
          list(
            setting = val,
            dimension = tolower(modal_chart$dimension2)
          ),
          translate("setting_extended", isolate(input$lang))
        ),
         modal_chart$ncountry, 
        modifier
      )
      
      mod_subtitle <- ""
    }
    
    if (plot_type == "plotDetailMap_explore_hc") {
      mod_title <- paste(
        modal_chart$indic_name,
        translate("graphby", isolate(input$lang)),
        tolower(translate("Subnational region", isolate(input$lang))), 
        translate("graphin", isolate(input$lang)),
        modal_chart$country, 
        "(DHS, ", modal_chart$year, ")"
      )
      
      mod_subtitle <- ""
    }
    
    if (mod_subtitle != "") {
      mod_subtitle <- paste(
        translate("graphin", isolate(input$lang)), 
        mod_subtitle
      )
    }
    
    modal_chart <- plotDataChart[nchart,]

    
    chart_subtitle <- paste0(
      "<i>",
      strsplit(
        translate(
          if (.rdata[["HEATversion"]] == "whodata") "heat_graph_footnote" else "heat_plus_graph_footnote", 
          isolate(input$lang)
        ), 
        "\\\\n"
      )[[1]],
      "<br/><i/>",
      collapse = ""
    )
    
    if (plot_type == "plotDetailMap_explore_hc") {
      chart_subtitle <- paste0(
        chart_subtitle,
        "<br/>",
        "<i>", translate("heat_map_disclaimer_1", isolate(input$lang)), "</i>",
        "<br/>",
        "<i>", translate("heat_map_disclaimer_2", isolate(input$lang)), "</i>"
      )
    } 
    
    chart_subtitle <- HTML(chart_subtitle)
    
    hc <- modal_chart$chart[[1]] %>% 
      hc_chart(
        spacingBottom = if (plot_type == "plotDetailMap_explore_hc") 170 else 90
      ) %>% 
      hc_xAxis(title = list(text = tmpTitleX)) %>% 
      hc_yAxis(title = list(text = tmpTitleY)) %>% 
      hc_title(
        text = paste(mod_title, mod_subtitle),
        widthAdjust = -200,
        style = list(
          fontSize = "24px"
        )
      ) %>% 
      hc_subtitle(
        text = chart_subtitle,
        widthAdjust = -200,
        style = list(
          fontSize = "12px"
        ),
        verticalAlign = "bottom",
        align = "left",
        floating = FALSE,
        y = 20
      ) %>% 
      hc_exporting(
        enabled = TRUE,
        filename = switch(
          plot_type,
          plotDisagBar_explore_hc = "disaggregated_data",
          plotDisagLine_explore_hc = "disaggregated_data",
          plotDetailBar_explore_hc = "disaggregated_data",
          plotDetailMap_explore_hc = "disaggregated_data",
          plotDisagLine_explore_hc = "disaggregated_data",
          plotDisagLine_compare_hc = "disaggregated_data",
          
          plotSummaryBar_explore_hc = "summary_measures",
          plotSummaryLine_explore_hc = "summary_measures",
          plotSummaryScatter_compare_hc = "summary_measures",
          "data"
        ),
        fallbacktoExportServer = FALSE,
        sourceHeight = 700,
        sourceWidth = 900,
        scale = 2,
        chartOptions = list(
          title = list(
            style = list(
              fontSize = "18px"
            )
          ),
          subtitle = list(
            style = list(
              fontSize = "10px"
            )
          )
        ),
        buttons = list(
          contextButton = list(
            menuItems = JS(
              "Highcharts.getOptions().exporting.buttons.contextButton.menuItems.slice(2, 5)"
            )
          )
        )
      )
    
    if (plot_type == "plotDetailBar_explore_hc") {
      hc <- hc %>% 
        hc_exporting(
          sourceHeight = max(20 * subgroup_count, 700),
          chartOptions = list(
            xAxis = list(
              min = modal_chart$x_axis_min,
              minRange = modal_chart$x_axis_max,
              max = modal_chart$x_axis_max,
              scrollbar = list(
                enabled = FALSE
              )
            )
          )
        )
    } else if (plot_type == "plotDisagBar_explore_hc") {
      if (modal_chart$show_modal_labels) {
        hc <- hc %>% 
          hc_plotOptions(
            series = list(
              dataLabels = list(
                enabled = TRUE
              )
            )
          )
      }
    }
    
    return(hc)
  } # END modal
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # 
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # if you have more than 2 dimesions then rotate
  rotation <- ifelse(length(dims)>2 , -45, 0)
  rotation <- ifelse(plot_type == "plotDetailBar_explore_hc", 0, rotation)
  plotDataChart$chart <- map(plotDataChart$chart, hc_xAxis, labels = list(rotation = rotation))
  # fix for bar (flip) charts
  plotDataChart$chart <- map_if(plotDataChart$chart,
                                function(hc){ !is.null(hc$x$hc_opts$chart$type) && hc$x$hc_opts$chart$type == "bar"  && plot_type != "plotDetailBar_explore_hc"},
                                hc_xAxis, labels = list(rotation = 0))
  
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # This is for click to make bigger (modal)
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  plotDataChart$chart <- map2(plotDataChart$chart, seq_along(plotDataChart$chart), function(hc, n){
    
    jsfun <- JS(
      "function() {",
      "  $('#zoomhc').find('div').empty();",
      "  /* the random hack :) always send and different input so force the renderHighcarts trigger */",
      sprintf("  Shiny.onInputChange('nchart', (%s + Math.random()));", n),
      "}"
    )
    
    
    # https://jsfiddle.net/jbkunst/m8szsfsm/
    # http://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/navigation/buttonoptions-theme/
    hc <- hc %>% 
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            theme = list(
              "fill-opacity" = 0,
              states = list(
                hover = list(
                  fill = "rgba(192,192,192,0.3)"
                )
              )
            ),
            x = if (plot_type == "plotDetailBar_explore_hc") -20 else -10, # -10 is the highcharts default
            symbolX = 12.5 + 7,
            symbolY = 10.5 + 7,
            menuItems = NULL,
            symbol = 'url(expandicon.ico)',
            useHTML = TRUE,
            onclick = JS(jsfun)
          )
        )
      )
    
    jsfun2 <- JS(
      "function() {",
      "this.setSize('100%');",
      "this.reflow();",
      "}"
    )
    
    hc <- hc %>% 
      hc_chart(
        events = list(
          beforePrint = jsfun2
        )
      )
    
    hc
  })  
  
  
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # Arrange the grid of facets
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # Groups by indicator
  # map(x, div) %>% append(list(div("abc")))
  
  # Here is what grid ends up as, a table with a row per indicator then a "rowchart"
  # which is a set of charts for the row of facets that will be a single list item
  # (so length(grid$rowcharts[1]) is 1) but within that list the length is the number
  # of dimensions + 1 for the title (so length(grid$rowcharts[1][[1]]) will be 3 if
  # the number of dimensions is two).
  
  # A tibble: 2 x 2
  # indic_name                                                                                  rowcharts
  # * <chr>                                                                                       <list>   
  #   1 Adolescent fertility rate (per 1000 women aged 15-19 years)                                 <list [3…
  #   2 Births attended by skilled health personnel (in the two or three years preceding the surve… <list [3…
  
  # if(plot_type != "plotDetailBar_explore_hc"){
  #   rd_extra <- NULL
  # }else{
  #   rd_extra <-       list(
  #     tags$button("Simple collapsible", class = "btn btn-info center-block", `data-toggle` = "collapse", `data-target`="#demo"),
  #     div("New R and D calcs", id = "demo", class = "collapse text-center")
  #   )
  # }
  # graph title head
  title_ht <- ifelse(plot_type == "plotDetailBar_explore_hc" & length(dims) >1, "80px", "40px")
  
  
  
  if(!(plot_type == "plotDetailBar_explore_hc" && "rd_percentiles"%in%names(plotDataChart))){
    
    grid <- plotDataChart %>% 
      group_by(indic_name) %>%
      # within each indicator name we take all the charts (for example, there
      # might be two charts for indicator ABC because the user selected two
      # dimensions). Each chart is wrapped in a div with map and becomes a list
      # item. Once done, a new div with the title is added as the final list item
      do(rowcharts = {
        
        # The . here is the table for one indicator (potentially more than one row if multiple dimensions)
        d <- .
        # here we put a div around each chart
        res <- map(d$chart, function(x) {
          
          list(
            div(x, class = "box chartbox", style = sprintf("width:%s", percent(width_dim)))
            #div("blah, blah")
          )
        })
        
        res <- res %>% append(
          list(
            
            div(unique(d$indic_title), class = "facet facetind indicator-title", style = sprintf("width:%s;background-color:%s", percent(width_facet_p),   bgindicCol))
            
          )
        )
        
        res <- div(class = "rowchart-holder", res)
        
        
      })  
    
    
    
    dim_title_div <-   list(
      map(dims, function(x){
        list(
          div(x, class = "facet facetdim box", style = sprintf("height:%s;width:%s", title_ht,percent(width_dim)))
        )
      })
    )
    
    grid <- grid$rowcharts %>%
      append(dim_title_div, .)
    
  } else{
    
    
    # CAREFUL: with detailed bar chart with lots of bars I had to reverse indicator
    # and dimension (transpose) so "dimension" is "indic_name" and "indic_title" is dimension
    grid <- plotDataChart %>% 
      group_by(indic_name) %>% 
      do(rowcharts = {
        
        
        # The . here is the table for one indicator (potentially more than one row if multiple dimensions)
        d <- .
        
        
        #names(rdvals) <- c("Percentile", "Value")
        # here we put a div around each chart
        res <- map2(d$chart, seq_along(d$chart), function(x,y) {
          
          buttonclass <- ifelse(d$n_valid_estimates[y]>=30, "btn btn-grey btn-default center-block",
                             "btn btn-grey btn-default center-block invisible")
          rdvals <- as_data_frame(unlist(d$rd_percentiles[y])) %>% tibble::rownames_to_column()
          names(rdvals) <- c(
            translate("tooltip_summary_measure", isolate(input$lang)),
            translate("tooltip_summary_estimate", isolate(input$lang))
          )
          
          list(
            div(
              list(
                tags$button(
                  translate("graph_show_summary", isolate(input$lang)),
                  class = buttonclass, `data-toggle` = "collapse", `data-target` = paste0("#demo", y)
                ),
                div(HTML(knitr::kable(rdvals, format = "html", table.attr = "class=\"rdtable\"", align = "lr")), id = paste0("demo", y), class = "collapse text-center"),
                div(dims[y], class = "facet facetdim", style = sprintf("height:%s", title_ht))
              ),  x, class = "box-dtl chartbox", style = sprintf("width:%s", percent(width_dim))
            )
          )
        }) 
        
        res <- res %>%  append(
          list(
            # this is dimension title actually due to transpose
            div(unique(d$indic_title), class = "facet facetind box-dtl-title", style = sprintf("width:%s;;background-color:%s", percent(width_facet_p),  bgindicCol))
          )
        )
        
        res <- div(class = "rowchart-holder", res)
        
      }) 
    
    #grid$rowcharts[[1]] <- list(div(class = "box-dtl-container", grid$rowcharts[[1]]))
    
  }
  
  # THIS IS FOR BELOW
  # each grid$rowchart is a list with the charts for one indicator (one row)
  # and the last list item is the title for the row. This piece will pre-pend
  # div(s) for the dimension names/titles and then a blank box div
  # so here is a set of divs for a 2 x 2
  # <div class="facet facetdim box" style="width:45%">Economic status</div> 
  # <div class="facet facetdim box" style="width:45%">Education</div>
  # <div class="box" style="width:10%"></div>
  # <div class="box" style="width:45%"><div id="htmlwidget-88888"></div></div>
  # <div class="box" style="width:45%"><div id="htmlwidget-55555"></div></div>
  # <div class="facet facetind box" style="width:10%">title</div>
  # <div class="box" style="width:45%"><div id="htmlwidget-88888"></div></div>
  # <div class="box" style="width:45%"><div id="htmlwidget-55555"></div></div>
  # <div class="facet facetind box" style="width:10%">title</div>
  
  
  
  
  
  
  
  
  
  grid <- addTitleY(grid, titleY, width_yaxis_px)
  
  
  if(!is.null(title) && title != "") {
    grid <- addTitle(grid, title, width_titles)
  }
  
  if(!is.null(titleX) && titleX != "") {
    grid <- addTitleX(grid, titleX, width_titles)
  }
  
  if(legend){
    grid <- addLegend(grid, plotDataChart, width_titles, legend_layout)
  }
  
  
  #browsable(div(style=sprintf("width:%spx", width_div), grid))
  
  browsable(grid)
}







completeCombinations <- function(plotDataChart) {
  
  dims <- unique(plotDataChart$dimension)
  inds <- unique(plotDataChart$indic_name)
  
  
  
  plotDataChart2 <- expand.grid(indic_name = inds, 
                                dimension = dims, stringsAsFactors = FALSE) %>% 
    tbl_df() %>% 
    mutate(indic_name = factor(indic_name, levels = inds),
           dimension = factor(dimension, levels = dims)) %>% 
    arrange(indic_name, dimension) %>% 
    mutate_if(is.factor, as.character) %>% 
    left_join(plotDataChart, by = c("indic_name", "dimension"))
  
  plotDataChart2$chart <- map(plotDataChart2$chart, function(x){
    if(is.null(x)){
      res <- highchart()
    } else {
      res <- x
    }
    res
  })
  
  plotDataChart <- plotDataChart2
  
  plotDataChart
}



addAdditionalStats <- function(grid){
  
}


addTitle <- function(grid, title, width){
  
  grid <- tagList(
    
    div(class = "box titlehc", style = sprintf("width:%s", percent(width)), title),
    grid
  )
  
  grid
  
}


addTitleY <- function(grid, titleY, width_yaxis_px){
  
  if(is.null(titleY)) titleY <- ""
  styles1 <- sprintf("width:%spx", width_yaxis_px)
  styles2 <- sprintf("width: calc(100%% - %spx);",100, width_yaxis_px)
  
  hc <- highchart() %>% 
    hc_xAxis(visible = FALSE) %>% 
    hc_yAxis(
      title = list(
        offset = 20,
        text = titleY, 
        style = list(fontSize = "1.25em")
      ),
      labels = list(enabled = FALSE),
      startOnTick = FALSE,
      endOnTick = FALSE,
      tickPositions = list()
    ) %>% 
    hc_tooltip(enabled = FALSE) %>% 
    hc_add_series(data = c(0, 0), color = "transparent", showInLegend = FALSE) 
  
  
  grid <- div(class = "plot-etc-holder",
              div(class = "y-axis", style = styles1, hc),
              div(class = "grid-holder", style = styles2, grid)
              
  )
  
  grid
  
}

addTitleX <- function(grid, titleX, width ){
  
  grid <- tagList(
    grid,
    div(class = "box facet facetdim titlex", style = sprintf("width:%s", percent(width)), titleX)
  )
  
  grid
  
}

addLegend <- function(grid, plotDataChart, width, legend_layout = "horizontal"){
  
  grid <- tagList(
    grid,
    div(class = "box legendbox", style = sprintf("width:%s", percent(width)), 
        getLegend(plotDataChart,legend_layout))
  )
  
  grid
}

getLegend <- function(plotDataChart, legend_layout) {
  
  dfseries <- plotDataChart$chart %>% 
    map(function(x){
      x$x$hc_opts$series
    }) %>% 
    unlist(recursive = FALSE) %>% 
    map_df(function(x){
      data_frame(
        dimension = ifelse(is.null(x$data[[1]]$dimension), NA, x$data[[1]]$dimension),
        name = ifelse(is.null(x$name), NA, x$name),
        type = x$type,
        color = x$color)
    }) %>% 
    filter(!is.na(name)) %>% 
    filter(name != "range") 
  
  
  # git1115
  dfseries$color <- rgba_str_to_hex(dfseries$color)
  
  # dfseries$color <- gsub("rgba", "rgb", dfseries$color)
  dfseries <- select(dfseries, -dimension) %>% distinct
  #dfseries$color <- c("red", "blue")
  
  
  highchart() %>% 
    hc_add_series(data = c(0, 0), color = "transparent", showInLegend = FALSE) %>% 
    hc_add_series_list(list_parse(dfseries)) %>% 
    hc_size(height = 100) %>% 
    hc_xAxis(visible = FALSE) %>% 
    hc_yAxis(visible = FALSE) %>% 
    hc_chart(spacing = rep(10, 4)) %>% 
    hc_tooltip(enabled = FALSE) %>% 
    hc_plotOptions(
      series = list(
        events = list(legendItemClick = JS("function(){ return false;}")),
        marker = list(
          symbol = "circle"
        )
      )
    ) %>% 
    hc_legend(
      enabled = TRUE, 
      padding = 0, 
      margin = 0, 
      verticalAlign = "middle",
      symbolRadius = 0,
      layout = legend_layout,
      align = "center"
    )
  
}
