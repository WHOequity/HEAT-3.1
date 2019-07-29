
plotSummaryScatter_compare_hc <- function(plotData, ...) {
  
  
  plotStyle <- input$disag_plot_summary_pts
  uselegend <- TRUE#ifelse(plotStyle == "labels", FALSE, TRUE)
  maintitle_val <- .rdata[['plotSummary_compare_title']]
  Xaxismin <- isolate(input$xaxis_limitsmin4)
  Xaxismax <- isolate(input$xaxis_limitsmax4)
  Yaxismin <- isolate(input$yaxis_limitsmin4)
  Yaxismax <- isolate(input$yaxis_limitsmax4)
  
  lang <- isolate(input$lang)
  
  if (HEATversion == "whodata") {
    plotData <- plotData %>% 
      mutate(
        country = map_chr(country, translate, lang),
        dimension = map_chr(dimension, translate, lang),
        indic_name = map_chr(indic, translate, lang = lang),
        indic_title = indic_name,
        measure_name = map_chr(measure, translate, lang = lang)
      )
  } else {
    plotData <- mutate(plotData, measure_name = map_chr(measure, translate, lang = lang))
  }

  measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
  
  ncountry <- length(unique(plotData$country))
  dimension2 <- unique(plotData$dimension)
  indic_name2 <- unique(plotData$indic_name)
  
  titleX <- if (!is.null(isolate(input$xaxis_title4)) && isolate(input$xaxis_title4) != "")
    isolate(input$xaxis_title4)
  else 
    translate("tooltip_setting_avg", isolate(input$lang))
    # "Setting average"
  titleY <- ifelse(!is.null(isolate(input$yaxis_title4)) && isolate(input$yaxis_title4) !="", 
                   input$yaxis_title4, measureName)
  
  
  logY <- as.logical(plotData$logscale[1])
  
  plotData <- mutate(plotData, setting_average = mean(inequal, na.rm = TRUE))
  
  if(sum(plotData$anchor==0) == 0){
    # this dummy is added with NA data except for the benchmark
    # so that the legend and color-coding works in the plot
    anchordummy <- plotData[1,]
    anchordummy[, names(anchordummy)!="anchor"]  <- NA
    anchordummy$anchor <- 0
    plotData <- rbind(plotData, anchordummy)
  }
  #"#235ba3", "#b54848"
  plotData$color <- factor(plotData$anchor, levels = c(0,1), labels = c("#235ba3", "#b54848"))
  
  benchmark_label <- translate("tooltip_benchmark_settings", lang)
  plotData <- plotData[order(plotData$anchor), ] %>% 
    mutate(
      anchor_label = ifelse(anchor == 1, country, benchmark_label)
    )
    # mutate(anchor_label = country) #ifelse(anchor, country, translate("tooltip_benchmark_settings", isolate(input$lang)))) #  "Benchmark settings")) 
  
  plotData <- filter(plotData, !is.na(estimate) | !is.na(inequal)) 
  if(nrow(plotData)){
    plotData$inequal <- round(plotData$inequal,2)
    plotData$estimate <- round(plotData$estimate,2)
  }
  
  plotData <- plotData %>%
    group_by(anchor_label) %>% 
    do(series = {
      d <- .
      lst <- list(data = list_parse(select(d, x = estimate,  y = inequal, indic_title, setting_average,
                                    country, source, year, indic_name, measure_name, dimension, ccode) ),
           type = "scatter",  name = unique(d$anchor_label), color = as.character(unique(.$color)),
           animation = FALSE
           )
      
      
      if (plotStyle == "labels") {
        lst[['dataLabels']] <- list(
          allowOverlap = TRUE,
          enabled = TRUE, 
          formatter = JS('function(){return this.point.ccode}'),
          style = list(color = unique(.$color)),
          align = "center",
          verticalAlign = "middle"
        )
        
        lst$color<- paste("rgba(", paste(col2rgb(lst$color), collapse=","), ",0.0)")
      }
      
      
      lst
    })

  chart <- highchart() %>% 
    hc_plotOptions(
    series = list(
      #dataLabels = list(enabled = TRUE),
      marker = list(
        radius = 6,
        fillOpacity = 0.2
      )
    )
  ) %>%
    hc_add_series_list(plotData$series) %>% 
    hc_tooltip(
      headerFormat = '', 
      pointFormatter = JS(str_glue(
        "function() {{",
        "let _this = Object.assign({{}}, this);",
        "Object.keys(_this).forEach(function(key) {{ if (typeof _this[key] === 'number' && key != 'year') _this[key] = _this[key].toFixed(1) }});",
        "return _this.country + ', ' + _this.source + ' ' + _this.year +",
        "  '<br/><br/>' +",
        "  '<b>' + _this.measure_name + ': ' + _this.y + '</b>' +",
        "  (_this.setting_average ? '<br/><br/><b>{ translate('tooltip_setting_avg', input$lang) }: ' + _this.x + '</b>' : '');",
        "}}"
      ))
    )

# "function(){
# 
# 
#     var tool = '<span class = \"tooltip-bold-bigger\">National average: ' + this.x + '</span><br>' + 
#     this.measure_name + ': ' + this.y + '<br><br>' +
#      this.country + ', ' + this.source + ' '  + this.year + '<br>' +
#      '<em>' + this.indic_name + '<br>' +
#      'By ' + this.dimension.toLowerCase() + '</em>'; 
#                                    return tool;
#                               }")
#     )
  

  plotDataChart <- data_frame(
    indic_name = "",
    dimension = "",
    dimension2 = dimension2,
    indic_name2 = indic_name2,
    ncountry = ncountry,
    chart = list(chart))
  
  getGrid(plotDataChart, title = maintitle_val, logY = logY, legend = uselegend,
          minY = Yaxismin, maxY = Yaxismax, plot_type = "plotSummaryScatter_compare_hc",
          minX = Xaxismin, maxX = Xaxismax, 
          indic_title = FALSE, titleX = titleX, titleY = titleY,...)
  
}


