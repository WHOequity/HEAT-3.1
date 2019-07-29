
plotSummaryBar_explore_hc <- function(plotData, confInt = FALSE, ...) {

  confInt <- isolate(input$summary_error_bars)
  #input$main_title2
  axismin <- isolate(input$axis_limitsmin2)
  axismax <- isolate(input$axis_limitsmax2)
  
  
  #     longnames <- input$long_names2
  # indic_title_var <- "indic_name"
  # if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
  #   indic_title_var <- "indic"
  # }
  cols <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628")
  
  maintitle_val <-  .rdata[["plotSummary_explore_title"]]
  
  titleX <- {
    if (!is.null(input$xaxis_title2) && input$xaxis_title2 != "") {
      input$xaxis_title2
    } else {
      ""
    }
  }
  titleY <- {
    if (!is.null(input$yaxis_title2) && input$yaxis_title2 != "") {
      input$yaxis_title2
    } else {
      gsub("\\s+\\(.*\\)\\s*$", "", translate(plotData$measure[1], input$lang))
    } 
  }
  
  logY <- as.logical(plotData$logscale[1])
  plotData <- plotData %>% 
    mutate(year = as.factor(year),
           yearn = as.numeric(year) - 1,
           color = colorize(indic_name, cols[1:length(unique(indic_name))]))
  

  show_labels <- !confInt # n_distinct(plotData$indic_name) == 1 # && n_distinct(plotData$subgroup) <= 7
  axislims <- c(min(plotData$yearn), max(plotData$yearn))
  
  plot_data <- count(plotData, indic_name) %>% inner_join(plotData, ., by = "indic_name")
  
  if (HEATversion == "whodata") {
    plot_data <- plot_data %>% 
      mutate_at(
        c("country", "dimension"),
        funs(map_chr(., translate, lang = input$lang))
      ) %>% 
      mutate(
        indic_name = map_chr(indic, translate, lang = input$lang),
        indic_title = indic_name
      )
  }
  
  plot_data <- mutate(plot_data, measure_name = map_chr(measure, translate, lang = input$lang))
  
  plotDataChart <- plot_data %>% 
    group_by(indic_name, dimension, indic_title) %>% 
    do(chart = {
      d <- .
      catgs <- getCats(d$year)
      hc <- highchart() %>%
        hc_xAxis(type = "category", reversed = FALSE, categories = catgs,min = axislims[1], 
                 max = axislims[2]) %>%
        hc_add_series(data = list_parse(select(d, x = yearn, y = inequal, country,measure_name = measure_name, 
                                               source, year, indic_name, dimension, estimate,
                                               selower = se.lowerci, seupper =  se.upperci) %>% 
                                          mutate(
                                            setting_average = mean(y, na.rm = TRUE)
                                          )), 
                      type = "column", 
                      threshold = ifelse(logY, 1, 0),
                      color = unique(d$color),
                      maxPointWidth = 100
                      ) %>% 

        hc_legend(enabled = FALSE) %>%         
        hc_tooltip(
          headerFormat = '',
          pointFormatter = JS(
            "function() {",
            "let _this = Object.assign({}, this);",
            "Object.keys(_this).forEach(function(key) { if (typeof _this[key] === 'number' && key != 'year') _this[key] = _this[key].toFixed(1) });",
            "return _this.country + ', ' + _this.source + ' ' + _this.year +",
            "  '<br/><br/>' +",
            "  '<b>' + _this.measure_name + ': ' + _this.y + '</b>' + (_this.seupper ? '; 95% CI: ' + _this.selower + '-' + _this.seupper : '');",
            "}"
          )
        ) %>% 
        hc_plotOptions(
          series = list(
            dataLabels = list(
              enabled = show_labels,
              crop = FALSE,
              allowOverlap = TRUE,
              format = "{y:.1f}",
              style = list(
                fontSize = "10px",
                fontWeight = 300,
                textOutline = "none"
              )
            )
          )
        )
      
      if (confInt) {
        hc <- hc %>% 
          hc_add_series(
            data = list_parse(select(d, x = yearn, low = se.lowerci, high = se.upperci)),
            type = "errorbar",
            color = "#606060",
            enableMouseTracking = FALSE
          ) 
      }
      
      hc
    })
  
  
  plotDataChart$dimension2 <- plotDataChart$dimension
  
  
  # TODO, fix this function so it doesn't create extra fields
  plotDataChart <- minmaxAllPly(plotDataChart, plotData %>% mutate(value = inequal), 
                                confInt = confInt, logscale = logY, 
                                forceMinMax = TRUE)
  
  # plotDataChart <- rename(plotDataChart, indic_title = indic_title.x,
  #                         dimension2 = dimension2.x,
  #                         ncountry = ncountry.x)
  
  
  # plotDataChart$show_stack_labels <- plotData %>% 
  #   group_by(indic_name) %>% 
  #   summarise(show_labels = n_distinct(subgroup) <= 7) %>% 
  #   pull(show_labels)
  
  getGrid(plotDataChart, title = maintitle_val, 
          logY = logY, minY = axismin, maxY = axismax, titleX = titleX, 
          titleY = titleY, plot_type = "plotSummaryBar_explore_hc", ...)
  
}




