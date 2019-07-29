
plotDisagBar_explore_hc <- function(plotData, confInt = FALSE, ...) {
  
  confInt <- isolate(input$disag_error_bars)
  maintitle_val <- .rdata[["plotDisag_explore_title"]]
  
  axismin <- isolate(input$axis_limitsmin1)
  axismin <- if (axismin == "") min(0, floor(plotData$estimate)) else axismin
  
  axismax <- isolate(input$axis_limitsmax1)
  axismax <- if (axismax == "") max(100, ceiling(plotData$estimate) + 10, na.rm = TRUE) + 1 else axismax
  
  titleX <- ifelse(!is.null(input$xaxis_title1) && input$xaxis_title1 !="", input$xaxis_title1, "")
  titleY <- ifelse(!is.null(input$yaxis_title1) && input$yaxis_title1 !="", input$yaxis_title1, "")
  
  lang <- input$lang  
  ERR_BARS <- FALSE # sample(c(TRUE, FALSE), size = 1)
  TYPE <- "column" #sample(c("column", "line"), size = 1, prob = c(0.9, .1))
  
  # @git1181
  if (HEATversion == "whodata") {
    plotData <- mutate(
      plotData,
      country = map_chr(country, translate, lang),
      subgroup = as.character(subgroup),
      subgroup = map2_chr(dimension, subgroup, ~ {
        if (.x != "Subnational region") translate(.y, lang) else .y
      }),
      indic_name = map_chr(indic, translate, lang),
      indic_title = indic_name,
      dimension = map_chr(dimension, translate, lang)
    )
  }

  plotData <- plotData %>% 
    mutate(year = as.factor(year),
           yearn = as.numeric(year) - 1)
  
  # show_labels <- n_distinct(plotData$dimension) == 1 && 
  #   n_distinct(plotData$subgroup) <= 7 &&
  #   !confInt
  
  axislims <- c(min(plotData$yearn), max(plotData$yearn))
  
  
  plotData <- count(plotData, indic_name) %>% inner_join(plotData, ., by = "indic_name")
  #plotData <- arrange(plotData, desc(estimate)) %>% group_by(year, indic_name) %>% mutate(rank = row_number())
  
  plotData <- group_by(plotData, dimension) %>% mutate(cnt_dimension = length(unique(subgroup))) %>% ungroup()
  
  plot_stack_labels <- plotData %>%
    group_by(dimension, indic, year) %>%
    summarise(labels_by_year = n_distinct(subgroup) <= 7) %>%
    summarise(
      show_modal_labels = all(labels_by_year) & !confInt,
      show_stack_labels = show_modal_labels & (n_distinct(plotData$dimension) == 1)
    ) %>%
    ungroup()
  
  plotData <- left_join(plotData, plot_stack_labels, by = c("indic", "dimension"))
  
  plotDataChart <- plotData %>% 
    group_by(indic_name, indic, dimension, indic_title) %>% 
    do(chart = {
      d <- .
      catgs <- getCats(d$year)
      
      hc <- highchart() %>%
        hc_xAxis(
          type = "category",
          reversed = FALSE, 
          categories = catgs,
          min = axislims[1], 
          max = axislims[2],
          maxPadding = 0.25,
          endOnTick = FALSE
        ) %>%
        hc_plotOptions(
          series = list(
            dataLabels = list(
              enabled = all(d$show_stack_labels),
              crop = FALSE,
              overflow = FALSE,
              allowOverlap = TRUE,
              format = "{y:.1f}",
              style = list(
                fontSize = "10px",
                fontWeight = 300,
                textOutline = "none"
              )
            )
          )
        ) %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_tooltip(
          headerFormat = '', 
          pointFormatter = JS(str_glue(
            "function() {{",
            "let _this = Object.assign({{}}, this);",
            "Object.keys(_this).forEach(function(key) {{ if (typeof _this[key] === 'number' && key != 'year') _this[key] = _this[key].toFixed(1) }});",
            "return _this.country + ', ' + _this.source + ' ' + _this.year +",
            "  '<br/><br/>' +",
            "  '<b>' + _this.subgroup + '</b>' + (_this.popshare ? ' (' + _this.popshare + '% { translate('tooltip_affected_pop', input$lang) })' : '') +",
            "  '<br/><br/>' +",
            "  '<b>{ translate('tooltip_estimate', input$lang) }: ' + _this.y + '</b>' + (_this.upper_95ci ? '; 95% CI: ' + _this.lower_95ci + '-' + _this.upper_95ci : '') +",
            " ",
            "  (_this.national ? '<br/><br/>{ translate('tooltip_setting_avg', input$lang) }: ' + _this.national : '');",
            "}}"
          ))
        )
      

      cnt <- unique(d$cnt_dimension)
      cnt_col <- length(unique(d$colors))
      if ((.rdata[['HEATversion']] == "whodata" & d$dimension[1] == translate("Subnational region", lang)) || 
          (.rdata[['HEATversion']] == "upload" & (cnt>7 | cnt_col == 1))) {
        
        d <- arrange(d, estimate) %>% group_by(year, indic_name) %>% mutate(rank = row_number())
        
        # git1073 align bars
        d <- group_by(d, year) %>% mutate(maxrank = max(rank))
        rankdiffs <- (max(d$maxrank)-min(d$maxrank))/max(d$maxrank)
        
        #browser()
        if(rankdiffs > 0.2){
          totmax <- max(d$maxrank)
          d$maxdif <- round((totmax - d$maxrank)/2)
          d$rank <- d$rank+d$maxdif

          tmpd <- select(d, year,  estimate, rank) %>%
            spread(year, estimate, fill = 0) %>%
            gather(year, estimate, -rank)

          d <- left_join(tmpd, d, by = c("rank", "year"))
          d$yearn <- as.numeric(as.character(factor(d$year, levels = sort(unique(d$year)), labels = sort(unique(d$yearn)))))
          d <- rename(d, estimate = estimate.y)
        }

        
        
        for(sg in unique(d$rank)){
          # sg <- 33
          d2 <- d %>%
            filter(rank == sg) %>%
            mutate(x = yearn, y = estimate, color = colors,
                   low = lower_95ci, high = upper_95ci)
          
          
          
          colval <- as.character(na.exclude(unique(d2$color, na.rm=T)))
          
          hc <- hc %>% 
            hc_add_series(
              data = list_parse(
                d2 %>% 
                  select(
                    x, y, country, 
                    source, year, indic_name, dimension, subgroup, 
                    popshare, estimate, lower_95ci, upper_95ci, national
                  )
              ), 
              name = d$dimension[1], type = TYPE, color = colval,
              pointPadding = 0, 
              showInLegend = FALSE,
              borderWidth = 0.5
            )
          
          if(confInt) {
            hc <- hc %>%
              hc_add_series(data = list_parse(select(d2, x, low, high)),
                            type = "errorbar", color = "#606060", enableMouseTracking = FALSE)
          }
          
        }
       dimval  <- as.character(na.exclude(unique(d$dimension, na.rm=T)))
        hc <- hc %>%
          hc_add_series(data = NULL, name = dimval, color = colval,
                        type = TYPE, showInLegend = TRUE)
      }else{
        for(sg in unique(d$subgroup)){
          # sg <- d$subgroup[1]
          d2 <- d %>%
            filter(subgroup == sg) %>%
            mutate(
              x = yearn, y = estimate, color = colors,
              low = lower_95ci, high = upper_95ci,
              subgroup = map_chr(as.character(subgroup), translate, input$lang)
            )
          
          
          
          
          hc <- hc %>% 
            hc_add_series(data = list_parse(select(d2, x, y, country, 
                                                   source, year, indic_name, dimension, subgroup, 
                                                   popshare, estimate, lower_95ci, upper_95ci, national)), 
                          name = sg, 
                          type = TYPE, 
                          color = unique(d2$color),
                          maxPointWidth = 100
            )
          
          if(input$disag_error_bars) {
            hc <- hc %>%
              hc_add_series(data = list_parse(select(d2, x, low, high)),
                            type = "errorbar", color = "#606060", enableMouseTracking = FALSE)
          }
          
        }
      }
      #}
      hc
    })
  
  plotDataChart <- maxIndPly(plotDataChart, plotData %>% mutate(value = estimate), confInt = confInt)
  #plotDataChart <- rename(plotDataChart, indic_title = indic_title.x)

  # Dont' apply justification for those with more than 7 subgroups
  cnts <- count(distinct(plotData, country,  dimension, subgroup), country, dimension)
  cnts <- which(!plotDataChart$dimension%in%cnts$dimension[cnts$n>7])
  
  #plotDataChart$chart[cnts] <- map(plotDataChart$chart[cnts], addJustify) # 57
  plotDataChart <- left_join(
    plotDataChart,
    select(plot_stack_labels, dimension, indic, show_modal_labels),
    by = c("dimension", "indic")
  )  
  
  getGrid(plotDataChart, title = maintitle_val, 
          minY = axismin, maxY = axismax, 
          titleX = titleX, 
          titleY = titleY,
          indic_title_var = indic_title_var,
          plot_type = "plotDisagBar_explore_hc", ...)
  
}

