
# function ----------------------------------------------------------------
plotDetailBar_explore_hc <- function(plotData, sortBy = NULL, regs = NULL, 
                                     showAVG = TRUE, showMedian = TRUE, 
                                     addGroupNames = TRUE, indicSort = NULL, ...) {
  
  if (.rdata$is_modal) {
    sortBy <- .rdata[["plotDisag_explore_dtl_sortvar"]]
    regs <- .rdata[["plotDisag_explore_dtl_subgroups"]]
    showAVG <- .rdata[["plotDisag_explore_dtl_showAvg"]]
    showMedian <- .rdata[["plotDisag_explore_dtl_showMedian"]]
    indicSort <- .rdata[["plotDisag_explore_dtl_indicSort"]]
  }
  
  
  maintitle_val <- .rdata[["plotDisag_explore_title_dtl"]]
  
  isolate({
    #   disag_plot_explore_dtl_sort      <- input$disag_plot_explore_dtl_sort
    #   disag_plot_explore_dtl_showAVG   <- input$disag_plot_explore_dtl_showAVG
    #   disag_plot_explore_dtl_subgroups <- input$disag_plot_explore_dtl_subgroups
    descend <- input$sortOrder_ind_dim == "Descending"
    #   disag_plot_explore_dtl_showMedian <- input$disag_plot_explore_dtl_showMedian
    #   disag_plot_explore_dtl_showNames <- input$disag_plot_explore_dtl_showNames %||% TRUE
    #   disag_indicSort <- input$focus_indicator_explore_plotdtl[input$focus_indicator_explore_plotdtl%in%unique(plotData$indic)]
  })
  
  
  
  isolate({
    #input$main_title_dtl
    xtitle_val <- ifelse(!is.null(input$xaxis_title_dtl) && input$xaxis_title_dtl !="", input$xaxis_title_dtl, "")
    ytitle_val <- ifelse(!is.null(input$yaxis_title_dtl) && input$yaxis_title_dtl !="", input$yaxis_title_dtl, "")
    longnames <- TRUE # input$long_names_dtl
  })
  
  
  
  maintitle_val <- .rdata[["plotDisag_explore_title_dtl"]]
  
  axismin <- isolate(input$axis_limitsmin_dtl)
  axismax <- isolate(input$axis_limitsmax_dtl)
  
  if (axismax == "") {
    axismax <- max(plotData$estimate) + 5
  }
  
  
  
  max_count_subgroup <- plotData %>% 
    group_by(indic_name) %>% 
    summarise(n = sum(!is.na(estimate))) %>% 
    .$n %>% max()
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # 
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
  n_subgroup <- n_distinct(plotData$subgroup)
  #n_valid_estimates <- sum(plotData$estimate != -9999)
  

  
  #git 717
  if(is.null(regs)) regs <- "None"
  
  # distinct indicators and national numbers

  dfindic <- distinct(plotData, indic,   dimension, subgroup, lower_95ci, upper_95ci, se, pop,
                      source, national,
                      country, year, popshare)
  
  
  cols <- plotData %>% 
    select(indic, subgroup, colors)
  
  indicinfo <- distinct(plotData, indic, indic_name)
  
  # Save the CI etc for tooltips git1045
  #plotData_info <- select(plotData, indic, dimension, subgroup, lower_95ci, upper_95ci, se, pop)
  
  # make wide with indicators so each indicator is a column
  plotData <- plotData %>% 
    select(indic, dimension, subgroup, estimate) %>% 
    mutate(indic = factor(indic, levels = unique(plotData$indic))) %>% 
    spread(indic, estimate, fill = -9999)
  
  
  
  
  if(!is.null(sortBy) && sortBy != "") {
    
    if(descend){
      plotData <- plotData %>% 
        arrange_(paste0("desc(", sortBy, ")"))
    }else{
      plotData <- plotData %>% 
        arrange_(sortBy)
    }
    
  }
  
  dataDim <- unique(plotData$dimension)
  
  
  
  plotData <- plotData %>% 
    gather(indic, estimate, -dimension, -subgroup) %>% 
    left_join(., cols, by = c("indic", "subgroup")) %>% 
    rename(color = colors) %>% 
    mutate(color = ifelse(subgroup %in% regs, "red", color))
  
  plotData <- left_join(plotData, indicinfo, by = "indic")
  plotData$color[is.na(plotData$color)] <- "white"
  
  
  
  # Here we join in the national numbers, we add the name of the 
  # dimension, we convert the color to rgba and drop NA values
  plotData <- left_join(plotData, dfindic, by = c("indic", "dimension", "subgroup")) %>%
  #plotData <- left_join(plotData, dfindic, by = c("indic")) %>%
    mutate(
      dimension = dataDim,
      color = color %>% 
        col2rgb() %>% 
        t() %>% 
        as_data_frame() %>% 
        mutate(
          rgba = sprintf("rgba(%s,%s,%s,0.95)", red, green, blue)
        ) %>% 
        pull(rgba)
    )  
    
  plotData <- plotData %>% 
    filter(
      !is.na(estimate)
    )

  

  # plotData <- mutate_at(plotData, vars(-subgroup), funs(replace(., is.na(.), 0)))
  if (HEATversion == "whodata") {
    plotData <- plotData %>% 
      mutate(
        subgroup = as.character(subgroup),
        indic_name = map_chr(indic, translate, lang = input$lang)
      ) %>% 
      mutate_at(
        c("dimension", "country", "subgroup"),
        funs(map_chr(., translate, input$lang))      
      )
  }
  
  n_valid_estimates <- group_by(plotData, indic_name) %>% 
    summarise(n_valid_estimates = sum(estimate != -9999)) 
  
  #git1175
  if (any(n_valid_estimates$n_valid_estimates >= 30)) {
    rd_percentiles <- r_d_percentile(plotData, n_subgroup)
  }
  
  plotDataChart <- plotData %>% 
    group_by(indic_name) %>% 
    mutate(
      ord = seq_len(n())
    ) %>% 
    do(chart = {
      d <- .
      
      ds <- d %>% 
        arrange(ord) %>% 
        mutate(
          x = ord,
          y = estimate, 
          name = subgroup
        )      
      
      hc <- highchart() %>%
        hc_chart(type = "bar") %>% 
        hc_legend(enabled = FALSE) %>% 
        hc_add_series(
          data = list_parse(ds), 
          type = "column",
          name = unique(d$indic_name)
          # pointRange = 1,
          # groupPadding = 0,
          # # pointWidth = barWidth(max_count_subgroup, detailedBar = TRUE),
          # pointPadding =  0,
          # borderWidth = 0
        ) 
      
      
      if(addGroupNames){
        hc <- hc %>% hc_xAxis(visible = addGroupNames,
                              categories = c("", as.character(ds$name)))
      }else{
        hc <- hc %>% hc_xAxis(visible = FALSE)
      }
      
      
      # if(d$indic[1] != indicSort[1] & addGroupNames){
      #   hc <- hc %>% hc_xAxis(labels = list(style = list(opacity=0)))
      # }
      
      
      addLines <- NULL
      
      if (showAVG) {
        avgLine <- list(
          color = "black",
          label = list(
            text = paste0(
              translate("tooltip_setting_avg", isolate(input$lang)), ": ", # "Setting average: ", 
              unique(d$national)
            ),
            style = list(
              `text-shadow` = "none" # "2px 2px white"
            )
          ),
          width = 1,
          zIndex = 5,
          value = unique(d$national)
        )
        
        addLines <- append(addLines, list(avgLine))
      }
      
      if (showMedian) {
        medianLine <- list(
          color = "#C66839",
          #dashStyle = "Dash",
          label = list(
            text = paste0(
              translate("tooltip_median", isolate(input$lang)), ": ", # "Median:", 
              median(d$estimate[d$estimate != -9999], na.rm = TRUE)
            ),
            align = "left",
            style = list(
              color = "#C66839",
              `text-shadow` = "none" # "2px 2px white"
            )
          ),
          width = 1,
          zIndex = 5,
          value = median(d$estimate, na.rm = TRUE)
        )
        
        addLines <- append(addLines, list(medianLine))
      }
      
      if (length(addLines) > 0) {
        hc <- hc %>%
          hc_yAxis(
            plotLines = addLines
          )
      }
      
      hc <- hc %>% 
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
            "  '<b>{ translate('tooltip_estimate', input$lang) }: ' + _this.estimate + '</b>' + (_this.upper_95ci ? '; 95% CI: ' + _this.lower_95ci + '-' + _this.upper_95ci : '') +",
            "  ",
            "  (_this.national ? '<br/><br/>{ translate('tooltip_setting_avg', input$lang) }: ' + _this.national : '');",
            "}}"
          ))
        ) %>% 
        hc_plotOptions(
          column = list(
            dataLabels = list(
              enabled = TRUE,
              crop = FALSE,
              overflow = FALSE,
              allowOverlap = TRUE,
              # format = "{y:.1f}",
              formatter = JS(
                "function() {",
                "  if (this.y === -9999) {",
                "    return '';",
                "  } else {",
                "    return this.y.toFixed(1);",
                "  }",
                "}"
              ),
              style = list(
                fontSize = "10px",
                fontWeight = 300,
                textOutline = "none"
              ),
              zIndex = 4
            )
          )
        ) %>% 
        hc_xAxis(
          min = 1,
          max =  min(d %>% pull(subgroup) %>% n_distinct(), 20),
          scrollbar = list(
            enabled = TRUE, # d %>% pull(subgroup) %>% n_distinct() > 7 # TRUE
            showFull = FALSE,
            minWidth = 4,
            margin = 20,
            zIndex = 6
          )
        ) %>% 
        hc_yAxis(
          min = min(d$estimate[d$estimate != -9999], 0)
        ) %>% 
        hc_chart(
          marginRight = 50
        )
      
      
      #         "function(){
      #                                    
      #   //if(this.indic_name == undefined){return false}
      #   var tool = '<span class = \"tooltip-bold-bigger\">Estimate: ' + this.estimate + '</span><br>' + 
      #   '95%CI: ' + this.lower_95ci +  '-' + this.upper_95ci +'<br><br>' +
      #   this.country + ', ' + this.source + ' '  + this.year + '<br>' +
      #   '<em>' + this.indic_name + '<br>' +
      #   '<span class = \"tooltip-bold\">' + this.subgroup + ' (' + this.popshare + '% of affected population)</em></span>'; 
      #   return tool;               
      # }")
      # )
      
      
      hc
    })
  
  plotDataChart <- maxIndPly(plotDataChart, 
                             plotData %>% mutate(value = estimate)) 

  plotDataChart <- plotDataChart %>% 
    mutate(
      dimension = indic_name, # already translated at this point
      indic_name = "",
      indic_title = map_chr(unique(plotData$dimension), translate, lang = input$lang),
      x_axis_min = 1,
      x_axis_max = n_distinct(plotData$subgroup)
    )
  
  
  ind <- distinct(plotData, indic, indic_name)
  indicSort <- ind$indic_name[match(indicSort, ind$indic)]
  
  if(!is.null(indicSort)){
    plotDataChart <- plotDataChart[match(indicSort, plotDataChart$dimension),]
  }
  
  
  
  if (any(n_valid_estimates$n_valid_estimates>=30)) {
    plotDataChart <- left_join(plotDataChart, rd_percentiles, by = c("dimension" = "indic_name"))
    plotDataChart <- left_join(plotDataChart, n_valid_estimates, by = c("dimension" = "indic_name"))
  }
  
  getGrid(plotDataChart, 
          title = maintitle_val, 
          minY = axismin,
          maxY = axismax, 
          titleX = xtitle_val,   
          titleY = ytitle_val,
          legend = FALSE,
          #indic_title_var = indic_title_var,
          plot_type = "plotDetailBar_explore_hc",
          subgroup_count = length(unique(plotData$subgroup)),...)
  
}
