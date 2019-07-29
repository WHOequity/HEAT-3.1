plotDetailMap_explore_hc <- function(plotData, geodata = NULL, minhc = NULL, maxhc = NULL, ...) {
  
  if(.rdata$is_modal){
    minhc <- plotData[["minhc"]]
    maxhc <- plotData[["maxhc"]]
    geodata <- plotData[["geodata"]]
    plotData <- plotData[["plotData"]]
  }
  
  if(!is.null(geodata)){
    geodata_national <- geodata$geodata_national
    geodata_subnational <- geodata$geodata_subnational
    geodata_disputed <- geodata$geodata_disputed
  }
  


  plotData <- disputed_map_rules(plotData, .rdata[["disputed_subnational"]])

  maintitle_val <- .rdata[["plotDisag_explore_title_map"]]
  subtitle_val <- .rdata[["plotDisag_explore_subtitle_map"]]
  
  
  my_stops <- color_stops(5, c(viridis(5, begin = 0.2, direction = -1)))

  # git1063 add extreme final category
  if(tolower(plotData$country[1])%in%.rdata$disputed_subnational){
    my_stops[[length(my_stops)]][[1]] <- 0.9999999
    my_stops[[(length(my_stops)+1)]] <- list(1, "rgb(120, 120, 120)")
  }
  
  # @git1181
  if (HEATversion == "whodata") {
    plotData <- mutate(
      plotData,
      country = translate(country[1], input$lang),
      dimension = translate(dimension[1], input$lang),
      indic_name = translate(isolate(input$focus_indicator_explore_map), input$lang)
    )
  }
  
  hc <- highchart(
    type = "map"
  ) %>%
    hc_add_series_map(
      map = geodata_subnational,
      df = plotData,
      joinBy = "subgroup",
      value = "estimate",
      nullInteraction = TRUE
    ) %>%
    hc_tooltip(
      formatter = JS(str_glue(
        "function() {{",
        "let _this = Object.assign({{}}, this.point.options);",
        "Object.keys(_this).forEach(function(key) {{ if (typeof _this[key] === 'number' && key != 'year') _this[key] = _this[key].toFixed(1) }});",
        "if (_this.estimate > 9999999) {{",
        "  return '{ translate('tooltip_not_applicable', input$lang) }';",
        "}}",
        "return _this.country + ', DHS ' + _this.year +",
        "  '<br/><br/>' +",
        "  '<b class=\"text-capitalize\">' + _this.subgroup + '</b>' + (_this.popshare ? ' (' + _this.popshare + '% { translate('tooltip_affected_pop', input$lang) })' : '') +",
        "  '<br/><br/>' +",
        "  '<b>{ translate('tooltip_estimate', input$lang) }: ' + _this.estimate + '</b>' + (_this.upper_95ci ? '; 95% CI: ' + _this.lower_95ci + '-' + _this.upper_95ci : '') +",
        " ",
        "  (_this.national ? '<br/><br/>{ translate('tooltip_setting_avg', input$lang) }: ' + _this.national : '');",
        "}}"
      ))
    ) %>%
    hc_colorAxis(
      stops = my_stops,
      min = minhc,
      max = maxhc
    ) %>% 
    hc_legend(enabled = TRUE)%>% 
    hc_add_series(
      mapData = geodata_national,
      type = "mapline",
      nullColor = "rgb(100,100,100)",
      lineWidth = 2
      
    )
  
  if(!any(class(geodata_disputed)=="tbl_df")){
    hc <- hc %>% 
      hc_add_series(
        mapData = geodata_disputed,
        type = "mapline",
        nullColor = "rgb(50,50,50)",
        dashStyle = "Dash",
        lineWidth = 2
      )
  }
  

  hc <- get_map_legend_addon(hc, plotData$country[1])


  
  plotDataChart <- tibble(
    dimension = plotData$dimension[1], 
    indic_name = plotData$indic_name[1],
    country = plotData$country[1],
    year = plotData$year[1],
    chart = list(hc)
  )
 
  getGrid(
    plotDataChart, 
    title = maintitle_val,
    subtitle = "ABC", 
    legend = FALSE,
    # indic_title_var = indic_title_var,
    plot_type = "plotDetailMap_explore_hc", 
    ...
  )
}
