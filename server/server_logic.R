# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.


#******************************************************************************
#******************************************************************************
# DISAGGREGATED EXPLORE DATA TABLE ----
#******************************************************************************
#******************************************************************************

# Creating the data tables requires three pieces. There is a reactive that 
# will grab the data based on user selections. This is called datasetInput. 
# This is used in the dataTable renderUI. Originally this was not a renderUI
# but a renderDataTable but since we wanted the labels on top of the table
# I changed to a renderUI. As a result, the renderDataTable is in the renderUI.
# The dataTable_options is the third piece and this sets the datatable items and,
# importantly, checks for the health indicator full name and then makes that column
# wider.


output$dataTable <- renderUI({
  
  debounce_disag_explore_table()
  # droptable <- FALSE
  
  # if(.rdata[['first_time_disag_table']]){
  #   .rdata[['first_time_disag_table']] <<- FALSE
  #   droptable <- TRUE
  # }
  
  
  
  # dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  dataMSG <- translate("noestimates", input$lang)
  
  isolate({
    focus_country_explore     <- input$focus_country_explore
    focus_year_explore        <- input$focus_year_explore
    mostrecent_explore        <- input$mostrecent_explore
    focus_data_source_explore <- input$focus_data_source_explore
    focus_indicator_explore   <- input$focus_indicator_explore
    focus_country_explore     <- input$focus_country_explore
    focus_dimension_explore <- input$focus_dimension_explore
    dataTableItems        <- input$dataTableItems
    sigfig                <- input$sumsigfig
    
    if(is.null(sigfig)) return()
    
    
  })
  
  
  # These will trigger a change
  debounce_disag_explore_table()
  
  
  droptable <- FALSE
  emptyData <- FALSE
  
  if(is.null(focus_year_explore)) droptable <- TRUE
  if(is.null(dataTableItems)) droptable <- TRUE
  if(trimws(focus_country_explore) == "") droptable <- TRUE
  
  
  if(!droptable){
    
    
    
    
    # grab the data
    theData <- getDisagData(indicator=focus_indicator_explore,
                            stratifier=focus_dimension_explore,  # in hetkdb.R
                            countries=focus_country_explore,
                            years=focus_year_explore,
                            mostrecent=mostrecent_explore,
                            datasource=focus_data_source_explore)
    
    
    # test if there are issues with the data like missing etc
    thetest <- !is.null(theData) && nrow(theData)>0 && sum(is.na(theData$estimate))!=nrow(theData)
    # nodataMSG <-"There is no data for this combination of variables."
    nodataMSG <- translate("nodata", input$lang)
    
    # if the data is problematic then return the no data message
    if(!thetest) {
      droptable <- TRUE
      emptyData <- TRUE
    }
    
    
    # format the data
    theData<-theData %>%
      rename(
        Setting                = country,
        Year                   = year,
        `Data source`          = source,
        `Health indicator abbreviation`     = indic,
        `Inequality dimension` = dimension,
        Subgroup               = subgroup,
        Estimate               = estimate,
        `95%CI lower bound`   = lower_95ci,
        `95%CI upper bound`    = upper_95ci,
        `Population share %`   = popshare,
        `Setting average`    = national,
        Flag                   = flag,
        `Health indicator name` = indic_name
      ) %>% 
      mutate(
        indic = `Health indicator abbreviation`
      )
    
    alltableitems <-  .rdata[['all_table_variables']]$table_vars
    
    selectedtableitems <- c(alltableitems[alltableitems%in%dataTableItems], "indic")
    
    theData <- theData[, selectedtableitems, drop=FALSE]
    
    
    vartypes <- .rdata[['all_table_variables']]$var_type
    alltableitems <- .rdata[['all_table_variables']]$table_vars
    selectedtableitems<-alltableitems[alltableitems%in%dataTableItems]
    
  }
  
  
  
  output$dataTable_inside <- DT::renderDataTable({
    
    if(droptable) return()
    cols <- 1:length(selectedtableitems)
    indx <- which("Health indicator name"==selectedtableitems)
    indxNarrow <- cols[!cols%in%indx]
    
    indxCenter1<- which(vartypes[alltableitems%in%selectedtableitems]=="numeric")
    indxCenter2 <- which(selectedtableitems%in%.rdata[["centered_table_variables"]])
    
    indxCenter <- c(indxCenter1, indxCenter2)
    
    
    # In case I want to edit the other columns
    indxOther <- cols[!cols%in%indxCenter]
    
    
    
    dataTable_options <- list(
      pageLength = 100,
      #autoWidth = TRUE,
      dom='frtp',
      scrollX = TRUE,
      scrollY = '600px',
      processing = FALSE,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(className = "table-text-center", targets = indxCenter-1),
        list(className = "table-text-nocenter", targets = indxOther-1)
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#E8E7E7', 'color': '#5A5959', 'padding-bottom': '3px dotted grey'});",
        
        "}"
      ),
      language = list(
        search = paste0(translate("table_search", input$lang), ":"),
        paginate = list(
          `next` = translate("table_next", input$lang),
          previous = translate("table_previous", input$lang)
        )
      )
    )
    
    if(length(indx)!=0){
      theData[["Health indicator name"]] <-
        forceNonBreak(theData[["Health indicator name"]], maxCharPerLine = 30)
    }
    
    
    
    # *************************************************
    # If the health indicator name or summary measure name exist
    # grab their index from above and use to tell datatables
    # which to be wider
    # *************************************************
    indxSigFig <- which(selectedtableitems%in%c("Estimate", "95%CI lower bound", "95%CI upper bound",
                                                "Population share %", "Setting average"))
    
    cloneData <- theData
    
    if ("Summary measure abbreviation" %in% colnames(theData)) {
      cloneData <- mutate(
        cloneData,
        "Summary measure abbreviation" = toupper(`Summary measure abbreviation`)
      )
    }
    
    if (HEATversion == "whodata") {
      cloneData <- cloneData %>% 
        mutate(
          Subgroup = as.character(Subgroup)
        ) %>% 
        mutate_at(
          c("Setting", "Inequality dimension", "Subgroup"),
          funs(map_chr(., translate, lang = input$lang))
        )
      
      if ("Health indicator name" %in% names(cloneData)) {
        cloneData <- mutate(
          cloneData,
          `Health indicator name` = map_chr(indic, translate, lang = input$lang)
        )
      }
    }
      
    cloneData <- cloneData %>% 
      select(-indic) %>% 
      select_all(funs(map_chr(., translate, lang = input$lang)))
    
    datatable(
      cloneData,
      options = dataTable_options,
      filter = "none",
      rownames = FALSE,
      escape = FALSE
    ) %>% 
      formatRound(columns = indxSigFig, digits = sigfig)
    
  }) # end renderDataTable
  
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  
  
  return(
    list(
      tags$div(class="datawarning", helpText(msg)),
      DT::dataTableOutput("dataTable_inside")
    )
  )
  
  
  
  
})



#******************************************************************************
#******************************************************************************
# SUMMARY EXPLORE DATA TABLE ----
#******************************************************************************
#******************************************************************************


output$dataTableInequal <- renderUI({
  
  
  debounce_summary_explore_table()
  
  droptable <- FALSE
  emptyData <- FALSE
  # dataMSG <- "If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  dataMSG <- translate("noestimates", input$lang)
  
  isolate({
    summary_plot_type_explore   <- input$summary_plot_type_explore
    summary_plot_mode_explore <-  if (toggleHC) "hc" else "ggplot" #input$summary_plot_mode_explore
    focus_indicator_explore          <- input$focus_indicator_explore
    focus_country_explore            <- input$focus_country_explore
    focus_year_explore               <- input$focus_year_explore
    mostrecent_explore               <- input$mostrecent_explore
    focus_data_source_explore        <- input$focus_data_source_explore
    focus_dimension_explore          <- input$focus_dimension_explore
    sigfig       <- input$sumsigfig2
    dataTableItemsSummary <- input$dataTableItemsSummary
    focus_inequal_type_explore_table  <- input$focus_inequal_type_explore_table 
    if(is.null(sigfig)) return()
    
    lang <- input$lang
  }) 
  
  
  if(is.null(focus_year_explore)) droptable <- TRUE
  if(is.null(dataTableItemsSummary)) droptable <- TRUE
  if(trimws(focus_country_explore) == "") droptable <- TRUE
  #if(!any(class(.rdata[['maindata']])%in%"data.frame")) droptable <- TRUE
  
  if(!droptable){
    
    theData <- getInequalData(indicator     = focus_indicator_explore,
                              stratifier    = focus_dimension_explore,
                              countries     = focus_country_explore,
                              years         = focus_year_explore,
                              mostrecent    = mostrecent_explore,
                              datasource    = focus_data_source_explore,
                              inequal_types = focus_inequal_type_explore_table)
    
    thetest <- !is.null(theData ) && nrow(theData )>0
    
    # nodataMSG <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
    nodataMSG <- translate("noestimates", lang)
    
    if(!thetest) {
      droptable <- TRUE
      emptyData <- TRUE
    }
    
    
    
    theData <- select(theData, -ccode)
    
    theData <- mutate(
      theData, 
      measure_name = map_chr(measure, translate, lang)
    )
    
    if (HEATversion == "whodata") { 
      theData <- mutate(
        theData, 
        indic_name = map_chr(indic, translate, lang),
        dimension = map_chr(dimension, translate, lang)
      )
    }
    
    theData <- theData %>% 
      rename(
        ##`Bootstrap 95%CI upper bound` = boot.upperci,
        #`Bootstrap 95%CI lower bound` = boot.lowerci,
        `95%CI upper bound`          = se.upperci,
        `95%CI lower bound`          = se.lowerci,
        Setting                = country,
        Year                   = year,
        `Health indicator abbreviation`     = indic,
        `Inequality dimension` = dimension,
        `Estimate`    = inequal,
        `Summary measure abbreviation` = measure,
        `Data source`          = source,
        `Setting average`               = estimate,
        `Health indicator name` = indic_name,
        `Summary measure name` = measure_name
      ) %>%
      mutate(
        indic = `Health indicator abbreviation`,
        measure = `Summary measure abbreviation`
      )
    
    
    alltableitems <- .rdata[['all_table_variables_summary']]$table_vars
    selectedtableitems <- c(alltableitems[alltableitems%in%dataTableItemsSummary], "indic", "measure")
    theData <- theData[, selectedtableitems, drop=FALSE]
    
    
    vartypes <- .rdata[['all_table_variables_summary']]$var_type
    alltableitems <- .rdata[['all_table_variables_summary']]$table_vars
    
    selectedtableitems<-alltableitems[alltableitems%in%dataTableItemsSummary]
    
    
    
  }
  
  output$dataTableInequal_inside <- DT::renderDataTable({
    if(droptable) return()
    
    indx1 <- which("Health indicator name"==selectedtableitems)
    indx2 <- which("Summary measure name"==selectedtableitems)
    
    
    cols <- 1:length(selectedtableitems)
    
    indxCenter1<- which(vartypes[alltableitems%in%selectedtableitems]=="numeric")
    indxCenter2 <- which(selectedtableitems%in%.rdata[["centered_table_variables"]])
    
    indxCenter <- c(indxCenter1, indxCenter2)
    
    
    # In case I want to edit the other columns
    indxOther <- cols[!cols%in%indxCenter]
    
    
    dataTableInequal_options <- list(
      pageLength = 100, 
      dom='frtp', 
      scrollX = TRUE, 
      scrollY = '600px',
      processing = FALSE,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(className = "table-text-center", targets = indxCenter-1),
        list(className = "table-text-nocenter", targets = indxOther-1)
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#E8E7E7', 'color': '#5A5959', 'padding-bottom': '3px dotted grey'});",
        
        "}"),
      language = list(
        search = paste0(translate("table_search", input$lang), ":"),
        paginate = list(
          `next` = translate("table_next", input$lang),
          previous = translate("table_previous", input$lang)
        )
      )
    )
    
    
    if(length(indx1)!=0){
      theData[["Health indicator name"]] <- forceNonBreak(theData[["Health indicator name"]], 
                                                          maxCharPerLine = 30)
    }
    
    if(length(indx2)!=0){
      theData[["Summary measure name"]] <- forceNonBreak(theData[["Summary measure name"]], 
                                                         maxCharPerLine = 30)
    }
    
    
    
    # *************************************************
    # Return the table
    # *************************************************
    
    
    indxSigFig <- which(selectedtableitems%in%c("Estimate", "95%CI lower bound", 
                                                "95%CI upper bound",
                                                "Setting average"))
    
    cloneData <- theData
    
    if ("Summary measure abbreviation" %in% colnames(theData)) {
      cloneData <- mutate(
        cloneData,
        "Summary measure abbreviation" = toupper(`Summary measure abbreviation`)
      )
    }
    
    if (HEATversion == "whodata") {
      cloneData <- cloneData %>% 
        mutate_at(
          c("Setting", "Inequality dimension"),
          funs(map_chr(., translate, lang = input$lang))
        )
      
      if ("Health indicator name" %in% names(cloneData)) {
        cloneData <- mutate(
          cloneData,
          `Health indicator name` = map_chr(indic, translate, lang = input$lang)
        )
      }
      
      if ("Summary measure name" %in% names(cloneData)) {
        cloneData <- mutate(
          cloneData,
          `Summary measure name` = measure %>% 
            map_chr(translate, lang = input$lang) %>% 
            str_remove("\\s+\\(.*\\)\\s*$")
        )
      }
    }
    
    cloneData <- cloneData %>% 
      select(-indic, -measure) %>% 
      select_all(funs(map_chr(., translate, lang = input$lang)))
    
    datatable(cloneData, options = dataTableInequal_options, 
              filter="none", escape=FALSE, rownames = FALSE) %>% 
      formatRound(columns=indxSigFig, digits = sigfig)
  })
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  
  return(
    list(
      tags$div(class="datawarning", helpText(msg)),
      DT::dataTableOutput("dataTableInequal_inside")
    )
  )
  
})

#******************************************************************************
#******************************************************************************
# REACTIVES TO GET COMPARE PLOTTING DATA
#******************************************************************************
#******************************************************************************
# 
# 
# getBenchmarkData <- reactive({
#   
#   
#   if(is.null(input$benchmarkYears)) return()
#   
#   anchordata<-getDisagData(indicator=input$focus_indicator_compare, 
#                            stratifier=input$focus_dimension_compare,  # in hetkdb.R
#                            countries=input$focus_country_compare, 
#                            years=input$focus_year_compare, 
#                            mostrecent=input$mostrecent_compare,
#                            datasource=input$focus_data_source_compare)
#   
#   
#   if(is.null(anchordata) || nrow(anchordata)==0) return()
#   anchordata$anchor <- 1
# 
#   
#       benchmarkdata <- NULL
#   
#   if(!is.null(input$benchmark_countries)){
#   benchmarkdata <- getDisagData(indicator = input$focus_indicator_compare, 
#                                 stratifier = input$focus_dimension_compare, 
#                                 countries = input$benchmark_countries, 
#                                 years =  input$focus_year_compare, 
#                                 mostrecent = input$mostrecent_compare,
#                                 datasource = input$focus_data_source_compare,
#                                 elasticity = input$benchmarkYears,
#                                 anchor_country = input$focus_country_compare)
# }
#   
#   if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
#     benchmarkdata$anchor <- 0
#     theData <- rbind(anchordata, benchmarkdata) 
#   }else{
#     
#     
#     theData <- anchordata
#   }
#   
# 
#   return(theData)
# })


# ----- SUMMARY DATA -----------------------------

# getBenchmarkDataSum <- reactive({
#   
#   
#   if(is.null(input$focus_inequal_type_compare)) return()
#   
#   anchordata <- getInequalData(indicator=input$focus_indicator_compare,  
#                                stratifier=input$focus_dimension_compare, 
#                                countries=input$focus_country_compare, 
#                                years=input$focus_year_compare, 
#                                mostrecent=input$mostrecent_compare,
#                                datasource=input$focus_data_source_compare,  
#                                inequal_types=input$focus_inequal_type_compare,
#                                elasticity = input$benchmarkYears,
#                                multiplier1 = input$summultiplier1,
#                                multiplier2 = input$summultiplier2)
#   
#   if(is.null(anchordata) || nrow(anchordata)==0) return()
#   anchordata$anchor <- 1
#   
#     benchmarkdata <- NULL
#   
#   if(!is.null(input$benchmark_countries)){
#   benchmarkdata <- getInequalData(indicator=input$focus_indicator_compare,  
#                                   stratifier=input$focus_dimension_compare, 
#                                   countries=input$benchmark_countries, 
#                                   years=input$focus_year_compare, 
#                                   mostrecent=input$mostrecent_compare,
#                                   datasource=input$focus_data_source_compare,  
#                                   inequal_types=input$focus_inequal_type_compare,
#                                   elasticity = input$benchmarkYears,
#                                   multiplier1 = input$summultiplier1,
#                                   multiplier2 = input$summultiplier2,
#                                   anchorCountry = input$focus_country_compare)
#   }
#   
#   if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
#     benchmarkdata$anchor <- 0
#     theData <- rbind(anchordata, benchmarkdata) 
#   }else{
#     
#     
#     
#     theData <- anchordata
#   }
#   
#   return(theData)
# })


#******************************************************************************
#******************************************************************************
# PLOTTING USER INTERFACES
#******************************************************************************
#******************************************************************************

# ----- DISAGGREGATED PLOT EXPLORE -----------------------------
# fakeFailure <- TRUE

output$disag_plot_explore <- renderUI({ 
  
  
  
  debounce_disag_explore()
  
  if(is.null(input$disag_plot_type_explore )) return()
  #if(is.null(input$disag_plot_mode_explore )) return()
  # if(is.null(input$plot_height1)) return()
  # if(is.null(input$plot_width1)) return()
  # h <- input$plot_height1
  # w <- input$plot_width1
  
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin1))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax1))
  isValid1 <- input$axis_limitsmin1 == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax1 == "" | !is.na(axismax)
  longnames <- input$long_names1
  disag_error_bars <- input$disag_error_bars
  
  isolate({
    
    if(is.null(input$axis_limitsmin1 )) return()
    if(is.null(input$axis_limitsmax1 )) return()
    
    
    disag_plot_type_explore <- input$disag_plot_type_explore
    disag_plot_mode_explore <- if (toggleHC) "hc" else "ggplot" #input$disag_plot_mode_explore
    focus_country_explore     <- input$focus_country_explore
    focus_year_explore        <- input$focus_year_explore
    mostrecent_explore        <- input$mostrecent_explore
    focus_data_source_explore <- input$focus_data_source_explore
    focus_indicator_explore   <- input$focus_indicator_explore
    focus_country_explore     <- input$focus_country_explore 
    focus_dimension_explore   <- input$focus_dimension_explore
    
  })
  
  
  
  
  
  
  .rdata[["disag_plot_explore"]] <<- .rdata[["blank_plot"]]
  
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  # dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  dataMSG <- translate("noestimates", input$lang)
  
  
  
  .rdata[["plotDisag_explore_title"]] <<- ""
  .rdata[["plotDisag_explore_title_static"]] <<- ""
  
  
  
  dropplot <- FALSE
  emptyData <- FALSE
  #emptySelect <- FALSE
  # nodataMSG <-"There is no data for this combination of variables."
  nodataMSG <- translate("nodata", input$lang)
  #emptySelectMsg <- "One of the required select boxes is empty."
  
  isolate({
    if(any(sapply(list(input$focus_indicator_explore, input$focus_data_source_explore, 
                       input$focus_dimension_explore, input$focus_year_explore), is.null))){
      dropplot <- TRUE
      emptyData <- TRUE
    }
  })
  
  
  # This was added so we could use debounce
  if(.rdata[['first_time']]){
    .rdata[['first_time']] <<- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  validAxis <- TRUE
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) {
    validAxis <- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  # if (fakeFailure) {
  #   dropplot <- TRUE
  # }
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if (!dropplot) {
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    plotData <- getDisagData(indicator  = focus_indicator_explore, 
                             stratifier = focus_dimension_explore,  # in hetkdb.R
                             countries  = focus_country_explore, 
                             years      = focus_year_explore, 
                             mostrecent = mostrecent_explore,
                             datasource = focus_data_source_explore)
    plotData <- filter(plotData, !is.na(estimate))
    plotData <- plot_decimals_setup(plotData, type = "disag")
    
    # This is definitely not ideal, but is intended to make the 
    # interactive graphics have long or non-long indicator names
    # if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
    #   plotData$indic_title <- translate(plotData$indic, input$lang)
    # }else{
    #   plotData$indic_title <- vapply(plotData$indic, translate, character(1), input$lang)
    # }
    if (HEATversion == "whodata") {
      plotData$indic_title <- vapply(plotData$indic, translate, character(1), input$lang)
    } else {
      plotData$indic_title <- plotData$indic_name
    }
    
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************
    
    thetest <- !is.null(plotData) && nrow(plotData)>0 && sum(is.na(plotData$estimate))!=nrow(plotData)
    
    
    if(!thetest) {
      dropplot <- TRUE
      emptyData <- TRUE
    }
    
    
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    yrs <- paste(sort(unique(plotData$year)), collapse = ", ")
    sources <- paste(sort(sort(unique(plotData$source))), collapse = " & ")
    
    if (HEATversion == "whodata") {
      focus_country <- translate(.rdata[['focus_country']], input$lang)
    } else {
      focus_country <- .rdata[["focus_country"]]
    }
    
    tmp_title <- paste0(focus_country, " (", sources, ", ", yrs, ")")
    
    if(thetest){
      .rdata[["plotDisag_explore_title"]] <<- tmp_title
      .rdata[["plotDisag_explore_title_static"]] <<- tmp_title
    }

    
    updateTextInput(session, "main_title1", value = .rdata[["plotDisag_explore_title"]])
    
    
    
    
    #tmpSubgroup <- plotData$subgroup
    #plotData$subgroup<-as.character(plotData$subgroup)
    
    plotData <- left_join(plotData, .rdata[['dimension_details']], 
                          by=c("dimension", "subgroup", "order"))
    
    
    plotData <- order_dimensions(plotData)
    
    
    # Saving data so we can use in the modal
    .rdata[['focus_plot_data_disag_explore']] <<- plotData
    
  }
  
  output$disag_plot_explore_insideGG <- renderPlot({
    
    input$xaxis_title1
    input$yaxis_title1
    .trigger$disag_plot_explore_title
    # input$plot_height1
    # input$plot_width1
    # input$axis_limitsmin1
    # input$axis_limitsmax1
    # input$plot_height1
    # input$plot_width1
    #input$long_names1
    
    # git681
    if(dropplot || disag_plot_mode_explore == "hc") return()
    doPlotType <- get(paste0("plotDisag", isolate(disag_plot_type_explore), "_explore_ggplot"))
    p <- doPlotType(plotData)
    
    .rdata[["disag_plot_explore"]] <<- p
    
    p  
  }, res=90, height = 650, width = 650)
  
  

  output$disag_plot_explore_insideHC <- renderUI({
    nudgeOnInit()
    
    input$xaxis_title1
    input$yaxis_title1
    #input$plot_height1
    #input$long_names1
    .trigger$disag_plot_explore_title
    
    # git681
    if(dropplot || disag_plot_mode_explore == "ggplot") return()
    doPlotType <- get(paste0("plotDisag", isolate(disag_plot_type_explore), "_explore_hc"))
    p <- doPlotType(plotData, confInt = disag_error_bars )
    
    
    p
  })
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  if(!validAxis) msg <- "Please supply a valid number"
  returnList <- list(tags$div(class="datawarning", helpText(msg)))
  
  if(disag_plot_mode_explore == "ggplot"){
    returnList[[2]] <- plotOutput("disag_plot_explore_insideGG")
  } else {
    returnList[[2]] <- uiOutput("disag_plot_explore_insideHC")
  }
  
  return(returnList)
  
  
})  

nudgeOnInit <- reactive({
  # message("<NUDGE>")
  debounce_disag_explore()
})

observeEvent(nudgeOnInit(), ignoreInit = TRUE, once = TRUE, {
  # message("<DESTROY NUDGE>")
  # fakeFailure <<- FALSE
  nudgeOnInit <<- reactive(NULL)
})

# ----- DETAILED BAR PLOT -----------------------------



output$disag_plot_explore_dtl <- renderUI({ 
  
  # This is triggered by subgroups
  
  debounce_disag_explore_dtl()
  
  # if(is.null(input$plot_height_dtl)) return()
  # if(is.null(input$plot_width_dtl)) return()
  # h <- input$plot_height_dtl
  # w <- input$plot_width_dtl
  
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin_dtl))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax_dtl))
  isValid1 <- input$axis_limitsmin_dtl == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax_dtl == "" | !is.na(axismax)
  
  
  isolate({
    longnames <- TRUE # input$long_names_dtl
    if(is.null(input$axis_limitsmin_dtl )) return()
    if(is.null(input$axis_limitsmax_dtl )) return()
    focus_country_explore <- input$focus_country_explore
    focus_data_source_explore_dtlmap <- input$focus_data_source_explore_dtlmap
    mostrecent_explore_dtlmap <- input$mostrecent_explore_dtlmap
    focus_year_explore_dtlmap <- input$focus_year_explore_dtlmap
    focus_indicator_explore_plotdtl<- input$focus_indicator_explore_plotdtl
    focus_dimension_explore_dtl <- input$focus_dimension_explore_dtl
    disag_plot_explore_dtl_sort <- input$disag_plot_explore_dtl_sort
    sortBy_ind_dim <- input$sortBy_ind_dim
    disag_plot_explore_dtl_subgroups <- input$disag_plot_explore_dtl_subgroups
    disag_plot_explore_dtl_showAVG <- input$disag_plot_explore_dtl_showAVG
    disag_plot_explore_dtl_showMedian <- input$disag_plot_explore_dtl_showMedian
    disag_plot_mode_explore_dtl <- if (toggleHC) "hc" else "ggplot" #input$disag_plot_mode_explore_dtl
    #disag_plot_explore_dtl_showNames <- input$disag_plot_explore_dtl_showNames
    
    if(is.null(disag_plot_explore_dtl_sort)) return()
  })
  
  sortBy_var <- disag_plot_explore_dtl_sort
  if(sortBy_ind_dim == "Dimension"){
    sortBy_var <- "subgroup"
  }
  
  
  # I had to use the global rather than the reactive because
  # the reactive was not being updated in time
  #disag_plot_explore_dtl_sort <- .rdata[['focus_plotdtl_sort']]
  #if(is.null(disag_plot_explore_dtl_sort)) return()
  
  .rdata[["plotDisag_explore_title_dtl"]] <<- ""
  .rdata[["plotDisag_explore_title_dtl_static"]] <<- ""
  
  
  dropplot <- FALSE
  emptyData <- FALSE
  # dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  dataMSG <- translate("noestimates", input$lang)
  
  # nodataMSG <-"There is no data for this combination of variables."
  nodataMSG <- translate("nodata", input$lang)
  
  if(!disag_plot_explore_dtl_sort[1]%in%focus_indicator_explore_plotdtl || 
     is.null(focus_dimension_explore_dtl)){
    dropplot <- TRUE
    if(disag_plot_explore_dtl_sort[1] == "No data") emptyData <- TRUE
  }
  
  
  
  validAxis <- TRUE
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) {
    validAxis <- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){
    
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    # I can isolate dimension because dimension change ALWAYS changes the 
    # subgroups. I can isolate indicator because indicator ALWAYS changes
    # the sort by list
    plotData <- getDisagData(indicator=focus_indicator_explore_plotdtl, 
                             stratifier=focus_dimension_explore_dtl,  # in hetkdb.R
                             countries=focus_country_explore, 
                             years=focus_year_explore_dtlmap, 
                             mostrecent=mostrecent_explore_dtlmap,
                             datasource=focus_data_source_explore_dtlmap)
    # plotData <- filter(plotData, !is.na(estimate))
    plotData <- mutate(plotData, estimate = ifelse(is.na(estimate), -9999, estimate))
    plotData <- plot_decimals_setup(plotData, type = "disag")
    
    
    
    
    
    # This is definitely not ideal, but is intended to make the 
    # interactive graphics have long or non-long indicator names
    
    if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
      plotData$indic_title <- plotData$indic
    }else{
      plotData$indic_title <- plotData$indic_name
    }
    
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************
    
    thetest <- !is.null(plotData) && 
      nrow(plotData)>0 && 
      sum(is.na(plotData$estimate))!=nrow(plotData) &&
      disag_plot_explore_dtl_sort %in% unique(plotData$indic)
    
    if(!thetest) {
      dropplot <- TRUE
      emptyData <- TRUE
    }
    #return(list(tags$div(class="datawarning", helpText(nodataMSG))))
    
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    yrs <- paste(sort(unique(plotData$year)), collapse = ", ")
    sources <- paste(sort(sort(unique(plotData$source))), collapse = " & ")
    if(thetest){
      if (HEATversion == "whodata") {
        focus_country <- translate(.rdata[["focus_country"]], input$lang)
      } else {
        focus_country <- .rdata[["focus_country"]]
      }
      .rdata[["plotDisag_explore_title_dtl"]] <<- paste0(focus_country, " (", sources, ", ", yrs, ")")
      .rdata[["plotDisag_explore_title_dtl_static"]] <<- .rdata[["plotDisag_explore_title_dtl"]]

    }
    updateTextInput(session, "main_title_dtl", value = .rdata[["plotDisag_explore_title_dtl"]])
    
    
    
    plotData <- left_join(plotData, .rdata[['dimension_details']], 
                          by=c("dimension", "subgroup", "order"))
    
    plotData <- order_dimensions(plotData)
    
    
    
    focus_indic <- focus_indicator_explore_plotdtl[focus_indicator_explore_plotdtl%in%unique(plotData$indic)]
    
    .rdata[['focus_plot_data_disag_dtl_explore']] <<- plotData
    .rdata[["plotDisag_explore_dtl_sortvar"]] <<- sortBy_var
    .rdata[["plotDisag_explore_dtl_subgroups"]] <<- disag_plot_explore_dtl_subgroups
    .rdata[["plotDisag_explore_dtl_showAvg"]] <<- disag_plot_explore_dtl_showAVG
    .rdata[["plotDisag_explore_dtl_showMedian"]] <<- disag_plot_explore_dtl_showMedian
    .rdata[["plotDisag_explore_dtl_indicSort"]] <<- focus_indic
    #************************************************
    # Get specialized R and D summary measures 
    #************************************************
    
    
    # n_subgroup <- length(unique(plotData$subgroup))
    # 
    # if(n_subgroup>=30){
    # rd_percentiles <- r_d_percentile(plotData, n_subgroup)
    # }
  }
  
  
  output$disag_plot_explore_dtl_insideGG <- renderPlot({
    
    input$xaxis_title_dtl
    input$yaxis_title_dtl
    .trigger$disag_plot_explore_dtl_title
    # input$long_names_dtl
    
    # git681
    if(dropplot || disag_plot_mode_explore_dtl == "hc") return()
    p <- plotDetailBar_explore_ggplot(plotData,
                                      sortBy = sortBy_var,
                                      regs = disag_plot_explore_dtl_subgroups,
                                      showAVG = disag_plot_explore_dtl_showAVG,
                                      showMedian = disag_plot_explore_dtl_showMedian,
                                      #addGroupNames = disag_plot_explore_dtl_showNames,
                                      indicSort = focus_indic)
    
    .rdata[["disag_plot_explore_dtl"]] <<- p
    
    p  
  }, res=90, height = 650, width = 650)
  
  
  
  #saveRDS(plotData, "/Users/zevross/git-repos/who-heat/who-heat/debug/detailed_bar/detailed_bar_data.RDS")
  output$disag_plot_explore_dtl_insideHC <- renderUI({
    
    .trigger$disag_plot_explore_dtl_title
    input$xaxis_title_dtl
    input$yaxis_title_dtl
    if(dropplot) return()
    p <- plotDetailBar_explore_hc(plotData,
                                  sortBy = sortBy_var,
                                  regs = disag_plot_explore_dtl_subgroups,
                                  showAVG = disag_plot_explore_dtl_showAVG,
                                  showMedian = disag_plot_explore_dtl_showMedian,
                                  #addGroupNames = disag_plot_explore_dtl_showNames,
                                  indicSort = focus_indic)#, 
    
    p
    
  })
  
  
  
  
  
  
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  returnList <- list(tags$div(class="datawarning", helpText(msg)))
  
  
  if(disag_plot_mode_explore_dtl == "ggplot"){
    returnList[[2]] <- plotOutput("disag_plot_explore_dtl_insideGG")
  } else {
    returnList[[2]] <- uiOutput("disag_plot_explore_dtl_insideHC")
  }
  
  
  return(returnList)
  
  
})  


# ----- DISAGGREGATED MAP-----------------------------



output$disag_plot_explore_map <- renderUI({ 
  #browser()
  #print(paste(Sys.time(), "In disag_plot_explore_map", "input$focus_year_explore_dtlmap is ", isolate(input$focus_year_explore_dtlmap)))

  # using debounce is making it disappear again
  #.trigger$runplot_disag_map
  debounce_disag_explore_map()
  
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin_map))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax_map))
  isValid1 <- input$axis_limitsmin_map == "" || !is.na(axismin)
  isValid2 <- input$axis_limitsmax_map == "" || !is.na(axismax)
  
  isolate({
    #   if(is.null(input$axis_limitsmin_dtl )) return()
    #   if(is.null(input$axis_limitsmax_dtl )) return()
    longnames <- TRUE # input$long_names_dtl
    focus_country_explore <- input$focus_country_explore
    focus_data_source_explore_dtlmap <- input$focus_data_source_explore_dtlmap
    mostrecent_explore_dtlmap <- input$mostrecent_explore_dtlmap
    focus_year_explore_dtlmap <- input$focus_year_explore_dtlmap
    focus_indicator_explore_map <- input$focus_indicator_explore_map
    #sub_title_map <- input$sub_title_map
    main_title_map <- input$main_title_map
    #focus_data_source_explore <- input$focus_data_source_explore
  })
  
  if (is.null(main_title_map)) return()
  
  .rdata[["plotDisag_explore_title_map"]] <<- ""
  .rdata[["plotDisag_explore_title_map_static"]] <<- ""
  
  dropplot <- FALSE
  noGeo <- FALSE
  
  # dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  # nodataMSG <-"There is no data for this combination of variables."
  # noGeoMSG <- "There is no geographic data for this country/year."
  lang <- input$lang
  dataMSG <- translate("noestimates_spatial", lang)
  nodataMSG <- translate("nodata", lang)
  noGeoMSG <- translate("nospatial", lang)
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!"DHS"%in%isolate(focus_data_source_explore_dtlmap)){
    dropplot <-TRUE
    noGeo <- TRUE
  }
  
  if(!dropplot){
    
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    # I can isolate dimension because dimension change ALWAYS changes the
    # subgroups. I can isolate indicator because indicator ALWAYS changes
    # the sort by list
    
    plotData <- getDisagData(indicator=focus_indicator_explore_map,
                             stratifier="Subnational region",  # in hetkdb.R
                             countries=focus_country_explore,
                             years=focus_year_explore_dtlmap,
                             mostrecent=mostrecent_explore_dtlmap,
                             datasource=focus_data_source_explore_dtlmap)
    
    n_data_rows <- plotData %>% 
      filter(source == "DHS") %>% 
      nrow()
    
    plotData <- filter(plotData, !is.na(estimate))
    plotData <- plot_decimals_setup(plotData, type = "disag")
    
    n_plotData <- nrow(plotData)
    
    plotData <- filter(plotData, source == "DHS")
    
    if(mostrecent_explore_dtlmap){
      focus_year_explore_dtlmap <- getFilteredYear(country=focus_country_explore, "DHS")[1]
    }
    
    geodata_both <- assemble_country_geography(focus_country_explore, 
                                               focus_year_explore_dtlmap)
    
    # This is definitely not ideal, but is intended to make the
    # interactive graphics have long or non-long indicator names
    
    if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
      plotData$indic_title <- plotData$indic
    }else{
      plotData$indic_title <- plotData$indic_name
    }
    
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************

    thetest <- !is.null(plotData) &&
      nrow(plotData)>0 &&
      sum(is.na(plotData$estimate))!=nrow(plotData) 
    
    if(!thetest) {
      dropplot <- TRUE
      noGeo <- TRUE
    }
    
    subs_geo <- subgroups_from_geojson(geodata_both$geodata_subnational)
    
    thetest <- !is.null(geodata_both$geodata_subnational) &&
      !"tbl_df"%in%class(geodata_both$geodata_subnational)>0 &&
      !focus_country_explore%in%.rdata$country_drop_geography &&
      length(subs_geo)>0 &&
      !(n_plotData>0 & nrow(plotData) == 0)
    
    if(!thetest) {
      dropplot <- TRUE
      noGeo <- TRUE
    }
    
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    
    yrs <- paste(sort(unique(plotData$year)), collapse = ", ")
    sources <- paste(sort(sort(unique(plotData$source))), collapse = " & ")
    
    if (HEATversion == "whodata") {
      focus_country <- translate(.rdata[['focus_country']], input$lang)
    } else {
      focus_country <- .rdata[["focus_country"]]
    }
    
    tmp_title <- paste0(focus_country, " (", sources, ", ", yrs, ")")
    
    if(thetest){
      .rdata[["plotDisag_explore_title_map"]] <<- tmp_title
      .rdata[["plotDisag_explore_title_map_static"]] <<- tmp_title
    }


    updateTextInput(session, "main_title_map", value = .rdata[["plotDisag_explore_title_map"]])
    
    # tmp_subtitle <- plotData$indic_name[1]
    # .rdata[["plotDisag_explore_subtitle_map"]] <<- tmp_subtitle
    # .rdata[["plotDisag_explore_subtitle_map_static"]] <<- tmp_subtitle
    # updateTextInput(session, "sub_title_map", value = .rdata[["plotDisag_explore_subtitle_map"]])
    
    # plotData <- left_join(plotData, .rdata[['dimension_details']],
    #                       by=c("dimension", "subgroup", "order"))
    # 
    # plotData <- order_dimensions(plotData)
    
    #.rdata[['focus_plot_data_disag_dtl_explore']] <<- plotData
    
    #focus_indic <- focus_indicator_explore_plotdtl[focus_indicator_explore_plotdtl%in%unique(plotData$indic)]
    
    minmaxvals <- select(plotData, country,  source, indic) %>% 
      slice(1) %>% 
      semi_join(.rdata$subregion_minmax, ., 
                by = c("country", "source", "indicator_abbr" = "indic"))

    plotData <- plotData %>% 
      select(
        country, year, subgroup, indic_name, estimate, lower_95ci, upper_95ci,
        national, dimension, popshare
      ) %>% 
      mutate(
        subgroup =  str_to_lower(str_remove(subgroup, "^\\s*\\d+\\s*"))
      )
    .rdata[['focus_plot_data_disag_map_explore']] <<- list(plotData = plotData, 
                                                           geodata = geodata_both,
                                                           minhc = minmaxvals$min,
                                                           maxhc = minmaxvals$max)
    
    #git1104

    subs_data <- unique(plotData$subgroup)
    
    # at least 50% of WHO data subgroups exist   
    if (!dropplot) {
      too_few_subregions <- (sum(subs_data %in% subs_geo) / n_data_rows) < 0.50

      if (too_few_subregions) {
        dropplot <- TRUE
        noGeo <- TRUE
      }
    }
  }
  
  
  
  output$disag_plot_explore_map_insideHC <- renderUI({
    
    #map_chr(geodata[[2]],  c( "properties", "subgroup"))
    .trigger$disag_plot_explore_map_title
    .trigger$disag_plot_explore_map_subtitle
    
    if(dropplot) return()
    
    #hc_min <- if_else(grepl("%", plotData$indic_name[1]), 0, min(plotData$estimate, na.rm = TRUE))
    #hc_max <- if_else(grepl("%", plotData$indic_name[1]), 100, max(plotData$estimate, na.rm = TRUE))
    
    list(
      plotDetailMap_explore_hc(plotData,geodata_both, 
                               minmaxvals$min, minmaxvals$max),
      tags$div(
        class = "datawarning font-10", 
        helpText(translate("heat_map_disclaimer_1", input$lang)),
        helpText(translate("heat_map_disclaimer_2", input$lang))
      )
    )
  })
  
  #if(emptyData){msg <- nodataMSG}
  #if(!emptyData && noGeo){msg <- noGeoMSG}
  if(!noGeo){msg <- dataMSG}
  if(noGeo) msg <- noGeoMSG
  returnList <- list(
    tags$div(
      class = "datawarning",
      helpText(msg)
    ),
    NULL
  )
  
  #returnList <- list(tags$div(class="datawarning", helpText("Test message")))
  returnList[[2]] <- uiOutput("disag_plot_explore_map_insideHC")
  
  
  
  return(returnList)
  
})  



# ----- SUMMARY PLOT EXPLORE -----------------------------

output$summary_plot_explore <- renderUI({ 
  
  .rdata[["summary_plot_explore"]] <<- .rdata[["blank_plot"]]
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  # dataMSG <- "If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  dataMSG <- translate("noestimates", input$lang)
  
  debounce_summary_explore()
  
  
  if(is.null(input$summary_plot_type_explore )) return()
  #if(is.null(input$summary_plot_mode_explore)) return()
  # if(is.null(input$plot_height2)) return()
  # if(is.null(input$plot_width2)) return()
  # h <- input$plot_height2
  # w <- input$plot_width2
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin2))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax2))
  isValid1 <- input$axis_limitsmin2 == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax2 == "" | !is.na(axismax)
  longnames <- input$long_names2
  summary_error_bars <- input$summary_error_bars
  
  isolate({
    
    if(is.null(input$axis_limitsmin2)) return()
    if(is.null(input$axis_limitsmax2)) return()
    if(is.null(input$focus_inequal_type_explore_plot )) return()
    
    summary_plot_type_explore <- input$summary_plot_type_explore
    summary_plot_mode_explore <- if (toggleHC) "hc" else "ggplot" #input$summary_plot_mode_explore
    focus_indicator_explore          <- input$focus_indicator_explore
    focus_country_explore            <- input$focus_country_explore
    focus_year_explore               <- input$focus_year_explore
    mostrecent_explore               <- input$mostrecent_explore
    focus_data_source_explore        <- input$focus_data_source_explore
    focus_dimension_explore          <- input$focus_dimension_explore
    focus_inequal_type_explore_plot  <- input$focus_inequal_type_explore_plot 
    
    if(trimws(focus_country_explore) == "") return()
  }) 
  
  
  
  .rdata[["plotSummary_explore_title"]] <<- ""
  .rdata[["plotSummary_explore_title_static"]] <<- ""
  
  
  # *********************************************************************
  # A block of code testing for whether the input values are equal to their 
  # equivalent global variables 
  # *********************************************************************
  
  dropplot <- FALSE
  emptyData <- FALSE
  # nodataMSG <-"If summary measures are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  nodataMSG <- translate("noestimates", input$lang)  
  
  
  if(is.null(focus_year_explore)){
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  
  #This was added so we could use debounce
  # if(.rdata[['first_time_summary_plot']]){
  #   .rdata[['first_time_summary_plot']] <<- FALSE
  #   dropplot <- TRUE
  #   emptyData <- TRUE
  # }
  
  validAxis <- TRUE
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) {
    validAxis <- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){ 
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    plotData <- getInequalData(indicator     = focus_indicator_explore,
                               stratifier    = focus_dimension_explore,
                               countries     = focus_country_explore,
                               years         = focus_year_explore,
                               mostrecent    = mostrecent_explore,
                               datasource    = focus_data_source_explore,
                               inequal_types = focus_inequal_type_explore_plot)
    
    
    
    plotData <- filter(plotData, 
                       measure == focus_inequal_type_explore_plot, !is.na(inequal))
    plotData <- plot_decimals_setup(plotData, type = "sum")
    
    if(.rdata[['HEATversion']] == "whodata" && !is.null(longnames) && longnames == FALSE){
      plotData$indic_title <- plotData$indic
    }else{
      plotData$indic_title <- plotData$indic_name
    }
    
    # *********************************************************************
    # Tests to see if data is (A) Not NULL; (B) there is some non-NA data
    # with the anchor field == 1
    # *********************************************************************
    
    thetest <- !is.null(plotData) && nrow(plotData)>0
    
    
    
    if(!thetest) {
      dropplot <- TRUE
      emptyData <- TRUE
    }
    
    # *********************************************************************
    # We can't have negative or 0 values as the y-axis if the variable
    # is log scale
    # *********************************************************************
    
    isLogScale <- plotData$logscale[1] == 1
    logscaleMSG <- "This summary measure is shown on a logarithmic scale, so axis limits must take values greater than zero."
    
    
    if(!is.na(isLogScale) && isLogScale){
      
      # test for log scale and incorrect y params
      okLogMin <- input$axis_limitsmin2 == "" | axismin > 0
      okLogMax <- input$axis_limitsmax2 == "" | axismax > 0
      
      logtest <- okLogMin & okLogMax
      
      
      if(!logtest){
        dropplot <- TRUE
        emptyData <- TRUE
      }
      
    }
    
    
    
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************
    sumMeasure <- plotData$measure[1]
    measureName <- gsub(" \\(.*\\)", "", plotData$measure_name[1])
    yrs <- paste(sort(unique(plotData$year)), collapse=", ")
    sources <- paste(sort(sort(unique(plotData$source))), collapse = " & ")
    

    if (thetest) {
      if (HEATversion == "whodata") {
        focus_country <- translate(.rdata[["focus_country"]], input$lang)
      } else {
        focus_country <- .rdata[["focus_country"]]
      }
      
      .rdata[["plotSummary_explore_title"]] <<- paste0(focus_country, " (", sources, ", ", yrs, ")")
      .rdata[["plotSummary_explore_title_static"]] <<- .rdata[["plotSummary_explore_title"]]

    }
    
    updateTextInput(session, "main_title2", value = .rdata[["plotSummary_explore_title"]])

    .rdata[['focus_plot_data_summary_explore']] <<- plotData
    
  }
  
  # *********************************************************************
  # Output the results
  # *********************************************************************
  
  
  output$summary_plot_explore_insideGG <- renderPlot({
    if(dropplot) return()
    .trigger$summary_plot_explore_title
    input$xaxis_title2
    input$yaxis_title2
    
    #input$long_names2
    
    
    doPlotType <- get(paste0("plotSummary", isolate(summary_plot_type_explore), "_explore_ggplot"))
    p <- doPlotType(plotData)
    .rdata[["summary_plot_explore"]] <<- p
    
    p  
  }, res=90, height = 650, width = 650) 
  
  
  output$summary_plot_explore_insideHC <- renderUI({
    
    if(dropplot) return()
    
    # input$plot_height2
    # input$plot_width2
    #input$long_names2
    .trigger$summary_plot_explore_title
    
    
    doPlotType <- get(paste0("plotSummary", isolate(summary_plot_type_explore), "_explore_hc"))
    p <- doPlotType(plotData, confInt = summary_error_bars )
    .rdata[["summary_plot_explore"]] <<- p
    
    p
  })
  
  
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  if(!validAxis) msg <- "Please supply a valid number"
  if(exists("logtest") && !logtest) msg <- logscaleMSG
  
  returnList <- list(tags$div(class="datawarning", helpText(msg)))
  
  if(summary_plot_mode_explore == "ggplot"){
    returnList[[2]] <- plotOutput("summary_plot_explore_insideGG")
  } else {
    returnList[[2]] <- uiOutput("summary_plot_explore_insideHC")
  }
  
  
  
  return(returnList)
  
  
  
})  

# ----- COMPARE DISAGGREGATED PLOT -----------------------------

output$disag_plot_compare <- renderUI({
  
  
  debounce_disag_compare_plot()
  
  if(.rdata[['first_time_disag_compare_plot']]){
    .rdata[['first_time_disag_compare_plot']] <<- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  .rdata[["disag_plot_compare"]] <<- .rdata[["blank_plot"]]
  # This is the general message that is usually shown even if data is avail
  # to let users know that there are situations where no data is available
  # dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available."
  dataMSG <- translate("noestimates", input$lang)
  disag_plot_mode_compare <- if (toggleHC) "hc" else "ggplot" #input$disag_plot_mode_compare
  
  
  
  # h <- input$plot_height3
  # w <- input$plot_width3
  if(is.null(input$axis_limitsmin3 )) return()
  if(is.null(input$axis_limitsmax3 )) return()
  axismin <- suppressWarnings(as.numeric(input$axis_limitsmin3))
  axismax <- suppressWarnings(as.numeric(input$axis_limitsmax3))
  isValid1 <- input$axis_limitsmin3 == "" | !is.na(axismin)
  isValid2 <- input$axis_limitsmax3 == "" | !is.na(axismax)
  
  isolate({
    if(is.null(input$focus_country_compare) || trimws(input$focus_country_compare) == "") return()
    
    focus_indicator_compare <- input$focus_indicator_compare
    focus_year_compare <-  input$focus_year_compare
    mostrecent_compare <-  input$mostrecent_compare
    focus_data_source_compare <- input$focus_data_source_compare
    focus_country_compare <- input$focus_country_compare
    focus_dimension_compare <- input$focus_dimension_compare
    benchmarkYears <- input$benchmarkYears
    benchmark_countries <- input$benchmark_countries
    #.trigger$focus_benchmark_compare
    
  })
  
  
  .rdata[["plotDisag_compare_title"]] <<-  ""
  
  dropplot <- FALSE
  emptyData <- FALSE
  # nodataMSG <-"There is no data for this combination of variables."
  nodataMSG <- translate("nodata", input$lang)
  
  
  
  validAxis <- TRUE
  # If the min or the max axis is not a "" or valid number
  if(!isValid1 | !isValid2) {
    validAxis <- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  
  if(!dropplot){
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    #if(is.null(input$benchmarkYears)) return()
    
    anchordata<-getDisagData(indicator=focus_indicator_compare, 
                             stratifier=focus_dimension_compare,  # in hetkdb.R
                             countries=focus_country_compare, 
                             years=focus_year_compare, 
                             mostrecent=mostrecent_compare,
                             datasource=focus_data_source_compare)
    
    
    #if(is.null(anchordata) || nrow(anchordata)==0) return()
    
    if(nrow(anchordata)>0) anchordata$anchor <- 1
    
    
    benchmarkdata <- NULL
    
    if(!is.null(benchmark_countries)){
      benchmarkdata <- getDisagData(indicator = focus_indicator_compare, 
                                    stratifier = focus_dimension_compare, 
                                    countries = benchmark_countries, 
                                    years =  focus_year_compare, 
                                    mostrecent = mostrecent_compare,
                                    datasource = focus_data_source_compare,
                                    elasticity = benchmarkYears,
                                    anchor_country = focus_country_compare)
    }
    
    
    if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
      benchmarkdata$anchor <- 0
      plotData <- rbind(anchordata, benchmarkdata) 
    }else{
      
      plotData <- anchordata
    }
    
    plotData <- plot_decimals_setup(plotData, type = "disag")
    plotData$indic_title <- plotData$indic_name
    
    # git 697
    plotData <- filter(plotData, !is.na(estimate))
    
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************
    
    nonNull_plotData <- !is.null(plotData)
    someAnchorData <- nrow(plotData[plotData$anchor==1,])>0
    # I suppress a warning here because if there is no data it will still
    # run and give a warning but this is fine because the next piece of
    # code is &&
    notAllNA_Anchor <- suppressWarnings(sum(is.na(plotData$estimate))!=nrow(plotData))
    
    thetest <- nonNull_plotData  && someAnchorData && notAllNA_Anchor
    
    
    if(!thetest) {
      dropplot <- TRUE
      emptyData <- TRUE
    }
    
    # *********************************************************************
    # I'm setting and updating the title here, perhaps it should be in
    # an observer but it's working
    # *********************************************************************

    if (thetest) {
      ncountry <- length(unique(plotData$country))
      modifier <- tolower(translate(if (ncountry == 1) "Setting" else "Settings", input$lang))      
      
      if (HEATversion == "whodata") {
        indicator_name <- translate(plotData$indic[1], input$lang)
        title_dimension <- tolower(translate(plotData$dimension[1], input$lang))
      } else {
        indicator_name <- plotData$indic_name[1]
        title_dimension <- tolower(plotData$dimension[1])
      }

                  
      .rdata[["plotDisag_compare_title"]] <<- paste(
        indicator_name, 
        translate("graphby", input$lang),
        title_dimension,
        translate("graphin", input$lang),
        ncountry, modifier
      )
    }
    
    updateTextInput(session, "main_title3", value = .rdata[["plotDisag_compare_title"]])
    
    plotData <- left_join(plotData, .rdata[['dimension_details']], by = c("dimension", "subgroup", "order"))
    
    plotData <- order_dimensions(plotData)
    
    .rdata[['focus_plot_data_disag_compare']] <<- plotData
  }
  
  # *********************************************************************
  # Output the results
  # *********************************************************************
  
  
  
  
  output$disag_plot_compare_insideGG <- renderPlot({
    
    input$xaxis_title3
    input$yaxis_title3
    .trigger$disag_plot_compare_title
    
    
    if(dropplot) return()
    p <- plotDisagLine_compare_ggplot(plotData)
    .rdata[["disag_plot_compare"]] <<- p
    
    p  
  }, res=90, height = 650, width = 650)
  
  
  output$disag_plot_compare_insideHC <- renderUI({
    
    input$xaxis_title3
    input$yaxis_title3
    .trigger$disag_plot_compare_title
    
    
    if(dropplot) return()
    p <- plotDisagLine_compare_hc(plotData)
    .rdata[["disag_plot_compare"]] <<- p
    
    p
  })
  
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  if(!validAxis) msg <- "Please supply a valid number"
  returnList <- list(tags$div(class="datawarning", helpText(msg)))
  
  if(disag_plot_mode_compare == "ggplot"){
    returnList[[2]] <- plotOutput("disag_plot_compare_insideGG")
  } else {
    returnList[[2]] <- uiOutput("disag_plot_compare_insideHC")
  }
  
  
  
  return(returnList)
  
})  



# ----- COMPARE SUMMARY PLOT -----------------------------
output$summary_plot_compare <- renderUI({
  
  
  
  debounce_summary_compare()
  #if(is.null(input$summary_plot_mode_compare)) return()
  # h <- input$plot_height4
  # w <- input$plot_width4
  if(is.null(input$yaxis_limitsmin4 )) return()
  if(is.null(input$yaxis_limitsmax4 )) return()
  if(is.null(input$xaxis_limitsmin4 )) return()
  if(is.null(input$xaxis_limitsmax4 )) return()
  
  
  axismin <- suppressWarnings(as.numeric(input$yaxis_limitsmin4))
  axismax <- suppressWarnings(as.numeric(input$yaxis_limitsmax4))
  Xaxismin <- suppressWarnings(as.numeric(input$xaxis_limitsmin4))
  Xaxismax <- suppressWarnings(as.numeric(input$xaxis_limitsmax4))
  
  isValid1 <- input$yaxis_limitsmin4 == "" | !is.na(axismin)
  isValid2 <- input$yaxis_limitsmax4 == "" | !is.na(axismax)
  isValid3 <- input$xaxis_limitsmin4 == "" | !is.na(Xaxismin)
  isValid4 <- input$xaxis_limitsmax4 == "" | !is.na(Xaxismax)
  
  isolate({
    
    
    
    focus_indicator_compare <- input$focus_indicator_compare  
    focus_year_compare <- input$focus_year_compare 
    mostrecent_compare <- input$mostrecent_compare
    focus_data_source_compare <- input$focus_data_source_compare  
    focus_country_compare <- input$focus_country_compare
    summultiplier1 <-  input$summultiplier1
    summultiplier2  <-  input$summultiplier2
    focus_dimension_compare <- input$focus_dimension_compare 
    benchmarkYears  <-  input$benchmarkYears
    benchmark_countries <- input$benchmark_countries
    summary_plot_mode_compare <- if (toggleHC) "hc" else "ggplot" #input$summary_plot_mode_compare
    focus_inequal_type_compare <- input$focus_inequal_type_compare
    
    
  })
  
  .rdata[["plotSummary_compare_title"]] <<- ""
  
  if(is.null(focus_country_compare) || trimws(focus_country_compare) == "") return()
  
  
  
  .rdata[["summary_plot_compare"]] <<- .rdata[["blank_plot"]]
  # dataMSG <- "If estimates are not shown for a selected combination of variables, then data are not available or summary measures cannot be calculated. Note that only relevant summary measures are calculated for each dimension."
  dataMSG <- translate("noestimates", input$lang)
  
  # *********************************************************************
  # If all inputs are equal to global variables then we don't have
  # to drop the plot and we can go ahead and get data
  # *********************************************************************
  dropplot <- FALSE
  emptyData <- FALSE
  # nodataMSG <-"There is no data for this combination of variables."
  nodataMSG <- translate("nodata", input$lang)
  
  if(is.null(focus_inequal_type_compare)){
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  
  # if(.rdata[['first_time_summary_compare_plot']]){
  #   .rdata[['first_time_summary_compare_plot']] <<- FALSE
  #   dropplot <- TRUE
  #   emptyData <- TRUE
  # }
  
  validAxis <- TRUE
  if(!isValid1 | !isValid2 | !isValid3 | !isValid4) {
    validAxis <- FALSE
    dropplot <- TRUE
    emptyData <- TRUE
  }
  
  
  
  if(!dropplot){
    
    # *********************************************************************
    # Get data
    # *********************************************************************
    
    #if(is.null(input$focus_inequal_type_compare)) return()
    
    anchordata <- getInequalData(indicator=focus_indicator_compare,  
                                 stratifier=focus_dimension_compare, 
                                 countries=focus_country_compare, 
                                 years=focus_year_compare, 
                                 mostrecent=mostrecent_compare,
                                 datasource=focus_data_source_compare,  
                                 inequal_types=focus_inequal_type_compare,
                                 multiplier1 = summultiplier1,
                                 multiplier2 = summultiplier2)
    
    #if(is.null(anchordata) || nrow(anchordata)==0) return()
    if(nrow(anchordata)>0) anchordata$anchor <- 1
    
    
    benchmarkdata <- NULL
    
    if(!is.null(benchmark_countries)){
      benchmarkdata <- getInequalData(indicator=focus_indicator_compare,  
                                      stratifier=focus_dimension_compare, 
                                      countries=benchmark_countries, 
                                      years=focus_year_compare, 
                                      mostrecent=mostrecent_compare,
                                      datasource=focus_data_source_compare,  
                                      inequal_types=focus_inequal_type_compare,
                                      elasticity = benchmarkYears,
                                      multiplier1 = summultiplier1,
                                      multiplier2 = summultiplier2,
                                      anchorCountry = focus_country_compare)
    }
    
    if(!is.null(benchmarkdata) && nrow(benchmarkdata)!=0){
      benchmarkdata$anchor <- 0
      plotData <- rbind(anchordata, benchmarkdata) 
    }else{
      
      
      
      plotData <- anchordata
    }
    plotData$indic_title <- plotData$indic_name
    
    
    # *********************************************************************
    # Tests to make sure we have data and it's what we need
    # *********************************************************************
    
    nonNull_plotData <- !is.null(plotData)
    someAnchorData <- nrow(plotData[plotData$anchor==1,])>0
    # I suppress a warning here because if there is no data it will still
    # run and give a warning but this is fine because the next piece of
    # code is &&
    notAllNA_Anchor <- suppressWarnings(sum(is.na(plotData$estimate))!=nrow(plotData))
    
    thetest <- nonNull_plotData  && someAnchorData && notAllNA_Anchor
    
    if(!thetest) {
      dropplot <- TRUE
      emptyData <- TRUE
    }else{
      
      
      
      
      # *********************************************************************
      # We can't have negative or 0 values as the y-axis if the variable
      # is log scale
      # *********************************************************************
      
      isLogScale <- plotData$logscale[1] == 1
      logscaleMSG <- "This summary measure is shown on a logarithmic scale, so axis limits must take values greater than zero."
      
      
      if(length(isLogScale)>0 && isLogScale){
        
        # test for log scale and incorrect y params
        okLogMin <- input$yaxis_limitsmin4 == "" | axismin > 0
        okLogMax <- input$yaxis_limitsmax4 == "" | axismax > 0
        
        logtest <- okLogMin & okLogMax
        
        if(!logtest) {
          dropplot <- TRUE
          emptyData <- TRUE
        }
        
      }
      
      
      # *********************************************************************
      # I'm setting and updating the title here, perhaps it should be in
      # an observer but it's working
      # *********************************************************************
      ncountry <- length(unique(plotData$country))
      
      .rdata[["plotSummary_compare_title"]] <<- paste0(
        if (HEATversion == "whodata")
          translate(plotData$indic[1], input$lang)
        else 
          plotData$indic_name[1],
        ": ",
        str_glue_data(
          list(
            setting = tolower(translate(if (HEATversion == "whodata") "Country" else "Setting", input$lang)),
            dimension = tolower(
              if (HEATversion == "whodata") 
                translate(plotData$dimension[1], input$lang)
              else 
                plotData$dimension[1]
            )
          ),
          translate("setting_extended", input$lang)
        ),
        " ",
        ncountry, 
        " ",
        tolower(
          translate(
            if (ncountry == 1) "Setting" else "Settings",
            input$lang
          )
        )
      )
      
      updateTextInput(session, "main_title4", value = .rdata[["plotSummary_compare_title"]])
      
      # *********************************************************************
      # I'm setting and updating the title here, perhaps it should be in
      # an observer but it's working
      # *********************************************************************
      .rdata[['focus_plot_data_summary_compare']] <<- plotData
      
    }
    
  }
  # *********************************************************************
  # Output the results
  # *********************************************************************
  
  
  
  
  output$summary_plot_compare_insideGG <- renderPlot({
    
    .trigger$summary_plot_compare_title
    input$disag_plot_summary_pts
    input$xaxis_title4
    input$yaxis_title4
    
    if(dropplot) return()
    
    p <- plotSummaryScatter_compare_ggplot(plotData)
    .rdata[["summary_plot_compare"]] <<- p
    
    p  
  }, res=90, height = 650, width = 650)
  
  
  output$summary_plot_compare_insideHC <- renderUI({
    
    .trigger$summary_plot_compare_title
    input$xaxis_title4
    input$yaxis_title4
    
    if(dropplot) return()
    
    p <- plotSummaryScatter_compare_hc(plotData)
    .rdata[["summary_plot_compare"]] <<- p
    
    p
  })
  
  
  
  msg <- ifelse(emptyData, nodataMSG, dataMSG)
  if(!validAxis) msg <- "Please supply a valid number"
  if(exists("logtest") && !logtest) msg <- logscaleMSG
  
  
  returnList <- list(tags$div(class="datawarning", helpText(msg)))
  
  if(summary_plot_mode_compare == "ggplot"){
    returnList[[2]] <- plotOutput("summary_plot_compare_insideGG")
  } else {
    returnList[[2]] <- uiOutput("summary_plot_compare_insideHC")
  }
  
  
  
  return(returnList)
})  





