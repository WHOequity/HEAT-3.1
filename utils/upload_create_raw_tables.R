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



outpath <- "./HEAT/data/upload/"


# inputdat <- read.csv("X:/projects/who_heat/web/who-heat/create_database/debugging_files/test_upload/db_w_scale_pop.csv", check.names = FALSE, stringsAsFactors = FALSE,fileEncoding = "cp1252")
#.rdata <- list()
# path <- "./HEAT/data/upload/"
# outpath <- "./HEAT/data/upload/"
upload_existing_tables <- function(path = "./data/upload/"){
  
  .rdata[['maindata']] <- readRDS(paste0(path, "maindata.RDS"))
  .rdata[['strata']] <- readRDS(paste0(path, "maindata.RDS"))
  
  
}



create_raw_tables <- function(inputdat) {
  inputdat$country <- make_unknown_encoding(inputdat$country)
  
  inputdat <- inputdat %>% 
    group_by(country, year, source, indicator_abbr, dimension) %>%
    mutate(popshare = population / sum(population)) %>% 
    ungroup()
  
  strata <- inputdat %>% 
    filter(!is.na(estimate)) %>% 
    select(country, iso3, year, source, indicator_abbr, indicator_name, dimension) %>% 
    distinct()
  
  strata$rec <- 1:nrow(strata)
  
  # git897 matching the whodata calculate_raw_inequal_updated.R
  inputdat <- semi_join(
    inputdat, strata, 
    by = c(
      "country", "year", "source", "indicator_abbr", "indicator_name",
      "dimension"
    )
  )
  
  list(strata = strata, maindata = inputdat)
}


change_names_preserve_data <- function(inequals = NULL, maindata = NULL, strata = NULL,
                                       savedata = TRUE, outpath = "./data/user-data/",
                                       prefix = ""){
  
  
  
  if (!dir.exists(outpath)) dir.create(outpath)
  
  outpath <- paste0(outpath, prefix, sep="____")
  
  if(!is.null(inequals)){
    
    names(inequals) <- convert_names_to_old(names(inequals), new_to_old_names)
    inequals <- data.table(inequals)
    setkey(inequals, country, year, indic, dimension, measure, source)
    .rdata[['inequals']] <<- inequals
    if(savedata) saveRDS(inequals, paste0(outpath, "inequals.RDS"))
  }
  
  
  if(!is.null(maindata)){
    
    names(maindata) <- convert_names_to_old(names(maindata), new_to_old_names)
    maindata <- data.table(maindata)
    setkey(maindata, country, year, indic, dimension, source)
    .rdata[['maindata']] <<- maindata
    
    
    
    dimensiondtl <- select(maindata, c(dimension, subgroup, order))%>% 
      distinct %>% group_by(dimension) %>% 
      mutate(maxn = n())
    dimensiondtl$dimension_type <- "other"
    dimensiondtl$dimension_type[dimensiondtl$maxn>7] <- "region"
    .rdata[['dimension_details']] <<- dimensiondtl
    
    
    .rdata[['dimension_summary']] <<- select(.rdata[['dimension_details']], dimension, order, maxn) %>% 
      mutate(ordered = order != 0, just2_subgroups = maxn <= 2) %>% select(-order, -maxn) %>% ungroup %>%  distinct
    
    .rdata[['inequal_rules']] <<- full_join(.rdata[['dimension_summary']], .rdata[['inequal_rules_tmp']], 
                                            by = c("ordered", "just2_subgroups"))
    
    
    if(savedata){
      saveRDS(dimensiondtl, paste0(outpath, "dimensions.RDS"))
      saveRDS(maindata, paste0(outpath, "maindata.RDS"))
      
    }
  }
  
  if(!is.null(strata)){
    
    names(strata) <- convert_names_to_old(names(strata), new_to_old_names)
    strata <- data.table(strata)
    setkey(strata, country, year, indic, dimension, source)
    .rdata[['strata']] <<- strata
    years <- unique(strata[,.(country, year, source)], by=c("country", "year", "source"))
    .rdata[['years']] <<- data.frame(years)
    if(savedata) saveRDS(years, paste0(outpath, "years.RDS"))
    if(savedata) saveRDS(strata, paste0(outpath, "strata.RDS"))
  }
  
  
  
  
  
  
}



use_existing_data <- function(datapath){
  
  .rdata[['strata']] <<- readRDS(paste0(datapath, "strata.RDS"))
  .rdata[['maindata']] <<- readRDS(paste0(datapath, "maindata.RDS"))
  .rdata[['inequals']] <<- readRDS(paste0(datapath, "inequals.RDS"))
  .rdata[['years']] <<- readRDS(paste0(datapath, "years.RDS"))
  .rdata[['dimension_details']] <<- readRDS(paste0(datapath, "dimensions.RDS"))
  
  
  
  .rdata[['dimension_summary']] <<- select(.rdata[['dimension_details']], dimension, order, maxn) %>% 
    mutate(ordered = order != 0, just2_subgroups = maxn <= 2) %>% select(-order, -maxn) %>% ungroup %>%  distinct
  
  .rdata[['inequal_rules']] <<- full_join(.rdata[['dimension_summary']], .rdata[['inequal_rules_tmp']], 
                                          by = c("ordered", "just2_subgroups"))
  
  
}



fill_in_global_list <- function(){
  countries <- sort(unique(.rdata[['strata']]$country))
  focus_country <- countries[1]
  
  focus <- filter(.rdata[['strata']], country == focus_country) %>% 
    arrange(desc(year), indic_name, dimension)
  
  #focus <- arrange(.rdata[['strata']], country) %>% slice(1)
  
  indicators <- unique(.rdata[['strata']][,.(indic, indic_name)])
  indicators <- arrange(indicators, indic_name)
  indicators_abbr <-  indicators$indic
  names(indicators_abbr) <- indicators$indic_name
  dimensions  <- sort(unique(.rdata[['strata']]$dimension))
  datasources <- sort(unique(.rdata[["strata"]]$source))
  years <- sort(.rdata[['years']]$year)
  
  
  .rdata[['all_countries']] <<-countries
  .rdata[['focus_indicator']] <<- focus$indic[1]
  .rdata[['focus_country']] <<- focus_country
  .rdata[['focus_dimension']]<<-focus$dimension[1]
  .rdata[['focus_year']]<<-focus$year[1]
  .rdata[['countryyrsource']] <<- select(.rdata[['strata']], country,year, source) %>% distinct
  .rdata[['focus_data_source']]<<- datasources#getFilteredSource(focus_country)
  
  
  indic_dim <- getFilteredIndDim(.rdata[['focus_country']], focus$year,
                                 .rdata[['focus_data_source']]) 
  
  
  
  .rdata[['equity_dimensions']] <<- sort(indic_dim$dimension)
  .rdata[['full_indicators']] <<- get_formatted_indicators(indic_dim)
  
  
  .rdata[['data_sources']] <<- datasources
  
  .rdata[['mostrecent']] <<- FALSE
  
  .rdata[['all_years']] <<- getFilteredYear(.rdata[['focus_country']])
  
  regionname <- unique(.rdata[['dimension_details']]$dimension[.rdata[['dimension_details']]$dimension_type=="region"])
  .rdata[['geo_dimension']] <<- ifelse(is.na(regionname), "None exist", regionname)
  #if(is.na(.rdata[['geo_dimension']])) .rdata[['geo_dimension']] <<- "None exist"
  
  
  .rdata[['dimension_details']] <<- assignColorsShapes(.rdata[['dimension_details']])
  
  
  newcountryinfo <- left_join(data.frame(country = .rdata[['all_countries']]), 
                              .rdata[['countryinfo']], by = "country")
  newcountryinfo$whoreg6_name[is.na(newcountryinfo$whoreg6_name)] <- "No WHO region defined"
  newcountryinfo$wbincome[is.na(newcountryinfo$wbincome)] <- "No income group defined"
  
  .rdata[['countryinfo']] <<- newcountryinfo
  .rdata[['who_regions']] <<- sort(unique(.rdata[['countryinfo']]$whoreg6_name))
  .rdata[['income_groups']] <<- sort(unique(.rdata[['countryinfo']]$wbincome))
  
  
  
  if(nrow(.rdata[['countryinfo']])<20){
    
    .rdata[['benchmark_countries']] <<- .rdata[['countryinfo']]$country
    .rdata[['focus_income_group']] <<-.rdata[['income_groups']]
    .rdata[['focus_who_regions']] <<- .rdata[['who_regions']]
    
    
    
  }else{
    
    
    focus <-.rdata[['focus_country']]
    focusinfo <- getCountryWHOregionIncome(focus)
    benchinfo <- filter(.rdata[['countryinfo']], 
                        whoreg6_name == focusinfo$region, wbincome == focusinfo$income,
                        !country%in%focus)
    
    .rdata[['benchmark_countries']] <<- benchinfo$country
    .rdata[['focus_income_group']] <<- focusinfo$income
    .rdata[['focus_who_regions']] <<- focusinfo$region
    
  }
  
  
  .rdata[['summary_measures_all']] <<- getFilteredInequal(mydimension = .rdata[['focus_dimension']])
  
  # Choose d as default unless it doesn't exist
  isD <- which(.rdata[['summary_measures_all']] == "d")
  isD <- ifelse(length(isD)>0, isD, 1)
  .rdata[['focus_inequal_type']] <<- .rdata[['summary_measures_all']][isD]
  
  
}




#******************************************************************************
# Load inequality functions and list functions
#******************************************************************************  





split_disag_data <- function(strata, disagdata){
  
  # strata <- .rdata[['strata']]
  # disagdata <- .rdata[['maindata']]
  # disagdata <- maindata
  disagdata<-inner_join(disagdata, strata[,c("country", "year", 
                                             "source", "indicator_abbr",  "dimension", "rec")],
                        by=c("country", "year", "source", "indicator_abbr",  "dimension"))
  disagdata.split<-split(disagdata, disagdata$rec)
  
  disagdata.split
  
}



which_inequal <- function(strata, summeasure_types){
  
  two.subgroups <- two_subgroups(strata)
  is.ordered <- is_ordered(strata)
  
  filter(summeasure_types, just2_subgroups == two.subgroups, ordered == is.ordered) 
  
  
}



#strata <- old.disag.split[[1]][[1]]

do_inequals <- function(strata, current, n, summeasure_types, bs = NULL, simulation_iterations = 200){

  inequals <- which_inequal(strata, summeasure_types)
  simulated_results <- NULL
  
  if(any(inequals$simulation_se == 1) && usefulSE(strata)){
    
    scaleval <- unique(strata$indicator_scale)
    estrand <- purrr::map(1:simulation_iterations, 
                          ~rnorm(nrow(strata), 
                                 strata$estimate/scaleval, 
                                 strata$se/scaleval))
    
    
    simulated_results <- list(estrand = estrand)
  }
  
  vals <- lapply(inequals$measure, function(x){
    #print(paste0(x, ":", current, " of ", n, "strata"))
    
    if(x == "par") x <- "PAR"
    thelist <- list(dat=strata)
    if(!is.null(bs)) thelist['bs'] <- bs
    if(!is.null(simulated_results)) thelist[['simulated_results']] <- simulated_results
    res<-do.call(x, thelist)
    whereBS <- grep("boot", names(res))
    whereAnalytic <- grep("formula", names(res))
    whereInequal <- grep("inequal", names(res))
    whereLowerCI <- grep("se.lower", names(res))
    whereUpperCI <- grep("se.upper", names(res))
    return(data.frame(measure = tolower(x),
                      inequal= res[[whereInequal]], 
                      se = res[[whereAnalytic]], 
                      boot.se=res[[whereBS]], 
                      se.lowerci = res[[whereLowerCI]],
                      se.upperci = res[[whereUpperCI]],
                      stringsAsFactors = FALSE))
    
  })
  
  
  
  vals <- do.call("rbind", vals)
  vals$country<-strata$country[1]
  vals$ccode <- strata$iso3[1]
  vals$year <- strata$year[1]
  vals$indicator_abbr <- strata$indicator_abbr[1]
  vals$indicator_name <- strata$indicator_name[1]
  vals$dimension <- strata$dimension[1]
  vals$source <- strata$source[1]
  vals$r_national <- strata$national[1]
  vals$rec <- strata$rec[1]
  
  vals
  
}


# The inShiny argument tells the function if it's being
# called in a Shiny app in which case it uses the progress
# indicator. Otherwise skip progress. Originally doBS was
# used to tell this if bootstraps should be performed

lapply_inequals <- function(strata, maindata, summeasure_types, inShiny = TRUE, simulation_iterations = 200) {
  
 
  #strata = res$strata; maindata = res$maindata
  disagdata.split <- split_disag_data(strata, maindata)
  n <- length(disagdata.split)
  
  if (inShiny) {
    withProgress(
      message = translate("heat_plus_landing_db_compute", isolate(input$lang)), 
      value = 0,
      expr = {
        #Sys.sleep(1000)
        res <- lapply(1:n, function(i) {
          incProgress(1/n, detail = paste("on", i, "of", n))
          #Sys.sleep(1000)
          do_inequals(disagdata.split[[i]], current = i, n = n, summeasure_types)
        }) # FALSE is don't do it, NULL is do it if appropriate
      }
    )
  } else {
    res <- lapply(1:n, function(i) {
      if(i%%1000 == 0) print(sprintf("On %s of %s", i, n))
      do_inequals(disagdata.split[[i]], current = i, n = n, summeasure_types, simulation_iterations = 200)
    })
  }
  
  
  res <- do.call('rbind', res)
  
  res <- set_outliers_to_missing(res)
  res <- filter(res, !is.na(res$inequal))
}



