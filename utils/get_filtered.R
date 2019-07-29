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

getCountryWHOregionIncome<-function(countryname){


  if(length(.rdata[['all_countries']])<20){
    res <- list(whoreg6_name = sort(unique(.rdata[['countryinfo']]$whoreg6_name)),
                wbincome = sort(unique(.rdata[['countryinfo']]$wbincome)))
  } else {
    res <- filter(.rdata[['countryinfo']], country==countryname)
  }

  return(list(region = res$whoreg6_name, income = res$wbincome))
}



getFilteredCountries <- function(WBgroup=NULL, WHOregion=NULL)
{

  filt_WBgroup   <- TRUE
  filt_WHOregion <- TRUE

  countries <- .rdata[['countryinfo']]
  
  if(!is.null(WBgroup) && all(WBgroup != ""))
    countries <- filter(countries, wbincome %in% WBgroup)
    #filt_WBgroup <- quote(wbincome %in% WBgroup)

  if(!is.null(WHOregion) && all(WHOregion != ""))
    countries <- filter(countries, whoreg6_name %in% WHOregion)
    #filt_WHOregion <- quote(whoreg6_name %in% WHOregion)

  countries <- countries %>%
    select(country) %>% .$country



  return(countries)


}



getFilteredSource <- function(countryname){
  # filt_country   <- quote(country %in% countryname)

  sources <- .rdata[['countryyrsource']] %>%
    filter(country %in% countryname) %>%
    arrange(source)


  unique(sources$source)

}


getFilteredYear <-  function(countryname, datasource = .rdata[["focus_data_source"]]){

  # filt_country   <- quote(country %in% countryname)

  years <- .rdata[['years']] %>%
    filter(country %in% countryname) %>%
    arrange(desc(year))

  years[years$source%in%datasource,] %>% .$year

}

# From the strata dataset it limits based on the
# country/year/source and returns the indicators/dimensions
getFilteredIndDim <- function(countryname, years, datasource = .rdata[["focus_data_source"]]){


  filt_datasource <- datasource

  countryname <- make_unknown_encoding(countryname)

  years <- as.integer(years)
  suppressWarnings(unique(.rdata[['strata']][CJ(countryname, years, unique(indic),
                                                unique(dimension), eval(filt_datasource)),
                                             .(country, year, indic, indic_name, dimension),
                                             nomatch = 0L])[rev(order(year))])


}


# Based on the current focus year, indicator etc find
# the inequality measures that will be available

getFilteredInequal <- function(mydimension = .rdata[['focus_dimension']]){
  #setkey(inequals, country, year, indic, dimension, measure, source)

  # added per git 773

  measure <- unique(filter(.rdata[['inequal_rules']], dimension%in%mydimension) %>% .$measure)

  summeasure <- .rdata[['summary_measures_table']]
  summeasure <- summeasure[summeasure$measure_abbr%in%measure,]
  summeasurevect <- summeasure$measure_abbr
  names(summeasurevect) <- paste0(summeasure$measure_name, " (", toupper(summeasure$measure_abbr), ")")

  return(summeasurevect)

}





get_formatted_indicators <- function(dat, focus_indicator = NULL) {
  
  #browser()
  # dat <- unique(indic.dimen[,.(indic, indic_name)])
  dat <- distinct(dat, indic, indic_name) %>% 
    select(indic, indic_name)
  
  if(!is.null(focus_indicator)){
    #if(arrange_by_name)   dat <- arrange(dat, indic_name)
    dat$indic <- factor(dat$indic, levels = focus_indicator,
                        labels = focus_indicator)
    dat <- dat[order(dat$indic),]
    dat$indic <- as.character(dat$indic)
  } else {
    dat <- arrange(dat, indic_name)
  }

  
  
  indic <- dat$indic
  if (HEATversion == "whodata") {
    names(indic) <- vapply(indic, translate, character(1), input$lang)
  } else {
    names(indic) <- dat$indic_name
  }
  
  #indic[match(indic, .rdata[['focus_indicator']])]
  
  return(indic)
}


choose_indicator_dimension <- function(focus_indicator, focus_dimension, newdat){


  # focus_indicator <- .rdata[['focus_indicator']]
  # focus_dimension <- .rdata[['focus_dimension']]
  # newdat <- indic_dim_focus

  # start by filtering by indicator

  just.indic <- newdat[newdat$indic%in%focus_indicator,]

  # if at least one of the indicators exist
  if(nrow(just.indic)>0){

    # filter on dimension
    indic.dimen <- newdat #just.indic[just.indic$dimension%in%focus_dimension,]

    # if both an indicator and a dimension exist
    if(nrow(indic.dimen)>0){
      # return those indicators/dimensions
      indics <- get_formatted_indicators(unique(indic.dimen[,.(indic, indic_name)]), 
                                         focus_indicator = focus_indicator)
      
      return(list(indic = indics, dimension = unique(indic.dimen$dimension)))
      # if an indicator exists but no dimension then return the indicator
      # but a dimension from the previous list
    } else {

      indics <- get_formatted_indicators(just.indic, 
                                         focus_indicator = focus_indicator)
      
      return(list(indic = indics, dimension = unique(just.indic$dimension)))
    }

    # if no indicators exist then choose the first indicator
    # and corresponding dimension
  } else {

    tmpindic <- newdat$indic[1]
    names(tmpindic) <- newdat$indic_name[1]
    return(list(indic = tmpindic, dimension = newdat$dimension))
  }


}



# This resets the global variables for both the full
# list of dimensions/indicators as well as the "focus"
# list of incidcators and dimensions. The first thing
# this does is sends the focus country/year/source
# to getFiteredIndDim to get the subset of strata and
# the list of dimensions and indicators that occur
# in those strata. These are the full sets. Then
# to choose the indicator and dimension to focus on
# it uses the choose_indicator_dimension function




reset_focus_indic_dim <- function(yr = .rdata[['all_years']]){

  
  #print(paste("IN rest_focus", Sys.time()))
  new_indicator <- FALSE
  
  # all possible indicators and dimensions for source, country and year
  indic_dim <- getFilteredIndDim(.rdata[['focus_country']], yr,
                                 .rdata[['focus_data_source']])

  #git709
  if (nrow(indic_dim) == 0) return()

  eq_dimensions <- sort(unique(indic_dim$dimension))
  
  if (HEATversion == "whodata") {
    names(eq_dimensions) <- vapply(eq_dimensions, translate, character(1), isolate(input$lang))
  } else {
    names(eq_dimensions) <- eq_dimensions
  }
  
  .rdata[['equity_dimensions']] <<- eq_dimensions
  
  .rdata[['full_indicators']] <<- get_formatted_indicators(indic_dim)

  indic_dim_focus <- subset(indic_dim, year%in%.rdata[['focus_year']] &
                              indic%in%.rdata[['focus_indicator']] &
                              dimension%in%.rdata[['focus_dimension']])

  indic_dim_focus <- indic_dim_focus[indic_dim_focus$indic%in%.rdata[['focus_indicator']],]
  
  # If there are no year/indicator/dimension options, then try year/indicator
  # and then year/dimension and then just first year
  if(nrow(indic_dim_focus) == 0){

    indic_dim_focus <- subset(indic_dim, year%in%.rdata[['focus_year']] &
                                indic%in%.rdata[['focus_indicator']])

    if(nrow(indic_dim_focus) == 0){

      indic_dim_focus <- subset(indic_dim, year%in%.rdata[['focus_year']] &
                                  indic%in%.rdata[['focus_dimension']])
      new_indicator <- TRUE

      if(nrow(indic_dim_focus) == 0){
        indic_dim_focus <- subset(indic_dim, year%in%.rdata[['focus_year']])[1,]
      }
    }


  }

  which.indic.dim <- choose_indicator_dimension(.rdata[['focus_indicator']],
                                                .rdata[['focus_dimension']],
                                                indic_dim_focus)
  if(new_indicator){
    .rdata[['focus_indicator']] <<- which.indic.dim$indic
  } else {
    #git885 related to existing and sorting indicators
    tmp <- sapply(unname(which.indic.dim$indic),
                  function(x) which(x == .rdata[['focus_indicator']]))
    names(tmp) <- unname(which.indic.dim$indic)
    tmp <- names(sort(tmp))
    .rdata[['focus_indicator']] <<- unname(which.indic.dim$indic[which.indic.dim$indic%in%tmp])
  }


  if(!any(which.indic.dim$dimension%in%.rdata[['focus_dimension']])){
    .rdata[['focus_dimension']] <<- which.indic.dim$dimension[1]
  }else{
    #if(all(which.indic.dim$dimension%in%.rdata[['focus_dimension']])){
    return() # don't change focus_dimension
    #}else{

    #}
  }




}

# reset_full_indic_dim <- function(){
#
#   indic_dim <- getFilteredIndDim(.rdata[['focus_country']], .rdata[['focus_year']],
#                                  .rdata[['focus_data_source']])
#
#   .rdata[['equity_dimensions']] <<- sort(indic_dim$dimension)
#   .rdata[['full_indicators']] <<- get_formatted_indicators(indic_dim)
#
# }



# Will reset the focus inequality measure and the full
# list of possible inequality measures
reset_focus_inequal <- function(mydimension = .rdata[['focus_dimension']]){


  # getFilteredInequal subsets the inequality dataset
  # based on the current dimension, indicator etc
  .rdata[['summary_measures_all']] <<- getFilteredInequal(mydimension = mydimension)

  # If any of the currently selected inequality measures are in the updated list
  # use these
  if(any(.rdata[['focus_inequal_type']]%in%.rdata[['summary_measures_all']])){
    .rdata[['focus_inequal_type']] <<- .rdata[['summary_measures_all']][.rdata[['summary_measures_all']]%in%.rdata[['focus_inequal_type']]]
  }else{
    .rdata[['focus_inequal_type']] <<- .rdata[['summary_measures_all']][.rdata[['summary_measures_all']] == "d"]
  }



}


getFullIndic <- function(indic, first = NULL, yr = .rdata[['focus_year']][1]){
  if(length(indic) == 1 && trimws(indic) == "") return()
  if(trimws(.rdata[['focus_country']]) == "") return()
  #if(!any(class(.rdata[['maindata']])%in%"data.frame")) return()


  indic <- .rdata[['full_indicators']][.rdata[['full_indicators']]%in%indic]
  if(!is.null(first)){
    indic <- c(indic[indic == first], indic[indic!=first])
  }

  # git 898
  indic2 <- indic
  tmp <- filter(.rdata[['strata']], country == .rdata[['focus_country']],
                year== yr, dimension == .rdata[['focus_dimension']][1],
                indic%in%indic2) %>% .$indic

  if(length(tmp) == 0) return(NULL)
  if(!indic[1]%in%tmp){

    first <- tmp[1]
    indic <- c(indic[indic == first], indic[indic!=first])
  }



  return(indic)
}




getDetailedSubgroups <- function(dat = .rdata[['maindata']],
                                 countryDT = .rdata[['focus_country']],
                                 yearDT = .rdata[['focus_year']],
                                 indicatorDT = .rdata[['focus_indicator']],
                                 dimensionDT = .rdata[['focus_dimension']]){

  tmp <- dat[CJ(countryDT,as.integer(yearDT),indicatorDT,dimensionDT),
             .(subgroup, r), nomatch = 0L]
  tmp$subgroup[!is.na(tmp$r)]


}



#
