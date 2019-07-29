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

######### All the measures of inequality
# This manages the return of the inequalities measures inequality measures
#########




getInequalData <- function(indicator = NULL, stratifier = NULL, countries = NULL, years = NULL,
                       mostrecent=NULL, datasource=NULL,  inequal_types=NULL, multiplier1=TRUE,multiplier2=TRUE, 
                       elasticity=NULL, anchorCountry = NULL, forplot = FALSE){

  
  countries <- make_unknown_encoding(countries)
  
  years <- as.integer(years)
  
  if(!is.null(mostrecent) && mostrecent){
    
    # for selecting the year for benchmark countries we want to use the year
    # from the anchor country
    tmpCountry <- ifelse(!is.null(anchorCountry), anchorCountry, countries[1])
    years <- getFilteredYear(country=tmpCountry, datasource)[1]
  }
  
  if(!is.null(elasticity) && !is.null(years)){
    # if mostrecent is TRUE then we want the focus_year, otherwise
    # we can use the input year
    focus_year <- years
    years<-seq(years-elasticity, years+elasticity)

    
  }
  

  filt_country <- quote(unique(country))
  filt_year <- quote(unique(year))
  filt_indicator <- quote(unique(indic))
  filt_dimension <- quote(unique(dimension))
  filt_inequaltype <- quote(unique(measure))
  filt_datasource <- quote(unique(source))
  
  if(!is.null(countries)) filt_country <- quote(countries)
  if(!is.null(years)) filt_year <- quote(years)
  #if(!is.null(years) && is.null(mostrecent)) filt_year <- quote(year %in% years)
  if(!is.null(indicator)) filt_indicator <- quote(indicator)
  if(!is.null(stratifier)) filt_dimension <- quote(stratifier)
  if(!is.null(inequal_types)) filt_inequaltype <- quote(inequal_types)
  if(!is.null(datasource)) filt_datasource <- quote(datasource)
  
  ineqDF <- .rdata[['inequals']][CJ(eval(filt_country), 
                             eval(filt_year), 
                             eval(filt_indicator), 
                             eval(filt_dimension),
                             eval(filt_inequaltype),
                             eval(filt_datasource)), 
                          .(country, year, indic, dimension,source, measure, inequal, boot.se, se, ccode, 
           indic_name, r_national, se.lowerci, se.upperci), nomatch = 0L]
  
  

  ineqDF[,year:=as.integer(year)]

  # careful here. We decided only to use either analytic
  # or the bootstrap SE and this is stored in final_se
  #I will replace se with final_se and not use boot.se
  
  #ineqDF$se <- ineqDF$final_se
  ineqDF$boot.se <- NULL
  ineqDF$se[ineqDF$se == 0] <- NA
  #ineqDF$boot.se <- as.numeric(ineqDF$boot.se)
  #ineqDF$boot.se[ineqDF$boot.se==0] <- NA

  #ineqDF[,se.lowerci:=inequal - (1.96 * se)]
  #ineqDF[,se.upperci:=inequal + (1.96 * se)]
  
  #ineqDF[,boot.lowerci:=inequal - (1.96 * se)]
  #ineqDF[,boot.upperci:=inequal + (1.96 * se)]
  

  # special rule for ineqDF rr and riikm only
  #special<-which(ineqDF$measure%in%c("rr", "riikm"))
  #ineqDF$se.lowerci[special] <- exp (log(ineqDF$inequal[special]) - (1.96 * ineqDF$se[special]/ineqDF$inequal[special]) )
  #ineqDF$se.upperci[special] <- exp (log(ineqDF$inequal[special]) + (1.96 * ineqDF$se[special]/ineqDF$inequal[special]) )
  #ineqDF$boot.lowerci[special] <-exp (log(ineqDF$inequal[special]) - (1.96 * ineqDF$boot.se[special]/ineqDF$inequal[special]) )
  #ineqDF$boot.upperci[special] <- exp (log(ineqDF$inequal[special]) + (1.96 * ineqDF$boot.se[special]/ineqDF$inequal[special]) )

  

  
  if(is.null(multiplier1) || multiplier1){


    ineqDF$inequal[ineqDF$measure=='ti'] <- ineqDF$inequal[ineqDF$measure=='ti'] *1000
    ineqDF$inequal[ineqDF$measure=='mld'] <- ineqDF$inequal[ineqDF$measure=='mld'] *1000
    ineqDF$se[ineqDF$measure=='ti'] <- ineqDF$se[ineqDF$measure=='ti'] *1000
    ineqDF$se[ineqDF$measure=='mld'] <- ineqDF$se[ineqDF$measure=='mld'] *1000
    ineqDF$se.lowerci[ineqDF$measure=='ti'] <- ineqDF$se.lowerci[ineqDF$measure=='ti'] *1000
    ineqDF$se.lowerci[ineqDF$measure=='mld'] <- ineqDF$se.lowerci[ineqDF$measure=='mld'] *1000
    ineqDF$se.upperci[ineqDF$measure=='ti'] <- ineqDF$se.upperci[ineqDF$measure=='ti'] *1000
    ineqDF$se.upperci[ineqDF$measure=='mld'] <- ineqDF$se.upperci[ineqDF$measure=='mld'] *1000
    # ineqDF$boot.se[ineqDF$measure=='ti'] <- ineqDF$boot.se[ineqDF$measure=='ti'] *1000
    # ineqDF$boot.se[ineqDF$measure=='mld'] <- ineqDF$boot.se[ineqDF$measure=='mld'] *1000
    # ineqDF$boot.lowerci[ineqDF$measure=='ti'] <- ineqDF$boot.lowerci[ineqDF$measure=='ti'] *1000
    # ineqDF$boot.lowerci[ineqDF$measure=='mld'] <- ineqDF$boot.lowerci[ineqDF$measure=='mld'] *1000
    # ineqDF$boot.upperci[ineqDF$measure=='ti'] <- ineqDF$boot.upperci[ineqDF$measure=='ti'] *1000
    # ineqDF$boot.upperci[ineqDF$measure=='mld'] <- ineqDF$boot.upperci[ineqDF$measure=='mld'] *1000

  }
  
  if(is.null(multiplier2) || multiplier2){
    #print("In dataTableInequal b")
    ineqDF$inequal[ineqDF$measure=='rci'] <- ineqDF$inequal[ineqDF$measure=='rci'] *100
    ineqDF$se[ineqDF$measure=='rci'] <- ineqDF$se[ineqDF$measure=='rci'] *100
    ineqDF$se.lowerci[ineqDF$measure=='rci'] <- ineqDF$se.lowerci[ineqDF$measure=='rci'] *100
    ineqDF$se.upperci[ineqDF$measure=='rci'] <- ineqDF$se.upperci[ineqDF$measure=='rci'] *100
    # ineqDF$boot.se[ineqDF$measure=='rci'] <- ineqDF$boot.se[ineqDF$measure=='rci'] *100
    # ineqDF$boot.lowerci[ineqDF$measure=='rci'] <- ineqDF$boot.lowerci[ineqDF$measure=='rci'] *100
    # ineqDF$boot.upperci[ineqDF$measure=='rci'] <- ineqDF$boot.upperci[ineqDF$measure=='rci'] *100
#     ineqDF$combo.se[ineqDF$measure=='rci'] <- ineqDF$combo.se[ineqDF$measure=='rci'] *100
#     ineqDF$combo.lowerci[ineqDF$measure=='rci'] <- ineqDF$combo.lowerci[ineqDF$measure=='rci'] *100
#     ineqDF$combo.upperci[ineqDF$measure=='rci'] <- ineqDF$combo.upperci[ineqDF$measure=='rci'] *100
  }
  

  
  if(!is.null(elasticity)){

    
    closestyr <- group_by(ineqDF, country) %>% 
      summarise(closestyr = closest_year(focus_year, year))

    ineqDF  <- semi_join( ineqDF , closestyr, by=c("country", "year" = "closestyr"))
  
    
  }
  
  

  ineqDF <- rename(ineqDF, estimate = r_national)
  
  ineqDF <- inner_join(ineqDF, .rdata[['summary_measures_table']], 
                        by=c("measure"="measure_abbr"))
  ineqDF <- arrange(ineqDF, country, year, source, indic, dimension, measure)
  return(ineqDF)
}





getDisagData <- function(indicator = NULL, stratifier = NULL, countries = NULL, 
                           years = NULL, mostrecent=FALSE, datasource=NULL, 
                           elasticity=NULL, anchor_country = NULL, forplot = FALSE){
  
  # countries <-c("Indonesia", "Afghanistan")
  # stratifier <- "Economic status"
  # indicator <- "sba"
  # years <- c(2012, 2007, 2002, 1997)
  # mostrecent <- FALSE
  # datasource <- "All"
  # mostrecent <- FALSE
  # elasticity <- NULL
  # anchor_country <- NULL
  #browser()
  countries <- make_unknown_encoding(countries)
  years <- as.integer(years)
  

  if(!is.null(mostrecent) && mostrecent){
    tmpcountry <- countries[1]
    if(!is.null(anchor_country)) tmpcountry <- anchor_country
    years <- getFilteredYear(country=tmpcountry, datasource)[1]
  }
  
  
  
  if(!is.null(elasticity) && !is.null(years)){
    # if mostrecent is TRUE then we want the focus_year, otherwise
    # we can use the input year
    focus_year <- years
    years<-seq(years-elasticity, years+elasticity)

  }
  
  
  filt_country <- quote(unique(country))
  filt_year <- quote(unique(year))
  filt_indicator <- quote(unique(indic))
  filt_dimension <- quote(unique(dimension))
  filt_datasource <- quote(unique(source)) # source is a variable in the data

  if(!is.null(countries)) filt_country <- quote(countries)
  if(!is.null(years)) filt_year <- quote(years)
  if(!is.null(indicator)) filt_indicator <- quote(indicator)
  if(!is.null(stratifier)) filt_dimension <- quote(stratifier)
  if(!is.null(datasource)) filt_datasource <- quote(datasource)
  

  hetk.data <- .rdata[['maindata']][CJ(eval(filt_country), 
                             eval(filt_year), 
                             eval(filt_indicator), 
                             eval(filt_dimension), 
                             eval(filt_datasource)), 
                          .(country, year, source, indic, dimension, subgroup, r, 
                            r_lower, r_upper, se, pop, iso3, 
                            rankable, maxoptimum, indicator_scale, popshare, flag, order, 
                            indic_name, r_national, reference_subgroup), nomatch = 0L]
  
  
  
  
  hetk.data$popshare <- 100 * hetk.data$popshare
  setnames(hetk.data, c("r", "r_national", "r_lower", "r_upper"),
                          c("estimate", "national", "lower_95ci", "upper_95ci"))

  hetk.data[,subgroup:=factor(subgroup)]
  hetk.data[,year:=as.integer(year)]
  #hetk.data[,estimate:=as.numeric(estimate)]
  #hetk.data[,se:=as.numeric(se)]
  hetk.data[,pop:=as.integer(pop)]
  #hetk.data[,lower_95ci:=as.numeric(lower_95ci)]
  #hetk.data[,upper_95ci:=as.numeric(upper_95ci)]
  hetk.data[,rankable:=as.integer(rankable)]


  
  if(!is.null(elasticity)){
    
    closestyr <- group_by(hetk.data, country) %>% 
      summarise(closestyr = closest_year(focus_year, year))
    hetk.data <- semi_join(hetk.data, closestyr, by=c("country", "year" = "closestyr"))
    
  }
  
  
  
  if(!is.data.table(hetk.data)) hetk.data <- data.table(hetk.data)
  hetk.data[,sg_nopunc:=trimws(gsub("[[:punct:]]", "", subgroup))]
  setorder(hetk.data, country, year, source, indic, dimension, order, sg_nopunc)
  hetk.data[,sg_nopunc:=NULL]


  

  return(data.frame(hetk.data))
}





closest_year <- function(focus, x){
  focus<-as.numeric(focus)
  x<-as.numeric(x)
  diff <- abs(x-focus)
  mins<-which(diff==min(diff))
  
  if(length(mins)==1) return(x[mins])
  
  if(all(diff[mins]==0)) return(x[mins[1]])
  
  return(max(x[mins]))
  
}







