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

# *****************************************************************************
# EDIT AS NEEDED: Choose the initial settings here ----
# *****************************************************************************
focus_country <- "Indonesia"
focus_dimension <- c("Economic status")
focus_year <- c(2012, 2007, 2002, 1997, 1994)
focus_indicator <- c("sba")
focus_summary_measure <- "d"



# *****************************************************************************
# EDIT AS NEEDED: Choose items to drop ----
# *****************************************************************************

drop_country <- c()                # e.g., c("Armenia", "Afghanistan")
drop_indicator <- c()              # e.g., c("anc1", 'asfr1')


# -----

drop_dimension <- c() # DON'T EDIT THIS ONE


# *****************************************************************************
# EDIT AS NEEDED: Restrict to these regions ----
# *****************************************************************************

# to keep all regions use: c()
keep_region <- c() # to limit to Americas use keep_region <- c("Americas")


# *****************************************************************************
# EDIT AS NEEDED: Number of characters in the title ----
# *****************************************************************************


#numTitleChars <- 50



# *****************************************************************************
# EDIT AS NEEDED: Alter color palettes ----
# *****************************************************************************

# ----- these are the original settings
# sex_palette <- c('#ad3c3c', '#ce0c0c')
# econ_palette <- colorRampPalette(c("#D8BD35", '#04660a'))
# educ_palette <- colorRampPalette(c('#f7aa4c', '#630202'))
# place_palette <- colorRampPalette(c('#3CB7CC', '#39737C'))
# geo_palette <- '#00008B'


sex_palette <- c('#ad3c3c', '#ce0c0c')
econ_palette <- colorRampPalette(c("#D8BD35", '#04660a'))
educ_palette <- colorRampPalette(c('#f7aa4c', '#630202'))
place_palette <- colorRampPalette(c('#3CB7CC', '#39737C'))
geo_palette <- '#5c87bc'




# *****************************************************************************
# DO NOT EDIT BELOW
# *****************************************************************************







# *****************************************************************************
# The focus indicator
# *****************************************************************************

.rdata[['focus_indicator']]<-focus_indicator


# *****************************************************************************
# The upload and whodata versions use different data
# *****************************************************************************

#.rdata[['plot_height']] <- 600
.rdata[['countryinfo']] <- .global[["countryinfo"]]
#.rdata[['countryinfo']]$country[.rdata[['countryinfo']]$country == "Côte d'Ivoire"] <- "Cote d'Ivoire"


# *****************************************************************************
# Read in spatial data
# *****************************************************************************


.rdata[['geojson_subnational']] <- .global[["geojson_subnational"]] # readRDS("data/geojson_subnational.RDS")
.rdata[['geojson_national']] <- .global[["geojson_national"]] # readRDS("data/geojson_national.RDS")
.rdata[['geojson_disputed']] <- .global[["geojson_disputed"]] # readRDS("data/geojson_disputed.RDS")



# countries with disputed territories that need to be dark grey in map
# careful, this is a hack git1063
.rdata[['disputed_subnational']] <- tibble(country = "india", 
                                           subgroups = list(list("[jm] jammu and kashmir", "jammu")))


# git1062 as example, countries where disputes mean we should drop geography
.rdata[["country_drop_geography"]] <- c("Morocco", "India", "Pakistan", "Comoros", "Gambia", "Haiti",
                                        "Maldives", "Sao Tome and Principe", "Yemen")


# *****************************************************************************
# Read in strata information
# *****************************************************************************


strata <- .global[["strata"]]
names(strata) <- convert_names_to_old(names(strata), new_to_old_names)
#strata$country[strata$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
# filt_indic <- quote(d)
# 
# if(!is.null(drop_country)) strata <- strata[!.(drop_country)]
# if(!is.null(drop_indicator)) strata <- strata[.(unique(country))][!.(drop_indicator)]
# 
# strata[CJ(drop_indicator, !drop_dimension, !drop_country)]

strata <- filter(strata, !indic%in%drop_indicator, !dimension%in%drop_dimension,
                            !country%in%drop_country)





if(!is.null(strata)){
  tmpindic <- select(strata, indic_name, indic) %>% distinct %>% 
    arrange(indic_name)
  
  #print(dim(tmpindic)) should be 37
   namestmpindic <- tmpindic$indic_name
tmpindic <- tmpindic$indic
names(tmpindic)<-namestmpindic
  
}

strata <- data.table(strata)
setkey(strata, country, year, indic, dimension, source)



.rdata[['strata']] <- strata
.rdata[['full_indicators']] <- tmpindic

rm(strata)




# *****************************************************************************
# Read in country information
# *****************************************************************************

uniqueCtry <- unique(.rdata[['strata']]$iso3)
.rdata[['countryinfo']] <- filter(.rdata[['countryinfo']], iso3%in%uniqueCtry)
regions <- unique(.rdata[['countryinfo']]$whoreg6_name)

# ----- DROP countrynames
if(is.null(keep_region) || keep_region == ""){
  drop_country <- drop_country
}else{
  
  drop_region<-regions[!regions%in%keep_region]
  drop_country_xtra <- filter(.rdata[['countryinfo']], whoreg6_name%in%drop_region) %>% .$country
  drop_country <- c(drop_country, drop_country_xtra)
}


.rdata[['countryinfo']] <- filter(.rdata[['countryinfo']], !country%in%drop_country)

#if(!is.null(keep_region) && keep_region != "") .rdata[['countryinfo']] <- filter(.rdata[['countryinfo']], whoreg6_name%in%keep_region)

.rdata[['all_countries']] <- sort(.rdata[['countryinfo']]$country)


.rdata[['countryyrsource']] <- dplyr::select(.rdata[['strata']], country,year, source) %>% distinct

# *****************************************************************************
# Set focus country, dimension and year
# *****************************************************************************

.rdata[['focus_country']]<-focus_country

.rdata[['focus_dimension']]<-focus_dimension

.rdata[['focus_year']]<-focus_year



# *****************************************************************************
# Read in dimensions
# *****************************************************************************

dimensions <- .global[["dimensions"]]
names(dimensions) <- convert_names_to_old(names(dimensions), new_to_old_names)
.rdata[['dimension_details']] <- filter(dimensions, !dimension%in%drop_dimension)
rm(dimensions)

.rdata[['dimension_summary']] <- select(.rdata[['dimension_details']], dimension, order, maxn) %>% 
  mutate(ordered = order != 0, just2_subgroups = maxn <= 2) %>% select(-order, -maxn) %>% ungroup %>%  distinct


.rdata[['equity_dimensions']] <- sort(unique(.rdata[['dimension_details']]$dimension))
.rdata[['geo_dimension']] <- unique(.rdata[['dimension_details']]$dimension[.rdata[['dimension_details']]$dimension_type=="region"])


# *****************************************************************************
# Read in maindata, inequality data and national data
# *****************************************************************************

maindata <- .global[["maindata"]]
# maindata <- filter(maindata, country == "Indonesia")
names(maindata) <- convert_names_to_old(names(maindata), new_to_old_names)
#maindata$country[maindata$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
maindata <- data.table(maindata)
setkey(maindata, country, year, indic, dimension, source)


inequals <- .global[["inequals"]]
names(inequals) <- convert_names_to_old(names(inequals), new_to_old_names)
inequals <- data.table(inequals)
setkey(inequals, country, year, indic, dimension, measure, source)




if(!all(is.null(c(drop_indicator, drop_dimension, drop_country)))){
  maindata<-filter(maindata, !indic%in%drop_indicator, 
                                !dimension%in%drop_dimension,
                            !country%in%drop_country)
  
  inequals<-filter(inequals, !indic%in%drop_indicator, 
                                !dimension%in%drop_dimension,
                              !country%in%drop_country)
}


.rdata[['maindata']] <- maindata
.rdata[['inequals']]<-inequals


rm(maindata, inequals)
# .rdata[['nationaldata']]<-filter(.rdata[['nationaldata']], !indic%in%drop_indicator, !dimension%in%drop_dimension,
#                               !country%in%drop_country)


.rdata[['inequal_rules']] <- full_join(.rdata[['dimension_summary']], 
                                        .rdata[['inequal_rules_tmp']], by = c("ordered", "just2_subgroups"))

# *****************************************************************************
# The initial groups for the detailed bar plot
# *****************************************************************************

.rdata[['focus_plotdtl_subgroups']] <- unique(.rdata[['maindata']][CJ(focus_country, 
                        focus_year, 
                        focus_indicator, 
                        focus_dimension), 
                     .(subgroup), nomatch = 0L])$subgroup





# *****************************************************************************
# Set the shapes and palettes
# *****************************************************************************
.rdata[['dimension_details']] <- assignColorsShapes(.rdata[['dimension_details']])

# #shapes <- rep(c(21, 22, 23, 24, 25),2)
# shapes <- rep(c(21), 10)
# sex <- filter(.rdata[['dimension_details']], dimension_type=="sex")
# sex$colors <- sex_palette
# sex$shapes <- shapes[1:2] 
# 
# economic <- filter(.rdata[['dimension_details']], dimension_type=="wealth")
# economic$colors <- econ_palette(nrow(economic))
# economic$shapes <- shapes[1:nrow(economic)]
# 
# education <- filter(.rdata[['dimension_details']], dimension_type=="educ")
# education$colors <- educ_palette(nrow(education))
# education$shapes <- shapes[1:nrow(education)]
# 
# 
# residence <- filter(.rdata[['dimension_details']], dimension_type=="area")
# residence$colors <- place_palette(nrow(residence))
# residence$shapes <- shapes[1:nrow(residence)]
# 
# 
# geo <- filter(.rdata[['dimension_details']], dimension_type=="region")
# geo$colors <- geo_palette
# geo$shapes <- 21
# 
# .rdata[['dimension_details']] <- rbind(sex, economic, education, residence, geo)




# *****************************************************************************
# Set data sources and some other initial settings
# *****************************************************************************

.rdata[['data_sources']] <- sort(unique(.rdata[["strata"]]$source))
.rdata[['focus_data_source']] <- .rdata[['data_sources']] # getFilteredSource(focus_country)
.rdata[['mostrecent']] <- FALSE


.rdata[['focus_inequal_type']]<-focus_summary_measure

anchor <- filter(.rdata[['countryinfo']], country == focus_country)
.rdata[['benchmark_countries']]<-filter(.rdata[['countryinfo']], 
                                         wbincome==anchor$wbincome, 
                                         whoreg6_name==anchor$whoreg6_name)%>% .$country
#.rdata[['benchmark_country_list']]<-.rdata[['all_countries']]
.rdata[['income_groups']] <- sort(unique(.rdata[['countryinfo']]$wbincome))
.rdata[['who_regions']] <- sort(unique(.rdata[['countryinfo']]$whoreg6_name))

.rdata[['focus_income_group']] <- filter(.rdata[['countryinfo']], country==.rdata[['focus_country']]) %>% .$wbincome 
.rdata[['focus_who_regions']] <- filter(.rdata[['countryinfo']], country==.rdata[['focus_country']]) %>% .$whoreg6_name 


# *****************************************************************************
# Read in years and set years
# *****************************************************************************
.rdata[['years']] <- .global[["years"]]
#rev(sort(c(1994, 1997,2002, 2007, 2012)))#
.rdata[['all_years']]<-getFilteredYear(focus_country)#c(1994, 1997,2002, 2007, 2012)


# *****************************************************************************
# Read in spatial min max
# *****************************************************************************

.rdata[['subregion_minmax']] <- .global[["subregion_minmax"]]

