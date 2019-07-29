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
# The upload and the whodata versions use different country info
# *****************************************************************************

.rdata[['first_time']] <- TRUE
#.rdata[['plot_height']] <- 600

.rdata[['countryinfo']] <- readRDS("data/HEAT-data/countryinfo.RDS")


# *****************************************************************************
# The focus indicator
# *****************************************************************************
.rdata[['focus_indicator']]<-" "




# *****************************************************************************
# Read in country information
# *****************************************************************************


#.rdata[['countryinfo']]<-readRDS("../../Data/testnew/countryinfo.RDS")
.rdata[['all_countries']]<-""


# *****************************************************************************
# Read in strata information
# *****************************************************************************


.rdata[['strata']]<-" "


# *****************************************************************************
# Set focus country, dimension and year
# *****************************************************************************

.rdata[['focus_country']]<-" "

.rdata[['focus_dimension']]<-" "
.rdata[['focus_year']]<-" "

# *****************************************************************************
# Key data
# *****************************************************************************


.rdata[['maindata']] <- " "
.rdata[['inequals']]<-" "



# *****************************************************************************
# Read in years and set years
# *****************************************************************************

.rdata[['all_years']]<-" "
.rdata[['dimension_details']] <- " "#rbind(sex, economic, education, residence, geo)




# *****************************************************************************
# Set data sources and some other initial settings
# *****************************************************************************

.rdata[['data_sources']] <- " "
.rdata[['focus_data_source']]<-" "
.rdata[['mostrecent']]<-FALSE


.rdata[['benchmark_countries']]<-" "


# *****************************************************************************
# Uploading settings
# *****************************************************************************

.rdata[['csv_path']] <- "none"
.rdata[['excel_path']] <- "none"




