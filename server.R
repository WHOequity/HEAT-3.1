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

# Set max upload size to 30 megabytes
options(shiny.maxRequestSize=30*1024^2)
options(shiny.launch.browser = TRUE)

shinyServer(function(input, output, session){ 
  .rdata <- list()
  .rdata[['save_objects']] <- ls(envir = globalenv(), all.names = TRUE)
  
  .rdata[['HEATversion']] <- HEATversion # see global.R
  
  
  #************************************************
  # Assemble spatial files ----
  #************************************************
  assemble_country_geography <- function(country1, year1){
    
    
    country1 <- tolower(country1)
    geodata <- filter(.rdata$geojson_subnational, 
                      country == country1,
                      year == year1)
    
    if(nrow(geodata)>0){
      geodata <- pull(geodata, geojson) %>% .[[1]]
    }
    
    
    geodata_natl <- filter(.rdata$geojson_national, 
                           country == country1)
    
    if(nrow(geodata_natl)>0){
      geodata_natl <- pull(geodata_natl, geojson) %>% .[[1]]
    }
    
    
    geodata_disputed <- filter(.rdata$geojson_disputed, 
                               country == country1)

    if(nrow(geodata_disputed)>0){
      geodata_disputed <- pull(geodata_disputed, geojson) %>% .[[1]]
    }
    
    
    list(geodata_subnational = geodata,
         geodata_national = geodata_natl,
         geodata_disputed = geodata_disputed)
  }
  

  if(.rdata[['HEATversion']] == "upload"){
    
    library(metafor) # rma.glmm
    library(msm) # for deltamethod in sii, kmi
    source("server/server_upload_data.R", local = TRUE)
    
    source("utils/data_upload_validate.R", local = TRUE)
    source("utils/upload_create_raw_tables.R", local = TRUE)
    
    source("utils/inequal_tests.R") # not local = TRUE to functions are available
    thefiles<-list.files("utils/inequal_functions", full.names=TRUE)
    for(i in thefiles) source(i)

  }
  
  
  
  source("utils/get_filtered.R", local=TRUE)
  source("utils/tab_reactive_coordination.R", local=TRUE)
  plotfiles<-list.files("plots/", full.names=TRUE)
  # plotfiles <- plotfiles[!grepl("_hc", plotfiles)]
  for(i in plotfiles) source(i, local = TRUE)
  source("utils/initial_settings_both.R", local=TRUE)
  
  initial_settings <- paste0("utils/", "initial_settings_", .rdata[['HEATversion']], ".R")
  source(initial_settings, local=TRUE)


  source("utils/get_data.R", local=TRUE)
  
  uifiles<-list.files("ui/", full.names=TRUE)
  
  for(i in uifiles) source(i, local = TRUE)

  source("server/server_translations.R", local = TRUE)
  source("server/server_ui_creation.R", local=TRUE)
  source("server/server_logic.R", local=TRUE)
  source("server/server_observers.R", local=TRUE)
  source("server/server_downloading.R", local=TRUE)
  
  
  outputOptions(output, "dataTableItems_explore", suspendWhenHidden = FALSE)
  #outputOptions(output, "disag_plot_dimensions_explore", suspendWhenHidden = FALSE)

  outputOptions(output, "disag_plot_error_bars", suspendWhenHidden = FALSE)
  outputOptions(output, "dataTableItemsSummary_explore", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_plot_type_explore_component", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_plot_error_bars", suspendWhenHidden = FALSE)
  #outputOptions(output, "summary_plot_dimensions_explore", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_plot_CI_type", suspendWhenHidden = FALSE)
  #outputOptions(output, "ui_collapse_explore_disag_map", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_collapse_explore_disag_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_collapse_explore_sum_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_collapse_compare_disag_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "ui_collapse_compare_sum_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "disag_plot_explore_dtl_btn", suspendWhenHidden = FALSE)
  #outputOptions(output, "disag_plot_dimensions_explore_dtl", suspendWhenHidden = FALSE)
  outputOptions(output, "longindicname_disag_explore", suspendWhenHidden = FALSE)
  outputOptions(output, "longindicname_explore_sum", suspendWhenHidden = FALSE) 
  outputOptions(output, "focus_indicator_explore_plotdtl_component", suspendWhenHidden = FALSE)
  #outputOptions(output, "focus_dimension_explore_dtl", suspendWhenHidden = FALSE)
  outputOptions(output, "longindicname_disag_explore_dtl", suspendWhenHidden = FALSE)
  outputOptions(output, "focus_source_year_explore_dtlmap", suspendWhenHidden = FALSE)
  
  
  # remove pieces of the global list that are not necessary but leave the first
  # few in case user reloads instead of quits and restarts
  session$onSessionEnded(function() {

    .rdata[!names(.rdata)%in%c('HEATversion', 'landing_page', 'extra_css')] <<- NULL

  })
  
})

# sessionInfo() # 2017-07-14
# R version 3.4.0 (2017-04-21)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Sierra 10.12.5
# 
# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets 
# [7] methods   base     
# 
# other attached packages:
#   [1] tidyr_0.6.2        readxl_1.0.0       data.table_1.10.4 
# [4] DT_0.2             dplyr_0.5.0        RColorBrewer_1.1-2
# [7] gridExtra_2.2.1    ggplot2_2.2.1      shinyBS_0.61      
# [10] shiny_1.0.3       
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.11     compiler_3.4.0   cellranger_1.1.0
# [4] plyr_1.8.4       tools_3.4.0      digest_0.6.12   
# [7] jsonlite_1.5     tibble_1.3.3     gtable_0.2.0    
# [10] rlang_0.1.1      DBI_0.6-1        yaml_2.1.14     
# [13] stringr_1.2.0    htmlwidgets_0.8  R6_2.2.0        
# [16] reshape2_1.4.2   magrittr_1.5     scales_0.4.1    
# [19] htmltools_0.3.6  rsconnect_0.8    assertthat_0.2.0
# [22] mime_0.5         xtable_1.8-2     colorspace_1.3-2
# [25] httpuv_1.3.3     labeling_0.3     stringi_1.1.5   
# [28] lazyeval_0.2.0   munsell_0.4.3   
