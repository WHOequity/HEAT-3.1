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


# get_upload_data() ----
observeEvent(input$filename, priority = 9999, {
  session$sendCustomMessage("enableUploadButton", list())
})

stop_bad_upload <- function(key) {
  stop(
    structure(
      class = c("upload_failed", "error", "condition"),
      list(
        key = key,
        message = "bad upload",
        call = NULL
      )
    )
  )
}

get_upload_data <- eventReactive(input$doUploadBtn, {

  if (is.null(input$filename)) {
    stop_bad_upload("warnchoose")
  }
  
  inFile <- input$filename
  ext <- file_ext(inFile$name)
  
  if (!(ext %in% c("xls", "xlsx", "csv", "txt"))) {
    stop_bad_upload("warnextension")
  }
  
  dat <- NULL
  
  if (ext %in% c("csv", "txt")) {
    dat <- tryCatch(
      read.csv(inFile$datapath, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) stop_bad_upload("warncsvformat")
    )
    
    if (NROW(dat) < 2) {
      stop_bad_upload("warncsvrows")
    }
  }
  
  if (ext %in% c("xls", "xlsx")) {
    file.rename(inFile$datapath, paste(inFile$datapath, ext, sep = "."))
    
    dat <- tryCatch(
      read_excel(paste(inFile$datapath, ext, sep = "."), sheet = 1),
      error = function(e) stop_bad_upload("warnexcelformat")
    )
    
    if (nrow(dat) < 2) {
      stop_bad_upload("warningexcelrows")
    }
  }
  
  names(dat)[names(dat) == "setting"] <- "country"
  names(dat)[names(dat) == "setting_average"] <- "national"
  
  dat <- mutate(dat, iso3 = as.character(iso3))
  
  # correct iso3 codes and countries ----
  isomap <- readRDS("data/HEAT-data/countryinfo.RDS") %>% 
    select(country, iso3) %>% 
    mutate(country_new = country, iso3_new = iso3)
  
  dat <- left_join(dat, select(isomap, -iso3, -country_new), by = "country") %>%
    left_join(select(isomap, -country, -iso3_new), by = "iso3") %>%
    mutate(
      iso3 = ifelse(is.na(iso3_new), iso3, iso3_new),
      country = ifelse(is.na(country_new), country, country_new)
    ) %>%
    select(-iso3_new, -country_new)
  
  tryCatch(
    {
      # initial test: missing columns ----
      missing_required_variables(dat)  
      
      # initial test: for integers ----
      not_discrete(dat)
      
      # initial test: for columns which must be integers ----
      vars_not_nums(dat)
      
      # test 2: mandatory columns with missing values ----
      missing_one_val(dat)
      
      # test 3:  key variables all missing ----
      empty_key_variables(dat)
      
      # test 4: variables that should be character are number ----
      vars_not_char(dat)
      
      # test 5: iso should be 3 digits ----
      iso_not_3_chars(dat)
      
      # test 6: vars_not_nums ----
      # in server_upload_data
      
      # test 7: year is length 4 and number ----
      year_test(dat)
      
      # test 8: must be discrete ----
      # Note that I run this also in server_upload_data so
      # may not be necessary here.
      not_discrete(dat)
      
      # test 9: variables that need to be 0/1 ----
      has_negative(dat)
      
      # test 10: population is zero ----
      population_not_zero(dat)
      
      # test 11: variables that need to be 0/1, favourable_indicator ----
      fav_indicator_is_binary(dat)
      
      # test 12: variables that need to be 0/1, ordered ----
      ordered_dim_is_binary(dat)

      # test 13: The subgroup order needs to be 0 if the ordered_dimension = 0 ----
      subgroup_matches_ordered(dat)
      
      # test 14: Subgroup_order is not an increasing sequence when ordered_dimension = 1 ----
      subgroup_is_sequence(dat)
      
      # test 15: Ordered_dimension is not 0 or 1 ----
      reference_is_binary(dat)
      
      # test 16: reference_subgroup is not 0 when ordered_dimension = 1 ----
      reference_matches_ordered(dat)
      
      # test 17: ref subgroup > 1 ----
      reference_has_single_1(dat)
      
      # test 18: unique country/iso3 combination ----
      country_iso3_nomatch(dat)
      
      # test 19.01: indicator abbr, name mismatch ----
      indicator_abbr_unique_names(dat)
      
      # test 19: indicator/indicator_abbr just one per country ----
      observations_when_grouped(dat)
            
      # test 20: ordered not same ----
      ordered_dim_when_grouped(dat)
      
      # test XX: ----       
      scale_has_zero_or_NA(dat)
    }, 
    invalid_data = function(e) {
      stop_bad_upload(e$key)
    }  
  )

  dat
})

do_upload <- reactive({  
  input$doUploadBtn
  
  create_new_data <- isolate(input$neworexisting) == "newdata"
  
  # if they choose new data
  if (create_new_data) {
    
    if (!grepl("^[0-9a-zA-Z ._-]+$", isolate(input$datafoldername))) {
      stop_bad_upload("heat_plus_landing_db_warning_name") 
    }
      
    # try to get the upload data, if any of the tests fail an `upload_message`
    # error is raised and a message key is returned, translated, and displayed
    # to the user
    upload_data <- tryCatch(
      get_upload_data(),
      upload_message = function(e) {
        session$sendCustomMessage(type = "resetFileInputHandler", "filename")
        stop_bad_upload(e$key)
    })
    
    # this returns the strata and maindata
    res <- create_raw_tables(upload_data)

    inequals <- lapply_inequals(res$strata, res$maindata, .rdata[['inequal_rules_tmp']])

    # Git 868, now that we don't need r_national or boot.se they are
    # all SE and end up as a logical and we get an error when we
    # round
    inequals <- mutate(
      inequals,
      boot.se  = as.numeric(boot.se),
      r_national = as.numeric(r_national)
    )

    # We've decided (per GitHub 480) that for all except mdb we 
    # we should use analytic SE
    inequals$final_se <- inequals$se
    inequals$final_se[inequals$measure %in% c("mdb", "mdm", "idis")] <- 
      inequals$boot.se[inequals$measure %in% c("mdb", "mdm", "idis")]
    
    outpath <- "./data/user-data/"
    
    change_names_preserve_data(
      inequals = inequals, 
      maindata = res$maindata, 
      strata = res$strata,
      savedata = TRUE,
      outpath = outpath, 
      prefix = isolate(input$datafoldername)
    )
    
    # updateSelectInput(session, "selectfolders", choices = get_data_folders())
    
    # end create new data section
  } else {
    selectfolders <- paste0(isolate(input$selectfolders), "____")
    
    if (selectfolders == "none") {
      stop_bad_upload("warnexistingdata")
    }
    
    existdatapath <- paste0("data/user-data/", selectfolders)
    thefiles <- list.files("data/user-data/")
    thefiles <- thefiles[grepl(selectfolders, thefiles)]
    thefiles <- gsub(selectfolders, "",  thefiles, fixed = TRUE)
    
    hasallfiles <- all(
      tolower(c("inequals.RDS", "maindata.RDS", "strata.RDS", "dimensions.RDS", "years.RDS")) %in%
        tolower(thefiles)
    )
    
    if (!hasallfiles) {
      stop_bad_upload("warnreqfiles")
    }
    
    use_existing_data(existdatapath)
    
    # end open existing data section
  }
  
  
  # this fills in the .rdata(?) globals
  fill_in_global_list()

  # this works but disabled for now
  updateSelectInput(session, "focus_country_explore", choices = .rdata[['all_countries']],
                    selected = .rdata[['focus_country']])
  
  updateSelectInput(session, "focus_dimension_explore", choices = .rdata[['equity_dimensions']],
                    selected = .rdata[['focus_dimension']])

  updateSelectInput(session, "focus_indicator_explore", choices = .rdata[['full_indicators']],
                    selected = .rdata[['focus_indicator']])
  
  updateSelectInput(session, "focus_data_source_explore", choices = .rdata[['data_sources']],
                    selected = .rdata[['focus_data_source']])
  
  updateSelectInput(session, "focus_year_explore", choices = .rdata[['all_years']],
                    selected = .rdata[['focus_year']])
  
  updateSelectInput(session, "focus_country_compare", choices = .rdata[['all_countries']],
                    selected = .rdata[['focus_country']])
  
    
  updateSelectInput(session, "focus_indicator_compare", choices = .rdata[['full_indicators']],
                    selected = .rdata[['focus_indicator']])
  
  updateSelectInput(session, "focus_dimension_compare", choices = .rdata[['equity_dimensions']],
                    selected = .rdata[['focus_dimension']])

  updateSelectInput(session, "focus_data_source_compare", choices = .rdata[['data_sources']],
                     selected = .rdata[['focus_data_source']])
  
  updateSelectInput(session, "focus_year_compare", choices = .rdata[['all_years']],
                    selected = .rdata[['focus_year']])
  
  
  updateSelectInput(session, "benchmarkWBgroup",              
                    choices = .rdata[['income_groups']],
                    selected = .rdata[['focus_income_group']])
  
  updateSelectInput(session, "benchmarkWHOregion",               
                    choices=.rdata[['who_regions']],
                    selected = .rdata[['focus_who_regions']])
  
  
  updateTextInput(session, "main_title1", value = .rdata[["plotDisag_explore_title"]])
  updateTextInput(session, "main_title2", value = .rdata[["plotSummary_explore_title"]])
  updateTextInput(session, "main_title3", value = .rdata[["plotDisag_compare_title"]])
  updateTextInput(session, "main_title4", value = .rdata[["plotSummary_compare_title"]])
  updateTextInput(session, "main_title4", value = .rdata[["plotSummary_compare_title"]])
  
  updateSelectInput(session, "benchmark_countries", 
                    choices=.rdata[['benchmark_countries']], 
                    selected=.rdata[['benchmark_countries']])
  
  outpath <- paste0("./data/user-data/", isolate(input$datafoldername))
  
  if (create_new_data) {
    session$sendCustomMessage(type = "resetFileInputHandler", "filename")
    return("successdataimport")
  } else {
    return("successdataopen")
  }
})
  
output$table_result <- renderUI({
  lang <- input$lang
  
  tryCatch(
    {
      success <- do_upload()
      
      updateSelectInput(
        session = session, 
        inputId = "selectfolders", 
        choices = get_data_folders()
      )
      
      tags$div(
        class = "datasuccess",
        tags$div(
          class = "msg_upload",
          translate(success, lang)
        ),
        tags$p(
          translate("heat_plus_landing_db_success_text", lang)
        )
      )
    },
    upload_failed = function(e) {
      tags$div(
        class = "datawarning",
        tags$div(
          class = "msg_upload",
          translate("heat_plus_landing_db_error", lang)
        ),
        tags$p(
          translate(e$key, lang)
        )
      )
    }
  )
})

observeEvent(input$neworexisting, {
  session$sendCustomMessage("toggleUploadButton", list(
    shown = input$neworexisting
  ));
})

