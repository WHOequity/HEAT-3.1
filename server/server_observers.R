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

# *******************************************************************
#****************** COUNTRY ----
# *******************************************************************

# -----focus_country_explore 

observeEvent(c(input$focus_country_explore, input$lang), {
  req(input$focus_country_explore)
  # print(paste(Sys.time(), "In focus_country_explore"))
  
  # This is for the upload version
  if(trimws(input$focus_country_explore) == "") return()
  # When the user changes the explore country then we need to update the 
  # years selector. If there is a compare country and it's not the same
  # as the newly set focus_country then we need to upate the compare country
  # and years
  
  # ----- Initial settings
  
  focus_country_explore <- input$focus_country_explore
  focus_country_compare <- input$focus_country_compare
  focus_data_source_explore <- input$focus_data_source_explore
  
  
  

  
  
  # ----- Update global
  .rdata[['focus_country']] <<- focus_country_explore
  
  
  #sources <- getFilteredSource(focus_country_explore)
  sources <- .rdata[['data_sources']]
  #.rdata[['data_sources']] <<- sources
  .rdata[['focus_data_source']] <<- sources
  
  # ----- Update the years selector with the possible years
  if(!is.null(input$focus_data_source_explore)){
    updateSelectInput(session, 'focus_data_source_explore', 
                      choices = sources, selected = sources) 
  }
  
  # if the selectYears is the same we still want to trigger
  # the years observer
  
  if(identical(input$focus_data_source_explore, sources)){
    .trigger$focus_data_source_explore <- !.trigger$focus_data_source_explore
  }
  
  
  
  # ----- Update the years selector with the possible years
  if(!is.null(input$focus_data_source_explore_dtlmap)){
    updateSelectInput(session, 'focus_data_source_explore_dtlmap', 
                      choices = sources, selected = sources) 
  }
  
  # if the selectYears is the same we still want to trigger
  # the years observer
  
  if(identical(input$focus_data_source_explore_dtlmap, sources)){
    .trigger$focus_data_source_explore_dtlmap <- !.trigger$focus_data_source_explore_dtlmap
  }
  
  
  # ----- Changes in Compare
  
  # Need to make corresponding changes to the compare country
  
  if(!is.null(focus_country_compare)){
    updateSelectInput(session, 'focus_country_compare', selected = focus_country_explore)
  }
  
  if(is.null(input$focus_country_compare)){
    .trigger$focus_WHO_info <- !.trigger$focus_WHO_info
  }
  
})


# -----focus_country_compare 

observeEvent(c(input$focus_country_compare, input$lang), {
  req(input$focus_country_compare)
  
  # This is for the upload version
  if(trimws(input$focus_country_compare) == "") return() 
  
  
  
  
  
  if(.rdata[['first_time_country_compare']]){
    #.trigger$focus_data_source_compare <- !.trigger$focus_data_source_compare
    .rdata[['first_time_country_compare']] <<- FALSE
    return()
  }
  
  # When the user changes the explore country then we need to update the
  # years selector. If there is a compare country and it's not the same
  # as the newly set focus_country then we need to upate the compare country
  # and years
  
  # An important note about this one is that it will not be triggered
  # until the compare tab has been clicked at first. So it IS triggered
  # by a change to the explore country -- this is how the bench
  
  
  # ----- Initial settings
  
  focus_country_explore <- input$focus_country_explore
  focus_country_compare <- input$focus_country_compare
  focus_year_explore_dtlmap <- input$focus_year_explore_dtlmap
  
  # ----- Update global
  .rdata[['focus_country']] <<- focus_country_compare
  
  #sources <- getFilteredSource(focus_country_compare)
  sources <- .rdata[['data_sources']]
  #.rdata[['data_sources']] <<- sources
  .rdata[['focus_data_source']] <<- .rdata[['data_sources']]
  
  # ----- Update the years selector with the possible years
  if(!is.null(input$focus_data_source_compare)){
    updateSelectInput(session, 'focus_data_source_compare', 
                      choices = sources, 
                      selected = sources) 
  }
  
  # if the selectYears is the same we still want to trigger
  # the years observer
  
  if(identical(input$focus_data_source_compare, sources)){
    .trigger$focus_data_source_compare <- !.trigger$focus_data_source_compare
  }
  
  
  # ----- Changes in Explore
  
  # Need to make corresponding changes to the compare country
  
  if(!is.null(focus_country_explore)){
    updateSelectInput(session, 
                      'focus_country_explore', 
                      selected = focus_country_compare)
  }
  
  
  
  
  # # ----- Update the who region and income groups
  
  .trigger$focus_WHO_info <- !.trigger$focus_WHO_info
  
}, priority = 15)





# *******************************************************************
#****************** DATA SOURCE -----
# *******************************************************************

observeEvent(c(debounce_focus_data_source_explore(), .trigger$focus_data_source_explore, input$lang), {
  req(c(debounce_focus_data_source_explore(), .trigger$focus_data_source_explore))
  
  if(is.null(input$focus_data_source_explore) || trimws(input$focus_data_source_explore) == "") return()
  
  focus_data_source_explore <- input$focus_data_source_explore
  
  .rdata[['focus_data_source']] <<- focus_data_source_explore 
  
  
  
  
  if(!is.null(input$focus_data_source_compare ) & 
     !setequal(input$focus_data_source_compare, input$focus_data_source_explore)){
    updateSelectInput(session,'focus_data_source_compare', 
                      choices = .rdata[['data_sources']],
                      selected = .rdata[['focus_data_source']])
  }
  

  if(!is.null(input$focus_data_source_explore_dtlmap ) & 
     !setequal(input$focus_data_source_explore_dtlmap, input$focus_data_source_explore)){
    updateSelectInput(session,'focus_data_source_explore_dtlmap',
                      choices = .rdata[['data_sources']],
                      selected = .rdata[['focus_data_source']])
  }
  # ----- Grab the possible years associated with the 
  # ----- country and data source
  selectYears <- getFilteredYear(country=input$focus_country_explore,
                                 focus_data_source_explore)
  #browser()
  # git709
  if(length(selectYears) == 0){
    .trigger$runplot_disag_explore <- !.trigger$runplot_disag_explore
    .trigger$runtable_disag_explore <- !.trigger$runtable_disag_explore
    .trigger$runtable_summary_explore <- !.trigger$runtable_summary_explore
    .trigger$runplot_summary_explore <- !.trigger$runplot_summary_explore
    return()
  }
  
  .rdata[['all_years']]<<-as.character(selectYears)
  .rdata[['focus_year']]<<-as.character(selectYears)
  
  # ----- Update the years selector with the possible years
  if(!is.null(input$focus_year_explore)){
    updateSelectInput(session, 'focus_year_explore', 
                      choices = selectYears, selected = selectYears) 
  }
  
  # if the selectYears is the same we still want to trigger
  # the years observer
  
  if(identical(input$focus_year_explore, as.character(selectYears))){
    .trigger$focus_year_explore <- !.trigger$focus_year_explore
  }
  
  
})


observeEvent(c(debounce_focus_data_source_compare(), .trigger$focus_data_source_compare, input$lang), {
  req(c(debounce_focus_data_source_compare(), .trigger$focus_data_source_compare))
  
  if(is.null(input$focus_data_source_compare) || trimws(input$focus_data_source_compare) == "") return()
  
  focus_data_source_compare<- input$focus_data_source_compare
  
  .rdata[['focus_data_source']] <<- focus_data_source_compare 
  
  if(!is.null(input$focus_data_source_explore ) & 
     !setequal(input$focus_data_source_explore, input$focus_data_source_compare)){
    updateSelectInput(session,'focus_data_source_explore', 
                      choices = .rdata[['data_sources']],
                      selected = .rdata[['focus_data_source']])
  }
  
  if(!is.null(input$focus_data_source_explore_dtlmap ) & 
     !setequal(input$focus_data_source_explore_dtlmap, input$focus_data_source_compare)){
    updateSelectInput(session,'focus_data_source_explore_dtlmap',
                      choices = .rdata[['data_sources']],
                      selected = .rdata[['focus_data_source']])
  }
  
  # ----- Grab the possible years associated with the 
  # ----- country and data source
  selectYears <- getFilteredYear(country=input$focus_country_compare,
                                 focus_data_source_compare)
  
  
    # git709
  if(length(selectYears) == 0){
    .trigger$runplot_disag_compare <- !.trigger$runplot_disag_compare
    return()
  }
  
  .rdata[['all_years']]<<-as.character(selectYears)
  .rdata[['focus_year']]<<-as.character(selectYears)
  
  # ----- Update the years selector with the possible years
  if(!is.null(input$focus_year_compare)){
    updateSelectInput(session, 'focus_year_compare', 
                      choices = selectYears, 
                      selected = selectYears[1]) 
  }
  
  # if the selectYears is the same we still want to trigger
  # the years observer
  
  if(identical(input$focus_year_compare, as.character(selectYears[1]))){
    .trigger$focus_year_compare <- !.trigger$focus_year_compare
  }
  
  
  
  
}, priority = 14)




observeEvent(c(debounce_focus_data_source_explore_dtlmap(), .trigger$focus_data_source_explore_dtlmap, input$lang), {
  req(c(debounce_focus_data_source_explore_dtlmap(), .trigger$focus_data_source_explore_dtlmap))
  
  # print(paste(Sys.time(), "In focus_data_source_explore_dtlmap"))
  
  if(is.null(input$focus_data_source_explore_dtlmap) || trimws(input$focus_data_source_explore_dtlmap) == "") return()
  
  # if(.rdata[['first_time_dtl']]){
  #   .rdata[['first_time_dtl']] <<- FALSE
  #   return()
  # }
  
  focus_data_source_explore_dtlmap <- input$focus_data_source_explore_dtlmap
  
  .rdata[['focus_data_source']] <<- focus_data_source_explore_dtlmap
  
  
  if(!is.null(input$focus_data_source_compare) & 
     !setequal(input$focus_data_source_compare, input$focus_data_source_explore_dtlmap)){
    
    updateSelectInput(session,'focus_data_source_compare',
                      choices = .rdata[['data_sources']],
                      selected = .rdata[['focus_data_source']])
  }
  
  if(!is.null(input$focus_data_source_explore) & 
     !setequal(input$focus_data_source_explore, input$focus_data_source_explore_dtlmap)){
    updateSelectInput(session,'focus_data_source_explore', 
                      choices = .rdata[['data_sources']],
                      selected = .rdata[['focus_data_source']])
  }
  
  
  
  # ----- Grab the possible years associated with the 
  # ----- country and data source
  selectYears <- getFilteredYear(country=input$focus_country_explore,
                                 focus_data_source_explore_dtlmap)
  
      # git709
  if(length(selectYears) == 0){
    .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
    .trigger$runplot_disag_map <- !.trigger$runplot_disag_map
    return()
  }
  
  
  .rdata[['all_years']]<<-as.character(selectYears)
  .rdata[['focus_year']]<<-as.character(selectYears)
  
  

  if(!is.null(input$focus_year_explore_dtlmap)){
    
    updateSelectInput(session, 'focus_year_explore_dtlmap',
                      choices = selectYears,
                      selected = selectYears[1])
  }
  
  
  if(identical(input$focus_year_explore_dtlmap, as.character(selectYears[1]))){
    .trigger$focus_year_explore_dtlmap <- !.trigger$focus_year_explore_dtlmap
  }
  
})





# *******************************************************************
#****************** YEAR ----
# *******************************************************************

# ----- focus_year_explore

observeEvent(c(input$focus_year_explore, .trigger$focus_year_explore, input$lang), {
  req(c(input$focus_year_explore, .trigger$focus_year_explore))
  #browser()
  #print(paste("In Year", Sys.time()))
  #print(paste("YEAR PRE .rdata[['focus_indicator']]", paste(.rdata[['focus_indicator']], collapse = ",")))
  if(is.null(input$focus_year_explore) || trimws(input$focus_year_explore) == "") return()
  
  
  focus_year_explore <- input$focus_year_explore
  .rdata[['focus_year']] <<- focus_year_explore

  #print(paste("YEAR before reset .rdata[['focus_indicator']]", paste(.rdata[['focus_indicator']], collapse = ",")))
  reset_focus_indic_dim(yr = .rdata[['focus_year']])
  #print(paste("YEAR after reset .rdata[['focus_indicator']]", paste(.rdata[['focus_indicator']], collapse = ",")))
  
  update_indicator_boxes("focus_indicator_explore", from_indicator_react = FALSE)
  # ----- Change the indicator in explore
  # updateSelectInput(session, "focus_indicator_explore", 
  #                   choices = .rdata[['full_indicators']],
  #                   selected = .rdata[['focus_indicator']])
  
  # if the focus indicator is the same as what is there
  # already we still want to trigger the indicator
  #print(paste("YEAR focus_indicator_explore", paste(input$focus_indicator_explore, collapse = ",")))
  #print(paste("YEAR .rdata[['focus_indicator']]", paste(.rdata[['focus_indicator']], collapse = ",")))
  #if(all(input$focus_indicator_explore%in%.rdata[['focus_indicator']])){
    #browser()
    .trigger$focus_ind_explore <- !.trigger$focus_ind_explore
 # }
  
  
  # ----- Change the year in compare
  
  if(!is.null(input$focus_year_compare) && 
     !input$focus_year_compare %in%.rdata[['focus_year']]){
    
    updateSelectInput(session, 'focus_year_compare', 
                      choices = .rdata[['all_years']], 
                      selected = .rdata[['focus_year']][1])
  }
  
  
  # ----- Change the year in detailed bars
  
  if(!is.null(input$focus_year_explore_dtlmap) && 
     !input$focus_year_explore_dtlmap %in%.rdata[['focus_year']]){
    
    updateSelectInput(session, 'focus_year_explore_dtlmap',
                      choices = .rdata[['all_years']], 
                      selected = .rdata[['focus_year']][1])
  }
  
  
})


# ----- focus_year_compare

observeEvent(c(input$focus_year_compare, .trigger$focus_year_compare, input$lang), {
  req(c(input$focus_year_compare, .trigger$focus_year_compare))
  
  #print(paste("YEAR PRE COMPARE .rdata[['focus_indicator']]", paste(.rdata[['focus_indicator']], collapse = ",")))
  if(is.null(input$focus_year_compare) || trimws(input$focus_year_compare) == "") return()
  
  if(!input$focus_year_compare%in%.rdata[['focus_year']]){
    .rdata[['focus_year']] <<- input$focus_year_compare
  }
  
  # ---- Resets the indicator and dimension lists
  
  reset_focus_indic_dim(yr = .rdata[['focus_year']])
  
  # ----- Change the indicator in compare
  
  update_indicator_boxes("focus_indicator_compare", from_indicator_react = FALSE)
  # updateSelectInput(session, "focus_indicator_compare", 
  #                   choices = .rdata[['full_indicators']],
  #                   selected = .rdata[['focus_indicator']][1])
  
  
  # if the focus indicator is the same as what is there
  # already we still want to trigger the indicator
  
  if(input$focus_indicator_compare%in%unname(.rdata[['focus_indicator']][1])){
    .trigger$focus_ind_compare <- !.trigger$focus_ind_compare
  }
  
  # ----- Change the year in explore
  
  if(!is.null(input$focus_year_explore) && 
     !input$focus_year_compare%in%input$focus_year_explore){
    updateSelectInput(session, 'focus_year_explore', 
                      choices = .rdata[['all_years']], 
                      selected = .rdata[['focus_year']])
  }
  
  
  # ----- Change the year in detailed bars
  
  if(!is.null(input$focus_year_explore_dtlmap) && 
     input$focus_year_explore_dtlmap != input$focus_year_compare){
    updateSelectInput(session, 'focus_year_explore_dtlmap',
                      choices = .rdata[['all_years']], 
                      selected = input$focus_year_compare)
  }
  
  
  
  
}, priority = 13)


# ----- focus_year_explore_dtlmap

observeEvent(c(input$focus_year_explore_dtlmap, .trigger$focus_year_explore_dtlmap, input$lang), {
  req(c(input$focus_year_explore_dtlmap, .trigger$focus_year_explore_dtlmap))
  # print(paste(Sys.time(), "In focus_year_explore_dtlmap"))
  
  .trigger$runplot_disag_map <- !.trigger$runplot_disag_map
  
  if(is.null(input$focus_year_explore_dtlmap) || trimws(input$focus_year_explore_dtlmap) == "") return()
  
  
  if(!input$focus_year_explore_dtlmap%in%.rdata[['focus_year']]){
    .rdata[['focus_year']] <<- input$focus_year_explore_dtlmap
  }
  
  focus_year_explore_dtlmap <- input$focus_year_explore_dtlmap
  
  

  # ---- Resets the indicator and dimension lists
  

  reset_focus_indic_dim(yr = .rdata[['focus_year']])
  
  update_indicator_boxes("focus_indicator_explore_plotdtl", from_indicator_react = FALSE)
  
  # ----- Change the indicator in explore dtl
  bareindic <- unname(c(na.exclude(.rdata[['focus_indicator']][1:3])))
  # updateSelectInput(session, "focus_indicator_explore_plotdtl", 
  #                   choices = .rdata[['full_indicators']],
  #                   selected = bareindic)
  
  if(identical(input$focus_indicator_explore_plotdtl, bareindic)){
    .trigger$focus_ind_explore_plotdtl <- !.trigger$focus_ind_explore_plotdtl
  }
  
  
  # # ----- Change the indicator in explore dtl
  bareindic <- unname(c(na.exclude(.rdata[['focus_indicator']][1])))
  # updateSelectInput(session, "focus_indicator_explore_map", 
  #                   choices = .rdata[['full_indicators']],
  #                   selected = bareindic)
  
  if(identical(input$focus_indicator_explore_map, bareindic)){
    .trigger$focus_ind_explore_map <- !.trigger$focus_ind_explore_map
  }
  
  
  # ----- Change the year in compare
  
  if(!is.null(input$focus_year_compare) && 
     input$focus_year_compare!=input$focus_year_explore_dtlmap){
    updateSelectInput(session, 'focus_year_compare', 
                      choices = .rdata[['all_years']], 
                      selected = input$focus_year_explore_dtlmap)
  }
  
  
  # ----- Change the year in detailed bars
  
  if(!is.null(input$focus_year_explore) && 
     !input$focus_year_explore_dtlmap%in%input$focus_year_explore){
    updateSelectInput(session, 'focus_year_explore',
                      choices = .rdata[['all_years']], 
                      selected = .rdata[['focus_year']])
  }
  
  
})




# *******************************************************************
#****************** MOST RECENT ----
# *******************************************************************

observeEvent(input$mostrecent_explore, {
  

  .rdata[['mostrecent']] <<- input$mostrecent_explore
  
  # Change most recent compare to match
  mostrecent_compare <- input$mostrecent_compare
  
  if(!is.null(mostrecent_compare) && mostrecent_compare!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_compare', value=.rdata[['mostrecent']])
  }
  
  # Change most recent explore map to match
  mostrecent_explore_dtlmap <- input$mostrecent_explore_dtlmap
  
  if(!is.null(mostrecent_explore_dtlmap)){
    updateCheckboxInput(session,'mostrecent_explore_dtlmap', value=.rdata[['mostrecent']])
  }
  
  # ----- Force the cascade of updates
  .trigger$focus_year_explore <- !.trigger$focus_year_explore
  
  
})


observeEvent(input$mostrecent_compare, {
  
  .rdata[['mostrecent']] <<- input$mostrecent_compare
  
  # Change most recent explore to match
  mostrecent_explore <- input$mostrecent_explore
  
  if(!is.null(mostrecent_explore) && mostrecent_explore!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_explore', value=.rdata[['mostrecent']])
  }
  
  
  # Change most recent explore map to match
  mostrecent_explore_dtlmap <- input$mostrecent_explore_dtlmap
  
  if(!is.null(mostrecent_explore_dtlmap)){
    updateCheckboxInput(session,'mostrecent_explore_dtlmap', value=.rdata[['mostrecent']])
  }
  
  # ----- Force the cascade of updates
  .trigger$focus_year_compare <- !.trigger$focus_year_compare
  
  
})


observeEvent(input$mostrecent_explore_dtlmap, {
  
  # print(paste(Sys.time(), "mostrecent_explore_dtlmap"))
  
  .rdata[['mostrecent']] <<- input$mostrecent_explore_dtlmap
  
  # Change most recent explore to match
  mostrecent_explore <- input$mostrecent_explore
  
  if(!is.null(mostrecent_explore) && mostrecent_explore!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_explore', value=.rdata[['mostrecent']])
  }
  
  
  # Change most recent compare to match
  mostrecent_compare <- input$mostrecent_compare
  
  if(!is.null(mostrecent_compare) && mostrecent_compare!=.rdata[['mostrecent']]){
    updateCheckboxInput(session,'mostrecent_compare', value=.rdata[['mostrecent']])
    
  }
  
  # ----- Force the cascade of updates
  .trigger$focus_year_explore_dtlmap <- !.trigger$focus_year_explore_dtlmap
  
  
})


# *******************************************************************
#****************** INDICATORS ----
# *******************************************************************

observeEvent(c(input$focus_indicator_explore, .trigger$focus_ind_explore, input$lang), {
  req(c(input$focus_indicator_explore, .trigger$focus_ind_explore))
  
  #print(paste("In indicator", Sys.time()))
   
  if(is.null(input$focus_indicator_explore)) return()
  #.rdata[['focus_indicator']] <<- input$focus_indicator_explore
  update_global_indicator("focus_indicator_explore", input$focus_indicator_explore)
  #print(paste("INDIC focus_indicator_explore", paste(input$focus_indicator_explore, collapse = ",")))
  #print(paste("INDIC .rdata[['focus_indicator']]", paste(.rdata[['focus_indicator']], collapse = ",")))
  
  updateSelectInput(session, "focus_dimension_explore", 
                    choices = .rdata[['equity_dimensions']],
                    selected = .rdata[['focus_dimension']])
  
  
  if(all(input$focus_dimension_explore%in%.rdata[['focus_dimension']])){
    .trigger$focus_dim_explore <- !.trigger$focus_dim_explore
  }
  
  
  update_indicator_boxes("focus_indicator_explore")
  
  # Update the compare indicators, limited to 1
  
  # if(!is.null(input$focus_indicator_compare) && 
  #    !input$focus_indicator_compare%in%.rdata[['focus_indicator']]){
  #   updateSelectInput(session, 'focus_indicator_compare', 
  #                     choices = .rdata[['full_indicators']],
  #                     selected = .rdata[['focus_indicator']][1])
  # }
  # 
  # 
  # bareindic <- unname(c(na.exclude(.rdata[['focus_indicator']][1:3])))
  # if(!is.null(input$focus_indicator_explore_plotdtl) && 
  #    !identical(input$focus_indicator_explore_plotdtl, bareindic)){
  # 
  #   updateSelectInput(session, 'focus_indicator_explore_plotdtl',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = bareindic)
  # }
  # 
  # 
  # bareindic <- unname(c(na.exclude(.rdata[['focus_indicator']][1])))
  # if(!is.null(input$focus_indicator_explore_map) && !identical(input$focus_indicator_explore_map, bareindic)){
  #   
  #   updateSelectInput(session, 'focus_indicator_explore_map',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = bareindic)
  # }
  # 
})




# ----- Indicator plotdtl

observeEvent(c(input$focus_indicator_explore_plotdtl, .trigger$focus_ind_explore_plotdtl, input$lang), {
  req(c(input$focus_indicator_explore_plotdtl, .trigger$focus_ind_explore_plotdtl))

  if(is.null(input$focus_indicator_explore_plotdtl)) return()

  update_global_indicator("focus_indicator_explore_plotdtl", input$focus_indicator_explore_plotdtl)
  # 
  # if(!all(input$focus_indicator_explore_plotdtl%in%.rdata[['focus_indicator']])){
  #   .rdata[['focus_indicator']] <<- input$focus_indicator_explore_plotdtl
  # }



  if(!is.null(input$focus_dimension_explore_dtl) && 
     !input$focus_dimension_explore_dtl%in%.rdata[['focus_dimension']]){
    
  updateSelectInput(session, "focus_dimension_explore_dtl", 
                    choices = .rdata[['equity_dimensions']],
                    selected = .rdata[['focus_dimension']][1])
}
  
  
  if(!is.null(input$focus_dimension_explore_dtl) && 
     input$focus_dimension_explore_dtl%in%.rdata[['focus_dimension']]){
    .trigger$focus_dim_explore_plotdtl <- !.trigger$focus_dim_explore_plotdtl
  }
  
  update_indicator_boxes("focus_indicator_explore_plotdtl")
  # 
  # # Update the explore indicators
  # focus_indicator_explore <- input$focus_indicator_explore
  # 
  # # if(!is.null(focus_indicator_explore)  &&
  # #   !all(.rdata[['focus_indicator']]%in%focus_indicator_explore)){
  # 
  # if(!is.null(focus_indicator_explore)  &&
  #    !all(focus_indicator_explore%in%.rdata[['focus_indicator']])){
  #   updateSelectInput(session, 'focus_indicator_explore',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = .rdata[['focus_indicator']])
  # }
  # 
  # 
  # #Update the compare indicators, limited to 1
  # focus_indicator_compare <- input$focus_indicator_compare
  # 
  # if(!is.null(focus_indicator_compare)  && 
  #    !all(focus_indicator_compare%in%.rdata[['focus_indicator']])){
  #   updateSelectInput(session, 'focus_indicator_compare',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = .rdata[['focus_indicator']][1])
  # }
  
  
  
  
})



# ----- Indicator map

observeEvent(c(input$focus_indicator_explore_map, .trigger$focus_ind_explore_map, input$lang), {
  req(c(input$focus_indicator_explore_map, .trigger$focus_ind_explore_map))
  
  # print(paste(Sys.time(),"In focus_indicator_explore_map"))
  
  
  if(is.null(input$focus_indicator_explore_map)) return()
  
  # if(!identical(input$focus_indicator_explore_map, .rdata[['focus_indicator']])){
  #   .rdata[['focus_indicator']] <<- input$focus_indicator_explore_map
  # }
  # 

  update_global_indicator("focus_indicator_explore_map", input$focus_indicator_explore_map)
  
  # Update the explore indicators
  focus_indicator_explore <- input$focus_indicator_explore
  
  update_indicator_boxes("focus_indicator_explore_map")
  
  # if(!is.null(focus_indicator_explore)  &&
  #   !all(.rdata[['focus_indicator']]%in%focus_indicator_explore)){
  

  
  # if(!is.null(focus_indicator_explore)  &&
  #    !input$focus_indicator_explore_map%in%focus_indicator_explore){
  #   updateSelectInput(session, 'focus_indicator_explore',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = .rdata[['focus_indicator']])
  # }
  # 
  # 
  # #Update the compare indicators, limited to 1
  # focus_indicator_compare <- input$focus_indicator_compare
  # 
  # if(!is.null(focus_indicator_compare)  && 
  #    !input$focus_indicator_explore_map%in%focus_indicator_compare){
  #   updateSelectInput(session, 'focus_indicator_compare',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = .rdata[['focus_indicator']][1])
  # }
  # 
  # 
  # if(!is.null(input$focus_indicator_explore_plotdtl)&&
  #    !input$focus_indicator_explore_map%in%input$focus_indicator_explore_plotdtl){
  #   updateSelectInput(session, 'focus_indicator_explore_plotdtl',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = na.exclude(.rdata[['focus_indicator']][1:3]))
  # }
  
  
  .trigger$runplot_disag_map <- !.trigger$runplot_disag_map  
})



# ----- Indicator compare

observeEvent(c(input$focus_indicator_compare, .trigger$focus_ind_compare, input$lang), {
  req(c(input$focus_indicator_compare, .trigger$focus_ind_compare))
  
  if(is.null(input$focus_indicator_compare) || trimws(input$focus_indicator_compare) == "") return()
  
  # 
  # if(!input$focus_indicator_compare%in%.rdata[['focus_indicator']]){
  #   .rdata[['focus_indicator']] <<- input$focus_indicator_compare
  # }

  update_global_indicator("focus_indicator_compare", input$focus_indicator_compare)
  
  
  if(!is.null(input$focus_dimension_compare) && 
     !input$focus_dimension_compare%in%.rdata[['focus_dimension']]){
    updateSelectInput(session, "focus_dimension_compare", 
                      choices = .rdata[['equity_dimensions']],
                      selected = .rdata[['focus_dimension']][1])
  }
  
  if(!is.null(input$focus_dimension_compare) && 
     input$focus_dimension_compare%in%.rdata[['focus_dimension']]){
    .trigger$focus_dim_compare <- !.trigger$focus_dim_compare
  }
  
  
  update_indicator_boxes("focus_indicator_compare")
  
  # 
  # 
  # if(!is.null(input$focus_indicator_explore) &&
  #    !input$focus_indicator_compare%in%input$focus_indicator_explore){
  #   updateSelectInput(session, 'focus_indicator_explore', 
  #                     choices = .rdata[['full_indicators']],
  #                     selected = .rdata[['focus_indicator']])
  # }
  # 
  # 
  # 
  # if(!is.null(input$focus_indicator_explore_plotdtl)&&
  #    !input$focus_indicator_compare%in%input$foucs_indicator_explore_plotdtl){
  #   updateSelectInput(session, 'focus_indicator_explore_plotdtl',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = na.exclude(.rdata[['focus_indicator']][1:3]))
  # }
  # 
  # 
  # if(!is.null(input$focus_indicator_explore_map)&&
  #    !input$focus_indicator_compare%in%input$foucs_indicator_explore_map){
  #   updateSelectInput(session, 'focus_indicator_explore_plotdtl',
  #                     choices = .rdata[['full_indicators']],
  #                     selected = na.exclude(.rdata[['focus_indicator']][1:3]))
  # }
  
  
  
})


# *******************************************************************
#****************** DIMENSIONS ----
# *******************************************************************

# ----- Focus dimension explore

observeEvent(c(input$focus_dimension_explore, .trigger$focus_dim_explore, input$lang), {
  req(c(input$focus_dimension_explore, .trigger$focus_dim_explore))
  
  if(is.null(input$focus_dimension_explore) || trimws(input$focus_dimension_explore) == "" || 
     trimws(input$focus_country_explore) == "") return()
  if(is.null(input$focus_indicator_explore)) return()
  
  
  .trigger$runplot_disag_explore  <- !.trigger$runplot_disag_explore
  .trigger$runtable_disag_explore <- !.trigger$runtable_disag_explore
  
  .rdata[['focus_dimension']] <<- input$focus_dimension_explore
  
  # ----- Update the compare pieces
  if(!is.null(input$focus_dimension_compare) && 
     !input$focus_dimension_compare%in%input$focus_dimension_explore){
    updateSelectInput(session, 'focus_dimension_compare',
                      choices = .rdata[['equity_dimensions']],
                      selected = .rdata[['focus_dimension']][1])
  }
  
 
  # ----- Update the detailed bar pieces
  if(!is.null(input$focus_dimension_explore_dtl) && 
              !input$focus_dimension_explore_dtl%in%input$focus_dimension_explore){
    updateSelectInput(session, 'focus_dimension_explore_dtl',
                      choices = .rdata[['equity_dimensions']],
                      selected = .rdata[['focus_dimension']][1])
  }
  
  
  # Update the inequality measures
  reset_focus_inequal()

  lang <- isolate(input$lang)
  
  updateSelectInput(
    session = session, 
    inputId = "focus_inequal_type_explore_table", 
    choices = setNames(
      .rdata[["summary_measures_all"]],
      vapply(.rdata[["summary_measures_all"]], translate, character(1), lang)
    ), 
    selected = .rdata[["focus_inequal_type"]]
  )
  
  
  updateSelectInput(
    session = session, 
    inputId = "focus_inequal_type_explore_plot", 
    choices = setNames(
      .rdata[["summary_measures_all"]],
      vapply(.rdata[["summary_measures_all"]], translate, character(1), lang)
    ), 
    selected = .rdata[["focus_inequal_type"]][1]
  )
  
  if(identical(input$focus_inequal_type_explore_plot, unname(.rdata[["focus_inequal_type"]]))){
    .trigger$focus_ineq_explore_plot <- !.trigger$focus_ineq_explore_plot
  } 
  
  if(identical(input$focus_inequal_type_explore_table, unname(.rdata[["focus_inequal_type"]]))){
    .trigger$focus_ineq_explore_table <- !.trigger$focus_ineq_explore_table
  } 
  
  
  
})


# ----- Focus dimension compare


observeEvent(c(input$focus_dimension_compare, .trigger$focus_dim_compare, input$lang), {
  req(c(input$focus_dimension_compare, .trigger$focus_dim_compare))
  
  if(is.null(input$focus_dimension_compare) || input$focus_dimension_compare == "") return()
  
  if(!input$focus_dimension_compare%in%.rdata[['focus_dimension']]){
    .rdata[['focus_dimension']] <<- input$focus_dimension_compare
  }
  
  
  
  # ----- Update the explore pieces
  if(!is.null(input$focus_dimension_explore) && 
     !input$focus_dimension_compare%in%input$focus_dimension_explore){
    updateSelectInput(session, 'focus_dimension_explore',
                      choices = .rdata[['equity_dimensions']],
                      selected = .rdata[['focus_dimension']])
  }
  
  
  # ----- Update the detailed bar pieces
  if(!is.null(input$focus_dimension_explore_dtl) && 
     input$focus_year_explore_dtlmap != input$focus_year_compare){
    updateSelectInput(session, 'focus_dimension_explore_dtl',
                      choices = .rdata[['equity_dimensions']],
                      selected = na.exclude(.rdata[['focus_dimension']][1]))
  }

  
  # Update the inequality measures
  
  if(!is.null(input$focus_inequal_type_compare)){

    reset_focus_inequal()
    
    updateSelectInput(session, "focus_inequal_type_compare", 
                      choices= .rdata[['summary_measures_all']], 
                      selected=.rdata[["focus_inequal_type"]])
    
    
    if(identical(input$focus_inequal_type_compare, unname(.rdata[["focus_inequal_type"]]))){
      .trigger$focus_ineq_compare<- !.trigger$focus_ineq_compare
    }
    
  }
  
  .trigger$focus_benchmark_compare <- !.trigger$focus_benchmark_compare
 
  
})



# ----- Focus dimension plotdtl


observeEvent(c(input$focus_dimension_explore_dtl, .trigger$focus_dim_explore_plotdtl, input$lang), {
  req(c(input$focus_dimension_explore_dtl, .trigger$focus_dim_explore_plotdtl))
  
  if(is.null(input$focus_dimension_explore_dtl)) return()
  if(is.null(input$focus_indicator_explore_plotdtl)) return()
  
  if(!input$focus_dimension_explore_dtl%in%.rdata[['focus_dimension']]){
    .rdata[['focus_dimension']] <<- input$focus_dimension_explore_dtl
  }
  
  #.trigger$runplot_disag_detail  <- !.trigger$runplot_disag_detail
  focus_dimension_explore <- input$focus_dimension_explore
  
  if(!is.null(input$focus_dimension_explore) && 
     !input$focus_dimension_explore_dtl%in%input$focus_dimension_explore){
    updateSelectInput(session, 'focus_dimension_explore',
                      choices = .rdata[['equity_dimensions']],
                      selected = .rdata[['focus_dimension']])
  }
  
  focus_dimension_compare <- input$focus_dimension_compare
  
  
  if(!is.null(input$focus_dimension_compare) && 
     input$focus_year_explore_dtlmap != input$focus_year_compare){
    updateSelectInput(session, 'focus_dimension_compare',
                      choices = .rdata[['equity_dimensions']],
                      selected = na.exclude(.rdata[['focus_dimension']][1]))
  }
  
  
  # need the indicator's full name, not just abbreviation
  # then update select for sorting detailed bars
  
  indics <- getFullIndic(input$focus_indicator_explore_plotdtl,
                         first = input$disag_plot_explore_dtl_sort,
                         yr = input$focus_year_explore_dtlmap)
  #indics <- .rdata[['full_indicators']][.rdata[['full_indicators']]%in%input$focus_indicator_explore_plotdtl]

  # Git659
  if(length(indics)>0){
    
    .rdata[['focus_plotdtl_sort']] <<- indics
    updateSelectizeInput(session, "disag_plot_explore_dtl_sort",
                         choices = indics,
                         selected = unname(indics[1]))
  }else{
    
    updateSelectInput(session, "disag_plot_explore_dtl_sort",
                      choices = "No data",
                      selected = "No data")
  }
  
  if(identical(input$disag_plot_explore_dtl_sort, unname(indics[1]))){
    .trigger$focus_sort_map <- !.trigger$focus_sort_map
    
  }
  
  
})

# *******************************************************************
#****************** SORT Observer ----- 
# *******************************************************************

observeEvent(c(input$disag_plot_explore_dtl_sort, .trigger$focus_sort_map, input$lang), {
  req(
    c(input$disag_plot_explore_dtl_sort, .trigger$focus_sort_map),
    input$focus_dimension_explore_dtl,
    input$focus_indicator_explore_plotdtl
  )
      
  if(trimws(input$focus_country_explore) == "") return()

  #if(!any(class(.rdata[['maindata']])%in%"data.frame")) return()
  
  .rdata[['focus_plotdtl_subgroups']] <<- 
    getDetailedSubgroups(yearDT = input$focus_year_explore_dtlmap,
                         indicatorDT = input$focus_indicator_explore_plotdtl,
                         dimensionDT = input$focus_dimension_explore_dtl)
  
  # If there is no data for the indicator/dimention combination
  # return no data
  
  subg <- .rdata[['focus_plotdtl_subgroups']]
  if (length(.rdata[['focus_plotdtl_subgroups']])<=1) {
    subg <- translate("no_data_simple", isolate(input$lang))
  }
  
  if (length(subg) &&
      HEATversion == "whodata" && 
      isolate(input$focus_dimension_explore_dtl) != "Subnational region") {
    names(subg) <- map_chr(subg, translate, isolate(input$lang))
  }
  
  updateSelectInput(
    session, 
    inputId = "disag_plot_explore_dtl_subgroups",
    choices = c(
      set_names("", translate("choose", isolate(input$lang))),
      subg
    )
  )
  
  
  if(is.null(input$disag_plot_explore_dtl_subgroups)){
    .trigger$focus_dtlsub_map <- !.trigger$focus_dtlsub_map
    
  }                   
  
})

# addresses secondary issue raised in #669
# see https://github.com/WHOequity/who-heat/issues/669#issuecomment-285681389
observeEvent(input$focus_indicator_explore_plotdtl, ignoreNULL = FALSE, {
  if (is.null(input$focus_indicator_explore_plotdtl)) {
    updateSelectizeInput(
      session = session, 
      inputId = "disag_plot_explore_dtl_subgroups", 
      choices = ""
    )
  }
})


observeEvent(c(input$disag_plot_explore_dtl_subgroups,.trigger$focus_dtlsub_map), {
  
  #if(is.null(input$disag_plot_explore_dtl_subgroups)) return()
  if(is.null(input$disag_plot_explore_dtl_sort)) return()
  .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
})


observeEvent(c(input$disag_plot_explore_dtl_showAVG,input$disag_plot_explore_dtl_showMedian, input$disag_plot_explore_dtl_showNames), {
  
  if(is.null(input$disag_plot_explore_dtl_sort)) return()
  .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
})




observeEvent(input$disag_plot_explore_dtl_sortname, {
  
  if(is.null(input$disag_plot_explore_dtl_showAVG)) return()
  .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
  
})


observeEvent(input$sortBy_ind_dim, {
  
  .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
  
})

observeEvent(input$sortOrder_ind_dim, {
  
  .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
  
})



# *******************************************************************
#****************** BENCHMARK GROUP OR REGION -----
# *******************************************************************

observeEvent(c(.trigger$focus_WHO_info, input$lang), {
  
  
  if(is.null(input$focus_country_compare) & is.null(input$focus_country_explore)) return()
  
  if(is.null(input$focus_country_compare) & !is.null(input$focus_country_explore)){
    country_info<-getCountryWHOregionIncome(input$focus_country_explore)
    
    .rdata[['focus_income_group']] <<- country_info$income
    .rdata[['focus_who_regions']] <<- country_info$region
  } else{
    country_info<-getCountryWHOregionIncome(input$focus_country_compare)
    
    .rdata[['focus_income_group']] <<- country_info$income
    .rdata[['focus_who_regions']] <<- country_info$region
  }


  
  
  updateSelectInput(session, "benchmarkWHOregion", selected = country_info$region)
  updateSelectInput(session, "benchmarkWBgroup", selected = country_info$income)


  country_choices<-getFilteredCountries(country_info$income, country_info$region)
  country_choices <- country_choices[!country_choices%in%input$focus_country_compare]

  .rdata[['benchmark_countries']] <<- country_choices

  # commented out git908
  # move down to who region and group and then into the plot
  # income_group <-  input$benchmarkWBgroup
  # who_region <- input$benchmarkWHOregion
  # tmpCountries<-getFilteredCountries(income_group, input$who_region)
  # tmpCountries <- tmpCountries[!tmpCountries%in%.rdata[['focus_country']]]

  
  
  choices <- " "
  selected <- " "

  if(length(country_choices)!=0){

    #tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
    choices <- country_choices
    selected <- country_choices
  }

  updateSelectInput(session, "benchmark_countries", choices = choices, selected= selected)
  
  
  updateSliderInput(session, "benchmarkYears", value = 2)
  
  
})





observeEvent(c(input$benchmarkWBgroup, input$benchmarkWHOregion, input$lang), {

  # if(.rdata[['first_time_benchmark']]){
  #   .rdata[['first_time_benchmark']] <<- FALSE
  #   return()
  # }


  if(identical(input$benchmarkWBgroup, .rdata[['focus_income_group']]) & 
     identical(input$benchmarkWHOregion, .rdata[['focus_who_regions']])) return()
  
  income_group <-  input$benchmarkWBgroup
  who_region <- input$benchmarkWHOregion
  .rdata[['focus_income_group']] <<- income_group
  .rdata[['focus_who_regions']] <<- who_region

  tmpCountries<-getFilteredCountries(income_group, who_region)
  tmpCountries <- tmpCountries[!tmpCountries%in%.rdata[['focus_country']]]

  choices <- " "
  selected <- " "

  if(length(tmpCountries)!=0){

    tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
    choices <- unique(tmpCountries)
    selected <- .rdata[['benchmark_countries']]
  }

  updateSelectInput(session, "benchmark_countries", choices = choices, selected = selected)
  
  
})





# *******************************************************************
#****************** BENCHMARK COUNTRIES ----- 
# *******************************************************************
observeEvent(c(input$benchmark_countries, .trigger$focus_benchmark_compare), {
  
  
  if(.rdata[['first_time_benchmark_countries']]){
    .rdata[['first_time_benchmark_countries']] <<- FALSE
    return()
  }

  
  .rdata[['benchmark_countries']] <<- input$benchmark_countries
  
  # income_group <-  input$benchmarkWBgroup
  # who_region <- input$benchmarkWHOregion
  # .rdata[['focus_income_group']] <<- income_group
  # .rdata[['focus_who_regions']] <<- who_region
  
  # tmpCountries<-getFilteredCountries(income_group, who_region)
  # tmpCountries <- tmpCountries[!tmpCountries%in%.rdata[['focus_country']]]
  # 
  # choices <- " "
  # selected <- " "
  # 
  # if(length(tmpCountries)!=0){
  #   
  #   tmpCountries <- append(.rdata[['benchmark_countries']], tmpCountries)
  #   choices <- unique(tmpCountries)
  #   selected <- .rdata[['benchmark_countries']]
  # }
  # 
  # updateSelectInput(session, "benchmark_countries", choices = choices, selected= selected)
  
  
  .trigger$runplot_disag_compare <- !.trigger$runplot_disag_compare
  .trigger$runplot_summary_compare <- !.trigger$runplot_summary_compare
  
})




# *******************************************************************
#****************** INEQUAL TYPE -----
# *******************************************************************

observeEvent(c(input$focus_inequal_type_explore_table, input$lang), {
  req(input$focus_inequal_type_explore_table)

  .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_explore_table
  
  focus_inequal_type <- input$focus_inequal_type_compare
  
  if(!"None available"%in%.rdata[['summary_measures_all']]){
    .rdata[['focus_inequal_type_previous']] <<-.rdata[["focus_inequal_type"]]
  }
  

  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateSelectInput(session,'focus_inequal_type_compare', 
                        choices = .rdata[['summary_measures_all']],
                        selected =.rdata[['focus_inequal_type']][1])
  }
  
  focus_inequal_type2 <- input$focus_inequal_type_explore_plot
  
  if(!is.null(focus_inequal_type2) && 
     !focus_inequal_type2%in%.rdata[['focus_inequal_type']]){
    updateSelectInput(session,'focus_inequal_type_explore_plot', 
                        choices = .rdata[['summary_measures_all']],
                        selected=.rdata[['focus_inequal_type']][1])
  }
  
  
})




observeEvent(c(input$focus_inequal_type_explore_plot, .trigger$focus_ineq_explore_plot, input$lang), {
  req(c(input$focus_inequal_type_explore_plot, .trigger$focus_ineq_explore_plot))
  
  if(is.null(input$focus_inequal_type_explore_plot)) return()
  if(is.null(input$focus_indicator_explore)) return()
  

  
  if(!input$focus_inequal_type_explore_plot%in%.rdata[['focus_inequal_type']]){
    .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_explore_plot
  }

  # If summary measures all is NOT "none available" AND if the focus
  # inequal is NOT the previous inequal then update previous
  if(!"None available"%in%.rdata[['summary_measures_all']] && 
     !.rdata[["focus_inequal_type"]]%in%.rdata[['focus_inequal_type_previous']]){
    .rdata[['focus_inequal_type_previous']] <<-.rdata[["focus_inequal_type"]]
  }
  
  
  .trigger$runplot_summary_explore <- !.trigger$runplot_summary_explore  
  focus_inequal_type <- input$focus_inequal_type_compare
  
  
  if(!is.null(focus_inequal_type) && focus_inequal_type!=.rdata[['focus_inequal_type']]){
    updateSelectInput(session,'focus_inequal_type_compare', 
                        choices = .rdata[['summary_measures_all']],
                        selected=.rdata[['focus_inequal_type']][1])
  }
  
  focus_inequal_type2 <- input$focus_inequal_type_explore_table
  
  if(!is.null(focus_inequal_type2) && focus_inequal_type2!=.rdata[['focus_inequal_type']]){
    updateSelectInput(session,'focus_inequal_type_explore_table', 
                        choices = .rdata[['summary_measures_all']],
                        selected =.rdata[['focus_inequal_type']][1])
  }
  
  
})



observeEvent(c(input$focus_inequal_type_compare, .trigger$focus_ineq_compare, input$lang), {
  req(c(input$focus_inequal_type_compare, .trigger$focus_ineq_compare))
  
  if(is.null(input$focus_inequal_type_compare)) return()
  

  if(!input$focus_inequal_type_compare%in%.rdata[['focus_inequal_type']]){
    .rdata[['focus_inequal_type']] <<- input$focus_inequal_type_compare
  }
  
  # If summary measures all is NOT "none available" AND if the focus
  # inequal is NOT the previous inequal then update previous
  if(!"None available"%in%.rdata[['summary_measures_all']] && 
     !.rdata[["focus_inequal_type"]]%in%.rdata[['focus_inequal_type_previous']]){
    .rdata[['focus_inequal_type_previous']] <<-.rdata[["focus_inequal_type"]]
  }
  
  
  
  .trigger$runplot_summary_compare <- !.trigger$runplot_summary_compare 
  
  focus_inequal_type_plot <- input$focus_inequal_type_explore_plot
  focus_inequal_type_table <- input$focus_inequal_type_explore_table
  

  if(!is.null(focus_inequal_type_plot) && focus_inequal_type_plot!=input$focus_inequal_type_compare){
    updateSelectInput(session,'focus_inequal_type_explore_plot', 
                      choices = .rdata[['summary_measures_all']],
                        selected=.rdata[['focus_inequal_type']])
  }
  
  if(!is.null(focus_inequal_type_table) && !all(focus_inequal_type_table%in%.rdata[['focus_inequal_type']])){
    updateSelectInput(session,'focus_inequal_type_explore_table', 
                        choices = .rdata[['summary_measures_all']],
                        selected=.rdata[['focus_inequal_type']])
  }
  
  
})


# *******************************************************************
#****************** DATA TABLE ITEMS -----
# *******************************************************************





# *******************************************************************
#****************** TITLES -----
# *******************************************************************

observeEvent(input$main_title1, {

 if(!.rdata[["plotDisag_explore_title"]] %in% c("Health Equity Disaggregated", input$main_title1) ){
   .rdata[["plotDisag_explore_title"]] <<- input$main_title1
   .trigger$disag_plot_explore_title <- !.trigger$disag_plot_explore_title
 }

  
}, priority = 1)



observeEvent(input$main_title2, {
  
  
  if(!.rdata[["plotSummary_explore_title"]] %in% c( input$main_title2) ){
    .rdata[["plotSummary_explore_title"]] <<- input$main_title2
    .trigger$summary_plot_explore_title <- !.trigger$summary_plot_explore_title
  }
  
  
  

  
}, priority = 1)


observeEvent(input$main_title_dtl, {
  

  if(!.rdata[["plotDisag_explore_title_dtl"]] %in% input$main_title_dtl){
    .rdata[["plotDisag_explore_title_dtl"]] <<- input$main_title_dtl
    .trigger$disag_plot_explore_dtl_title <- !.trigger$disag_plot_explore_dtl_title
  }
  
  
  
  
  
}, priority = 1)



observeEvent(input$main_title_map, {
  
  
  if(!.rdata[["plotDisag_explore_title_map"]] %in% input$main_title_map){
    .rdata[["plotDisag_explore_title_map"]] <<- input$main_title_map
    .trigger$disag_plot_explore_map_title <- !.trigger$disag_plot_explore_map_title
  }
  

}, priority = 1)


# observeEvent(input$main_subtitle_map, {
#   
#   
#   if(!.rdata[["plotDisag_explore_title_map"]] %in% input$main_subtitle_map){
#     .rdata[["plotDisag_explore_subtitle_map"]] <<- input$main_subtitle_map
#     .trigger$disag_plot_explore_map_subtitle <- !.trigger$disag_plot_explore_map_subtitle
#   }
#   
#   
# }, priority = 1)



observeEvent(input$main_title3, {
  
  
  if(!.rdata[["plotDisag_compare_title"]] %in% c( input$main_title3) ){
    .rdata[["plotDisag_compare_title"]] <<- input$main_title3
    .trigger$disag_plot_compare_title <- !.trigger$disag_plot_compare_title
  }
  
  
  
  
}, priority = 1)


observeEvent(input$main_title4, {
  
  if(!.rdata[["plotSummary_compare_title"]] %in% c( input$main_title4) ){
    .rdata[["plotSummary_compare_title"]] <<- input$main_title4
    .trigger$summary_plot_compare_title <- !.trigger$summary_plot_compare_title
  }
  

  
}, priority = 1)



# ****************************************************
# Remove landing page warning ----
# ****************************************************

observe({ 
  req(input$neworexisting, input$lang)
  
  removeUI(".datawarning", immediate = TRUE)
  
  updateActionButton(
    session = session, 
    inputId = "doUploadBtn", 
    label = if (input$neworexisting == "newdata") {
      translate("heat_plus_landing_db_upload", input$lang)
    } else {
      translate("heat_plus_landing_db_open", input$lang)
    }
  )
})

# ****************************************************
# Table disag explore ----
# ****************************************************

observeEvent(input$sumsigfig, {
  .trigger$runtable_disag_explore <- !.trigger$runtable_disag_explore
})

observeEvent(input$dataTableItems ,{
  
  .rdata[['focus_table_variables']] <<- input$dataTableItems
  
  .trigger$runtable_disag_explore <- !.trigger$runtable_disag_explore
})

#****************************************************
#****************** Table summary explore ----
#****************************************************

observeEvent(input$sumsigfig2, {
  .trigger$runtable_summary_explore <- !.trigger$runtable_summary_explore
})



observeEvent(input$dataTableItemsSummary ,{
  
  .trigger$runtable_summary_explore<- !.trigger$runtable_summary_explore
})


observeEvent(input$focus_inequal_type_explore_table  ,{

  .trigger$runtable_summary_explore<- !.trigger$runtable_summary_explore
})



observeEvent(c(input$focus_inequal_type_explore_table, .trigger$focus_ineq_explore_table)  ,{
  
  .trigger$runtable_summary_explore<- !.trigger$runtable_summary_explore
})


 
observeEvent(input$benchmarkYears  ,{
  
  .trigger$runplot_disag_compare <- !.trigger$runplot_disag_compare
  .trigger$runplot_summary_compare <- !.trigger$runplot_summary_compare
})


observeEvent(c(input$disag_plot_mode_explore_dtl,input$plot_height_dtl, input$plot_width_dtl), {

  .trigger$runplot_disag_detail <- !.trigger$runplot_disag_detail
})


# misc ----
observeEvent(input$lang, {
  updateSelectInput(session, "selectfolders", label = translate("heat_plus_landing_db_select", input$lang))
})
