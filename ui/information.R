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

output$information_ui <- renderUI({
  
  
  fluidPage(
    fluidRow(
      column(2),
      column(8,       tabsetPanel(id = "info_panels",

                                  tabPanel(h6("User manual"), value='gloss_panel', includeHTML("www/manual.html")),
                                  tabPanel(h6("Technical notes"), value='gloss_panel', includeHTML("www/technical.html")),
                                  tabPanel(h6("Indicator compendium"), value='gloss_panel', includeHTML("www/compendium.html")),
                                  tabPanel(h6("Software"), value='gloss_panel', includeHTML("www/software.html")),  
                                  tabPanel(h6("License"), value='gloss_panel', includeHTML("www/license.html")),
                                  tabPanel(h6("Acknowledgements"), value='gloss_panel', includeHTML("www/acknowledgement.html"))
                                  
                                 
                                  
                                  
                                  
                                  #tabPanel(h6("Administration"), value='admin_panel',
                                  #         conditionalPanel(condition='input.admin_show==true', source('ui/administration-panel.R')$value),
                                  #         conditionalPanel(condition='input.admin_show==false', includeHTML("./www/adminFail.html"))
                                  #)
      )),
      column(2)
      
  
    ))
  
#   
#   sidebarLayout(
#     sidebarPanel(
#       
#       helpText("This application uses the"),
#       tags$div(class="sectionhead1", "Health Equity Monitor Database")
#       
#     ),# end sidebarpanel
#     
#     
#     
#     
#     mainPanel(
# 
#       
#       
#     )# end mainPanel
#   )# end sidebarLayout
#   
  
  
  
})