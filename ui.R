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

#devtools::install_github('jbkunst/highcharter')




source("ui/ui_widgets_landing_page.R", local=TRUE)

# Originally this was broken into clean separate files but I found
# that loading these took longer than having them all in this one
# super-long UI
shinyUI(
  
  tagList(
    tags$script(HTML('function resetFormElement(e) {
                     e.wrap("<form>").closest("form").get(0).reset();
                     e.unwrap();
                     
                     // Prevent form submission
                     //e.stopPropagation();
                     //e.preventDefault();
                     }')),
    tags$script('
      Shiny.addCustomMessageHandler("resetFileInputHandler", function(msg) {   
        //$("#filename").parent()[0].reset();
        $("#filenametxt").val("");
        var prog = $("#filename_progress");
        prog.removeClass("complete");
        prog.css("visibility", "hidden");
        prog.find(".progress-bar").width("0%");
        $("#doUploadBtn").prop("disabled", true);
      });
      Shiny.addCustomMessageHandler("enableUploadButton", function(msg) {
        $("#filename_progress").addClass("complete");
        $("#doUploadBtn").prop("disabled", false);
      });
      Shiny.addCustomMessageHandler("toggleUploadButton", function(msg) {
        if (msg.shown === "newdata") {
          $("#doUploadBtn").prop("disabled", $("#filenametxt").val() === "");
        } else {
          $("#doUploadBtn").prop("disabled", $("#selectfolders").val() === "none");
        }
      })
    '),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="spacelab.min.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="style.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css", href =  paste0("style_", HEATversion, ".css"))), #.rdata[['extra_css']])),
    tags$head(tags$link(rel="stylesheet", type="text/css",href= 'style_hc.css')),
    tags$head(tags$script(src = "script.js")),
    #tags$head(tags$script(src = "indonesia_subnational_boundaries01.js")),
    #tags$head(tags$script(src = "mapdata.js")),
    
    tags$head(tags$title("Health Equity Assessment Toolkit")),
    
    # UNCOMMENT HERE TO RESTORE MODAL
    tags$head(
      HTML(
        '<div id="myModal" class="modal fade">
        <div class="modal-dialog">
        <div class="modal-content">
        <div class="modal-header">
        <h4 class="modal-title">Terms of use and software license agreement</h4>
        </div>
        <div class="modal-body" >
        <div id="modal-license">
        
        </div>
        <button  class="btn btn-primary" data-dismiss="modal">I accept</button>
        </div>
        </div>
        </div>
        </div>'
      )),
    
    # The JS below is designed to activate the modal and
    # to change the navbar if the upload version is being
    # used (remove logo and and add "Plus")
    
    tags$script(
      "$(document).ready(function() {",
      "  $('#myModal').modal('show');",
      "  $('#modal-license').load('license_agreement.html');",
      "  if ($('#backimg-upload').length) {",
      "    $('.whoimg').remove();",
      "  }",
      "});"
    ),
    
    tags$script(
      "$(document).on('shiny:connected', function(e) {",
      "$('ul.dropdown-menu').addClass('dropdown-menu-right');",
      "});"
    ),
    
    tags$script(HTML(paste0(
      collapse = "\n",
      "function isChrome() {",
      " return window.chrome !== null &&",
      "  typeof window.chrome !== 'undefined' &&",
      "  window.navigator.vendor === 'Google Inc.' &&",
      "  (typeof window.opr !== 'undefined') === false &&",
      "  (window.navigator.userAgent.indexOf('Edge') > -1) === false;",
      "}"
    ))),
    
    tags$script(HTML(paste0(
      collapse = "\n",
      "$(function() {",
      "Highcharts.wrap(Highcharts.Axis.prototype, 'getPlotLinePath', function(proceed) {",
      "  var path = proceed.apply(this, Array.prototype.slice.call(arguments, 1));",
      "  if (path) {",
      "    path.flat = false;",
      "  }",
      "  return path;",
      "});",
      "});"
    ))),
    
    if (HEATversion == "whodata") {
      tags$script(HTML("
        $(function() {
          $(`#who_heat a[data-value='explore']`).one('shown.bs.tab', function(e) {
            setTimeout(function() {
              if (!$('#disag_plot_explore_insideHC').children().length) {
                $(`#who_heat a[data-value='home']`).tab('show');
                $(`#who_heat a[data-value='explore']`).tab('show');
                
                var $disagExplore = $('#disag_plot_explore');
                var swapTabs = function() {
                  //console.log('[ SWAP TABS ]');
                  $(`a[data-value='dataplot_dtl']`).tab('show');
                  $(`a[data-value='dataplot']`).tab('show');
                  setTimeout(function() {
                    if (!$disagExplore.children('#disag_plot_explore_insideHC').children().length) {
                      swapTabs();
                    }                  
                  }, 1200);
                };
                
                swapTabs();
              }
            }, 2000);
          });
        })
      "))
      },
    
    # tags$script(HTML(paste(
    #   "$(document).on('shiny:recalculating', function(e) {",
    #   # "  console.log(e.target);",
    #   "  if (e.target.id && e.target.id === 'disage_plot_explore_dtl_insideHC') {",
    #   "    document.documentElement.classList.add('shiny-busy');",
    #   "    $(e.target).one('shiny:recalculated', function(e) { document.documentElement.classList.remove('shiny-busy') });",
    #   "  }",
    #   # "  $('#disag_plot_explore_dtl_insideHC').on('shiny:recalculating', e => console.log('hello, world!'))",
    #   "});",
    #   collapse = "\n"
    # ))),
    
    tags$script(
      HTML(
        paste(
          "$(document).ready(function() {",
          "$('.navbar-nav').append(",
          "$('<form class=\"navbar-form navbar-right\">",
          gsub(
            "\n", "",
            selectInput(
              inputId = "lang", 
              label = NULL, 
              choices = c(
                "EN" = "english",
                "FR" = "french",
                "PT" = "portuguese",
                "ES" = "spanish"
              ),
              selectize = FALSE, 
              width = "auto"
            )
          ),
          "</form>')",
          ");",
          "});"
        )
      )
    ),
    
    navbarPage(
      title = tags$span(
        class = "navtitle",
        tags$a(rel = "home", href = "#", title = "World Health Organization"),
        tags$img(class = "whoimg", src = "who_logo_white40px.png"),
        if (HEATversion == "whodata") {
          translationOutput("navbar_whodata_title", class = "navtext")
        } else {
          translationOutput("navbar_plus_title", class = "navtext")
        }
      ),
      id = "who_heat", 
      inverse=TRUE, 
      collapsible = TRUE,
      
      # landing page tab ----
      tabPanel(
        title = translationOutput("navbar_button_home"),
        value = "home",
        htmlTemplate(
          filename = paste0("www/landing_page_", HEATversion, ".html"), # .rdata[['landing_page']],
          heat_plus_upload_new_db = heat_plus_upload_new_db,
          heat_plus_select_existing_db = heat_plus_select_existing_db,
          busyindicator = busyIndicator()
        )
      ),

      # explore tab ----      
      tabPanel(
        title = translationOutput("navbar_button_explore"), 
        value = "explore",
        
        sidebarLayout(
          # ~ control panel ----
          sidebarPanel(
            translationOutput("control_panel_explore", class = "sectionhead1", parent = div),
            uiOutput("focus_country_explore_component"), 
            
            conditionalPanel(
              condition = "!(input.assessment_panel == 'datamap' || input.assessment_panel == 'dataplot_dtl')",
              uiOutput('focus_source_year_explore'),
              uiOutput("focus_indicator_explore_component"),
              uiOutput("focus_dimension_explore_component")
            ),
            conditionalPanel(
              condition = "(input.assessment_panel == 'datamap')",
              uiOutput('focus_source_explore_map')
            ),
            conditionalPanel(
              condition = "(input.assessment_panel == 'dataplot_dtl')",
              uiOutput('focus_source_explore_dtl')
            ),
            conditionalPanel(
              condition = "(input.assessment_panel == 'datamap' || input.assessment_panel == 'dataplot_dtl')",
              uiOutput('focus_source_year_explore_dtlmap')
            ),
            
            conditionalPanel(
              condition = "input.assessment_panel == 'datamap'",
              uiOutput("focus_indicator_explore_map"),
              uiOutput("ui_collapse_explore_disag_map")
            ),
            
            # In the detailed graph tab the rules are similar to the map
            # but for indicator we are allowing 3 rather than 1
            conditionalPanel(
              condition = "input.assessment_panel == 'dataplot_dtl'",
              uiOutput("focus_indicator_explore_plotdtl_component"),
              uiOutput("focus_dimension_explore_dtl")
            ),
            
            conditionalPanel(
              condition = "(input.assessment_panel == 'datamap')",
              uiOutput("focus_dimension_explore_map")
            ),
            
            conditionalPanel(
              condition = "input.assessment_panel == 'dataplot_dtl'",
              #uiOutput("disag_plot_mode_explore_dtl"),
              uiOutput("disag_plot_explore_dtl_btn")
            ),
            
            conditionalPanel(
              condition = "input.assessment_panel == 'dataplot'",  
              uiOutput("disag_plot_type_explore_component"),
              #uiOutput("disag_plot_mode_explore"),
              uiOutput("ui_collapse_explore_disag_plot")
            ),
            
            conditionalPanel(
              condition = "input.assessment_panel == 'datatable'",
              uiOutput("ui_collapse_explore_disag_data")       
            ),
            
            conditionalPanel(
              condition = "input.assessment_panel == 'sumplot'",
              uiOutput("focus_summeasure_explore_summary_plot"),
              uiOutput("summary_plot_type_explore_component"),
              # uiOutput("summary_plot_mode_explore"),
              uiOutput("ui_collapse_explore_sum_plot")
              
            ),
            
            conditionalPanel(
              condition = "input.assessment_panel == 'sumtable'",
              uiOutput("focus_summeasure_explore_summary_table"),
              uiOutput("ui_collapse_explore_sum_data")
              
            )
            
          ), # end sidebarpanel
          
          mainPanel(
            # ~ zoom & download modals ----
            tagAppendAttributes(
              class = "modal-zoom-chart", {
                z_modal <- bsModal(
                  id = "hc_model_explore", 
                  title = "", 
                  trigger = NULL, 
                  size = "large",
                  highchartOutput("zoomhc_explore")
                )
                
                z_modal$
                  children[[1]]$
                  children[[1]]$
                  children[[3]]$
                  children[[1]]$
                  children[[1]] <- translationOutput("close_zoom_modal1", "download_close_label")
                
                z_modal
              }
            ),
            dataDownloadModal(
              modalId = "datatableModal_explore",
              triggerId = "btnDownloadDisagData_explore",
              radioId = "filetype1",
              downloadId = "btnStartDownloadDisagData_explore",
              n = 1
            ),
            chartDownloadModal(
              modalId = "dataplotModal_explore",
              triggerId = "btnDownloadDisagPlot_explore",
              selectId = "disagPlotType_explore",
              downloadId = "btnStartDownloadDisagPlot_explore",
              n = 1
            ),
            chartDownloadModal(
              modalId = "dataplotModal_explore_dtl",
              triggerId = "btnDownloadDisagPlot_explore",
              selectId = "disagPlotType_explore_dtl",
              downloadId = "btnStartDownloadDisagPlot_explore_dtl",
              n = 2
            ),
            dataDownloadModal(
              modalId = "compdataDisagModal_explore",
              triggerId = "btnDownloadDisagPlotData_explore",
              radioId = "filetype_explore_disag",
              downloadId = "btnStartDownloadDisagPlotData_explore",
              n = 2
            ),
            dataDownloadModal(
              modalId = "compdataDisagModal_explore_dtl",
              triggerId = "btnDownloadDisagPlotData_explore_dtl",
              radioId = "filetype_explore_disag_dtl",
              downloadId = "btnStartDownloadDisagPlotData_explore_dtl",
              n = 3
            ),
            dataDownloadModal(
              modalId = "compdataDisagModal_explore_map",
              triggerId = "btnDownloadDisagPlotData_explore_map",
              radioId = "filetype_explore_disag_map",
              downloadId = "btnStartDownloadDisagPlotData_explore_map",
              n = 4
            ),
            dataDownloadModal(
              modalId = "summtableModal_explore",
              triggerId = "btnDownloadSummaryData_explore",
              radioId = "filetype2",
              downloadId = "btnStartDownloadSummaryData_explore",
              n = 5
            ),
            chartDownloadModal(
              modalId = "summplotModal_explore",
              triggerId = "btnDownloadSummaryPlot_explore",
              selectId = "summaryPlotType_explore",
              downloadId = "btnStartDownloadSummaryPlot_explore",
              n = 3
            ),
            dataDownloadModal(
              modalId = "compdataSummaryModal_explore",
              triggerId = "btnDownloadSummaryPlotData_explore",
              radioId = "filetype_explore_summary",
              downloadId = "btnStartDownloadSummaryPlotData_explore",
              n = 6
            ),
            # ~ create tabs ----
            justifiedTabsetPanel(
              id = "assessment_panel",
              tabPanel(
                translationOutput(
                  id = "disaggraphs1", 
                  key = "disaggraphs",
                  parent = h6, 
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), 
                value = "dataplot",
                busyIndicator(),
                uiOutput("btnDownloadDisagPlotData_explore_component"),
                htmlOutput("disag_plot_explore")
              ), 
              tabPanel(
                translationOutput(
                  id = "disagdetail",
                  parent = h6, 
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Disaggregated data</br>(detailed bar graphs)<h6>"), 
                value = "dataplot_dtl",
                busyIndicator(),
                uiOutput("btnDownloadDisagPlotData_explore_dtl"),
                htmlOutput("disag_plot_explore_dtl")
              ), 
              if (HEATversion == "whodata") {
                tabPanel(
                  translationOutput(
                    id = "disagmaps",
                    parent = h6, 
                    html = TRUE,
                    style = css(text_align = "center")
                  ),
                  value = "datamap",
                  uiOutput("btnDownloadDisagPlotData_explore_map"),
                  uiOutput("disag_plot_explore_map")
                )
              },
              tabPanel(
                translationOutput(
                  id = "disagtables", 
                  parent = h6,
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Disaggregated data</br>(tables)<h6>"), 
                value = "datatable", 
                busyIndicator(),
                uiOutput("btnDownloadDisagData_explore"),
                uiOutput("dataTable")
              ), 
              tabPanel(
                translationOutput(
                  id = "summarygraphs1", 
                  key = "summarygraphs", 
                  parent = h6, 
                  html = TRUE, 
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), 
                value = "sumplot",
                busyIndicator(),
                uiOutput("btnDownloadSummaryPlotData_explore"),
                htmlOutput("summary_plot_explore")
              ),
              tabPanel(
                translationOutput(
                  id = "summarytables",
                  parent = h6,
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Summary measures</br>(tables)<h6>"), 
                value = "sumtable", 
                busyIndicator(),
                uiOutput("btnDownloadSummaryData_explore"),
                uiOutput(outputId = "dataTableInequal")
              )
            ) # end justified tabsetPanel
            
          ) # end mainPanel
        )
      ),
      # compare tab ----
      tabPanel(
        title = translationOutput("navbar_button_compare"),
        value = "compare",
        
        # ~ control panel ----
        sidebarLayout(
          sidebarPanel(
            translationOutput("control_panel_compare", class = "sectionhead1", parent = div),
            # tags$div(class="sectionhead1", "Compare inequality"),
            uiOutput("focus_country_compare"),
            uiOutput('focus_source_year_compare'),
            #uiOutput("focus_year_compare"),
            uiOutput("focus_indicator_compare"),
            uiOutput("focus_dimension_compare"),
            
            conditionalPanel(
              condition = "input.comparison_panel == 'inequalsum'",
              uiOutput("focus_summeasure_compare_summary")
            ),
            # tags$div(class="sectionhead", "Benchmark options"),
            translationOutput("benchmark", class = "sectionhead"),
            uiOutput("benchmarkWBgroup"),
            uiOutput("benchmarkWHOregion"),
            uiOutput("benchmark_countries"),
            uiOutput("benchmarkYears"),
            
            conditionalPanel(
              condition = "input.comparison_panel == 'inequaldisag'",
              #uiOutput("disag_plot_mode_compare"),
              uiOutput("ui_collapse_compare_disag_plot")
            ),
            
            conditionalPanel(
              condition = "input.comparison_panel == 'inequalsum'",
              #uiOutput("summary_plot_mode_compare"),
              uiOutput("disag_plot_type_compare"),
              uiOutput("ui_collapse_compare_sum_plot")       
            )
          ), # end sidebarpanel
          
          
          mainPanel(
            tagAppendAttributes(
              class = "modal-zoom-chart", {
                zc_modal <- bsModal(
                  id = "hc_model_compare", 
                  title = "", 
                  trigger = NULL, 
                  size = "large",
                  highchartOutput("zoomhc_compare")
                )
                
                zc_modal$
                  children[[1]]$
                  children[[1]]$
                  children[[3]]$
                  children[[1]]$
                  children[[1]] <- translationOutput("close_zoom_modal2", "download_close_label")
                
                zc_modal
              }
            ),
            dataDownloadModal(
              modalId = "compdataModal_compare", 
              triggerId = "btnDownloadDisagData_compare",
              radioId = "filetype_benchmark",
              downloadId = "btnStartDownloadDisagData_compare",
              n = 7
            ),
            chartDownloadModal(
              modalId = "compplot1Modal_compare",
              triggerId = "btnDownloadDisagPlot_compare",
              selectId = "disagPlotType_compare", 
              downloadId = "btnStartDownloadDisagPlot_compare",
              n = 4
            ),
            dataDownloadModal(
              modalId = "compdataDisagModal_compare",
              triggerId = "btnDownloadDisagPlotData_compare",
              radioId = "filetype_benchmark_disag",
              downloadId = "btnStartDownloadDisagPlotData_compare",
              n = 8
            ),
            chartDownloadModal(
              modalId = "compplot2Modal_compare",
              triggerId = "btnDownloadSummaryPlot_compare",
              selectId = "summaryPlotType_compare",
              downloadId = "btnStartDownloadSummaryPlot_compare",
              n = 5
            ),
            dataDownloadModal(
              modalId = "compdataSummaryModal_compare",
              triggerId = "btnDownloadSummaryPlotData_compare",
              radioId = "filetype_benchmark_summary",
              downloadId = "btnStartDownloadSummaryPlotData_compare",
              n = 9
            ),
            tabsetPanel(
              id = "comparison_panel", 
              tabPanel(
                translationOutput(
                  id = "disaggraphs2",
                  key = "disaggraphs", 
                  parent = h6,
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), 
                value = "inequaldisag", 
                busyIndicator(),
                uiOutput("btnDownloadDisagPlotData_compare"),
                htmlOutput("disag_plot_compare")
              ),
              tabPanel(
                translationOutput(
                  id = "summarygraphs2",
                  key = "summarygraphs", 
                  parent = h6, 
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), 
                value = "inequalsum", 
                busyIndicator(),
                uiOutput("btnDownloadSummaryPlotData_compare"),
                conditionalPanel(
                  condition = "input.summary_plot_mode_compare == 'ggplot'",
                  checkboxInput(
                    inputId = "points_ccode", 
                    "Show setting codes", 
                    value = FALSE
                  )
                ),
                # uiOutput('btnDownloadSummaryPlotData_compare'),
                # uiOutput('btnDownloadSummaryPlot_compare'),
                uiOutput("summary_plot_compare")
                # div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('summary_plot_compare'))
              )
            )#endtabsetpanel
            
            
            )# end mainPanel
            )# end sidebarLayout
      ),
      
      navbarMenu_drop(
        title = translationOutput("navbar_button_about", parent = span), 
        drop = if (HEATversion == "upload") 3 else -1,
        
        tabPanel(
          title = translationOutput("navbar_dropdown_manual", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/manual.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_notes", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/technical.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_compendium", parent = h6),
          value = 'gloss_panel',
          includeHTML("www/compendium.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_software", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/software.html")
        ),  
        tabPanel(
          title = translationOutput("navbar_dropdown_versions", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/versions.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_license", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/license.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_feedback", parent = h6), 
          value = 'gloss_panel', 
          includeHTML("www/feedback.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_acknowledgements", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/acknowledgement.html")
        )
      )
      ) # end navbarpage
)  # End shinyUi
)
