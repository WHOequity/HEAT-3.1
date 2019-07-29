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

# These are the widgets that get added to the landing page.
# They are added via an htmlTemplate (landing_page_upload.html) that
# is reference in ui.R

# If user chooses savesavenew then we need all of these widgets
heat_plus_landing_file_select <- tagList(
  tags$p(
    tags$strong(
      translationOutput("heat_plus_landing_db_select"),
      HTML("&nbsp;"),
      htmlOutput("heat_plus_landing_db_tooltip_format", container = span)
    )
  ),
  tags$div(
    class = "form-group shiny-input-container",
    tags$div(
      class = "input-group",
      tags$label(
        class = "input-group-btn",
        tags$span(
          class = "btn btn-default btn-file",
          translationOutput("heat_plus_landing_db_browse"),
          tags$form(
            tags$input(
              id =  "filename",
              name = "filename",
              type = "file",
              style = "display: none;"
            )
          )
        )
      ),
      tags$input(
        id = "filenametxt",
        type = "text",
        class = "form-control",
        placeholder = "", 
        readonly = NA
      )
    ),
    tags$div(
      id = "filename_progress",
      class = "progress active shiny-file-input-progress",
      translationOutput("heat_plus_landing_db_checking", class = "processing"),
      translationOutput("heat_plus_landing_db_ready", class = "ready"),
      tags$div(
        class = "progress-bar"
      )
    )
  ),
  translationOutput("heat_plus_landing_db_file_links", parent = p, html = TRUE, class = "italic"),
  tags$br(),
  tags$br()
)

heat_plus_landing_save_as <- tagList(
  textInput(
    inputId = "datafoldername", 
    label = tags$p(
      tags$strong(
        translationOutput("heat_plus_landing_db_save"),
        HTML("&nbsp;"),
        htmlOutput("heat_plus_landing_db_tooltip_name", container = span)
        # tags$span(
        #   id = "heat_plus_landing_db_tooltip_name",
        #   class = "shiny-html-output",
        #   tags$span(
        #     class = "glyphicon glyphicon-info-sign tooltip-upload",
        #     `data-toggle` = "tooltip",
        #     `data-original-title` = "" # translate("heat_plus_landing_db_tooltip_name", "english")
        #   )
        # )
      )
    ),
    format(Sys.Date(), format = "data%Y%m%d")
  )
)

# If user chooses previously calculated then we need these widgets
heat_plus_upload_new_db <- conditionalPanel(
  condition = "input.neworexisting == 'newdata'", 
  heat_plus_landing_file_select,
  heat_plus_landing_save_as
)

heat_plus_select_existing_db <- conditionalPanel(
  condition = "input.neworexisting == 'existdata'",
  selectInput(
    inputId = "selectfolders",
    label = translate("heat_plus_landing_db_select", "english"),
    choices = get_data_folders()
  )
)
