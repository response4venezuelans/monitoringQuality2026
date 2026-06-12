library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(waiter)
library(readxl)
library(writexl)
library(purrr)
ui <- page_navbar(
  title = "Quality Control of R4V Monitoring Data - ActivityInfo 5W 2026",
  selected = "About",
  navbar_options = navbar_options(collapsible = TRUE),
  theme = R4Vtheme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "shortcut icon", href = "img/logo.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "img/r4vLogo.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
  ),
  
  # Home panel
  nav_panel(
    title = "About",
    tags$div(
      style = "text-align: center;",
      tags$img(src = "img/r4vLogo.png", height = "100px"),
      tags$h1("Quality Control of Monitoring Data ActivityInfo - 5W 2026"),
      tags$div(
        style = "text-align: justify;",
        tags$p(
          "This app is designed to check existing data in ActivityInfo or to 
          review offline data before bulk uploading it into the Regional 5W 
          database."
        ),
        tags$p(
          tags$i(
            "To get started, navigate to the following tabs to perform 
          the Quality Assurance (QA) revision or analyze data from an excel file."
          )
        ),
        tags$p(
          tags$i("For any comments to this app, please contact the R4V regional IM TEAM")
        )
      )
    )
  ),
  # QA Panel
  nav_panel(
    title = "QA from database",
    layout_sidebar(
      open = TRUE,
      sidebar = tagList(
        tags$h4("Filter Options"),
        tags$p("Use bellow filters to analyze a subset of the data"),
        radioButtons(
          "filterRadioButton",
          "Select option for filter",
          choices = list("Country" = "country", "Partner" = "partner"),
          selected = "country"
        ),
        
        selectInput(
          "filterItemSelection",
          "Select a value for the filter:",
          choices = NULL,
          selected = "All"
        ),
        use_waiter(),
        actionButton("getDataFromActivityInfoDB", "Pull data from ActivityInfo"),
        waiter::waiter_preloader(html = spin_folding_cube()),
        actionButton("checkDataFromActivityInfoDB", "QA analysis", disabled = TRUE),
        waiter::waiter_preloader(html = spin_folding_cube()),
        downloadButton("downloadDataAI", "Error Report", disabled = TRUE)
      ),
      tags$div(
        tags$h3("Read data from Regional ActivityInfo Database"),
      ),
      fluidRow(
        column(
          width = 6,
          tags$div(
            tags$p("Select a country from the sidebar to filter the database."),
            tags$p("After data is displayed please use the QA button to get the results"),
            tags$p("Use download button to get an excel report of your data")
          )
        ),
        column(
          width = 2,
          uiOutput("total_activities_box")
        ),
        column(
          width = 2,
          uiOutput("total_error_box")
        ),
        column(
          width = 2,
          uiOutput("total_percent_box")
        ),
      ),
      DTOutput("dataTable")
    )
  ),
  nav_panel(
    title = "QA from Excel file",
    layout_sidebar(
      open = TRUE,
      sidebar = tagList(
        tags$h4("How to use this QA Test?"),
        tags$p(
          "To upload your Excel file to Activity Info, ensure it complies with the
        reporting template. Once the data is loaded, press the button to run the
        QA test. The results will be displayed on the screen, and you can download
        them in a new Excel file to make corrections. After fixing the issues,
        upload the corrected file back into the Activity Info Database."
        ),
        tags$a(
          href = "template_5w_2026.xlsx",
          target = "_blank",
          download = NA,
          "Download Excel Template"
        )
      ),
      fluidRow(
        column(
          width = 6,
          tags$div(
            tags$h3("Read data from excel file"),
            tags$p(
              "Please upload an Excel file using the template categories."
            ),
            fileInput(
              "uploadExcelFile",
              "Upload file...",
              multiple = FALSE,
              accept = ".xlsx",
            ),
            actionButton(
              "analizeDataFromExcelFile",
              "Execute analysis from excel file"
            ),
            downloadButton("downloadDataExcel", "Error Report", disabled = TRUE)
          )
        ),
        column(
          width = 2,
          uiOutput("total_activities_box_xlsx")
        ),
        column(
          width = 2,
          uiOutput("total_error_box_xlsx")
        ),
        column(
          width = 2,
          uiOutput("total_percent_box_xlsx")
        )
      ),
      DTOutput("previewXlsTable")
    )
  )
)
