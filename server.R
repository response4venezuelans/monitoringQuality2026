library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(waiter)
library(readxl)
library(writexl)
library(purrr)

server <- function(input, output, session) {
  metrics_db <- reactiveValues(
    total_activities = 0,
    total_errors = 0,
    percent_error = 0
  )
  metrics_excel <- reactiveValues(
    total_activities = 0,
    total_errors = 0,
    percent_error = 0
  )
  # Initialize fetchedData as an empty reactive value
  fetchedData <- reactiveVal(tibble(Message = "No data available"))
  fetchedDataExcel <- reactiveVal(tibble(Message = "No data available"))
  
  # Observe filter selection changes and update choices
  observe({
    req(input$filterRadioButton)
    updateSelectInput(session, "filterItemSelection",
                      choices = if (input$filterRadioButton == "country") 
                        countryList else 
                          partnerList,
                      selected = "All")
  })
  
  # Fetch data when the button is clicked
  observeEvent(input$getDataFromActivityInfoDB, {
    waiter <- Waiter$new(id = "dataTable") # Attach waiter to dataTable
    waiter$show()
    # Fetch data
    monitoring5WData <- getDataFromAI(input$filterRadioButton, input$filterItemSelection)

    # Ensure the fetched data is a valid data frame
    if (is.null(monitoring5WData) || nrow(monitoring5WData) == 0) {
      fetchedData(data.frame(Message = "No data available", stringsAsFactors = FALSE))
    } else {
      fetchedData(monitoring5WData)
      updateActionButton(session, "checkDataFromActivityInfoDB", disabled = FALSE)
      updateActionButton(session, "downloadDataAI", disabled = FALSE)
    }
    waiter$hide()
  })

  # Add any additional processing or analysis here
  observeEvent(input$checkDataFromActivityInfoDB, {
    req(fetchedData())
    data_to_check <- fetchedData()

    tryCatch({
      checked_data <- qa_check(data_to_check)
      fetchedData(checked_data)
      metrics_db$total_activities <- get_total_activities(checked_data)
      metrics_db$total_errors <- get_total_activities_to_review(checked_data, "QA_sum")
      metrics_db$percent_error <- get_percentage_activities(metrics_db$total_errors, metrics_db$total_activities)
      updateActionButton(session, "downloadDataAI", disabled = FALSE)
    }, error = function(e) {
      showNotification(
        paste("QA check error:", conditionMessage(e)),
        type = "error",
        duration = 15
      )
    })
  })
  
  # Render DataTable safely
  output$dataTable <- renderDT({
    datatable(fetchedData())
  })
  
  output$downloadDataAI <- downloadHandler(
    filename = function() {
      "error_report.xlsx"
    },
    content = function(file) {
      error_data <- fetchedData() |> filter(QA_sum == 1) |> select(-QA_sum)
      qa_cols_with_errors <- error_data |>
        select(starts_with("QA_")) |>
        select(where(~ any(. == 1, na.rm = TRUE))) |>
        names()
      writexl::write_xlsx(
        error_data |> select(-starts_with("QA_"), all_of(qa_cols_with_errors)),
        path = file
      )
    }
  )
  
  output$downloadDataExcel <- downloadHandler(
    filename = function() {
      "error_report.xlsx"
    },
    content = function(file) {
      # Write your dataset to the file
      writexl::write_xlsx(fetchedDataExcel(), path = file)
    }
  )

  ### Metric Boxes
  
  output$total_activities_box <- renderUI({
    value_box( 
      title = "Activities",
      showcase =  activities_icon,
      value = metrics_db$total_activities, 
      theme = "info",
      class = "my-valuebox"
    )
  })
  
  output$total_activities_box_xlsx <- renderUI({
    value_box( 
      title = "Activities",
      showcase =  activities_icon,
      value = metrics_excel$total_activities, 
      theme = "info",
      class = "my-valuebox"
    )
  })
  
  output$total_error_box <- renderUI({
    value_box( 
      title = "Total Errors",
      showcase = error_icon,
      value = metrics_db$total_errors, 
      theme = "warning",
      class = "my-valuebox"
    )
  })
  
  output$total_error_box_xlsx <- renderUI({
    value_box( 
      title = "Total Errors",
      showcase = error_icon,
      value = metrics_excel$total_errors, 
      theme = "warning",
      class = "my-valuebox"
    )
  })
  
  output$total_percent_box <- renderUI({
    value_box( 
      title = "Percentage of errors", 
      showcase = percent_icon,
      value = metrics_db$percent_error, 
      theme = "danger",
      class = "my-valuebox"
    )
  })
  output$total_percent_box_xlsx <- renderUI({
    value_box( 
      title = "Percentage of errors", 
      showcase = percent_icon,
      value = metrics_excel$percent_error, 
      theme = "danger",
      class = "my-valuebox"
    )
  })  
  
  # Reactive expression to read the uploaded Excel file
  uploaded_data <- reactive({
    req(input$uploadExcelFile)  # Ensure a file is uploaded
    file <- input$uploadExcelFile$datapath
    read_excel(file)  # Read the Excel file
  })
  
  # Render the preview of the uploaded Excel file or the processed data
  output$previewXlsTable <- renderDT({
    req(fetchedDataExcel())  # Ensure data is available
    datatable(fetchedDataExcel(), options = list(pageLength = 5))
  })
  
  observeEvent(input$analizeDataFromExcelFile, {
    # Perform QA analysis on the uploaded data
    data <- uploaded_data()
    isTemplateValid <- check_dataframe_structure(data, "www/template_5w_2025.xlsx", sheet = 1)
    if (!isTemplateValid) {
      showModal(
        modalDialog(
          title = "Error: Data structure check failed!!",
          easy_close = TRUE,
          "The structure of the uploaded data does not match the expected template."
        )
      )
      return(NULL) # Stop here if template invalid
    }
    data <- rename_columns(data)
    data <- add_platform_column(data)
    data <- addIndicatorType(data, indicatorDF)
    data <- addCountryISOCodes(data, countryListDF)
    checked_data <- qa_check(data)
    # Update the reactive value with the processed data
    fetchedDataExcel(checked_data)
    # Update metrics based on the Excel file analysis
    metrics_excel$total_activities <- get_total_activities(checked_data)
    metrics_excel$total_errors <- get_total_activities_to_review(checked_data, "QA_sum")
    metrics_excel$percent_error <- get_percentage_activities(metrics_excel$total_errors, metrics_excel$total_activities)
  })
}