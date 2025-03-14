# ---- This module defines the UI and server logic for the overview tabpanel

overviewUI_module <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        # Table output for showing study details
        withSpinner(DT::dataTableOutput(ns("study_details")))
      )
    )
  )
}

overviewServer_module <- function(id, input_study_select, study_data, shared_data) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    #  From here, dynamically choose between study_data and shared_data$filtered_data()
    # Show study details based on selection
    output$study_details <- DT::renderDataTable({

      req(input_study_select())  # Ensure that a selection has been made and detailed_data is available


      # Dynamically choose between study_data and hared_data$filtered_data() to display study details
      data <- if (is.null(shared_data$selected_dataverse)) study_data() else shared_data$filtered_data()

      # Filter data first before transposing
      overview_data <- data %>%
        mutate(PeriodCovered = str_replace_all(PeriodCovered, "/", " - ")) %>%
        mutate(DOI = paste0('<a class="badge badge-info" href="https://doi.org/',
                            DOI, '" target="_blank">', DOI, '</a>')) %>%
        filter(Title == input_study_select()) %>% # Filter data first before transposing
        t() # Transpose the filtered data

      # Render the datatable
      datatable(overview_data,
                escape = FALSE,
                rownames = TRUE,
                colnames = c("Parameter", "Value"),
                options = list(dom = 'tp',
                               autoWidth = FALSE,
                               scrollX = TRUE,
                               pageLength = nrow(overview_data))  # Set page length to number of rows
      )
    })
  })
}
