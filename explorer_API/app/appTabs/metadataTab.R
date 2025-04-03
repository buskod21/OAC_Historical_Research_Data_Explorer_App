# ---- This module defines the UI and server logic for the metadata tabpanel
metadataUI_module <- function(id){

  ns<- (NS(id))
  tagList(
      fluidRow(
        column(
          12,
          # Dropdown for selecting metadata
          selectInput(
            ns("metadata_select"),
            "Select a Metadata",
            choices = NULL, # Initial choices are empty
            selected = NULL # No initial selection
          ),
          br(), # Add a line break for spacing

          # Section for displaying metadata description and schema
          tags$b("Data Description"), # Bold text for the section
          hr(),                       # Horizontal line separator
          uiOutput(ns("meta")),       # Output for metadata description
          tags$b("Data Schema"),      # Bold text for the schema section
          hr(),                       # Horizontal line separator
          uiOutput(ns("schema"))      # Output for data schema
      )
    )
  )
}


Metadataserver_module <- function(id, input_metadata_select, shared_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive expression for updating metadata select choices
    observe({
      req(shared_data$file_list)

      # Filter text files from the file_list
      metadata_files <- filter_filelist(shared_data$file_list, is_txt = TRUE)

      # Update the metadata_select input choices dynamically
      updateSelectInput(session, "metadata_select", choices = basename(metadata_files), selected = NULL)
    })


    # Reactive expression for loading and handling metadata
    loaded_metadata <- reactive({
      req(input$metadata_select)  # Ensure a file is selected

      # Use the full path for reading the file
      selected_file_path <- shared_data$full_paths[input$metadata_select]

      req(file.exists(selected_file_path))  # Ensure the file exists

      # Read the file and suppress warning
      raw_lines <- suppressWarnings(readLines(selected_file_path))

      # Replace only the first '=' with ':'
      corrected_lines <- sapply(raw_lines, function(line) {
        sub("=", ":", line, fixed = TRUE)
      })

      # Replace only the first ':' with '|'
      final_lines <- sapply(corrected_lines, function(line) {
        sub(":", "|", line, fixed = TRUE)
      })

      # Read the modified lines as a table, using '|' as the separator
      meta_file <- read.table(text = final_lines,
                              sep = "|",
                              fill = TRUE,
                              header = FALSE,
                              stringsAsFactors = FALSE)

      # Optionally, clean rows with all NA values
      meta_file <- meta_file[rowSums(is.na(meta_file)) != ncol(meta_file), ]

      return(meta_file)
    })


    # UI output for metadata
    output$meta <- renderUI({
      # Attempt to fetch the metadata
      req(loaded_metadata())

      meta_data <- loaded_metadata()

      if (is.null(meta_data)) {

        # Return a UI element displaying the error message
        tagList(
          h3("The schema file is either not there or restricted.")
        )

      } else {

        # Process and display the DataTable if metadata is available
        separate_by_tab <- grep("^\\t", meta_data$V1)
        if (length(separate_by_tab) > 0) {
          first_tab <- separate_by_tab[1]
          datatable_data <- meta_data[1:(first_tab - 1), , drop = FALSE]
        } else {
          datatable_data <- meta_data
        }

        DT::datatable(datatable_data,
                      rownames = FALSE,
                      colnames = c("Key", "Value"),
                      options = list(dom = "tp", autoWidth = FALSE, scrollX = TRUE))
      }
    })

    # UI output for schema
    output$schema <- renderUI({
      # Attempt to fetch the metadata
      req(loaded_metadata())

      # Fetch the metadata again
      meta_data <- loaded_metadata()

      # Check if the metadata is NULL
      if (is.null(meta_data)) {
        # Output a message if schema is NULL or not available
        tagList(
          h3("The schema file is either not there or restricted.")
        )
      } else {
        # Render DataTable if schema is available
        separate_by_tab <- grep("^\\t", meta_data$V1)

        if (length(separate_by_tab) > 0) {
          first_tab <- separate_by_tab[1]  # Use only the first tab if there are multiple
          datatable_data <- meta_data[first_tab:nrow(meta_data), , drop = FALSE]
        } else {
          datatable_data <- meta_data
        }

        DT::datatable(datatable_data,
                      rownames = FALSE,
                      colnames = c("Key", "Value"),
                      options = list(dom = "tp",
                                     autoWidth = FALSE,
                                     scrollX = TRUE))
      }
    })
  }
  )}
