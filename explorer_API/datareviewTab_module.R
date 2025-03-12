# ----- Define the UI and server logic for the data review tab
datareviewTab_UI <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  # Build the UI elements using tagList
  tagList(
    # Create a column to hold the study selection dropdown menu
    column(
      width = 12,
      div(
        id = "selectDiv",
        # Dropdown for selecting a study
        selectizeInput(
          inputId = ns("study_select"),  # Namespace-aware input ID
          label = "Study selection",    # Label for the dropdown
          choices = NULL,               # Initial choices are empty
          selected = "",                # No initial selection
          multiple = TRUE,              # Allow multiple selections
          options = list(               # Dropdown options
            maxItems = 1,               # Allow only one selection
            placeholder = "Nothing selected ..."  # Placeholder text
          ),
          width = "100%"                # Full-width dropdown
        )
      )
    ),

    # Add a line break for spacing
    br(),

    # Use shinyjs for enabling/disabling and hiding/showing elements
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      # Div to hold the content of the tab when it becomes visible
      div(
        id = ns("overviewBox"),
        # Add a spinner to show loading while content loads
        withSpinner(
          tabBox(
            title = "",           # No title for the tabBox
            width = 12,           # Full-width tabBox
            collapsible = TRUE,   # Make the box collapsible
            maximizable = TRUE,   # Allow maximizing the box
            elevation = 1,        # Elevation style for the box
            solidHeader = FALSE,  # Do not use a solid header
            status = "lightblue", # Style the box with light blue color

            # Tab 1: Study Overview
            tabPanel("Study Overview", overviewUI_module(ns("overview"))),

            # Tab 2: Metadata
            tabPanel("Metadata", metadataUI_module(ns("metadata"))),

            # Tab 3: Data Exploration
            tabPanel("Data exploration", dataexplorationUI_module(ns("dataexploration")))
          )
        )
      )
    )
  )
}


datareviewTab_server <- function(id, study_data, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update study_select i.e., select a study dropdown menu when select_dataverse is changed
    observeEvent(shared_data$selected_dataverse, {
      data <- if (is.null(shared_data$selected_dataverse) || length(shared_data$selected_dataverse) == 0) {
        study_data()
      } else {
        shared_data$filtered_data()
      }

      choices <- lapply(
        split(data$Title, data$DataverseName),
        as.list
      )

      if ("Title" %in% names(data)) {
        updateSelectizeInput(session, "study_select", choices = choices, selected = NULL)
      }
    }, ignoreNULL = FALSE)


    # Observe changes to shared_data$study_choices and update study_select dropdown
    observeEvent(shared_data$study_choices, {
      # Ensure there is data to update the dropdown
      req(shared_data$study_choices)

      # Extract unique study titles for the dropdown
      choices <- lapply(
        split(shared_data$study_choices$Title, shared_data$study_choices$DataverseName),
        as.list
      )

      # Update the dropdown menu with these choices
      updateSelectizeInput(
        session,
        "study_select",
        choices = choices,
        selected = NULL # Clear previous selections
      )
    }, ignoreNULL = TRUE) # Trigger only when study_choices is updated

    # Observe changes in the tabPanel selection
    observeEvent(input$study_select, {
      # Check if a study is selected
      if (!is.null(input$study_select) && input$study_select != "") {
        # Show the box
        shinyjs::show("overviewBox")
      } else {
        # Hide the box if no study is selected
        shinyjs::hide("overviewBox")
      }
    })

    # Server logic for the overview tab
    overviewServer_module("overview",
                          input_study_select = reactive(input$study_select),
                          study_data = study_data,
                          shared_data= shared_data)

    # This observe should trigger once a study is selected
    observe({
      # Ensure a selection has been made
      req(input$study_select)

      # Dynamically choose between study_data() and shared_data$filtered_data() to display study details
      data <- if (is.null(shared_data$selected_dataverse)) study_data() else shared_data$filtered_data()

      # Fetch DOI, file lists, and update inputs
      selected_title <- input$study_select
      selected_doi <- data %>%
        filter(Title == selected_title) %>%
        pull(DOI) %>%
        unique()

      # Ensure that selected_doi is a single value
      if (length(selected_doi) != 1) {
        # Simply reset input fields and return early
        updateSelectInput(session, "metadata_select", choices = NULL, selected = NULL)
        updateSelectInput(session, "dataset_select", choices = NULL, selected = NULL)
        shared_data$full_paths <- list()  # Ensure any previous paths are cleared
        return()  # Exit the observer without further action
      }

      # Fetch the file list
      shared_data$file_list <- access_data(selected_doi)

      # Update full_paths right after file_list is validated and stored
      shared_data$full_paths <- setNames(shared_data$file_list, basename(shared_data$file_list))
    })

    Metadataserver_module("metadata",
                          reactive(input$metadata_select),
                          shared_data)
    dataexplorationserver_module("dataexploration",
                                 reactive(input$dataset_select),
                                 shared_data)
  })
}


