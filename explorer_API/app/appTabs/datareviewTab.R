# ----- Define the UI and server logic for the data review tab

# UI function for the data review tab
datareviewTab_UI <- function(id) {
  
  # Create a namespace function using the provided id for scoping the inputs/outputs
  ns <- NS(id)
  
  # Build the UI elements using tagList
  tagList(
    # Div containing the dropdown for selecting a study
    div(
      id = "selectDiv",
      # TabBox containing the study selection dropdown menu
      tabBox(
        title = "",           # No title for the tabBox
        width = 12,           # Full-width tabBox
        collapsible = FALSE,   # Make the box non-collapsible
        maximizable = FALSE,   # Allow maximizing the box
        elevation = 1,        # Elevation style for the box
        solidHeader = FALSE,  # Do not use a solid header
        status = "lightblue", # Light blue styling for the box
        tabPanel("Select a Study", # Tab for selecting a study
                 selectizeInput(
                   inputId = ns("study_select"),  # Namespace-aware input ID
                   label = "",
                   choices = NULL,               # Initial choices are empty
                   selected = "",                # No initial selection
                   multiple = TRUE,              # Allow multiple selections
                   options = list(               # Dropdown options
                     maxItems = 1,               # Allow only one selection at a time
                     placeholder = "Nothing Selected ..."  # Placeholder text
                   ),
                   width = "100%"                # Full-width dropdown
                 )
        )
      )
    ),
    
    # Line break to create space between elements
    br(),
    
    # Use shinyjs for enabling/disabling and hiding/showing elements
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      # Div to hold the content of the tab once a study is selected
      div(
        id = ns("overviewBox"),
        # Add a spinner to show loading while content is loading
        withSpinner(
          tabBox(
            title = "",           # No title for the tabBox
            width = 12,           # Full-width tabBox
            collapsible = TRUE,   # Make the box collapsible
            maximizable = TRUE,   # Allow maximizing the box
            elevation = 1,        # Elevation style for the box
            solidHeader = FALSE,  # Do not use a solid header
            status = "lightblue", # Light blue styling for the box
            
            # Tab 1: Study Overview
            tabPanel("Study Overview", overviewUI_module(ns("overview"))),
            
            # Tab 2: Metadata
            tabPanel("Metadata", metadataUI_module(ns("metadata"))),
            
            # Tab 3: Data Exploration
            tabPanel("Data Exploration", dataexplorationUI_module(ns("dataexploration")))
          )
        )
      )
    )
  )
}

# Server function for the data review tab
datareviewTab_server <- function(id, study_data, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update study_select dropdown when the selected_dataverse changes
    observe({
      # Ensure selected_collegeDept is valid before processing
      req(shared_data$selected_collegeDept)  # Require the selected college/dept column to be set
      
      # Determine which data to use (filtered or full)
      data <- if (is.null(shared_data$selected_dataverse) || length(shared_data$selected_dataverse) == 0) {
        study_data()  # Use all study data if no dataverse is selected
      } else {
        shared_data$filtered_data()  # Use filtered data when a dataverse is selected
      }
      
      # Dynamically split based on the selected column (CollegeName or DepartmentName)
      choices <- lapply(
        split(data$Title, data[[shared_data$selected_collegeDept]]),  # Split by the dynamic column
        as.list
      )
      
      # Update the study selection dropdown with the new choices
      if ("Title" %in% names(data)) {
        updateSelectizeInput(session, "study_select", choices = choices, selected = NULL)
      }
    })
    
    # Observe changes to shared_data$study_choices and update the dropdown
    observeEvent(shared_data$study_choices, {
      # Ensure data is available to update the dropdown
      req(shared_data$study_choices)
      
      # Extract unique study titles for the dropdown, split by selected column (College/Department)
      choices <- split(shared_data$study_choices$Title, shared_data$study_choices[[shared_data$selected_collegeDept]])
      
      # Update the study select dropdown with the new choices
      updateSelectizeInput(
        session,
        "study_select",
        choices = choices,
        selected = NULL  # Clear previous selections
      )
    }, ignoreNULL = TRUE)  # Trigger only when study_choices is updated
    
    
    # Observe changes in the study selection
    observeEvent(input$study_select, {
      # Check if a study is selected
      if (!is.null(input$study_select) && input$study_select != "") {
        # Show the overviewBox if a study is selected
        shinyjs::show("overviewBox")
      } else {
        # Hide the overviewBox if no study is selected
        shinyjs::hide("overviewBox")
      }
    })
    
    # Server logic for the overview tab
    overviewServer_module("overview",
                          input_study_select = reactive(input$study_select),
                          study_data = study_data,
                          shared_data= shared_data)
    
    # Observe the study selection and update paths accordingly
    observe({
      # Ensure a study is selected before proceeding
      req(input$study_select)
      
      # Dynamically choose between full study data or filtered data based on dataverse selection
      data <- if (is.null(shared_data$selected_dataverse)) study_data() else shared_data$filtered_data()
      
      # Fetch DOI, file lists, and update inputs accordingly
      selected_title <- input$study_select
      selected_doi <- data %>%
        filter(Title == selected_title) %>%
        pull(DOI) %>%
        unique()
      
      # Ensure only one DOI is selected
      if (length(selected_doi) != 1) {
        # Reset inputs if no valid DOI is found
        updateSelectInput(session, "metadata_select", choices = NULL, selected = NULL)
        updateSelectInput(session, "dataset_select", choices = NULL, selected = NULL)
        shared_data$full_paths <- list()  # Clear previous file paths
        return()  # Exit early if no valid DOI is selected
      }
      
      # Fetch file list for the selected study
      shared_data$file_list <- access_data(selected_doi)
      
      # Update full_paths after file list is validated
      shared_data$full_paths <- setNames(shared_data$file_list, basename(shared_data$file_list))
    })
    
    # Server logic for the metadata tab
    Metadataserver_module("metadata",
                          reactive(input$metadata_select),
                          shared_data)
    
    # Server logic for the data exploration tab
    dataexplorationserver_module("dataexploration",
                                 reactive(input$dataset_select),
                                 shared_data)
  })
}
