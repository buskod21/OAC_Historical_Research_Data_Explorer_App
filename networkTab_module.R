# UI Module for Network Tab
networkTab_UI <- function(id) {
  ns <- NS(id) # Create a namespace for module

  tagList(
    fluidRow(
      column(12,
             box(
               title = tags$b("Select dataverse to view"),  # Box title
               width = 12,  # Full-width box
               collapsible = TRUE,  # Allows collapsing
               side = "right",  # Sidebar position
               type = "pills",  # Style of tabs

               # Sidebar to display additional node information
               sidebar = boxSidebar(
                 id = ns("infoPanel"),
                 uiOutput(ns("nodeInfo")),  # Output placeholder for dynamic node info
                 width = 25,  # Sidebar width
                 startOpen = FALSE  # Sidebar starts closed
               ),

               # Input for selecting dataverse(s)
               pickerInput(
                 inputId = ns("select_dataverse"),
                 label = "",
                 choices = NULL,  # Populate with unique dataverse names
                 multiple = TRUE  # Allow multi-select
               ),

               br(),

               # Input for choosing event type: Keywords or Authors
               awesomeRadio(
                 inputId = ns("event_type"),
                 label = "View network by :",  # Label for input
                 choices = c("Keywords", "Authors"),  # Options
                 selected = "Keywords",  # Default selection
                 inline = TRUE  # Display options inline
               ),

               br(),hr(),

               fluidRow(
                 column(10,
                        # Spinner while the network plot is loading
                        withSpinner(visNetworkOutput(
                          ns("networkPlot"),  # Network plot output
                          width = "100%",  # Full width
                          height = "800px"  # Height of the plot
                        ))),
                 column(2,
                        div(class = "custom-legend",
                            uiOutput(ns("customLegend"),
                                     style = "position: relative; top: 150px; right: 50px;")
                        )
                 )

               )
             ))

    )
  )
}

# Server Module for Network Tab
networkTab_server <- function(id, study_data, shared_data, conn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module

    # Observer to update the PickerInput based on study_data()
    observe({
      # Retrieve the current study data.
      data <- study_data()
      # Extract unique study titles (or any other field) to use as choices.
      choices <- unique(data$DataverseName)
      # Update the PickerInput with the new choices.
      updatePickerInput(session, "select_dataverse", choices = choices)
    })

    observe({
      # Sync the selected dataverse with shared_data for further use
      shared_data$selected_dataverse <- input$select_dataverse
    })

    # Reactive expression to filter data based on selected dataverse(s)
    shared_data$filtered_data <- reactive({
      req(shared_data$selected_dataverse, !is.null(study_data()), nrow(study_data()) > 0)  # Ensure inputs are valid
      study_data() %>%
        filter(DataverseName %in% shared_data$selected_dataverse)  # Filter for selected dataverses
    })

    # Fetch nodes data from the database based on the selected event type (Keywords or Authors)
    nodes_data <- reactive({
      req(input$event_type)  # Ensure input exists
      table_name <- paste0(input$event_type, "_node")  # Construct table name dynamically
      dbGetQuery(conn, paste0("SELECT * FROM ", table_name)) # Query the nodes table
    })

    # Fetch edges data from the database based on the selected event type (Keywords or Authors)
    edges_data <- reactive({
      req(input$event_type) # Ensure input exists
      table_name <- paste0(input$event_type, "_edge")  # Construct table name dynamically
      dbGetQuery(conn, paste0("SELECT * FROM ", table_name)) # Query the nodes table
    })

    # Filter nodes based on selected dataverse(s)
    filteredNodes <- reactive({
      req(input$select_dataverse)  # Ensure input exists
      nodes_data() %>%
        filter(
          str_detect(DataverseName, paste(input$select_dataverse, collapse = "|")))  # Filter nodes based on selected dataverses
    })

    # Filter edges based on valid node IDs
    filteredEdges <- reactive({
      req(filteredNodes())  # Ensure filtered nodes are available
      valid_ids <- filteredNodes()$id
      edges_data() %>% filter(from %in% valid_ids & to %in% valid_ids)  # Filter edges using valid node IDs
    })

    output$customLegend <- renderUI({
      req(nodes_data())

      if (nrow(nodes_data()) == 0 || is.null(input$select_dataverse)) return(NULL)

      legend_data <- nodes_data() %>%
        mutate(label = ifelse(color == "gray", "+1 Dataverse", DataverseName)) %>%
        distinct(label, color) %>%
        arrange(label)

      legend_items <- lapply(1:nrow(legend_data), function(i) {
        tags$div(
          style = paste0(
            "display: flex; align-items: center; margin-bottom: 5px; margin-top: 5px;",
            "padding: 5px; background-color: #fff; border-radius: 5px;"
          ),
          tags$div(
            style = paste0(
              "flex-shrink: 0; width: 20px; height: 20px; background-color: ",
              legend_data$color[i], "; ",
              "border-radius: 2px; margin-right: 10px; margin-top: 5px; border: 1px solid #ccc;"
            )
          ),
          legend_data$label[i]
        )
      })

      bs4Card(
        title = tags$div(
          "Dataverse Legend",
          style = "text-align: center; font-size: 20px; font-weight: bold;"
        ),
        collapsible = FALSE,
        width = 12,
        solidHeader = TRUE,
        status = "lightblue",
        do.call(tags$div, legend_items)
      )
    })


    # Render the network plot using the reactive dataset 'network_data_reactive()'
    output$networkPlot <- renderVisNetwork({
      req(filteredNodes(), filteredEdges())  # Ensure that both nodes and edges data are available

      visNetwork(filteredNodes(), filteredEdges(), width = "100%", height = "800px",
                 main = "Connections Between Dataverses") %>%

        # Configure the appearance of nodes
        visNodes(
          size = 200,  # Default node size
          shape = "ellipse",  # Node shape
          borderWidth = 2,  # Border thickness
          color = list(
            background = "field:color",  # Use field data for color assignment
            border = "black",  # Black border for nodes
            highlight = "#ff0"  # Yellow highlight when selected
          ),
          labelHighlightBold = TRUE,  # Bold text on hover
          scaling = list(
            label = list(
              enabled = TRUE,
              min = 10,  # Minimum font size
              max = 30   # Maximum font size
            )
          ),
          font = list(
            color = "black",
            face = "bold",
            size = 50  # Font size for labels
          )
        ) %>%

        # Edge styling
        visEdges(smooth = FALSE) %>%

        # Enable options for highlighting nodes and selecting by ID
        visOptions(
          highlightNearest = TRUE,  # Highlight connected nodes when a node is selected
          nodesIdSelection = list(
            enabled = TRUE,  # Enable dropdown selection for nodes
            main = paste("Select by ", input$event_type),  # Dropdown label
            style = 'width: 300px; height: 26px;'  # Custom styling for the selection box
          )
        ) %>%

        # Apply a consistent layout using a random seed
        visLayout(randomSeed = 123) %>%

        # Define physics properties for the network layout
        visPhysics(
          solver = "forceAtlas2Based",  # Use Force Atlas 2 layout
          forceAtlas2Based = list(
            gravitationalConstant = -150,  # Adjust repulsion force
            avoidOverlap = 0.1  # Reduce node overlap
          ),
          stabilization = list(iterations = 600),  # Increase stabilization iterations
          timestep = 0.4,
          minVelocity = 70  # Stop movement when velocity is below threshold
        ) %>%

        # Interaction settings
        visInteraction(
          hover = FALSE,  # Disable hover tooltips
          navigationButtons = TRUE,  # Enable zoom and pan buttons
          keyboard = TRUE,  # Enable keyboard navigation
          tooltipDelay = 0  # No delay for tooltips
        ) %>%

        # Define click event behavior using JavaScript
        visEvents(click = glue::glue(
          "function(nodes) {{
        if (nodes.nodes.length > 0) {{
          Shiny.setInputValue('{ns('selectedEvent')}', nodes.nodes[0], {{priority: 'event'}});
        }} else {{
          Shiny.setInputValue('{ns('selectedEvent')}', null, {{priority: 'event'}});
        }}
      }}"
        ))
    })



    # Event: Update UI based on selected node
    observeEvent(input$selectedEvent, {
      req(shared_data$filtered_data(), shared_data$selected_dataverse, input$selectedEvent, conn)

      # Get filtered data based on selected dataverse(s)
      data <- shared_data$filtered_data()

      # Determine which table to query (Keywords or Authors)
      table_name <- if (input$event_type == "Keywords") "keywords_node" else "authors_node"

      # Fetch the event name from the database using selectedEvent (ID)
      selected_event_query <- paste0("SELECT label FROM ", table_name, " WHERE id = ?")
      selected_event <- dbGetQuery(conn, selected_event_query, params = list(input$selectedEvent))$label

      if (length(selected_event) == 0) {
        output$nodeInfo <- renderUI({
          tags$div(class = "alert alert-warning", "No node found in the database.")
        })
        return()
      }

      # Determine the correct column in the data for filtering
      event_column <- if (input$event_type == "Keywords") "Keywords" else "Authors"

      # Filter the data based on the selected event
      selected_data <- data %>%
        dplyr::filter(stringr::str_detect(.data[[event_column]],
                                          stringr::regex(selected_event, ignore_case = TRUE)))

      # Store filtered results for study selection
      shared_data$study_choices <- selected_data

      # Retrieve study titles and unique DOIs
      studies <- selected_data %>% dplyr::pull(Title)
      dois <- selected_data %>% dplyr::pull(DOI) %>% unique()

      # Generate DOI links if available
      if (length(dois) == 0) {
        doi_text <- "No DOI found."
      } else {
        doi_links <- sapply(dois, function(doi) {
          paste0('<a class="badge badge-info" href="https://doi.org/', doi, '" target="_blank">', doi, '</a>')
        })
        doi_text <- paste(doi_links, collapse = ", ")
      }

      # Prepare UI output based on event type
      if (input$event_type == "Authors") {
        collaborators <- selected_data %>%
          dplyr::pull(Authors) %>%
          stringr::str_split(",") %>%
          unlist() %>%
          stringr::str_trim() %>%
          unique() %>%
          setdiff(selected_event) %>%
          length()

        info_html <- tags$div(
          tags$p(tags$b("Number of studies: "), length(unique(studies))),
          tags$p(tags$b("Number of collaborators: "), collaborators),
          tags$p(tags$b("DOIs of studies: "), HTML(doi_text))
        )
      } else {
        info_html <- tags$div(
          tags$p(tags$b("Number of studies: "), length(unique(studies))),
          tags$p(tags$b("DOIs of studies: "), HTML(doi_text))
        )
      }

      # Update UI with selected event details
      output$nodeInfo <- renderUI({ info_html })
    })
  })
}

