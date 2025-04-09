
explorer_list$networkTab_ui <-tabItem(
  tabName = "network_tab",
  fluidRow(
    box(
      title = tags$b("Filter Data by :", 
                     style = "padding-left: 5px; margin-bottom: 2px;"),
      width = 12,  # Full-width box
      collapsible = FALSE,
      maximizable = FALSE,
      elevation = 1,
      solidHeader = FALSE,
      status = "lightblue",
      side = "right",
      type = "pills",
      
      div(style ="margin-left: 10px;",
          awesomeRadio(
            inputId = "collegeDept_filter",
            label = "",
            choices = c(
              "Colleges / Campus / Institution" = "CollegeName",
              "Department / Research Centre" = "DepartmentName"
            ),
            selected = "CollegeName",
            inline = TRUE
          )),
      
      uiOutput("dynamic_filter_ui"),
      
      sidebar = boxSidebar(
        id = "infoPanel",
        uiOutput("nodeInfo"), 
        width = 25,  # Sidebar width
        startOpen = FALSE  # Sidebar starts closed
      ),
      
      hr(),
      
      h6(tags$b("View Network By : ", style = "padding-left: 10px; margin-bottom: 2px;")),
      
      div(style = "margin-left: 10px;",
          awesomeRadio(
            inputId = "event_type",
            label = "",
            choices = c("Keywords", "Authors"),
            selected = "Keywords",
            inline = TRUE
          )
      ),
      
      hr(),
      
      fluidRow(
        column(10,  # 100% width for the plot on small screens
               withSpinner(visNetworkOutput(
                 "networkPlot",
                 width = "100%",
                 height = "800px"
               ))
        ),
        column(2,  # Full width on small screens for legend
               div(class = "custom-legend",
                   uiOutput("customLegend", 
                            style = "display: flex; justify-content: center; align-items: center;
                            position: relative; top: 120px;")
               )
               
        )
      )
    )
  )
)




# Server Module for Network Tab
explorer_list$networkTab_server <- function(input, output, session, study_data, shared_data, conn) {
  
  
  # Dynamic filter UI rendering based on college/dept selection
  output$dynamic_filter_ui <- renderUI({
    req(input$collegeDept_filter, study_data())
    
    # Dynamically create the choices based on the selected filter
    choices <- sort(unique(study_data()[[input$collegeDept_filter]]))
    
    div(style = "margin-left: 10px; margin-bottom: 4px;",
        pickerInput(
          inputId = "select_dataverse",
          label = "",
          choices = choices,
          multiple = TRUE
        ))
  })
  
  # Sync selected college/dept with reactiveVal shared_data for further use
  observe({
    req(input$collegeDept_filter)
    shared_data$selected_collegeDept <- input$collegeDept_filter
  })
  
  # Sync selected dataverse with shared_data for further use
  observe({
    req(input$select_dataverse)
    shared_data$selected_dataverse <- input$select_dataverse
  })
  
  # Reactive expression to filter data based on selected dataverse(s)
  shared_data$filtered_data <- reactive({
    req(study_data(), shared_data$selected_dataverse, shared_data$selected_collegeDept)
    
    # Filter the data based on the selected dataverse
    study_data() %>%
      dplyr::filter(.data[[shared_data$selected_collegeDept]] %in% shared_data$selected_dataverse)
  })
  
  
  # Fetch nodes data from the database based on the selected event type (Keywords or Authors)
  nodes_data <- reactive({
    req(input$event_type)  # Ensure input exists
    table_name <- paste0(input$event_type, "_node")  # Construct table name dynamically
    dbGetQuery(connection, paste0("SELECT * FROM ", table_name)) # Query the nodes table
  })
  
  # Fetch edges data from the database based on the selected event type (Keywords or Authors)
  edges_data <- reactive({
    req(input$event_type) # Ensure input exists
    table_name <- paste0(input$event_type, "_edge")  # Construct table name dynamically
    dbGetQuery(connection, paste0("SELECT * FROM ", table_name)) # Query the nodes table
  })
  

  # Filter nodes based on selected college(s) or department(s)
  filteredNodes <- reactive({
    req(nodes_data(), shared_data$selected_collegeDept, shared_data$selected_dataverse)  # Ensure data is loaded
    
    df <- nodes_data()
    # selected_value <- shared_data$selected_collegeDept  # The selected value (either College or Department)
    
    # Filter based on the selected value (College or Department)
    if (!is.null(shared_data$selected_dataverse) && length(shared_data$selected_dataverse) > 0) {
      df <- df %>%
        filter(
          str_detect(CollegeName, paste(shared_data$selected_dataverse, collapse = "|")) | 
            str_detect(DepartmentName, paste(shared_data$selected_dataverse, collapse = "|"))
        )
    }
    
    # Return the filtered dataframe
    df
  })
  
  
  # Filter edges based on valid node IDs
  filteredEdges <- reactive({
    req(filteredNodes())  # Ensure filtered nodes are available
    valid_ids <- filteredNodes()$id
    edges_data() %>% filter(from %in% valid_ids & to %in% valid_ids)  # Filter edges using valid node IDs
  })
  
  # # Customize the legend for the network
  # output$customLegend <- renderUI({
  #   req(nodes_data())
  #   
  #   if (nrow(nodes_data()) == 0 || is.null(input$select_dataverse)) return(NULL)
  #   
  #   legend_data <- nodes_data() %>%
  #     mutate(label = ifelse(color == "gray", "+1 Dataverse", DataverseName)) %>%
  #     distinct(label, color) %>%
  #     arrange(label)
  #   
  #   bs4Card(
  #     title = tags$div(
  #       "Dataverse Legend",
  #       style = "text-align: center; font-size: 24px; font-weight: bold;"
  #     ),
  #     collapsible = FALSE,
  #     width = 12,  # Changed to a smaller card width
  #     solidHeader = TRUE,
  #     status = "lightblue",
  #     
  #     # Wrapper for the reactable with proper width and overflow handling
  #     reactable(
  #       legend_data,
  #       columns = list(
  #         label = colDef(
  #           name = "",
  #           minWidth = 200  # Increase the width of the label column
  #         ),
  #         color = colDef(
  #           name = "",
  #           cell = function(value) {
  #             div(style = paste("background-color:", value, "; width: 20px; height: 20px; border-radius: 4px;"))
  #           },
  #           headerStyle = list(textAlign = "center"),  # Centers header
  #           style = list(
  #             textAlign = "right"        # Centers the color code content
  #           ),
  #           minWidth = 50    # The width of the label column
  #         )
  #       ),
  #       width = "100%",
  #       outlined = TRUE,
  #       borderless = TRUE,
  #       sortable = FALSE
  #     )
  #   )
  # })
  
  
  
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
          style = "width: 250px; height: 26px; margin-left: 
            20px; position: relative; top: -10px;"  # Moves box to the right
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
          Shiny.setInputValue('{'selectedEvent'}', nodes.nodes[0], {{priority: 'event'}});
        }} else {{
          Shiny.setInputValue('{'selectedEvent'}', null, {{priority: 'event'}});
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
}

