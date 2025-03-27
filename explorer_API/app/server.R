
# Define server for the app ----

server <- function(input, output, session) {
  
  # Establish database connection: create a single connection for the session
  db_file <- "appData/Explorer_cache.db"  # Specify the path to the SQLite database
  conn <- get_db_conn(db_file)  # Get the database connection
  
  # Check if 'update_info' table exists and extract last update time
  if ("update_info" %in% dbListTables(conn)) {
    update_info <- dbReadTable(conn, "update_info")  # Read the 'update_info' table
    
    if (nrow(update_info) > 0) {
      last_update <- as.POSIXct(update_info$last_update[1])  # Get the last update time
      time_diff <- difftime(Sys.time(), last_update, units = "hours")  # Calculate time difference
    } else {
      time_diff <- NA  # If no data, set time_diff as NA
    }  
  } else {
    time_diff <- NA  # If table doesn't exist, set time_diff as NA
  }
  
  # Dynamically update the notification item in the UI
  output$update_notification <- renderUI({
    if (!is.na(time_diff) && time_diff < 48) {
      notificationItem(
        text = paste0("Cache updated ", round(time_diff, 1), " hours ago")  # Show the last update time if it's less than 48 hours
      )
    } else {
      notificationItem(
        text = "⚠️ No recent update found. Data may be outdated."  # Show a warning if data is outdated or no update found
      )
    }
  })
  
  # Reactive expression that updates the data every 48 hours.
  study_data <- reactive({
    # Fetch data and update cache every time study_data is called.
    cache_raw_data(conn)  # Call the function to fetch and cache raw data
  })
  
  # Initialize the home tab server logic with study_data
  explorer_list$homeTab_server(input, output, session, study_data)
  
  # Observe study_data() and process the keywords and authors into nodes and edges when necessary
  observeEvent(study_data(), {
    req(study_data())  # Ensure study_data() is available
    
    # Fetch the raw study data
    new_data <- study_data()
    
    # Retrieve processed DOIs from the 'processed_studies' table (if it exists)
    processed_dois <- c()
    if (dbExistsTable(conn, "processed_studies")) {
      processed_dois <- tbl(conn, "processed_studies") %>%
        distinct(DOI) %>%
        dplyr::pull(DOI)  # Extract distinct DOIs from the processed_studies table
    } else {
      message("processed_studies table does not exist. It will be created.")  # Message if the table doesn't exist
    }
    
    # Filter new data to only include studies not yet processed
    new_data_filtered <- new_data %>% filter(!DOI %in% processed_dois)
    
    # Check if there is new data to process
    if(nrow(new_data_filtered) > 0) {
      print("Processing new study data...")
      
      # Process Keywords and Authors separately using the respective function
      process_and_cache_new_data(new_data_filtered, "Keywords", conn)
      process_and_cache_new_data(new_data_filtered, "Authors", conn)
      print("Data successfully processed and stored.")
      
      # Update the 'processed_studies' table with new DOIs
      new_processed <- new_data_filtered %>% select(DOI) %>% distinct()
      if(dbExistsTable(conn, "processed_studies")) {
        DBI::dbWriteTable(conn, "processed_studies", new_processed, append = TRUE, row.names = FALSE)
      } else {
        DBI::dbWriteTable(conn, "processed_studies", new_processed, row.names = FALSE)
      }
    } else {
      print("No new study data found. Skipping processing.")  # If no new data, print a message and skip processing
    }
  })
  
  # Shared reactive values across all modules
  shared_data <- reactiveValues(
    study_choices = NULL, # Holds filtered data based on clicked node
    filtered_data = NULL, # Holds filtered data based on selected dataverse
    selected_dataverse = NULL, # Holds the input from the network tab (selected dataverse)
    file_list = NULL,  # Holds the file list after fetching data based on DOI
    full_paths = list() # Holds the full paths of the files
  )
  
  # Initialize the network tab server logic with study_data
  explorer_list$networkTab_server(input, output, session, 
                                  study_data, shared_data, conn)
  
  # Initialize the data review tab server logic with study_data
  datareviewTab_server("data", study_data, shared_data)
  
  # Clean up: disconnect from the database when the session ends.
  session$onSessionEnded(function() {
    dbDisconnect(conn)  # Ensure the database connection is closed when the session ends
  })
}
