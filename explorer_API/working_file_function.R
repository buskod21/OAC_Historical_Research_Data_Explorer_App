# Function to extract datasets information using recursive function

fetch_all_datasets <- function(id, layer_titles = character()) {
  base_url <- "https://borealisdata.ca/api/dataverses/"
  results <- tibble()
  
  url <- paste0(base_url, id, "/contents")
  response <- tryCatch({
    request(url) %>%
      req_headers(`X-Dataverse-key` = Api_token) %>%
      req_perform()
  }, error = function(e) {
    message("Request failed for Dataverse ", id, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(response) && response$status == 200) {
    dataverse_raw_data <- fromJSON(rawToChar(response$body), flatten = TRUE) %>%
      as_tibble()
    
    title <- tryCatch({
      meta_url <- paste0(base_url, id)
      meta_response <- request(meta_url) %>%
        req_headers(`X-Dataverse-key` = Api_token) %>%
        req_perform()
      meta_data <- fromJSON(rawToChar(meta_response$body), flatten = TRUE)
      meta_data$data$name
    }, error = function(e) {
      message("Failed to retrieve title for Dataverse ", id, ": ", e$message)
      return(NA_character_)
    })
    
    current_layer_titles <- c(layer_titles, title)
    
    if (!is.null(dataverse_raw_data$data) && length(dataverse_raw_data$data) > 0) {
      datasets <- dataverse_raw_data$data %>%
        filter(type == "dataset") %>%
        mutate(
          Agri_Environment = current_layer_titles[1],
          College_Campus_Institution = ifelse(!is.na(current_layer_titles[2]), current_layer_titles[2], current_layer_titles[1]),
          Departments_ResearchCentres = ifelse(!is.na(current_layer_titles[3]),
                                              current_layer_titles[3],
                                              ifelse(!is.na(current_layer_titles[2]), current_layer_titles[2], current_layer_titles[1]))
        )
      
      results <- bind_rows(results, datasets)
      
      dataverses <- dataverse_raw_data$data %>% filter(type == "dataverse")
      if (nrow(dataverses) > 0) {
        for (dv_id in dataverses$id) {
          results <- bind_rows(results, fetch_all_datasets(dv_id, current_layer_titles))
        }
      }
      
      results <- results %>%
        mutate(
          persistent_id = ifelse(
            !is.na(protocol) & !is.na(authority) & !is.na(identifier),
            paste0(protocol, ":", authority, "/", identifier),
            NA_character_
          )
        ) %>%
        filter(!is.na(persistent_id)) 
      
    } else {
      message("No data available for ID ", id)
    }
  } else {
    message("Failed to fetch data for Dataverse ", id, ": HTTP status ", response$status)
  }
  
  return(results)
}


all <-fetch_all_datasets(16)

pidTitle <- all %>%
  select(persistent_id, College_Campus_Institution, Departments_ResearchCentres) %>%
  tibble()

fetch_study_details <- function(data) {
  if (!is.data.frame(data) || !"persistent_id" %in% names(data) || nrow(data) == 0) {
    warning("Invalid input: Ensure 'data' is a dataframe and contains 'persistent_id' column.")
    return(tibble())  # Return an empty tibble if input is invalid
  }
  
  BaseURL_details2 <- "https://borealisdata.ca/api/datasets/export?exporter=schema.org&persistentId="
  
  detailed_data_list <- vector("list", nrow(data))  # Preallocate for efficiency
  
  for (i in seq_len(nrow(data))) {
    persistent_id <- data$persistent_id[i]  # Extract persistent_id from dataframe
    Full_url_details <- paste0(BaseURL_details2, persistent_id)
    
    tryCatch({
      response_details <- request(Full_url_details) %>%
        req_headers(`X-Dataverse-key` = Api_token) %>%
        req_perform()
      
      if (response_details$status == 200) {
        detail_oac2 <- jsonlite::fromJSON(rawToChar(response_details$body), flatten = TRUE)
        
        # Extract and handle missing data safely
        detailed_data_list[[i]] <- tibble(
          Title = detail_oac2[["name"]],
          PublicationDate = detail_oac2[["datePublished"]],
          Authors = if (!is.null(detail_oac2[["author"]])) paste(detail_oac2[["author"]][["name"]], collapse = "; ") else NA,
          Affiliations = if (!is.null(detail_oac2[["author"]][["affiliation.name"]])) paste(detail_oac2[["author"]][["affiliation.name"]], collapse = "; ") else NA,
          Keywords = if (!is.null(detail_oac2[["keywords"]])) paste(detail_oac2[["keywords"]], collapse = "; ") else NA,
          Objectives = detail_oac2[["description"]],
          Citation = if (!is.null(detail_oac2[["citation"]])) paste(detail_oac2[["citation"]][["name"]], collapse = ", ") else NA,
          PeriodCovered = if (!is.null(detail_oac2[["temporalCoverage"]])) paste(detail_oac2[["temporalCoverage"]], collapse = "; ") else NA,
          StudyLocation = if (!is.null(detail_oac2[["spatialCoverage"]])) paste(detail_oac2[["spatialCoverage"]], collapse = ", ") else NA,
          Funder = if (!is.null(detail_oac2[["funder"]])) paste(detail_oac2[["funder"]][["name"]], collapse = "; ") else NA,
          FileList = if (!is.null(detail_oac2[["distribution"]])) paste(detail_oac2[["distribution"]][["name"]], collapse = "; ") else NA,
          DOI = persistent_id,  # Ensure DOI is linked correctly
          CollegeName = data$College_Campus_Institution[i],
          DepartmentName = data$Departments_ResearchCentres[i]
        )
      } else {
        warning(paste("Failed to fetch data for DOI:", persistent_id, "Status code:", response_details$status))
      }
    }, error = function(e) {
      message("Error fetching details for DOI ", persistent_id, ": ", e$message)
      detailed_data_list[[i]] <- tibble()  # Store an empty tibble on error
    })
  }
  
  # Combine all tibbles into one and remove duplicates
  detailed_data <- bind_rows(detailed_data_list) %>% unique()
  
  return(detailed_data)
}

all_details <-fetch_study_details(pidTitle)

# Create (or connect to) the SQLite database
get_db_conn <- function(db_file) {
  dbConnect(SQLite(), db_file)
}

connection <- get_db_conn("database_file.db")

cache_raw_data <- function(conn) {
  update_needed <- TRUE  # Assume we need an update unless proven otherwise
  
  # Check if update_info table exists and when it was last updated
  if ("update_info" %in% dbListTables(conn)) {
    update_info <- dbReadTable(conn, "update_info")
    if (nrow(update_info) > 0) {
      last_update <- as.POSIXct(update_info$last_update[1])
      time_diff <- difftime(Sys.time(), last_update, units = "hours")
      if (time_diff < 48) {
        update_needed <- FALSE
        message("Less than 48 hours since last update (", round(time_diff, 2), " hours). Using cached data.")
      }
    }
  }
  
  if (!update_needed && ("research_data" %in% dbListTables(conn))) {
    # Return cached data if no update is needed
    return(dbReadTable(conn, "research_data"))
  }
  
  message("Updating cache with new data...")
  
  # Step 1: Get all available dataset metadata
  all_info <- fetch_all_datasets(16)
  
  pidTitle <- all_info %>%
    select(persistent_id, College_Campus_Institution, Departments_ResearchCentres) %>%
    tibble()
  
  result_details <- fetch_study_details(pidTitle)
  
  # Step 2: Store or update the database
  new_rows <- tibble()  # Initialize to hold only new rows
  
  if (!"research_data" %in% dbListTables(conn)) {
    message("No existing data found. Initializing cache with fetched data.")
    dbWriteTable(conn, "research_data", result_details, overwrite = TRUE)
  } else {
    existing_data <- dbReadTable(conn, "research_data")
    existing_dois <- existing_data$DOI
    new_dois <- all_info$persistent_id
    
    # Filter for new DOIs
    new_dois_to_process <- new_dois[!(new_dois %in% existing_dois)]
    
    if (length(new_dois_to_process) > 0) {
      message("Found new DOIs: ", paste(new_dois_to_process, collapse = ", "))
      
      new_dois_df <- pidTitle %>% filter(persistent_id %in% new_dois_to_process)
      
      new_study_details <- fetch_study_details(new_dois_df)
      
      new_rows <- bind_rows(new_rows, new_study_details)
    } else {
      message("No new DOIs found.")
    }
    
    if (nrow(new_rows) > 0) {
      message("Appending ", nrow(new_rows), " new record(s) to the research_data table.")
      dbWriteTable(conn, "research_data", new_rows, append = TRUE)
    } else {
      message("No new records to append.")
    }
  }
  
  # Step 3: Update timestamp in update_info
  current_time <- tibble(last_update = as.character(Sys.time()))
  dbWriteTable(conn, "update_info", current_time, overwrite = TRUE)
  
  # Step 4: Return full cached dataset
  updated_data <- dbReadTable(conn, "research_data")
  return(updated_data)
}

cache_raw_data(connection)
