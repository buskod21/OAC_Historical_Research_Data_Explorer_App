fetch_all_datasets <- function(id, parent_title = NULL) {
  
  base_url <- "https://borealisdata.ca/api/dataverses/"
  
  results <- tibble()  # Create an empty tibble to store results
  
  # Construct the URL for the API request
  url <- paste0(base_url, id, "/contents")
  
  # Fetch the data from the API
  response <- tryCatch({
    request(url) %>%
      req_headers(`X-Dataverse-key` = Api_token) %>%
      req_perform()
  }, error = function(e) {
    message("Request failed for Dataverse ", id, ": ", e$message)
    return(NULL)
  })
  
  # If the request is successful and response is not empty
  if (!is.null(response) && response$status == 200) {
    dataverse_raw_data <- fromJSON(rawToChar(response$body), flatten = TRUE) %>%
      as_tibble()
    
    # Retrieve metadata title
    title <- tryCatch({
      meta_url <- paste0(base_url, id)
      meta_response <- request(meta_url) %>%
        req_headers(`X-Dataverse-key` = Api_token) %>%
        req_perform()
      
      meta_data <- fromJSON(rawToChar(meta_response$body), flatten = TRUE)
      meta_data$data$name  # Extracting the title
      
    }, error = function(e) {
      message("Failed to retrieve title for Dataverse ", id, ": ", e$message)
      return(NA_character_)  # Always return character, even if NA
    })
    
    # Set the hierarchical title
    current_title <- if (!is.null(parent_title)) {
      paste(parent_title, "-", title)
    } else {
      title
    }
    
    # Check if the data is not empty or NULL
    if (!is.null(dataverse_raw_data$data) && length(dataverse_raw_data$data) > 0) {
      
      # Extract datasets (filter type == "dataset")
      datasets <- dataverse_raw_data$data %>%
        filter(type == "dataset") %>%
        mutate(dataverse_title = as.character(current_title))  # Ensure character type
      
      # Add the datasets to the results
      results <- bind_rows(results, datasets)
      
      # Recursively process any nested dataverses
      dataverses <- dataverse_raw_data$data %>% filter(type == "dataverse")
      if (nrow(dataverses) > 0) {
        for (dv_id in dataverses$id) {
          # Recursively call fetch_all_datasets for nested dataverses
          results <- bind_rows(results, fetch_all_datasets(dv_id, current_title))
        }
      }
      
      # Ensure 'protocol', 'authority', and 'identifier' exist before using them
      results <- results %>%
        mutate(
          persistent_id = ifelse(
            !is.na(protocol) & !is.na(authority) & !is.na(identifier),
            paste0(protocol, ":", authority, "/", identifier),
            NA_character_  # Ensure missing values remain character type
          )
        ) %>%
        filter(!is.na(persistent_id))  # Keep only rows where persistent_id is not NA
       
      
    } else {
      message("No data available for ID ", id)
    }
  } else {
    message("Failed to fetch data for Dataverse ", id, ": HTTP status ", response$status)
  }
  
  # Return the results
  return(results)
}


d_data<-fetch_all_datasets("ugardr") 
write.csv(d_data, "final.csv")

pidTitle <- d_data %>%
  select(persistent_id, dataverse_title) %>%
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
          DataverseName = data$dataverse_title[i]
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

detail_2 <- fetch_study_details(pidTitle)
write.csv(detail_2, file = "agri-environment all dataverse info.csv", row.names = FALSE)
