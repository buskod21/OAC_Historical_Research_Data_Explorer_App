fetch_all_datasets <- function(id) {
  
  base_url <- "https://borealisdata.ca/api/dataverses/"
  
  results <- tibble()  # Create an empty tibble to store results
  
  # Construct the URL for the API request
  url <- paste0(base_url, id, "/contents")
  
  # Fetch the data from the API
  response <- tryCatch({
    request(url) %>%
      req_headers(`X-Dataverse-key` = api_token) %>%
      req_perform()
  }, error = function(e) {
    message("Request failed for Dataverse ", id, ": ", e$message)
    return(NULL)
  })
  
  # If the request is successful and response is not empty
  if (!is.null(response) && response$status == 200) {
    dataverse_raw_data <- fromJSON(rawToChar(response$body), flatten = TRUE)
    
    # Check if the data is not empty or NULL
    if (!is.null(dataverse_raw_data$data) && length(dataverse_raw_data$data) > 0) {
      
      # Extract datasets (filter type == "dataset")
      datasets <- dataverse_raw_data$data %>%
        filter(type == "dataset") 
      
      # Add the datasets to the results
      results <- bind_rows(results, datasets)
      
      # Recursively process any nested dataverses
      dataverses <- dataverse_raw_data$data %>% filter(type == "dataverse")
      if (nrow(dataverses) > 0) {
        for (dv_id in dataverses$id) {
          # Recursively call fetch_all_datasets for nested dataverses
          results <- bind_rows(results, fetch_all_datasets(dv_id))
        }
      }
      
      # After recursion, handle persistent_id creation
      results <- results %>%
        mutate(
          persistent_id = ifelse(
            !is.na(protocol) & !is.na(authority) & !is.na(identifier),
            paste0(protocol, ":", authority, "/", identifier),
            NA
          )
        ) %>%
        filter(!is.na(persistent_id))  # Keep only rows where persistent_id is not NA
    } else {
      message("No data available for ID ", id)
    }
  } else {
    message("Failed to fetch data for Dataverse ", id, ": HTTP status ", response$status)
  }
  
  # Return only the persistent_id column, even if some values are NA
  return(results)
}


# Call the function for the 'ugardr' dataverse (replace with actual ID)
final <- fetch_all_datasets("ugardr")
persistent_id <- tibble(persistent_id = final$persistent_id) %>%
  mutate(persistent_id = persistent_id) 
details <-fetch_study_details(persistent_id)


fetch_all_datasets <- function(id) {
  base_url <- "https://borealisdata.ca/api/dataverses/"
  results <- tibble()  # Create an empty tibble to store results
  
  # Construct the API URL
  url <- paste0(base_url, id, "/contents")
  
  # Fetch the data
  response <- tryCatch({
    request(url) %>%
      req_headers(`X-Dataverse-key` = api_token) %>%
      req_perform()
  }, error = function(e) {
    message("Request failed for Dataverse ", id, ": ", e$message)
    return(NULL)
  })
  
  # Check if the response is valid
  if (!is.null(response) && response$status == 200) {
    dataverse_raw_data <- fromJSON(rawToChar(response$body), flatten = TRUE)
    
    if (!is.null(dataverse_raw_data$data) && length(dataverse_raw_data$data) > 0) {
      
      # Extract datasets
      datasets <- dataverse_raw_data$data %>%
        filter(type == "dataset") %>%
        mutate(dataverse_title = dataverse_raw_data$data$title[1], 
               dataverse_id = as.character(id))
      
      # Add datasets to results
      results <- bind_rows(results, datasets)
      
      # Process nested dataverses
      dataverses <- dataverse_raw_data$data %>% filter(type == "dataverse")
      if (nrow(dataverses) > 0) {
        nested_results <- map_df(dataverses$id, fetch_all_datasets)
        results <- bind_rows(results, nested_results)
      }
    } else {
      message("No data available for ID ", id)
    }
  } else {
    message("Failed to fetch data for Dataverse ", id, ": HTTP status ", response$status)
  }
  
  # Compute persistent_id after gathering all data
  if (nrow(results) > 0) {
    results <- results %>%
      mutate(
        persistent_id = ifelse(
          !is.na(protocol) & !is.na(authority) & !is.na(identifier),
          paste0(protocol, ":", authority, "/", identifier),
          NA
        )
      ) %>%
      filter(!is.na(persistent_id))
  }
  
  return(results)
}



# Server Module for Network Tab
explorer_list$homeTab_server <- function(input, output, session, study_data) {
  
  # Render ValueBox for 'Total Studies'
  output$total_dataverses <- renderValueBox({
    
    # Compute the value only once study_data() is available
    req(study_data())  # Ensures that the data is available
    
    valueBox(value = tags$span(tags$b(class = "animated-box", style = "font-size: 60px; color: white;", "6")),
             subtitle = tags$span(style = "font-size: 20px; color: white;",
                                  "Dataverses in the OAC repository to explore!"),
             icon = icon("database"),
             color = "maroon",
             elevation = 2,
             gradient = TRUE)
  })
  
  # Render ValueBox for 'Total Studies'
  output$total_studies <- renderValueBox({
    
    # Compute the value only once study_data() is available
    req(study_data())  # Ensures that the data is available
    
    valueBox(value = tags$span(tags$b(class = "animated-box",
                                      style = "font-size: 60px;",
                                      nrow(study_data()))),
             subtitle = tags$span(style = "font-size: 20px;",
                                  "Research papers available for exploration!"),
             icon = icon("book"),
             color = "info",
             elevation = 2,
             gradient = TRUE)
  })
  
  # Render ValueBox for 'Unique Authors'
  output$unique_authors <- renderValueBox({
    # Compute the value only once study_data() is available
    req(study_data())  # Ensures that the data is available
    
    # Calculate the number of unique authors once the data is ready
    author_count <- length(unique(unlist(strsplit(study_data()$Authors, ";"))))
    
    valueBox(value = tags$span(tags$b(class = "animated-box",
                                      style = "font-size: 60px;",
                                      author_count)),
             subtitle = tags$span(style = "font-size: 20px;", "Researchers whose work you can discover!"),
             icon = icon("user"),
             color = "purple",
             elevation = 2,
             gradient = TRUE)
  })
  # Render ValueBox for 'Unique Keywords'
  output$unique_keywords <- renderValueBox({
    
    # Compute the value only once study_data() is available
    study_data = study_data()
    req(study_data())  # Ensures that the data is available
    
    valueBox(value = tags$span(tags$b(class = "animated-box",
                                      style = "font-size: 60px;",
                                      length(unique(unlist(strsplit(study_data()$Keywords, ";")))))),
             subtitle = tags$span(style = "font-size: 20px;",
                                  "Topics and themes to guide your research!"),
             icon = icon("tags"),
             color = "success",
             elevation = 2,
             gradient = TRUE)
  })
  
  # Render ValueBox for 'Total Files'
  output$total_file <- renderValueBox({
    
    # Compute the value only once study_data() is available
    req(study_data())  # Ensures that the data is available
    
    valueBox(value = tags$span(tags$b(class = "animated-box",
                                      style = "font-size: 60px;",
                                      length(unique(unlist(strsplit(study_data()$FileList, ";")))))),
             subtitle = tags$span(style = "font-size: 20px;",
                                  "Metadata and Data files to explore!"),
             icon = icon("file"),
             color = "danger",
             elevation = 2,
             gradient = TRUE)
  })
  
  # When the 'go_to_network' image or button is clicked, navigate to the 'network_tab'
  observeEvent(input$go_to_network, {
    updateNavbarTabs(session= session, 
                     inputId = "navmenu", 
                     selected = "network_tab")
  })
  
  # When the 'go_to_borealis' image or button is clicked, navigate to the 'borealis_tab'
  observeEvent(input$go_to_borealis, {
    updateNavbarTabs(session= session,  
                     inputId = "navmenu", 
                     selected = "borealis_tab")
  })
}