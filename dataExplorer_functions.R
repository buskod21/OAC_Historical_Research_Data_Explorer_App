Api_token <- Sys.getenv("API_TOKEN")  # Retrieve the API token from environment variables

# Function to create and perform the request to get oac_dataverse content
## The IDs for each of the dataverse is extracted for the study detail API call
fetch_oac_info <- function() {

  # Define full url for the API call
  oac_fullurl <- "https://borealisdata.ca/api/dataverses/oac/contents"


  # Perform the API request
  oac_response <- request(oac_fullurl) %>%
    req_headers(`X-Dataverse-key` = Api_token) %>%
    req_perform()

  # Check the response status
  if (oac_response$status == 200) {
    return(tibble(fromJSON(rawToChar(oac_response$body), flatten = TRUE)$data))
  } else {
    message("Error in req_perform(): HTTP status ", oac_response$status)
    return(tibble())
  }
}

# Fetch the content of studies in each dataverse; This function returns the DOIs
fetch_dataverse_data <- function(ids) {
  Base_url <- "https://borealisdata.ca/api/dataverses/"

  results <- lapply(ids, function(id) {
    dataverse_fullurl <- paste0(Base_url, id, "/contents")

    tryCatch({
      response <- request(dataverse_fullurl) %>%
        req_headers(`X-Dataverse-key` = Api_token) %>%
        req_perform()

      if (response$status == 200) {
        dataverse_raw_data <- fromJSON(rawToChar(response$body), flatten = TRUE)

        if (!is.null(dataverse_raw_data$data) && nrow(dataverse_raw_data$data) > 0) {
          data <- as_tibble(dataverse_raw_data$data) %>%
            mutate(dataverse_id = as.character(id)) %>%  # Directly use `id`
            mutate(
              # Handle missing components of persistent_id
              persistent_id = ifelse(
                !is.na(protocol) & !is.na(authority) & !is.na(identifier),
                paste0(protocol, ":", authority, "/", identifier),
                NA
              )
            ) %>%  # Vectorized creation of persistent ID
            filter(!is.na(persistent_id)) %>%  # Remove rows with NA in persistent_id column
            select(persistent_id)  # Only keep persistent_id column

          return(data)
        } else {
          message("No data or empty data for ID ", id)
          return(NULL)
        }
      } else {
        message("Failed to fetch data for ID ", id, ": HTTP status ", response$status)
        return(NULL)
      }
    }, error = function(e) {
      message("Error during request for ID ", id, ": ", e$message)
      return(NULL)
    })
  })

  # Combine all tibbles into one, ignoring NULLs
  combined_data <- bind_rows(results)

  return(combined_data)
}


# Function to fetch metadata of each dataverse in OAC repo based on their DOIs
## DOIs are gotten from the fetch_dataverse_data()

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
          DOI = persistent_id  # Ensure DOI is linked correctly
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


#  This function download the data for each study based on the study DOIs
access_data <- function(doi) {
  Acess_data_url <- "https://borealisdata.ca/api/access/dataset/"

  # Full URL construction and file path setup
  full_url3 <- paste0(Acess_data_url, ":persistentId/?persistentId=", doi)
  # zip_path <- file.path(unique_dir, "downloaded_data.zip")

  # Make the API request and download the ZIP file
  tryCatch({
    response <- request(full_url3) %>%
      req_headers(`X-Dataverse-key`= Api_token) %>%
      req_perform()

    if (response$status_code == 200) {

      # Get the content of the response as a raw vector
      zip_content <- resp_body_raw(response)

      # Use tempfile to create a temporary file for the zip content
      temp_zip <- tempfile(fileext = ".zip")

      # Write the raw vector to the temporary file
      writeBin(zip_content, temp_zip)

      # Use a temporary directory to extract the files
      temp_unzip_dir <- tempfile()

      # Extract the files to the temporary directory
      unzip(temp_zip, exdir = temp_unzip_dir)

      # List files in the temporary directory
      file_list <- list.files(temp_unzip_dir, full.names = TRUE)

      # Clean up the temporary zip file
      unlink(temp_zip)

      # Process the files as needed (this example simply returns the list of files)
      return(file_list)
    }
    else {
      return(NULL)
    }

  }, error = function(e) {
    print("Restricted data not accessible.")
  })
}


# Function to filter for .txt file and .tab/.csv file in the filelist and extract just the basename
filter_filelist <- function(file_list, is_txt) {
  if (is_txt) {
    # Filter for .txt files with "README" in the basename
    filtered_files <- file_list[grep("REA.*\\.txt$", basename(file_list), ignore.case = TRUE)]
  } else {
    # Filter for .tab or .csv files
    filtered_files <- file_list[grep("(\\.tab$|\\.csv$)", basename(file_list), ignore.case = TRUE)]
  }
  return(filtered_files)
}

# Check if a column contains any letters
contains_letters <- function(x) {
  any(grepl("[a-zA-Z]", x))
}


# Create (or connect to) the SQLite database
get_db_conn <- function(db_file = "Explorer_cache") {
  dbConnect(SQLite(), db_file)
}


# --- Functions to Manage the SQLite Cache ---
# # The functions does the API, extracts new DOIs and updates the research data

cache_raw_data <- function(conn) {

  # Check if an update_info table exists.
  update_needed <- TRUE  # Assume we need an update unless proven otherwise.
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
    # Return the cached research data without updating.
    return(dbReadTable(conn, "research_data"))
  }

  # If we need to update or if no cached data exists, proceed to fetch and update.
  message("Updating cache with new data...")

  # Initialize an empty tibble for storing all results.
  result_details <- tibble()

  # Fetch OAC dataverse content
  OAC_info <- fetch_oac_info()

  # Assuming 'OAC_info' contains the IDs and titles
  ids <- as.character(OAC_info$id)
  id_to_title <- setNames(OAC_info$title, OAC_info$id)

  # If "research_data" table doesn't exist, create and store all data in the database.
  if (!"research_data" %in% dbListTables(conn)) {
    message("No existing data found. Initializing cache with fetched data.")
    for (id in ids) {
      message("Processing ID: ", id)

      # Fetch dataverse data and study details for the current ID
      dataverse_data <- fetch_dataverse_data(id)
      study_details  <- fetch_study_details(dataverse_data)

      # Append the dataverse name to the data using the title
      dataverse_name <- id_to_title[id]
      study_details <- mutate(study_details, DataverseName = dataverse_name)

      # Combine with previous results
      result_details <- bind_rows(result_details, study_details)
    }
    # Store the result data into the database for the first time
    dbWriteTable(conn, "research_data", result_details, overwrite = TRUE)
  } else {
    # If the "research_data" table exists, get existing DOIs from the database
    existing_data <- dbReadTable(conn, "research_data")
    existing_dois <- existing_data$DOI

    # Initialize a tibble for new rows
    new_rows <- tibble()

    # Loop through each ID and compare DOI to existing records
    for (id in ids) {
      message("Processing ID: ", id)

      # Fetch dataverse data for the current ID
      dataverse_data <- fetch_dataverse_data(id)

      # Get the DOIs from the fetched data
      new_dois <- dataverse_data$persistent_id

      # Filter out the DOIs that already exist in the database
      new_dois_to_process <- new_dois[!(new_dois %in% existing_dois)]

      # Process the new DOIs
      if (length(new_dois_to_process) > 0) {
        message("Found new DOIs: ", paste(new_dois_to_process, collapse = ", "))

        # Convert new_dois_to_process into a dataframe
        new_dois_df <- tibble(persistent_id = new_dois_to_process)

        # Fetch and process the study details for the new DOIs
        new_study_details <- fetch_study_details(new_dois_df)

        # Append the dataverse name to the data using the title
        dataverse_name <- id_to_title[id]
        new_study_details <- mutate(new_study_details, DataverseName = dataverse_name)

        # Append new rows to the result
        new_rows <- bind_rows(new_rows, new_study_details)
      } else {
        message("No new DOIs found.")
      }
    }

    # If there are new rows, append them to the existing research_data table
    if (nrow(new_rows) > 0) {
      message("Appending ", nrow(new_rows), " new record(s) to the research_data table.")
      dbWriteTable(conn, "research_data", new_rows, append = TRUE)
    } else {
      message("No new records to append.")
    }
  }

  # Update the timestamp in the update_info table.
  current_time <- tibble(last_update = as.character(Sys.time()))
  dbWriteTable(conn, "update_info", current_time, overwrite = TRUE)

  # Return the complete cached data.
  updated_data <- dbReadTable(conn, "research_data")
  return(updated_data)
}


# Function to extract and process nodes and edges for Keywords and Authors
process_and_cache_new_data <- function(raw_data, input_event_type, conn) {
  if (!(input_event_type %in% c("Keywords", "Authors"))) {
    stop("input_event_type must be either 'Keywords' or 'Authors'")
  }

  table_prefix <- tolower(input_event_type)
  nodes_table <- paste0(table_prefix, "_node")
  edges_table <- paste0(table_prefix, "_edge")
  color_table <- "dataverse_colors"  # Table for storing colors

  message("Processing all Dataverses...")

  all_data <- raw_data
  unique_dataverses <- unique(all_data$DataverseName)

  # Ensure color table exists
  if (!DBI::dbExistsTable(conn, color_table)) {
    DBI::dbExecute(conn, sprintf("CREATE TABLE %s (DataverseName TEXT PRIMARY KEY, Color TEXT)", color_table))
  }

  # Fetch existing color mappings from the database
  existing_colors <- DBI::dbReadTable(conn, color_table)
  existing_color_map <- setNames(existing_colors$Color, existing_colors$DataverseName)

  # Identify new Dataverses that need color assignments
  new_dataverses <- setdiff(unique_dataverses, names(existing_color_map))

  if (length(new_dataverses) > 0) {
    available_colors <- RColorBrewer::brewer.pal(min(length(unique_dataverses), 12), "Set3")
    assigned_colors <- unique(existing_colors$Color)
    unused_colors <- setdiff(available_colors, assigned_colors)  # Avoid reusing colors

    # Assign colors to new Dataverses
    new_color_map <- setNames(rep(NA, length(new_dataverses)), new_dataverses)
    for (dataverse in new_dataverses) {
      new_color_map[[dataverse]] <- ifelse(length(unused_colors) > 0,
                                           unused_colors[1],
                                           sample(available_colors, 1))
      unused_colors <- unused_colors[-1]  # Remove assigned color from available pool
    }

    # Convert new colors to a data frame and insert into database
    new_colors_df <- tibble::tibble(DataverseName = names(new_color_map), Color = unname(new_color_map))
    DBI::dbWriteTable(conn, color_table, new_colors_df, append = TRUE, row.names = FALSE)

    # Merge with existing color map
    existing_color_map <- c(existing_color_map, new_color_map)
  }

  # Create final color mapping
  color_map <- existing_color_map

  # Process events (same logic as before)
  if (input_event_type == "Keywords") {
    events <- all_data$Keywords %>%
      stringr::str_split(";\\s*") %>%
      unlist() %>%
      na.omit() %>%
      stringr::str_replace_all("[^a-zA-Z0-9\\s-]", "") %>%
      stringr::str_squish() %>%
      stringr::str_to_title() %>%
      .[!grepl("\\bAgricultural Science(s)?\\b", ., ignore.case = TRUE)] %>%
      unique() %>%
      sort()
  } else {
    events <- all_data$Authors %>%
      stringr::str_split(";\\s*") %>%
      unlist() %>%
      na.omit() %>%
      sort() %>%
      stringr::str_replace_all("\\.", "") %>%
      stringr::str_to_title() %>%
      stringr::str_replace_all("\\b([A-Z])\\b", "") %>%
      unique() %>%
      stringr::str_trim()
  }

  # Process nodes
  nodes <- purrr::map_df(seq_along(events), function(i) {
    event <- events[i] %>%
      stringr::str_squish() %>%
      stringr::str_to_title()

    matched_papers <- all_data %>%
      dplyr::filter(stringr::str_detect(
        stringr::str_to_lower(.data[[input_event_type]]),
        fixed(stringr::str_to_lower(event))
      ))

    if (nrow(matched_papers) == 0) {
      return(NULL)
    }

    studies_count <- nrow(matched_papers)
    year_range <- paste(min(matched_papers$PublicationDate, na.rm = TRUE), "to",
                        max(matched_papers$PublicationDate, na.rm = TRUE))

    dataverse_names <- unique(matched_papers$DataverseName)
    node_color <- ifelse(length(dataverse_names) > 1, "gray", color_map[dataverse_names])

    tibble::tibble(
      label = event,
      node_group = input_event_type,
      title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range),
      color = node_color,
      DataverseName = paste(unique(dataverse_names), collapse = ", "),
      study_count = studies_count,
      year_range = year_range,
      DOI = paste(unique(matched_papers$DOI), collapse = "; ")
    )
  }) %>%
    dplyr::mutate(label = stringr::str_squish(label)) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(
      node_group = first(node_group),
      title = first(title),
      color = first(color),
      DataverseName = paste(unique(DataverseName), collapse = ", "),
      study_count = sum(study_count, na.rm = TRUE),
      year_range = paste(unique(year_range), collapse = " | "),
      DOI = paste(unique(DOI), collapse = "; ")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = row_number())

  event_occurrences <- lapply(nodes$label, function(ev) which(grepl(ev, all_data[[input_event_type]], ignore.case = TRUE)))
  node_pairs <- combn(nrow(nodes), 2, simplify = FALSE)
  edges_list <- purrr::map(node_pairs, function(pair) {
    i <- pair[1]
    j <- pair[2]
    count <- length(intersect(event_occurrences[[i]], event_occurrences[[j]]))
    if (count > 0) tibble::tibble(from = nodes$id[i], to = nodes$id[j], weight = count) else NULL
  }) %>% purrr::compact()

  edges <- dplyr::bind_rows(edges_list)

  DBI::dbWriteTable(conn, nodes_table, nodes, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(conn, edges_table, edges, append = TRUE, row.names = FALSE)

  message("Processing complete. Data stored in: ", nodes_table, " and ", edges_table)
}




# process_and_cache_new_data <- function(raw_data, input_event_type, conn) {
#   if (!(input_event_type %in% c("Keywords", "Authors"))) {
#     stop("input_event_type must be either 'Keywords' or 'Authors'")
#   }
#
#   table_prefix <- tolower(input_event_type)
#   nodes_table <- paste0(table_prefix, "_node")
#   edges_table <- paste0(table_prefix, "_edge")
#
#   message("Processing all Dataverses...")
#
#   all_data <- raw_data
#
#   unique_dataverses <- unique(all_data$DataverseName)
#   dataverse_colors <- RColorBrewer::brewer.pal(min(length(unique_dataverses), 12), "Set3")
#   color_map <- setNames(dataverse_colors, unique_dataverses)
#
#   if (input_event_type == "Keywords") {
#     events <- all_data$Keywords %>%
#       stringr::str_split(";\\s*") %>%
#       unlist() %>%
#       na.omit() %>%
#       stringr::str_replace_all("[^a-zA-Z0-9\\s-]", "") %>%
#       stringr::str_squish() %>%
#       stringr::str_to_title() %>%
#       .[!grepl("\\bAgricultural Science(s)?\\b", ., ignore.case = TRUE)] %>%
#       unique() %>%
#       sort()
#   } else {
#     events <- all_data$Authors %>%
#       stringr::str_split(";\\s*") %>%
#       unlist() %>%
#       na.omit() %>%
#       sort() %>%
#       stringr::str_replace_all("\\.", "") %>%
#       stringr::str_to_title() %>%
#       stringr::str_replace_all("\\b([A-Z])\\b", "") %>%
#       unique() %>%
#       stringr::str_trim()
#   }
#
#   # Process nodes (authors or keywords)
#   nodes <- purrr::map_df(seq_along(events), function(i) {
#     event <- events[i] %>%
#       stringr::str_squish() %>%  # Remove extra spaces
#       stringr::str_to_title()  # Standardize capitalization
#
#     matched_papers <- all_data %>%
#       dplyr::filter(stringr::str_detect(
#         stringr::str_to_lower(.data[[input_event_type]]),
#         fixed(stringr::str_to_lower(event))
#       ))
#
#     if (nrow(matched_papers) == 0) {
#       return(NULL)
#     }
#
#     studies_count <- nrow(matched_papers)
#     year_range <- paste(min(matched_papers$PublicationDate, na.rm = TRUE), "to",
#                         max(matched_papers$PublicationDate, na.rm = TRUE))
#
#     dataverse_names <- unique(matched_papers$DataverseName)
#     node_color <- ifelse(length(dataverse_names) > 1, "gray", color_map[dataverse_names])
#
#     tibble::tibble(
#       label = event,
#       node_group = input_event_type,
#       title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range),
#       color = node_color,
#       DataverseName = paste(unique(dataverse_names), collapse = ", "),
#       study_count = studies_count,
#       year_range = year_range,
#       DOI = paste(unique(matched_papers$DOI), collapse = "; ")
#     )
#   }) %>%
#     dplyr::mutate(label = stringr::str_squish(label)) %>%  # Remove spaces again for safety
#     dplyr::group_by(label) %>%
#     dplyr::summarise(
#       node_group = first(node_group),
#       title = first(title),
#       color = first(color),
#       DataverseName = paste(unique(DataverseName), collapse = ", "),
#       study_count = sum(study_count, na.rm = TRUE),
#       year_range = paste(unique(year_range), collapse = " | "),
#       DOI = paste(unique(DOI), collapse = "; ")
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(id = row_number())  # Assign unique IDs after merging
#
#
#   event_occurrences <- lapply(nodes$label, function(ev) which(grepl(ev, all_data[[input_event_type]], ignore.case = TRUE)))
#   node_pairs <- combn(nrow(nodes), 2, simplify = FALSE)
#   edges_list <- purrr::map(node_pairs, function(pair) {
#     i <- pair[1]
#     j <- pair[2]
#     count <- length(intersect(event_occurrences[[i]], event_occurrences[[j]]))
#     if (count > 0) tibble::tibble(from = nodes$id[i], to = nodes$id[j], weight = count) else NULL
#   }) %>% purrr::compact()
#
#   edges <- dplyr::bind_rows(edges_list)
#
#   DBI::dbWriteTable(conn, nodes_table, nodes, append = TRUE, row.names = FALSE)
#   DBI::dbWriteTable(conn, edges_table, edges, append = TRUE, row.names = FALSE)
#
#   message("Processing complete. Data stored in: ", nodes_table, " and ", edges_table)
# }

