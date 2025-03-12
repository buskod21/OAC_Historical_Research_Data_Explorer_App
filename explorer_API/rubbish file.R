process_and_cache_new_data <- function(raw_data, input_event_type, conn) {
  # Validate input_event_type
  if (!(input_event_type %in% c("Keywords", "Authors"))) {
    stop("input_event_type must be either 'Keywords' or 'Authors'")
  }

  # Determine dynamic table names based on input_event_type
  table_prefix <- tolower(input_event_type)
  nodes_table <- paste0(table_prefix, "_node")
  edges_table <- paste0(table_prefix, "_edge")

  # Initialize global keyword-to-node mapping (persisted for reusability)
  global_mapping <- list()

  # Get unique Dataverses
  unique_dataverses <- unique(updated_data$DataverseName)

  # Initialize lists to store nodes and edges
  all_nodes <- list()
  all_edges <- list()

  # Generate a consistent color mapping across Dataverses using 'Set3' palette
  dataverse_colors <- RColorBrewer::brewer.pal(length(unique_dataverses), "Set3")
  color_map <- setNames(dataverse_colors, unique_dataverses)

  # Process each Dataverse separately
  for (dataverse in unique_dataverses) {
    message("Processing Dataverse: ", dataverse)
    input_event_type <-"Keywords"

    dv_data <- updated_data %>% dplyr::filter(DataverseName == dataverse)

    # Process either Keywords or Authors based on input_event_type
    if (input_event_type == "Keywords") {
      events <- dv_data$Keywords %>%
        stringr::str_split(";\\s*") %>%
        unlist() %>%
        unique() %>%
        .[!grepl("\\bAgricultural Science(s)?\\b", ., ignore.case = TRUE)] %>%
        na.omit() %>%
        sort()

    } else if (input_event_type == "Authors") {
      events <- dv_data$Authors %>%
        stringr::str_split(";\\s*") %>%
        unlist() %>%
        unique() %>%
        stringr::str_to_title() %>%
        na.omit() %>%
        sort()
    }

    # Create or fetch global node IDs for the keywords/authors
    for (i in seq_along(events)) {
      event <- events[i]

      # Check if the event already has a global node ID (from previous Dataverses)
      node_id <- if (event %in% names(global_mapping)) {
        global_mapping[[event]]
      } else {
        # If not, assign a new node ID and update the global mapping
        new_node_id <- length(global_mapping) + 1
        global_mapping[[event]] <- new_node_id
        new_node_id
      }

      # Process studies related to the keyword/author
      matched_papers <- dv_data %>%
        dplyr::filter(stringr::str_detect(
          tolower(.data[[input_event_type]]),
          fixed(tolower(event))
        ))

      studies_count <- nrow(matched_papers)
      year_range <- if (studies_count > 0) {
        paste(min(matched_papers$PublicationDate, na.rm = TRUE), "to",
              max(matched_papers$PublicationDate, na.rm = TRUE))
      } else {
        "No studies found"
      }

      # Create the node for this event
      node <- tibble::tibble(
        id = node_id,  # Use global node ID
        label = event,
        node_group = input_event_type,
        title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range),
        color = color_map[[dataverse]],  # Keep color by Dataverse
        DataverseName = dataverse,
        study_count = studies_count,
        year_range = year_range,
        DOI = matched_papers$DOI[1]  # Include DOI
      )

      # Append the node to the list of nodes
      all_nodes <- append(all_nodes, list(node))
    }

    # Create edges within and across Dataverses for shared keywords/authors
    for (i in 1:(length(events) - 1)) {
      for (j in (i + 1):length(events)) {
        event_1 <- events[i]
        event_2 <- events[j]

        # Check if both events exist in the current Dataverse
        edges <- tibble::tibble(
          from = global_mapping[[event_1]],
          to = global_mapping[[event_2]],
          count = 1
        )

        all_edges <- append(all_edges, list(edges))
      }
    }
  }

  # Combine nodes and edges from all Dataverses into single data frames
  all_nodes_df <- dplyr::bind_rows(all_nodes) %>% dplyr::distinct()
  all_edges_df <- dplyr::bind_rows(all_edges) %>% dplyr::distinct()

  # Store the nodes and edges in the database using dynamic table names
  DBI::dbWriteTable(conn, nodes_table, all_nodes_df, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(conn, edges_table, all_edges_df, append = TRUE, row.names = FALSE)

  message("Processing complete. Data has been stored in tables: ",
          nodes_table, " and ", edges_table)
}
