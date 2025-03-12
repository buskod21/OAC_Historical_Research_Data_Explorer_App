# process_and_cache_new_data <- function(raw_data, input_event_type, conn) {
#   if (!(input_event_type %in% c("Keywords", "Authors"))) {
#     stop("input_event_type must be either 'Keywords' or 'Authors'")
#   }
#
#   table_prefix <- tolower(input_event_type)
#   nodes_table <- paste0(table_prefix, "_node")
#   edges_table <- paste0(table_prefix, "_edge")
#
#   # Create table if it doesn't exist
#   if (!DBI::dbExistsTable(conn, nodes_table)) {
#     DBI::dbExecute(conn, paste0(
#       "CREATE TABLE ", nodes_table, " (
#         id INTEGER PRIMARY KEY,
#         label TEXT,
#         node_group TEXT,
#         title TEXT,
#         color TEXT,
#         DataverseName TEXT,
#         study_count INTEGER,
#         year_range TEXT,
#         DOI TEXT
#       )"
#     ))
#   }
#
#   if (!DBI::dbExistsTable(conn, edges_table)) {
#     DBI::dbExecute(conn, paste0(
#       "CREATE TABLE ", edges_table, " (
#         source INTEGER,
#         target INTEGER,
#         count INTEGER
#       )"
#     ))
#   }
#
#   # Retrieve existing DOIs and max ID to avoid duplicate IDs
#   existing_dois <- dbGetQuery(conn, paste0("SELECT DISTINCT DOI FROM ", nodes_table))$DOI
#   max_existing_id <- dbGetQuery(conn, paste0("SELECT MAX(id) FROM ", nodes_table))[[1]]
#   if (is.na(max_existing_id)) max_existing_id <- 0  # Handle case when table is empty
#
#   new_data <- raw_data %>% filter(!(DOI %in% existing_dois))
#   if (nrow(new_data) == 0) {
#     message("No new data found. Skipping processing.")
#     return(NULL)
#   }
#
#   unique_dataverses <- unique(new_data$DataverseName)
#   all_nodes <- list()
#   all_edges <- list()
#   dataverse_colors <- rainbow(length(unique_dataverses), alpha = 0.5, start = 0.75, end = 0.1)
#   color_map <- setNames(dataverse_colors, unique_dataverses)
#
#   for (dataverse in unique_dataverses) {
#     dv_data <- new_data %>% dplyr::filter(DataverseName == dataverse)
#
#     if (input_event_type == "Keywords") {
#       events <- dv_data$Keywords %>%
#         stringr::str_split(";\\s*") %>%
#         unlist() %>%
#         unique() %>%
#         .[!grepl("\\bAgricultural Science(s)?\\b", ., ignore.case = TRUE)] %>%
#         na.omit() %>%
#         sort()
#     } else if (input_event_type == "Authors") {
#       events <- dv_data$Authors %>%
#         stringr::str_split(";\\s*") %>%
#         unlist() %>%
#         unique() %>%
#         stringr::str_to_title() %>%
#         na.omit() %>%
#         sort()
#     }
#
#     nodes <- purrr::map_df(seq_along(events), function(i) {
#       event <- events[i]
#       matched_papers <- dv_data %>%
#         dplyr::filter(stringr::str_detect(
#           tolower(.data[[input_event_type]]),
#           fixed(tolower(event))
#         ))
#
#       studies_count <- nrow(matched_papers)
#       year_range <- if (studies_count > 0) {
#         paste(min(matched_papers$PublicationDate, na.rm = TRUE), "to",
#               max(matched_papers$PublicationDate, na.rm = TRUE))
#       } else {
#         "No studies found"
#       }
#
#       tibble::tibble(
#         id = max_existing_id + i,  # Ensure unique ID
#         label = event,
#         node_group = input_event_type,
#         title = paste("Study Count:", studies_count, "<br>", "Year Range:", year_range),
#         color = color_map[[dataverse]],
#         DataverseName = dataverse,
#         study_count = studies_count,
#         year_range = year_range,
#         DOI = matched_papers$DOI[1]
#       )
#     })
#
#     all_nodes <- append(all_nodes, list(nodes))
#     max_existing_id <- max_existing_id + nrow(nodes)  # Update max_existing_id
#
#     edges <- expand.grid(source = nodes$id, target = nodes$id) %>%
#       dplyr::filter(source != target) %>%
#       dplyr::mutate(count = purrr::map2_dbl(source, target, function(from_id, to_id) {
#         label_from <- nodes$label[nodes$id == from_id]
#         label_to   <- nodes$label[nodes$id == to_id]
#         sum(
#           grepl(label_from, dv_data[[input_event_type]], ignore.case = TRUE) &
#             grepl(label_to, dv_data[[input_event_type]], ignore.case = TRUE),
#           na.rm = TRUE
#         )
#       })) %>%
#       dplyr::filter(count > 0) %>%
#       dplyr::select(source, target, count)
#
#     all_edges <- append(all_edges, list(edges))
#   }
#
#   all_nodes_df <- dplyr::bind_rows(all_nodes) %>% dplyr::distinct()
#   all_edges_df <- dplyr::bind_rows(all_edges) %>% dplyr::distinct()
#
#   DBI::dbWriteTable(conn, nodes_table, all_nodes_df, append = TRUE, row.names = FALSE)
#   DBI::dbWriteTable(conn, edges_table, all_edges_df, append = TRUE, row.names = FALSE)
#
#   message("Processing complete. New data appended to tables: ", nodes_table, " and ", edges_table)
# }
