# Script to process data for the app

# Fetch the initial information about OAC (Open Access Content)
OAC_info <- fetch_oac_info()

# Set the file path for caching the processed data
cache_file <- "all_study_data.rds"

# Function to load data, using a cache mechanism to reduce API calls
load_data <- function(cache_file, OAC_info, cache_valid_duration = 24 * 60 * 60) {
  # Check if the cache file exists and whether it is still valid
  if (file.exists(cache_file)) {
    # Get the last modified time of the cache file
    last_modified <- file.info(cache_file)$mtime
    # Check if the cache file is within the valid duration
    if (as.numeric(Sys.time() - last_modified, units = "secs") < cache_valid_duration) {
      message("Loading data from cache...")  # Inform the user about cache usage
      return(readRDS(cache_file))           # Load and return the cached data
    } else {
      message("Cache expired. Fetching new data...")  # Cache is outdated
    }
  } else {
    message("Cache not found. Fetching data...")      # No cache exists
  }

  # Fetch new data from the API if cache is invalid or missing
  allIDs <- as.character(OAC_info$id)   # Ensure IDs are stored as character strings
  allIDs_details <- tibble()            # Initialize an empty tibble to store results

  # Create a mapping of IDs to titles for identifying dataverses
  id_to_title <- setNames(OAC_info$title, OAC_info$id)

  # Loop through each ID to fetch corresponding dataverse and study data
  for (id in allIDs) {
    # Fetch dataverse data using the current ID
    alldataverse_data <- fetch_dataverse_data(id)

    # Fetch study details for the fetched dataverse data
    allstudy_details <- fetch_study_details(alldataverse_data)

    # Add a new column for the dataverse name (mapped from the ID)
    dataverse_name <- id_to_title[id]
    allstudy_details <- mutate(allstudy_details, DataverseName = dataverse_name)

    # Append the fetched study details to the cumulative results
    allIDs_details <- bind_rows(allIDs_details, allstudy_details)
  }

  # Save the fetched and processed data to the cache file
  saveRDS(allIDs_details, cache_file)
  message("Data cached successfully.")  # Notify the user about caching

  return(allIDs_details)  # Return the processed data
}

# Load the study data, using cache if available and valid
# study_data <- load_data(cache_file, OAC_info)



