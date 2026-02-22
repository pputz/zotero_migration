# 03_create_zotero_collections.R
# Phase 7: Recreate Folder Structure as Zotero Collections
# Scan local folder, create collections via API, assign items

library(dplyr)
library(readr)
library(fs)
library(httr)
library(stringr)

# ---- CONFIG -------------------------------------------------------------

local_root <- "/Users/pp/Docs/ancestors/Ahnentafel/Personen"
enriched_csv <- "data/sources_step01_enriched.csv"

# Zotero API credentials (set as environment variables or input)
zotero_user_id <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=username --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)

zotero_api_key <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=credential --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)


if (zotero_user_id == "" || zotero_api_key == "") {
  stop("Set ZOTERO_USER_ID and ZOTERO_API_KEY environment variables")
}

base_url <- paste0("https://api.zotero.org/users/", zotero_user_id)


# ---- LOAD DATA ----------------------------------------------------------

sources <- read_csv(enriched_csv, col_types = cols(.default = "c"))

# ---- SCAN LOCAL FOLDER --------------------------------------------------

# Get all files recursively
all_files <- dir_ls(local_root, recurse = TRUE, type = "file")

# Create map: filename -> relative path
file_map <- setNames(path_rel(all_files, local_root), path_file(all_files))

# ---- DETERMINE COLLECTIONS FOR ITEMS ------------------------------------

sources <- sources %>%
  mutate(
    collection_path = path_dir(file_map[detected_filenames]),
    collection_hierarchy = str_split(collection_path, "/")
  )

# ---- CREATE COLLECTIONS VIA API -----------------------------------------

# Function to create collection
create_collection <- function(name, parent_key = NULL) {
  data <- list(name = name)
  if (!is.null(parent_key)) {
    data$parentCollection <- parent_key
  }
  
  cat("Creating collection:", name, "with parent:", ifelse(is.null(parent_key), "none", parent_key), "\n")
  
  response <- POST(
    url = paste0(base_url, "/collections"),
    add_headers("Zotero-API-Key" = zotero_api_key),
    body = list(data),  # Send as array
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    content(response)[[1]]$key  # Since it's an array, take first
  } else {
    cat("Failed to create collection:", name, "\nStatus:", status_code(response), "\nResponse:", content(response, "text"), "\n")
    NULL
  }
}

# Get existing collections to avoid duplicates
existing_collections_resp <- GET(
  url = paste0(base_url, "/collections"),
  add_headers("Zotero-API-Key" = zotero_api_key)
)

if (status_code(existing_collections_resp) != 200) {
  resp_text <- content(existing_collections_resp, "text", encoding = "UTF-8")
  stop(
    "Failed to fetch existing collections from Zotero API. HTTP status: ",
    status_code(existing_collections_resp),
    ". Response body: ",
    resp_text
  )
}

existing_collections <- content(existing_collections_resp)

# Validate that the content structure is a list of collection objects
if (!is.list(existing_collections)) {
  stop("Unexpected response format when fetching existing collections: expected a list.")
}

# Handle the valid case where no collections exist yet (empty list from API)
if (!length(existing_collections)) {
  existing_names <- character(0)
} else {
  # Filter to only valid collection objects with 'name' field
  valid_collections <- existing_collections[sapply(existing_collections, function(x) is.list(x) && !is.null(x$name))]
  existing_names <- vapply(valid_collections, function(x) x$name, character(1))
  existing_collections <- valid_collections  # Update for later use
}
# Create a map of path to key
collection_keys <- list()

# For each unique hierarchy, create collections
unique_paths <- unique(sources$collection_path[!is.na(sources$collection_path) & sources$collection_path != "."])
# For testing, take a subset of 5 collections
if (length(unique_paths) > 5) {
    unique_paths <- sample(unique_paths, 5)
}

for (path in unique_paths) {
  hierarchy <- str_split(path, "/")[[1]]
  current_parent <- NULL
  current_path <- ""
  
  for (level in hierarchy) {
    current_path <- if (current_path == "") level else paste(current_path, level, sep = "/")
    
    if (!(current_path %in% names(collection_keys))) {
      if (!(level %in% existing_names)) {
        key <- create_collection(level, current_parent)
        if (!is.null(key)) {
          collection_keys[[current_path]] <- key
          existing_names <- c(existing_names, level)
        }
      } else {
        # Find existing key
        existing <- existing_collections[sapply(existing_collections, function(x) x$name == level)]
        if (length(existing) > 0) {
          collection_keys[[current_path]] <- existing[[1]]$key
        }
      }
    }
    current_parent <- collection_keys[[current_path]]
  }
}

# ---- ASSIGN ITEMS TO COLLECTIONS ----------------------------------------

# First, get all items (assuming imported with a tag, e.g., "MacFamilyTree")
# To find items, query by tag
items_response <- GET(
  url = paste0(base_url, "/items"),
  query = list(tag = "MacFamilyTree"),
  add_headers("Zotero-API-Key" = zotero_api_key)
)

items <- content(items_response)

# Create a map of title to item key (assuming titles are unique)
item_map <- setNames(sapply(items, function(x) x$key), sapply(items, function(x) x$data$title))

# For each source, assign to collection
for (i in 1:nrow(sources)) {
  title <- sources$title[i]
  path <- sources$collection_path[i]
  
  if (!is.na(path) && title %in% names(item_map)) {
    item_key <- item_map[title]
    coll_key <- collection_keys[[path]]
    
    if (!is.null(coll_key)) {
      # Update item
      PATCH(
        url = paste0(base_url, "/items/", item_key),
        add_headers("Zotero-API-Key" = zotero_api_key),
        body = list(collections = list(coll_key)),
        encode = "json"
      )
    }
  }
}

cat("Collections created and items assigned.\n")