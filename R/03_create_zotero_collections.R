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

  if (!(status_code(response) %in% c(200, 201))) {
    cat("Failed to create collection:", name, "\nStatus:", status_code(response), "\nResponse:", content(response, "text"), "\n")
    return(NULL)
  }

  resp_content <- content(response)
  key <- NULL

  # Several possible Zotero response shapes — handle common ones
  if (is.list(resp_content)) {
    # 1) Array of created objects: list(list(key=...))
    if (length(resp_content) > 0 && is.list(resp_content[[1]]) && !is.null(resp_content[[1]]$key)) {
      key <- resp_content[[1]]$key
    }
    # 2) Single-object with $key
    if (is.null(key) && !is.null(resp_content$key)) {
      key <- resp_content$key
    }
    # 3) success map: { "success": { "0": ["KEY"] } }
    if (is.null(key) && !is.null(resp_content$success) && length(resp_content$success) > 0) {
      first <- resp_content$success[[1]]
      if (is.character(first) && length(first) > 0) key <- first[1]
    }
    # 4) nested successful object: { "successful": { "0": { data: { key: ["KEY"] }}}}
    if (is.null(key) && !is.null(resp_content$successful) && length(resp_content$successful) > 0) {
      firstsuc <- resp_content$successful[[1]]
      if (is.list(firstsuc) && !is.null(firstsuc$data) && !is.null(firstsuc$data$key)) {
        k <- firstsuc$data$key
        if (is.character(k) && length(k) > 0) key <- k[1]
      }
    }
  }

  # register newly created collection locally so lookups work
  if (!is.null(key)) {
    new_entry <- list(name = name, key = key, parentCollection = parent_key)
    existing_collections <<- c(existing_collections, list(new_entry))
    existing_names <<- unique(c(existing_names, name))
  }

  if (is.null(key)) {
    cat("Failed to parse created key for collection:", name, "\nResponse raw:", toString(resp_content), "\n")
  }

  key
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
# Create/find root collection "Familytree" and ensure all created collections go under it
# Create a map of path to key
collection_keys <- list()

# Helper: find existing collection by name and parent key
find_existing_collection_key <- function(name, parent_key) {
  matches <- sapply(existing_collections, function(x) {
    if (!is.list(x) || is.null(x$name)) return(FALSE)
    pc <- if (!is.null(x$parentCollection)) x$parentCollection else NULL
    name_match <- identical(x$name, name)
    parent_match <- (is.null(parent_key) && is.null(pc)) || (!is.null(parent_key) && !is.null(pc) && identical(pc, parent_key))
    name_match && parent_match
  })
  if (any(matches)) {
    existing_collections[[which(matches)[1]]]$key
  } else {
    NULL
  }
}

# Ensure root Familytree exists
family_key <- find_existing_collection_key("Familytree", NULL)
if (is.null(family_key)) {
  family_key <- create_collection("Familytree", NULL)
}
if (is.null(family_key)) stop("Failed to create/find root collection 'Familytree'.")
collection_keys[["."]] <- family_key

# For each unique hierarchy, create collections under Familytree
unique_paths <- unique(sources$collection_path[!is.na(sources$collection_path) & sources$collection_path != "."])
# For testing, take a subset of 5 collections
if (length(unique_paths) > 5) {
    unique_paths <- sample(unique_paths, 5)
}

for (path in unique_paths) {
  hierarchy <- str_split(path, "/")[[1]]
  current_parent <- family_key
  current_path <- ""

  for (level in hierarchy) {
    current_path <- if (current_path == "") level else paste(current_path, level, sep = "/")

    if (!(current_path %in% names(collection_keys))) {
      # try to find by name+parent
      ex_key <- find_existing_collection_key(level, current_parent)
      if (!is.null(ex_key)) {
        collection_keys[[current_path]] <- ex_key
      } else {
        key <- create_collection(level, current_parent)
        if (!is.null(key)) {
          collection_keys[[current_path]] <- key
          # small pause to avoid rate limits
          Sys.sleep(0.12)
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