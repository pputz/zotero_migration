#!/usr/bin/env Rscript
library(httr)
library(jsonlite)

# Simple test script to create a root collection "Familytree"
# and a child collection "Linie Putz" under it.

# ---- CREDENTIALS (env vars preferred, fallback to 1Password CLI) ----
zotero_user_id <- Sys.getenv("ZOTERO_USER_ID")
zotero_api_key <- Sys.getenv("ZOTERO_API_KEY")

if (zotero_user_id == "" || zotero_api_key == "") {
  zotero_user_id <- system(
    "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=username --vault 6ndkbpbof2pyn5hctyb5fuingq",
    intern = TRUE
  )
  zotero_api_key <- system(
    "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=credential --vault 6ndkbpbof2pyn5hctyb5fuingq",
    intern = TRUE
  )
}

if (zotero_user_id == "" || zotero_api_key == "") {
  stop("Zotero credentials not found. Set ZOTERO_USER_ID and ZOTERO_API_KEY or ensure op CLI is available.")
}

base_url <- paste0("https://api.zotero.org/users/", zotero_user_id)

get_collections <- function() {
  resp <- GET(url = paste0(base_url, "/collections"), add_headers("Zotero-API-Key" = zotero_api_key))
  if (status_code(resp) != 200) stop("Failed to fetch collections: ", status_code(resp), " ", content(resp, "text", encoding = "UTF-8"))
  content(resp)
}

find_collection_key <- function(name, parent_key = NULL, collections = NULL) {
  if (is.null(collections)) collections <- get_collections()
  for (c in collections) {
    if (!is.list(c) || is.null(c$name)) next
    pc <- if (!is.null(c$parentCollection)) c$parentCollection else NULL
    name_match <- identical(c$name, name)
    parent_match <- (is.null(parent_key) && is.null(pc)) || (!is.null(parent_key) && !is.null(pc) && identical(pc, parent_key))
    if (name_match && parent_match) return(c$key)
  }
  NULL
}

create_collection <- function(name, parent_key = NULL) {
  body_obj <- list(name = name)
  if (!is.null(parent_key)) body_obj$parentCollection <- parent_key
  resp <- POST(
    url = paste0(base_url, "/collections"),
    add_headers("Zotero-API-Key" = zotero_api_key, "Content-Type" = "application/json"),
    body = toJSON(list(body_obj), auto_unbox = TRUE),
    encode = "json"
  )
  if (!(status_code(resp) %in% c(200, 201))) {
    stop("Failed to create collection '", name, "'. Status: ", status_code(resp), " Response: ", content(resp, "text", encoding = "UTF-8"))
  }
  resp_c <- content(resp)
  # Common Zotero responses:
  # - Array of created objects: list(list(key=...))
  # - Single object with $key
  # - Nested wrapper with $success or $successful and/or $success
  if (is.list(resp_c)) {
    # array of objects
    if (length(resp_c) > 0 && is.list(resp_c[[1]]) && !is.null(resp_c[[1]]$key)) return(resp_c[[1]]$key)
    # single object
    if (!is.null(resp_c$key)) return(resp_c$key)
    # top-level success mapping: success: {"0": ["KEY"]}
    if (!is.null(resp_c$success) && length(resp_c$success) > 0) {
      first <- resp_c$success[[1]]
      if (is.character(first) && length(first) > 0) return(first[1])
    }
    # nested successful object with data$key
    if (!is.null(resp_c$successful) && length(resp_c$successful) > 0) {
      firstsuc <- resp_c$successful[[1]]
      if (is.list(firstsuc) && !is.null(firstsuc$data) && !is.null(firstsuc$data$key)) {
        # data$key may be a vector
        k <- firstsuc$data$key
        if (is.character(k) && length(k) > 0) return(k[1])
      }
    }
  }
  stop("Unexpected response when creating collection: ", toJSON(resp_c))
}

cat("Testing Zotero collection creation against:", base_url, "\n")

all_cols <- get_collections()

# Ensure Familytree exists at root
family_key <- find_collection_key("Familytree", NULL, all_cols)
if (is.null(family_key)) {
  cat("Creating root collection 'Familytree'...\n")
  family_key <- create_collection("Familytree", NULL)
  cat("Created Familytree key:", family_key, "\n")
} else {
  cat("Found existing Familytree key:", family_key, "\n")
}

# Refresh collections list to include newly created
all_cols <- get_collections()

# Ensure child 'Linie Putz' under Familytree
child_key <- find_collection_key("Linie Putz", family_key, all_cols)
if (is.null(child_key)) {
  cat("Creating child collection 'Linie Putz' under Familytree...\n")
  child_key <- create_collection("Linie Putz", family_key)
  cat("Created Linie Putz key:", child_key, "\n")
} else {
  cat("Found existing Linie Putz key:", child_key, "\n")
}

cat("Done. Familytree:", family_key, "Linie Putz:", child_key, "\n")
