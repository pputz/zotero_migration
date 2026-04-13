#!/usr/bin/env Rscript
# Script: 05_upload_liniekim_to_zotero.R
# Purpose: Find local files referenced in detected_filenames in
# data/sources_step02a_biblio_fixed.csv under the root
# /Users/pp/Docs/ancestors/Ahnentafel/Personen, filter
# for LINIE, and upload items+attachments to Zotero
# if Zotero API env vars are provided. Otherwise write a
# plan JSON for manual import.

suppressPackageStartupMessages({
  library(readr)
  library(jsonlite)
  library(httr)
  library(dplyr)
  library(stringr)
})

ROOT_DIR <- "/Users/pp/Docs/ancestors/Ahnentafel/Personen"
CSV_PATH <- "data/sources_step02a_biblio_fixed.csv"
# ---- PARAMETER: set the Linie to process ----------------------------
LINIE      <- "Linie Putz"
# previously: LINIE <- "Linie Kim"
# ---------------------------------------------------------------------

# derive slugified linie name for output filenames, e.g. "linie-putz"
linie_slug <- tolower(gsub("\\s+", "-", LINIE))
OUT_PLAN   <- paste0("data/", linie_slug, "_upload_plan.json")
LOG_PATH   <- paste0("data/", linie_slug, "_upload_api_log.jsonl")

csv <- read_csv(CSV_PATH, show_col_types = FALSE)
stopifnot("detected_filenames" %in% names(csv))
stopifnot("z_title" %in% names(csv))
stopifnot("z_ItemType" %in% names(csv))

all_files <- list.files(ROOT_DIR, recursive = TRUE, full.names = TRUE)

# 1) Find all file paths that contain "Linie Kim"
matched_paths <- all_files[grepl(LINIE, all_files, fixed = TRUE)]
if (length(matched_paths) == 0) {
  message("No files under ", ROOT_DIR, " contain '", LINIE, "' in their path")
  quit(status = 0)
}

matched_basenames <- basename(matched_paths)
# map: basename -> vector of full paths
paths_by_basename <- split(matched_paths, matched_basenames)

# 2) Subset CSV rows whose detected_filenames contains any matched basename
liniekim <- csv |>
  filter(!is.na(detected_filenames)) |>
  filter(
    map_lgl(detected_filenames, function(fnames) {
      any(str_split(fnames, ";\\s*")[[1]] %in% names(paths_by_basename))
    })
  )

if (nrow(liniekim) == 0) {
  message(
    "No rows in ",
    CSV_PATH,
    " have detected_filenames matching filenames found for '", LINIE, "'"
  )
  quit(status = 0)
}

# 3) Attach matched full paths to each row
liniekim$matched_files <- lapply(liniekim$detected_filenames, function(fnames) {
  if (is.na(fnames) || fnames == "") {
    return(character(0))
  }
  basenames <- str_split(fnames, ";\\s*")[[1]]
  unlist(lapply(basenames, function(b) {
    paths_by_basename[[b]] %||% character(0)
  }))
})

# 4) Prepare plan entries using z_* columns
plan_entries <- lapply(seq_len(nrow(liniekim)), function(i) {
  row <- liniekim[i, ]
  list(
    source_id = as.character(row$source_id),
    title = as.character(row$z_title),
    itemType = as.character(row$z_ItemType),
    extra = as.character(row$z_Extra),
    archive = as.character(row$z_Archive),
    date = as.character(row$z_Date),
    url = as.character(row$z_URL),
    language = as.character(row$z_Language),
    creator = if (
      !is.na(row$z_last_name) && nchar(str_trim(row$z_last_name)) > 0
    ) {
      str_trim(paste0(
        row$z_last_name,
        if (!is.na(row$z_first_name) && nchar(str_trim(row$z_first_name)) > 0) {
          paste0(", ", row$z_first_name)
        } else {
          ""
        }
      ))
    } else if (!is.na(row$z_name) && nchar(str_trim(row$z_name)) > 0) {
      as.character(row$z_name)
    } else {
      NA_character_
    },
    files = as.character(liniekim$matched_files[[i]])
  )
})

# Zotero upload helpers
z_base <- function() {
  lib_type <- Sys.getenv("ZOTERO_LIBRARY_TYPE", "users")
  if (tolower(lib_type) %in% c("group", "groups")) {
    return("groups")
  }
  return("users")
}

z_user <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=username --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)

z_key <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=credential --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)

# Allow forcing a dry-run (write plan) by setting env var FORCE_DRY_RUN=1
if (identical(Sys.getenv("FORCE_DRY_RUN"), "1")) {
  z_user <- ""
  z_key <- ""
}

perform_upload <- function(entries) {
  if (z_key == "" || z_user == "") {
    message(
      "ZOTERO_API_KEY or ZOTERO_USER_ID not set; writing plan to ",
      OUT_PLAN
    )
    write_json(entries, OUT_PLAN, pretty = TRUE, auto_unbox = TRUE)
    return(invisible(FALSE))
  }

  base <- sprintf("https://api.zotero.org/%s/%s", z_base(), z_user)

  # helper to log API responses (append JSON lines)
  log_api <- function(resp, context = "") {
    out <- list(
      time = as.character(Sys.time()),
      context = context,
      status = status_code(resp),
      url = resp$url,
      headers = headers(resp),
      content = NULL
    )
    # try to parse body as text
    body_txt <- tryCatch(
      {
        httr::content(resp, as = "text", encoding = "UTF-8")
      },
      error = function(e) NULL
    )
    out$content <- body_txt
    line <- jsonlite::toJSON(out, auto_unbox = TRUE)
    cat(line, file = LOG_PATH, append = TRUE, sep = "\n")
  }

  # cache collections (handle pagination) and build full paths
  cols <- list()
  start <- 0
  per <- 100
  repeat {
    cres <- GET(
      paste0(base, "/collections?start=", start, "&limit=", per),
      add_headers("Zotero-API-Key" = z_key)
    )
    log_api(cres, paste0("list_collections?start=", start))
    if (status_code(cres) != 200) {
      warning(
        "Failed to list collections; proceeding without collection matching"
      )
      break
    }
    page <- content(cres, as = "parsed")
    if (length(page) == 0) {
      break
    }
    cols <- c(cols, page)
    hdrs <- headers(cres)
    total <- as.numeric(hdrs$`total-results` %||% hdrs$`total_results` %||% NA)
    start <- start + length(page)
    if (!is.na(total) && start >= total) {
      break
    }
    if (length(page) < per) break
  }

  # build map and fullpath helper
  col_map <- list()
  col_fullobjs <- list()
  for (c in cols) {
    key <- c$data$key
    parent <- if (
      is.logical(c$data$parentCollection) &&
        identical(c$data$parentCollection, FALSE)
    ) {
      NULL
    } else {
      c$data$parentCollection
    }
    col_map[[key]] <- list(name = c$data$name, parent = parent)
    col_fullobjs[[key]] <- c
  }
  compute_fullpath <- function(k, seen = character(0)) {
    if (is.null(k) || k == "") {
      return("")
    }
    if (k %in% seen) {
      return(col_map[[k]]$name)
    }
    info <- col_map[[k]]
    if (is.null(info)) {
      return("")
    }
    if (is.null(info$parent) || info$parent == FALSE) {
      return(info$name)
    }
    parent_path <- compute_fullpath(info$parent, c(seen, k))
    if (parent_path == "") {
      return(info$name)
    }
    paste(parent_path, info$name, sep = "/")
  }
  if (length(col_map) > 0) {
    col_fullpaths <- sapply(names(col_map), compute_fullpath)
    names(col_fullpaths) <- names(col_map)
  } else {
    col_fullpaths <- character(0)
  }

  # helper: find an existing collection key by folder name; prefer Familytree/LINIE matches
  ensure_collection <- function(folder_name, fullpath_hint = NULL) {
    if (is.null(folder_name) || folder_name == "") {
      return(NULL)
    }
    if (length(col_fullpaths) == 0) {
      return(NULL)
    }
    # exact name match and under Familytree/LINIE
    cand <- names(col_fullpaths)[
      basename(col_fullpaths) == folder_name &
        grepl(paste0("Familytree/", LINIE), col_fullpaths, fixed = TRUE)
    ]
    if (length(cand) > 0) {
      return(cand[[1]])
    }
    # exact name match anywhere
    cand <- names(col_fullpaths)[basename(col_fullpaths) == folder_name]
    if (length(cand) > 0) {
      return(cand[[1]])
    }
    # if fullpath_hint provided, try endsWith
    if (!is.null(fullpath_hint)) {
      cand <- names(col_fullpaths)[endsWith(col_fullpaths, fullpath_hint)]
      if (length(cand) > 0) return(cand[[1]])
    }
    # fallback: contains folder_name
    cand <- names(col_fullpaths)[grepl(
      folder_name,
      col_fullpaths,
      fixed = TRUE
    )]
    if (length(cand) > 0) {
      return(cand[[1]])
    }
    return(NULL)
  }

  # derive a compact ASCII tag from a collection name
  # e.g. "金 東岱 Kim Dong Dae 1939" -> "KimDongDae1939"
  collection_to_tag <- function(name) {
    if (is.null(name) || name == "") {
      return(NULL)
    }
    gsub("[^A-Za-z0-9]", "", name)
  }

  for (e in entries) {
    title <- if (!is.null(e$title)) e$title else e$source_id
    # use parent folder name as collection name if files exist
    coll_name <- NULL
    if (length(e$files) > 0) {
      f0 <- e$files[1]
      coll_name <- basename(dirname(f0))
    }
    coll_key <- NULL
    if (!is.null(coll_name)) {
      coll_key <- ensure_collection(coll_name)
    }

    # only derive a tag when the folder resolved to a real Zotero collection
    tag_str <- if (!is.null(coll_key)) {
      collection_to_tag(col_map[[coll_key]]$name)
    } else {
      NULL
    }

    item <- list(
      itemType = if (!is.null(e$itemType) && !is.na(e$itemType)) {
        e$itemType
      } else {
        "document"
      },
      title = title,
      extra = if (!is.null(e$extra) && !is.na(e$extra)) e$extra else NULL,
      archive = if (!is.null(e$archive) && !is.na(e$archive)) {
        e$archive
      } else {
        NULL
      },
      date = if (!is.null(e$date) && !is.na(e$date)) e$date else NULL,
      language = if (!is.null(e$language) && !is.na(e$language)) {
        e$language
      } else {
        NULL
      },
      tags = if (!is.null(tag_str)) list(list(tag = tag_str)) else list()
    )
    item <- Filter(Negate(is.null), item)
    if (!is.null(coll_key)) {
      item$collections <- list(coll_key)
    }

    # Zotero requires the POST body to be a JSON array of item objects
    res <- POST(
      paste0(base, "/items"),
      add_headers(
        "Zotero-API-Key" = z_key,
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(item), auto_unbox = TRUE)
    )
    log_api(res, context = paste0("create_item:", e$source_id))
    if (status_code(res) %in% c(200, 201)) {
      created <- content(res, as = "parsed")
      # extract created item key from response structure
      item_key <- NULL
      if (!is.null(created$successful) && length(created$successful) > 0) {
        first <- created$successful[[1]]
        if (!is.null(first$data) && !is.null(first$data$key)) {
          item_key <- first$data$key
        }
      } else if (!is.null(created$data) && !is.null(created$data$key)) {
        item_key <- created$data$key
      }
      message(
        "Created item for source_id=",
        e$source_id,
        " -> itemKey=",
        item_key
      )
      # create linked-file attachment items instead of uploading binary
      if (length(e$files) > 0 && !is.null(item_key)) {
        for (fp in e$files) {
          if (!file.exists(fp)) {
            next
          }
          fname <- basename(fp)
          attach_item <- list(
            itemType = "attachment",
            title = fname,
            linkMode = "linked_file",
            path = fp,
            parentItem = item_key
          )
          ar <- POST(
            paste0(base, "/items"),
            add_headers(
              "Zotero-API-Key" = z_key,
              "Content-Type" = "application/json"
            ),
            body = toJSON(list(attach_item), auto_unbox = TRUE)
          )
          log_api(
            ar,
            context = paste0(
              "create_linked_attachment:",
              fname,
              ":item",
              item_key
            )
          )
          if (status_code(ar) %in% c(200, 201)) {
            message(" Linked ", fname, " to item ", item_key)
          } else {
            warning("Failed linking ", fname, " (", status_code(ar), ")")
          }
        }
      }
      # If we identified a collection key, perform a version-checked PATCH to set collections
      if (!is.null(coll_key) && !is.null(item_key)) {
        # fetch current item version via HEAD/GET
        item_head <- GET(
          paste0(base, "/items/", item_key),
          add_headers("Zotero-API-Key" = z_key)
        )
        log_api(item_head, context = paste0("get_item_for_version:", item_key))
        cur_ver <- headers(item_head)$`last-modified-version`
        if (is.null(cur_ver)) {
          # fallback to parsed content
          it_parsed <- content(item_head, as = "parsed")
          cur_ver <- as.character(it_parsed$data$version %||% NA)
        }
        if (!is.null(cur_ver) && !is.na(cur_ver)) {
          patch_res <- PATCH(
            paste0(base, "/items/", item_key),
            add_headers(
              "Zotero-API-Key" = z_key,
              "If-Unmodified-Since-Version" = cur_ver,
              "Content-Type" = "application/json"
            ),
            body = toJSON(
              list(collections = list(coll_key)),
              auto_unbox = TRUE
            ),
            encode = "raw"
          )
          log_api(
            patch_res,
            context = paste0("patch_item_collections:", item_key)
          )
          if (status_code(patch_res) %in% c(200, 201, 204)) {
            message("Patched item ", item_key, " into collection ", coll_key)
          } else {
            warning(
              "Failed to patch collections for item ",
              item_key,
              " (",
              status_code(patch_res),
              ")"
            )
          }
        } else {
          warning(
            "Could not determine current version for item ",
            item_key,
            "; skipping PATCH for collections"
          )
        }
      }
    } else {
      warning(
        "Failed to create item for source_id=",
        e$source_id,
        " (",
        status_code(res),
        ")"
      )
    }
  }
  return(invisible(TRUE))
}

# Run
ok <- tryCatch(
  {
    perform_upload(plan_entries)
    TRUE
  },
  error = function(e) {
    warning(e)
    FALSE
  }
)
if (inherits(ok, "logical") && !ok) {
  message("Upload plan written to ", OUT_PLAN)
}

message("Done. Processed ", length(plan_entries), " '", LINIE, "' references.")
