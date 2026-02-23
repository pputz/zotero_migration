#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(httr)
  library(dplyr)
})

ROOT_DIR <- "/Users/pp/Docs/ancestors/Ahnentafel/Personen"
PLAN_PATH <- "data/liniekim_upload_plan.json"
LOG_PATH <- "data/liniekim_verify_api_log.jsonl"

plan <- tryCatch(read_json(PLAN_PATH), error = function(e) stop("Plan file not found: ", PLAN_PATH))

z_user <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=username --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)
z_key <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=credential --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)
if(identical(Sys.getenv("FORCE_DRY_RUN"), "1")){
  z_user <- ""
  z_key <- ""
}
if(z_user == "" || z_key == "") stop("Zotero credentials missing; set credentials or unset FORCE_DRY_RUN")

base <- sprintf("https://api.zotero.org/%s/%s", ifelse(tolower(Sys.getenv("ZOTERO_LIBRARY_TYPE", "users")) %in% c("group","groups"), "groups", "users"), z_user)

log_api <- function(resp, context=""){
  out <- list(
    time = as.character(Sys.time()),
    context = context,
    status = status_code(resp),
    url = resp$url,
    headers = headers(resp),
    content = NULL
  )
  body_txt <- tryCatch({ httr::content(resp, as = "text", encoding = "UTF-8") }, error = function(e) NULL)
  out$content <- body_txt
  cat(toJSON(out, auto_unbox = TRUE), file = LOG_PATH, append = TRUE, sep = "\n")
}

# 1) List all collections (handle pagination) and build full paths by walking parentCollection
cols <- list()
start <- 0
per <- 100
repeat{
  cres <- GET(paste0(base, "/collections?start=", start, "&limit=", per), add_headers("Zotero-API-Key" = z_key))
  log_api(cres, paste0("list_collections?start=", start))
  if(status_code(cres) != 200) stop("Failed to list collections (start=)", start)
  page <- content(cres, as = "parsed")
  if(length(page) == 0) break
  cols <- c(cols, page)
  hdrs <- headers(cres)
  total <- as.numeric(hdrs$`total-results` %||% hdrs$`total_results` %||% NA)
  start <- start + length(page)
  if(!is.na(total) && start >= total) break
  # safety
  if(length(page) < per) break
}
# Build maps
col_map <- list()
for(c in cols){
  key <- c$data$key
  name <- c$data$name
  parent <- if(is.logical(c$data$parentCollection) && identical(c$data$parentCollection, FALSE)) NULL else c$data$parentCollection
  col_map[[key]] <- list(name = name, parent = parent)
}
# also keep full collection objects for logging
col_fullobjs <- list()
for(c in cols){
  col_fullobjs[[c$data$key]] <- c
}
# function to compute full path
compute_fullpath <- function(k, seen = character(0)){
  if(is.null(k) || k == "") return("")
  if(k %in% seen) return(col_map[[k]]$name) # prevent cycles
  info <- col_map[[k]]
  if(is.null(info)) return("")
  if(is.null(info$parent) || info$parent == FALSE) return(info$name)
  parent_path <- compute_fullpath(info$parent, c(seen, k))
  if(parent_path == "") return(info$name)
  paste(parent_path, info$name, sep = "/")
}
col_fullpaths <- sapply(names(col_map), compute_fullpath)
names(col_fullpaths) <- names(col_map)

# 2) List items tagged 'Linie Kim' (should capture the created items)
items_res <- GET(paste0(base, "/items?tag=", URLencode("Linie Kim")), add_headers("Zotero-API-Key" = z_key))
log_api(items_res, "list_items_liniekim")
if(status_code(items_res) != 200) stop("Failed to list items by tag")
items_list <- content(items_res, as = "parsed")
# normalize: items_list may be a list of item objects
items_by_title <- list()
for(it in items_list){
  t <- it$data$title
  items_by_title[[t]] <- it
}

moved <- list()
not_found <- list()
no_collection_found <- list()

for(e in plan){
  title <- if(!is.null(e$title)) e$title else e$refn
  message("Processing: ", title)
  it <- items_by_title[[title]]
  if(is.null(it)){
    not_found[[length(not_found)+1]] <- title
    warning("Item not found in Zotero: ", title)
    next
  }
  item_key <- it$data$key
  # determine desired relative directory under ROOT_DIR
  if(length(e$files) == 0){
    warning("No local files for plan entry: ", title)
    next
  }
  fp <- e$files[[1]]
    # compute relative path under ROOT_DIR without regex escaping issues
    prefix1 <- paste0(normalizePath(ROOT_DIR, winslash = "/"), "/")
    fp_norm <- normalizePath(fp, winslash = "/")
    if(startsWith(fp_norm, prefix1)){
      rel <- substring(fp_norm, nchar(prefix1) + 1)
    } else if(startsWith(fp_norm, normalizePath(ROOT_DIR, winslash = "/"))){
      rel <- substring(fp_norm, nchar(normalizePath(ROOT_DIR, winslash = "/")) + 1)
    } else {
      rel <- fp_norm
    }
    rel_dir <- dirname(rel)
  # try to find collection whose fullpath ends with rel_dir
  candidates <- names(col_fullpaths)[grepl(paste0("/", gsub("\\ ", "\\ ", rel_dir), "$"), col_fullpaths, fixed = FALSE)]
  # fallback: contains
  if(length(candidates) == 0) candidates <- names(col_fullpaths)[grepl(rel_dir, col_fullpaths, fixed = TRUE)]
  # final fallback: match last folder name only
  last_folder <- basename(dirname(fp))
  if(length(candidates) == 0) candidates <- names(col_fullpaths)[sapply(col_fullpaths, function(p) basename(p) == last_folder)]

  if(length(candidates) == 0){
    no_collection_found[[length(no_collection_found)+1]] <- list(title = title, desired = rel_dir)
    warning("No matching collection found for ", title, " (desired: ", rel_dir, ")")
    next
  }
  # prefer exact match including 'Familytree' if present
  pick <- NULL
  for(k in candidates){
    if(grepl("Familytree/", col_fullpaths[[k]], fixed = TRUE) && grepl(rel_dir, col_fullpaths[[k]], fixed = TRUE)){
      pick <- k; break
    }
  }
  if(is.null(pick)) pick <- candidates[[1]]
  # prepare and log library info for debug
  item_lib <- it$library
  coll_obj <- col_fullobjs[[pick]]
  coll_lib <- if(!is.null(coll_obj)) coll_obj$library else NULL
  lib_log <- list(time = as.character(Sys.time()), context = "library_info", title = title, item_key = item_key, item_library = item_lib, collection_key = pick, collection_library = coll_lib, collection_fullpath = col_fullpaths[[pick]])
  cat(toJSON(lib_log, auto_unbox = TRUE), file = LOG_PATH, append = TRUE, sep = "\n")

  # prepare request info (mask API key)
  req_url <- paste0(base, "/collections/", pick, "/items")
  req_body <- toJSON(list(item_key), auto_unbox = TRUE)
  req_headers <- list(
    `Zotero-API-Key-present` = nzchar(z_key),
    `Content-Type` = "application/json"
  )
  req_log <- list(time = as.character(Sys.time()), context = "add_item_request", url = req_url, headers = req_headers, body = req_body)
  cat(toJSON(req_log, auto_unbox = TRUE), file = LOG_PATH, append = TRUE, sep = "\n")

  # add item to collection
  # send JSON array properly using httr json encoder (avoid server treating body as string)
  add_res <- POST(req_url, add_headers("Zotero-API-Key" = z_key), encode = "json", body = list(item_key))
  log_api(add_res, paste0("add_item_to_collection:", title,":", pick))
  if(status_code(add_res) %in% c(200,201,204)){
    moved[[length(moved)+1]] <- list(title = title, item = item_key, collection = col_fullpaths[[pick]], coll_key = pick)
    message("Added ", title, " -> ", col_fullpaths[[pick]])
  } else {
    warning("Failed to add ", title, " to collection ", col_fullpaths[[pick]], " (", status_code(add_res), ")")
  }
}

# summary
cat("\nSummary:\n")
cat("Moved:", length(moved), "items\n")
if(length(moved) > 0) for(m in moved) cat(" - ", m$title, " -> ", m$collection, " (", m$coll_key, ")\n", sep = "")
cat("Not found in Zotero:", length(not_found), "items\n")
if(length(not_found) > 0) for(n in not_found) cat(" - ", n, "\n")
cat("No matching collection found:", length(no_collection_found), "entries\n")
if(length(no_collection_found) > 0) for(n in no_collection_found) cat(" - ", n$title, " desired:", n$desired, "\n")

cat("Detailed API log written to ", LOG_PATH, "\n")

