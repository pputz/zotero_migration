#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(httr)
})

PLAN_PATH <- "data/liniekim_upload_plan.json"
COLS_CSV <- "data/zotero_collections_familytree.csv"

plan <- fromJSON(PLAN_PATH, simplifyVector = FALSE)
cols <- read.csv(COLS_CSV, stringsAsFactors = FALSE)

entry <- plan[[1]]
title <- if(!is.null(entry$title)) entry$title else entry$refn
file <- entry$files[[1]]

z_user <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=username --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)
z_key <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=credential --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)
if(identical(Sys.getenv("FORCE_DRY_RUN"), "1")){
  stop("FORCE_DRY_RUN=1; aborting one-by-one test")
}
if(z_user == "" || z_key == "") stop("Zotero credentials missing")

base <- sprintf("https://api.zotero.org/%s/%s", ifelse(tolower(Sys.getenv("ZOTERO_LIBRARY_TYPE", "users")) %in% c("group","groups"), "groups", "users"), z_user)

cat("Testing item:", title, "\n")

# list items with tag to find item
items_res <- GET(paste0(base, "/items?tag=", URLencode("Linie Kim")), add_headers("Zotero-API-Key" = z_key))
cat("List items status:", status_code(items_res), "\n")
items_txt <- content(items_res, as = "text", encoding = "UTF-8")
cat(items_txt, "\n\n")
items <- content(items_res, as = "parsed")
it <- NULL
for(itm in items){ if(!is.null(itm$data$title) && itm$data$title == title) it <- itm }
if(is.null(it)) stop("Item not found by title")
item_key <- it$data$key
cat("Found item key:", item_key, "\n")

# find desired collection path & key from cols.csv
make_desired <- function(fp){ dir <- dirname(fp); subpath <- sub("^.*/Linie Kim/", "", dir); paste0("Familytree/Linie Kim/", subpath) }
desired <- make_desired(file)
match_row <- cols[cols$path == desired, ]
if(nrow(match_row) == 0) stop("No matching collection for desired path: ", desired)
coll_key <- match_row$key[1]
cat("Target collection:", desired, "->", coll_key, "\n")

# GET item raw
get_item_res <- GET(paste0(base, "/items/", item_key), add_headers("Zotero-API-Key" = z_key))
cat("GET item status:", status_code(get_item_res), "\n")
cat(content(get_item_res, as = "text", encoding = "UTF-8"), "\n\n")

# POST to add item to collection (send JSON array)
post_url <- paste0(base, "/collections/", coll_key, "/items")
cat("POST URL:", post_url, "\n")
post_res <- POST(post_url, add_headers("Zotero-API-Key" = z_key, "Content-Type" = "application/json"), encode = "json", body = list(item_key))
cat("POST status:", status_code(post_res), "\n")
cat("Response headers:\n")
print(headers(post_res))
cat("Response body:\n")
cat(content(post_res, as = "text", encoding = "UTF-8"), "\n")

if(status_code(post_res) %in% c(200,201,204)) cat("Add succeeded\n") else cat("Add failed\n")
