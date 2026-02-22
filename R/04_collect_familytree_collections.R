# 04_collect_familytree_collections.R
# Fetch all Zotero collections, find 'Familytree' root, and collect its subtree

library(httr)
library(jsonlite)
library(dplyr)

# get credentials (same method used elsewhere in repo)
zotero_user_id <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=username --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)

zotero_api_key <- system(
  "op item get 3tjvhcebmukgrd3dvbhgi2bol4 --reveal --fields label=credential --vault 6ndkbpbof2pyn5hctyb5fuingq",
  intern = TRUE
)

if (zotero_user_id == "" || zotero_api_key == "") stop("Missing Zotero credentials")

base_url <- paste0("https://api.zotero.org/users/", zotero_user_id)

# fetch all collections with pagination
fetch_all_collections <- function() {
  all <- list()
  start <- 0
  limit <- 100

  repeat {
    res <- GET(paste0(base_url, "/collections"),
               query = list(start = start, limit = limit),
               add_headers("Zotero-API-Key" = zotero_api_key))

    if (status_code(res) != 200) stop("Failed to fetch collections: ", status_code(res))

    page <- content(res)
    if (length(page) == 0) break
    all <- c(all, page)
    if (length(page) < limit) break
    start <- start + limit
  }

  all
}

cols <- fetch_all_collections()

# helper extractors
get_name <- function(c) {
  if (!is.null(c$name)) return(c$name)
  if (!is.null(c$data) && !is.null(c$data$name)) return(c$data$name)
  if (!is.null(c$meta) && !is.null(c$meta$name)) return(c$meta$name)
  NA_character_
}
get_key <- function(c) {
  if (!is.null(c$key)) return(c$key)
  if (!is.null(c$data) && !is.null(c$data$key)) return(c$data$key)
  NA_character_
}
get_parent <- function(c) {
  if (!is.null(c$parentCollection)) return(c$parentCollection)
  if (!is.null(c$data) && !is.null(c$data$parentCollection)) return(c$data$parentCollection)
  NA_character_
}

# build data.frame
keys <- sapply(cols, function(c) { k <- get_key(c); if (is.null(k)) NA_character_ else as.character(k) }, USE.NAMES = FALSE)
names <- sapply(cols, function(c) { n <- get_name(c); if (is.null(n)) NA_character_ else as.character(n) }, USE.NAMES = FALSE)
parents <- sapply(cols, function(c) { p <- get_parent(c); if (is.null(p)) NA_character_ else as.character(p) }, USE.NAMES = FALSE)

df <- tibble(
  key = keys,
  name = names,
  parent = parents
)

# find Familytree root (exact match first, then substring)
fam_rows <- which(df$name == "Familytree")
if (length(fam_rows) == 0) fam_rows <- which(grepl("Familytree", df$name, ignore.case = TRUE))

if (length(fam_rows) == 0) {
  stop("No collection named 'Familytree' found in Zotero account")
}

family_keys <- df$key[fam_rows]

# build parent -> children map
children_map <- split(df$key, df$parent)

# collect descendants for each family key
collect_descendants <- function(root_key) {
  result <- character(0)
  queue <- c(root_key)
  while (length(queue) > 0) {
    k <- queue[1]
    queue <- queue[-1]
    children <- children_map[[k]]
    if (!is.null(children) && length(children) > 0) {
      result <- c(result, children)
      queue <- c(queue, children)
    }
  }
  unique(result)
}

all_desc_keys <- unique(unlist(lapply(family_keys, collect_descendants)))

# include the root(s) as well
all_keys_in_tree <- unique(c(family_keys, all_desc_keys))

result_df <- df %>% filter(key %in% all_keys_in_tree) %>% arrange(name)

# construct full path for each node by walking up parents
build_path <- function(k, df_map) {
  parts <- character(0)
  cur <- k
  while (!is.na(cur) && cur != "") {
    row <- df_map[[cur]]
    if (is.null(row)) break
    parts <- c(row$name, parts)
    cur <- row$parent
    if (!is.null(cur) && cur == row$key) break
  }
  paste(parts, collapse = "/")
}

# create map keyed by key
df_map <- setNames(lapply(seq_len(nrow(df)), function(i) list(key=df$key[i], name=df$name[i], parent=df$parent[i])), df$key)

result_df <- result_df %>% rowwise() %>% mutate(path = build_path(key, df_map)) %>% ungroup()

# write CSV
out_file <- "data/zotero_collections_familytree.csv"
write.csv(result_df, out_file, row.names = FALSE, fileEncoding = "UTF-8")
cat("Wrote", nrow(result_df), "collections to", out_file, "\n")
