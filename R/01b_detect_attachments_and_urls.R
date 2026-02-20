# ------------------------------------------------------------
# Step 1.5: Space-safe, URL-safe, 8-digit-prefixed filenames
# ------------------------------------------------------------

library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# ---- CONFIG -------------------------------------------------

input_csv <- "sources_step01_raw.csv"
output_csv <- "sources_step01_enriched.csv"

# File extensions (case-insensitive)
file_ext_pattern <- "(?i)\\.(jpg|jpeg|png|tif|tiff|pdf|gif)$"

# URL pattern
url_pattern <- "(?i)^https?://"

# Filenames must start with 8 digits
prefix_pattern <- "^\\d{8}"

# ---- LOAD ---------------------------------------------------

sources <- read.csv(input_csv, stringsAsFactors = FALSE)

# ---- HELPERS -----------------------------------------------

split_lines <- function(raw_block) {
  str_split(raw_block, "\n")[[1]]
}

strip_gedcom_prefix <- function(line) {
  str_replace(line, "^\\d+\\s+\\S+\\s+", "")
}

extract_urls <- function(lines) {
  values <- strip_gedcom_prefix(lines)
  urls <- values[str_detect(values, regex(url_pattern, ignore_case = TRUE))]
  unique(urls)
}

extract_local_filenames <- function(lines, urls) {
  values <- strip_gedcom_prefix(lines)

  # 1. Must end with a valid extension
  candidates <- values[str_detect(
    values,
    regex(file_ext_pattern, ignore_case = TRUE)
  )]

  # 2. Must start with 8-digit number
  candidates <- candidates[str_detect(candidates, regex(prefix_pattern))]

  # 3. Exclude URLs
  url_filenames <- unlist(str_extract_all(
    urls,
    regex(file_ext_pattern, ignore_case = TRUE)
  ))
  locals <- setdiff(candidates, url_filenames)

  unique(locals)
}

# ---- APPLY EXTRACTION --------------------------------------

sources_enriched <- sources %>%
  rowwise() %>%
  mutate(
    lines = list(split_lines(raw_block)),
    urls = list(extract_urls(lines)),
    local_files = list(extract_local_filenames(lines, urls)),
    detected_urls = if (length(urls) == 0) NA_character_ else
      paste(urls, collapse = " | "),
    detected_filenames = if (length(local_files) == 0) NA_character_ else
      paste(local_files, collapse = " | ")
  ) %>%
  ungroup() %>%
  select(-lines, -urls, -local_files)

# ---- VERIFY -------------------------------------------------

print(
  sources_enriched %>%
    select(source_id, title, detected_filenames, detected_urls) %>%
    slice_head(n = 10)
)

# ---- SAVE ---------------------------------------------------

write.csv(
  sources_enriched,
  output_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Saved final cleaned data to", output_csv, "\n")
