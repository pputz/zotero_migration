# ------------------------------------------------------------
# Step 1.5: Detect attachment filenames and URLs from GEDCOM
# ------------------------------------------------------------

library(dplyr)
library(purrr)
library(readr)
library(stringr)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step01_raw.csv"
output_csv <- "data/sources_step01_enriched.csv"

file_ext_pattern <- "\\.(jpg|jpeg|png|tif|tiff|pdf|gif)$"
url_extract_pattern <- "https?://\\S+"
prefix_pattern <- "^\\d{8}"

# ---- LOAD ---------------------------------------------------

sources <- readr::read_csv(input_csv, show_col_types = FALSE)

# ---- HELPERS -----------------------------------------------

split_lines <- function(raw_block) {
  if (is.na(raw_block) || !nzchar(raw_block)) {
    return(character())
  }

  str_split(raw_block, "\n", simplify = FALSE)[[1]]
}

strip_gedcom_prefix <- function(lines) {
  lines %>%
    str_replace("^\\d+\\s+\\S+\\s*", "") %>%
    str_squish()
}

extract_urls <- function(lines) {
  values <- strip_gedcom_prefix(lines)

  urls <- values %>%
    str_extract_all(regex(url_extract_pattern, ignore_case = TRUE)) %>%
    unlist(use.names = FALSE)

  unique(urls)
}

extract_local_filenames <- function(lines) {
  values <- strip_gedcom_prefix(lines)

  candidates <- values[
    str_detect(values, regex(file_ext_pattern, ignore_case = TRUE)) &
      str_detect(values, regex(prefix_pattern)) &
      !str_detect(values, regex("^https?://", ignore_case = TRUE))
  ]

  unique(candidates)
}

collapse_or_na <- function(x) {
  if (length(x) == 0) {
    NA_character_
  } else {
    paste(x, collapse = " | ")
  }
}

# ---- APPLY EXTRACTION --------------------------------------

lines_list <- map(sources$raw_block, split_lines)
urls_list <- map(lines_list, extract_urls)
files_list <- map(lines_list, extract_local_filenames)

sources_enriched <- sources %>%
  mutate(
    detected_urls = map_chr(urls_list, collapse_or_na),
    detected_filenames = map_chr(files_list, collapse_or_na)
  )

# ---- SAVE ---------------------------------------------------

readr::write_csv(sources_enriched, output_csv)

sources_enriched
