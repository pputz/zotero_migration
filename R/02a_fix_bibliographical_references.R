# 02a_fix_bibliographical_references.R
# Phase 4a: Fix bibliographical references
# Standardize bibliographic source fields from the enriched export

library(dplyr)
library(readr)
library(stringr)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step01_enriched.csv"
output_csv <- "data/sources_step02_biblio_fixed.csv"

# ---- LOAD ---------------------------------------------------

sources <- read_csv(input_csv, col_types = cols(.default = "c"))

# ---- HELPERS ------------------------------------------------

na_if_empty <- function(x) {
  x %>%
    str_squish() %>%
    na_if("")
}

normalize_author <- function(x) {
  x <- na_if_empty(x)

  x %>%
    str_replace_all("\s*;\s*", "; ") %>%
    str_replace_all("\s*,\s*", ", ") %>%
    str_replace_all("\s+", " ") %>%
    str_trim()
}

normalize_title <- function(x) {
  x %>%
    na_if_empty() %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_replace_all("\s+([,.;:])", "\1") %>%
    str_trim()
}

normalize_place_publication <- function(x) {
  x %>%
    na_if_empty() %>%
    str_replace_all("\s*:\s*", ": ") %>%
    str_replace_all("\s*;\s*", "; ") %>%
    str_replace_all("\s+", " ") %>%
    str_trim()
}

# ---- TRANSFORM ---------------------------------------------

sources_fixed <- sources %>%
  mutate(
    source_id = na_if_empty(source_id),
    title = normalize_title(title),
    author = normalize_author(author),
    publication = normalize_place_publication(publication),
    place = normalize_place_publication(place),
    detected_urls = na_if_empty(detected_urls),
    detected_filenames = na_if_empty(detected_filenames)
  )

# ---- SAVE ---------------------------------------------------

write_csv(sources_fixed, output_csv)

sources_fixed
