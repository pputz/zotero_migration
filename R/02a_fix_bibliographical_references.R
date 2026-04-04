# 02a_fix_bibliographical_references.R
# Phase 4a: Fix bibliographical references
# Standardize bibliographic source fields from the enriched export

library(dplyr)
library(readr)
library(stringr)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step01_enriched.csv"
author_mapping_csv <- "data/manual data wrangling/z_authors.csv"
output_csv <- "data/sources_step02_biblio_fixed.csv"

# ---- LOAD ---------------------------------------------------

sources <- read_csv(input_csv, col_types = cols(.default = "c"))
author_mapping <- read_csv(author_mapping_csv, col_types = cols(.default = "c"))

# ---- HELPERS ------------------------------------------------

na_if_empty <- function(x) {
  x |>
    str_squish() |>
    na_if("")
}

normalize_author <- function(x) {
  x <- na_if_empty(x)

  x |>
    str_replace_all("\\s*;\\s*", "; ") |>
    str_replace_all("\\s*,\\s*", ", ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

normalize_title <- function(x) {
  x |>
    na_if_empty() |>
    str_replace_all("[[:space:]]+", " ") |>
    str_replace_all("\\s+([,.;:])", "\\1") |>
    str_trim()
}

normalize_place_publication <- function(x) {
  x |>
    na_if_empty() |>
    str_replace_all("\\s*:\\s*", ": ") |>
    str_replace_all("\\s*;\\s*", "; ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

# ---- TRANSFORM ---------------------------------------------

sources_fixed <- sources |>
  mutate(
    source_id = na_if_empty(source_id),
    title = normalize_title(title),
    author = normalize_author(author),
    publication = normalize_place_publication(publication),
    place = normalize_place_publication(place),
    detected_urls = na_if_empty(detected_urls),
    detected_filenames = na_if_empty(detected_filenames)
  )

# ---- FIX AUTHORS --------------------------------------------

sources_fixed <- sources_fixed |>
  left_join(author_mapping, by = c("author" = "original"))


# ---- IDENTIFY GENRE -----------------------------------------

sources_fixed_1 <- sources_fixed |>
  mutate(
    genre = case_when(
      str_detect(detected_filenames, "Tauf") ~ "Taufbuch",
      str_detect(detected_filenames, "Trauung") ~ "Trauungsbuch",
      str_detect(detected_filenames, "Toten") ~ "Totenbuch",
      str_detect(detected_filenames, "Sterb") ~ "Totenbuch",
      str_detect(detected_filenames, "Pflegeakt") ~ "Pflegeakt",
      TRUE ~ NA_character_
    )
  )

# ---- TITLE KIRCHENBUECHER -----------------------------------
# Pfarre Pichl bei Wels: Geburt Maria Königsmayr, 22. Juni 1842,
# in: Taufbuch 106/1842, S. 6, Digitalisat über Matricula Online, URL.

# Set locale to German for month names
Sys.setlocale("LC_TIME", "de_AT.UTF-8")

sources_fixed_2 <- sources_fixed_1 |>
  mutate(
    # Extract name (first two words from title)
    name = str_extract(title, "^\\S+\\s+\\S+"),

    # Extract date (first 8 digits from detected_filenames for YYYYMMDD format)
    date_raw = str_extract(detected_filenames, "\\d{8}"),

    # Convert to date and format in German
    date_formatted = if_else(
      !is.na(date_raw),
      format(
        ymd(date_raw),
        "%d. %B %Y"
      ),
      NA_character_
    ),

    # Create z_title for Taufbuch entries
    z_title = case_when(
      genre == "Taufbuch" & !is.na(name) & !is.na(date_formatted) ~
        paste0("Geburt und Taufe ", name, " ", date_formatted),
      genre == "Trauungsbuch" & !is.na(name) & !is.na(date_formatted) ~
        paste0("Trauung ", name, " ", date_formatted),
      genre == "Totenbuch" & !is.na(name) & !is.na(date_formatted) ~
        paste0("Sterbeeintrag ", name, " ", date_formatted),
      #TRUE ~ title
      TRUE ~ NA_character_
    )
  )

# ---- SAVE ---------------------------------------------------

write_csv(sources_fixed, output_csv)

sources_fixed
