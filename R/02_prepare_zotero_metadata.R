# 02_prepare_zotero_metadata.R
# Phase 4: Prepare Zotero-Compatible Metadata
# Transform enriched CSV into Zotero CSV format

library(dplyr)
library(readr)
library(stringr)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step01_enriched.csv"
output_csv <- "data/zotero_import.csv"

# Zotero item type mapping (customize as needed)
item_type_mapping <- c(
  "Taufbuch" = "book",
  "Trauungsbuch" = "book",
  "Totenbuch" = "book",
  "Pflegeakt" = "document",
  "default" = "document"
)

# Field mappings (MacFamilyTree to Zotero)
field_mappings <- list(
  title = "title",
  author = "author",
  place = "archive",
  publication = "archiveLocation",
  date = "date",
  text = "abstractNote",
  detected_filenames = "attachments"  # Store as note or extra
)

# ---- LOAD ---------------------------------------------------

sources <- read_csv(input_csv, col_types = cols(.default = "c"))

# ---- TRANSFORM ---------------------------------------------

# Determine item type based on title keywords
sources <- sources %>%
  mutate(
    itemType = case_when(
      str_detect(title, "Taufbuch") ~ item_type_mapping["Taufbuch"],
      str_detect(title, "Trauungsbuch") ~ item_type_mapping["Trauungsbuch"],
      str_detect(title, "Totenbuch") ~ item_type_mapping["Totenbuch"],
      str_detect(title, "Pflegeakt") ~ item_type_mapping["Pflegeakt"],
      TRUE ~ item_type_mapping["default"]
    )
  )

# Map fields to Zotero columns
zotero_data <- sources %>%
  mutate(
    title = title,
    author = author,
    archive = place,
    archiveLocation = publication,
    date = date,
    abstractNote = text,
    extra = paste0("MacFamilyTree ID: ", source_id),
    tags = "MacFamilyTree;Imported-2026"
  ) %>%
  select(itemType, title, author, date, archive, archiveLocation, abstractNote, extra, tags, detected_filenames, detected_urls)

# ---- SAVE ---------------------------------------------------

write_csv(zotero_data, output_csv, na = "")

cat("Prepared Zotero metadata for", nrow(zotero_data), "items. Saved to", output_csv, "\n")