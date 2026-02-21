# 02_prepare_zotero_metadata.R
# Phase 4: Prepare Zotero-Compatible Metadata
# Transform enriched CSV into Zotero CSV format

library(dplyr)
library(readr)
library(stringr)
library(xml2)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step01_enriched.csv"
output_rdf <- "data/zotero_import.rdf"

# Zotero item type mapping (customize as needed)
item_type_mapping <- c(
  "Taufbuch" = "book",
  "Trauungsbuch" = "book",
  "Totenbuch" = "book",
  "Pflegeakt" = "document",
  "default" = "document"
)


# ---- LOAD ---------------------------------------------------

sources <- read_csv(input_csv, col_types = cols(.default = "c"))

# ---- TRANSFORM ---------------------------------------------

# Data transformations
sources <- sources %>%
  mutate(
    itemType = case_when(
      str_detect(title, "Taufbuch") ~ item_type_mapping["Taufbuch"],
      str_detect(title, "Trauungsbuch") ~ item_type_mapping["Trauungsbuch"],
      str_detect(title, "Totenbuch") ~ item_type_mapping["Totenbuch"],
      str_detect(title, "Pflegeakt") ~ item_type_mapping["Pflegeakt"],
      TRUE ~ item_type_mapping["default"]
    ),
    date = {
      digits <- str_extract(detected_filenames, "\\d{8}")
      ifelse(!is.na(digits) & nchar(digits) == 8,
             paste0(substr(digits, 1, 4), "-", substr(digits, 5, 6), "-", substr(digits, 7, 8)),
             NA_character_)
    }
  )

# Map fields to Zotero columns
zotero_data <- sources %>%
  mutate(
    title = title,
    author = author,
    archive = place,
    archiveLocation = publication,
    URL = detected_urls,
    extra = detected_filenames,
    tags = paste0("MacFamilyTree;Imported-2026;MacFamilyTree ID: ", source_id)
  ) %>%
  select(itemType, title, author, date, archive, archiveLocation, URL, extra, tags)

# ---- SAVE ---------------------------------------------------

# Create Zotero RDF
rdf <- xml_new_root("rdf:RDF",
  `xmlns:rdf` = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  `xmlns:z` = "http://www.zotero.org/namespaces/export#",
  `xmlns:dcterms` = "http://purl.org/dc/terms/",
  `xmlns:dc` = "http://purl.org/dc/elements/1.1/"
)

for (i in 1:min(10, nrow(zotero_data))) {
  item <- xml_add_child(rdf, "z:Item", `rdf:about` = paste0("#item_", i))
  xml_add_child(item, "z:itemType", zotero_data$itemType[i])
  xml_add_child(item, "dcterms:title", zotero_data$title[i])
  if (!is.na(zotero_data$author[i])) {
    xml_add_child(item, "dc:creator", zotero_data$author[i])
  }
  if (!is.na(zotero_data$date[i])) {
    xml_add_child(item, "dcterms:date", zotero_data$date[i])
  }
  if (!is.na(zotero_data$archive[i])) {
    xml_add_child(item, "z:archive", zotero_data$archive[i])
  }
  if (!is.na(zotero_data$archiveLocation[i])) {
    xml_add_child(item, "z:archiveLocation", zotero_data$archiveLocation[i])
  }
  if (!is.na(zotero_data$URL[i])) {
    xml_add_child(item, "dc:identifier", zotero_data$URL[i])
  }
  if (!is.na(zotero_data$extra[i])) {
    xml_add_child(item, "z:extra", zotero_data$extra[i])
  }
  if (!is.na(zotero_data$tags[i])) {
    tag_list <- str_split(zotero_data$tags[i], ";")[[1]]
    for (tag in tag_list) {
      if (str_trim(tag) != "") {
        xml_add_child(item, "dc:subject", str_trim(tag))
      }
    }
  }
}

write_xml(rdf, output_rdf, encoding = "UTF-8")

cat("Prepared Zotero RDF for", min(10, nrow(zotero_data)), "items. Saved to", output_rdf, "\n")