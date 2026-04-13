# 02b_prepare_zotero_metadata.R
# Phase 4b: Prepare Zotero-compatible metadata
# Transform z_* fields from 02a into Zotero RDF format

library(tidyverse)
library(xml2)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step02a_biblio_fixed.csv"
output_rdf <- "data/zotero_import.rdf"

# ---- LOAD ---------------------------------------------------

sources <- read_csv(input_csv, col_types = cols(.default = "c"))

# ---- BUILD RDF ----------------------------------------------

rdf <- xml_new_root(
  "rdf:RDF",
  `xmlns:rdf` = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  `xmlns:z` = "http://www.zotero.org/namespaces/export#",
  `xmlns:dcterms` = "http://purl.org/dc/terms/",
  `xmlns:dc` = "http://purl.org/dc/elements/1.1/",
  `xmlns:bib` = "http://purl.org/net/biblio#"
)

add_if <- function(parent, element, value) {
  if (!is.na(value) && nchar(str_trim(value)) > 0) {
    xml_add_child(parent, element, value)
  }
}

for (i in seq_len(nrow(sources))) {
  row <- sources[i, ]

  item <- xml_add_child(
    rdf,
    "z:Item",
    `rdf:about` = paste0("#item_", i)
  )

  # Item type & title
  add_if(item, "z:itemType", row$z_ItemType)
  add_if(item, "dcterms:title", row$z_title)

  # Creator: personal author (z_last_name + z_first_name) takes precedence
  if (!is.na(row$z_last_name) && nchar(str_trim(row$z_last_name)) > 0) {
    creator_str <- str_trim(paste0(
      row$z_last_name,
      if_else(
        !is.na(row$z_first_name) & nchar(str_trim(row$z_first_name)) > 0,
        paste0(", ", row$z_first_name),
        ""
      )
    ))
    xml_add_child(item, "dc:creator", creator_str)
  } else if (!is.na(row$z_name) && nchar(str_trim(row$z_name)) > 0) {
    # Institutional / corporate author
    xml_add_child(item, "dc:creator", row$z_name)
  }

  # Bibliographic fields
  add_if(item, "dcterms:date", row$z_Date)
  add_if(item, "z:archive", row$z_Archive)
  add_if(item, "z:archiveLocation", row$z_LocArchive)
  add_if(item, "z:series", row$z_Series)
  add_if(item, "z:volume", row$z_Volume)
  add_if(item, "z:publicationTitle", row$z_Publication)
  add_if(item, "z:pages", row$z_Pages)
  add_if(item, "z:reportNumber", row$z_ReportNumber)
  add_if(item, "z:language", row$z_Language)
  add_if(item, "dc:identifier", row$z_URL)
  add_if(item, "z:extra", row$z_Extra)
}

write_xml(rdf, output_rdf, encoding = "UTF-8")

list(
  rows_loaded = nrow(sources),
  rows_exported_rdf = nrow(sources),
  output_file = output_rdf
)
