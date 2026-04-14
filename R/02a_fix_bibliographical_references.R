# 02a_fix_bibliographical_references.R
# Phase 4a: Fix bibliographical references
# Standardize bibliographic source fields from the enriched export

# library(dplyr)
# library(readr)
# library(stringr)
library(tidyverse)

# ---- CONFIG -------------------------------------------------

input_csv <- "data/sources_step01_enriched.csv"
author_mapping_csv <- "data/manual data wrangling/z_authors.csv"
output_csv <- "data/sources_step02a_biblio_fixed.csv"

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
  left_join(author_mapping, by = c("author" = "original")) |>
  rename(
    z_last_name = `z_last name`,
    z_first_name = `z_first name`
  )


# ---- IDENTIFY GENRE -----------------------------------------

sources_fixed_1 <- sources_fixed |>
  mutate(
    genre = case_when(
      str_detect(detected_filenames, "Tauf") ~ "Taufbuch",
      str_detect(detected_filenames, "Trauung") ~ "Trauungsbuch",
      str_detect(detected_filenames, "Toten") ~ "Totenbuch",
      str_detect(detected_filenames, "Sterb") ~ "Totenbuch",
      str_detect(detected_filenames, "Pflegeakt") ~ "Pflegeakt",
      str_detect(refn, "Taufbuch|Mischband") ~ "Taufbuch",
      str_detect(refn, "Trauungsbuch") ~ "Trauungsbuch",
      str_detect(title, regex("census", ignore_case = TRUE)) ~ "Census",
      str_detect(
        title,
        regex("Todesanzeige", ignore_case = TRUE)
      ) ~ "Zeitungsartikel",
      str_detect(
        author,
        regex("post|zeitung|blatt|nachrichten", ignore_case = TRUE)
      ) ~ "Zeitungsartikel",
      str_detect(
        publication,
        regex("post|zeitung|blatt|nachrichten", ignore_case = TRUE)
      ) ~ "Zeitungsartikel",
      str_detect(
        title,
        regex("certificate|license", ignore_case = TRUE)
      ) ~ "Certificate",
      str_detect(title, regex("parte", ignore_case = TRUE)) ~ "Parte",
      str_detect(title, regex("grundbuch", ignore_case = TRUE)) ~ "Grundbuch",
      str_detect(detected_urls, regex("grave", ignore_case = TRUE)) ~ "Grave",
      TRUE ~ "Other"
    )
  )

# Create table which counts the number of unique genres
genre_counts <- sources_fixed_1 |>
  group_by(genre) |>
  summarise(count = n_distinct(source_id)) |>
  arrange(desc(count))

# ---- KIRCHENBUECHER ----------------------------------------
# Item Type: Report
# Title: Geburt und Taufe Kreuzinger Augustine 24. August 1905
# Institution: Katholische Kirche Diözese Linz - Pfarre Linz St. Mathias
# Date:
# ReportType: Taufbuch
# ReportNumber: XI/42/307
# Archive: Matricula Online; Landesarchiv Oberösterreich
# URL: http://data.matricula-online.eu/en/oesterreich/oberoesterreich/linz-st-matthias-st-mathias/106%252F1905/?pg=44
# Extra: Household members listed?

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

    # Create z_title for Kirchenbücher
    z_title = case_when(
      genre == "Taufbuch" & !is.na(name) & !is.na(date_formatted) ~
        paste0("Geburt und Taufe ", name, " ", date_formatted),
      genre == "Trauungsbuch" & !is.na(name) & !is.na(date_formatted) ~
        paste0("Trauung ", name, " ", date_formatted),
      genre == "Totenbuch" & !is.na(name) & !is.na(date_formatted) ~
        paste0("Sterbeeintrag ", name, " ", date_formatted),
      #TRUE ~ title
      TRUE ~ NA_character_
    ),

    # Create z_ItemType for Kirchenbücher
    z_ItemType = case_when(
      genre == "Taufbuch" ~ "report",
      genre == "Trauungsbuch" ~ "report",
      genre == "Totenbuch" ~ "report",
      TRUE ~ NA_character_
    ),

    # Create z_Institution for Kirchenbücher
    z_Institution = case_when(
      genre %in% c("Taufbuch", "Trauungsbuch", "Totenbuch") ~ z_name, # Use extracted name as institution for Kirchenbücher
      TRUE ~ NA_character_
    ),

    # Set z_name to NA for Kirchenbücher since we are using it for institution
    z_name = case_when(
      genre %in% c("Taufbuch", "Trauungsbuch", "Totenbuch") ~ NA_character_,
      TRUE ~ z_name
    ),

    # Create z_ReportType for Kirchenbücher
    z_ReportType = case_when(
      genre %in% c("Taufbuch", "Trauungsbuch", "Totenbuch") ~
        str_extract(title, "Taufbuch|Trauungsbuch|Totenbuch"),
      TRUE ~ NA_character_
    ),

    # Create z_ReportNumber for Kirchenbücher
    z_ReportNumber = case_when(
      genre %in% c("Taufbuch", "Trauungsbuch", "Totenbuch") ~
        str_remove(publication, "Taufbuch|Trauungsbuch|Totenbuch") |>
        str_remove("^\\.|\\s+") |>
        str_remove("^\\.|\\s+"), # take entire publication field but exclude the strings "Taufbuch", "Trauungsbuch", "Totenbuch"; delete leading "." and whitespace if present
      TRUE ~ NA_character_
    ),

    # Create z_Archive for Kirchenbücher, set to "Matricula Online" or "Landesarchiv Oberösterreich" based on detected_URL
    z_Archive = case_when(
      genre %in%
        c("Taufbuch", "Trauungsbuch", "Totenbuch") &
        str_detect(detected_urls, "matricula") ~ "Matricula Online",
      genre %in%
        c("Taufbuch", "Trauungsbuch", "Totenbuch") &
        str_detect(
          detected_urls,
          "landesarchiv"
        ) ~ "Landesarchiv Oberösterreich",
      TRUE ~ NA_character_
    ),

    # Create z_Language for Kirchenbücher
    z_Language = case_when(
      genre %in% c("Taufbuch", "Trauungsbuch", "Totenbuch") ~ "de_AT",
      TRUE ~ NA_character_
    ),

    # Create z_URL for Kirchenbücher
    z_URL = case_when(
      genre %in%
        c("Taufbuch", "Trauungsbuch", "Totenbuch") &
        !is.na(detected_urls) ~ detected_urls,
      TRUE ~ NA_character_
    )
  )

# ---- CENSUS -------------------------------------------------
# Item Type: Document
# Title: 1850 U.S. Census – John Smith household, Springfield, Sangamon County, Illinois
# Author: United States Census Bureau
# Date: 1850-06-01
# Archive: National Archives (NARA); via Ancestry
# Archive Location: Springfield, Ward 3, p. 142, dwelling 210, family 225
# Call Number: RG 29, M432 roll 123
# URL: (Ancestry link)
# Extra: Household members listed?

sources_fixed_2 <- sources_fixed_2 |>
  mutate(
    z_ItemType = case_when(
      genre == "Census" ~ "document",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Census
    # Reformulate title to "1850 U.S. Census – John Smith household"
    # Extract year from date_raw and name from name, then combine with "U.S. Census –" to create z_Title
    z_title = case_when(
      genre == "Census" ~
        paste0(
          str_extract(date_raw, "^\\d{4}"),
          " U.S. Census – ",
          name,
          " household"
        ),
      TRUE ~ z_title
    ),
    # Create z_name for Census
    z_name = case_when(
      genre == "Census" ~ "United States Census Bureau",
      TRUE ~ z_name
    ),
    # Create z_Date for Census
    # Extract date from date_raw (yyyymmdd) and reformat to "YYYY-MM-DD" for z_Date
    z_Date = case_when(
      genre == "Census" & !is.na(date_raw) ~
        format(ymd(date_raw), "%Y-%m-%d"),
      TRUE ~ NA_character_
    ),
    # Create z_Archive for Census
    z_Archive = case_when(
      genre == "Census" ~ "National Archives (NARA); via Ancestry",
      TRUE ~ z_Archive
    ),
    # Create z_URL for Census
    z_URL = case_when(
      genre == "Census" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    ),
    # Create z_Language for Census
    z_Language = case_when(
      genre == "Census" ~ "en_US",
      TRUE ~ z_Language
    )
  )

# ---- ZEITUNGSARTIKEL -----------------------------------------
# Item Type: Newspaper Article
# Title: Todesanzeige für John Smith
# Publication: OÖ Nachrichten
# Pages: 5
# Date: 1950-06-01
# Archive: Österreichische Nationalbibliothek; via ANNO
# URL: (ANNO link)
# Language: de_AT
# Extra: Filename: OON_1950_06_01_p5.jpg

sources_fixed_3 <- sources_fixed_2 |>
  mutate(
    z_ItemType = case_when(
      genre == "Zeitungsartikel" ~ "newspaperArticle",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Newspaper Articles
    # Reformulate title to "Todesanzeige für John Smith"
    # Extract notice type (Todesanzeige) from title, then combine with "für" to create z_Title
    # Extract name from name and combine with "Todesanzeige für" to create z_Title
    z_title = case_when(
      genre == "Zeitungsartikel" ~ paste0(
        str_extract(
          title,
          regex(
            "\\S*(anzeige|verband|nachricht|nachruf)\\S*",
            ignore_case = TRUE
          )
        ),
        " für ",
        name
      ),
      TRUE ~ z_title
    ),
    # Create z_Publication for Newspaper Articles
    z_Publication = case_when(
      genre == "Zeitungsartikel" & !is.na(publication) ~ author, # Extract publication name from author field for newspaper articles
      TRUE ~ NA_character_
    ),
    #Create z_Pages for Newspaper Articles - use all information from publication field
    z_Pages = case_when(
      genre == "Zeitungsartikel" & !is.na(publication) ~ publication, # Extract page information from publication field for newspaper articles
      TRUE ~ NA_character_
    ),
    # Create z_Date for Newspaper Articles
    # Extract date from refn (yyyymmdd) and reformat to "YYYY-MM-DD" for z_Date
    z_Date = case_when(
      genre == "Zeitungsartikel" & !is.na(refn) ~
        format(ymd(str_extract(refn, "\\d{8}")), "%Y-%m-%d"),
      TRUE ~ z_Date
    ),
    # Create z_Archive for Newspaper Articles; if detected_urls contains "anno" set to "Österreichische Nationalbibliothek; via ANNO"
    z_Archive = case_when(
      genre == "Zeitungsartikel" &
        str_detect(
          detected_urls,
          regex("anno", ignore_case = TRUE)
        ) ~ "Österreichische Nationalbibliothek; via ANNO",
      TRUE ~ z_Archive
    ),
    # Create z_URL for Newspaper Articles
    z_URL = case_when(
      genre == "Zeitungsartikel" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    ),
    # Create z_Language for Newspaper Articles
    z_Language = case_when(
      genre == "Zeitungsartikel" ~ "de_AT",
      TRUE ~ z_Language
    )
  )


# ---- OTHER --------------------------------------------------
# Item Type: Document
# Title:
# Publication:
# Pages:
# Date:
# Archive:
# LocArchive:
# URL:
# Language:
# Extra: Filename:

sources_fixed_4 <- sources_fixed_3 |>
  mutate(
    z_ItemType = case_when(
      genre == "Other" ~ "document",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Other genre - use title field
    z_title = case_when(
      genre == "Other" & !is.na(title) ~ title,
      TRUE ~ z_title
    ),
    # Create z_LocArchive - extract location information from publication field
    z_LocArchive = case_when(
      genre == "Other" & !is.na(publication) ~ publication, # Extract location information from publication field for Other genre
      TRUE ~ NA_character_
    ),
    z_URL = case_when(
      genre == "Other" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    ),
    # Create z_Date for Other genre - use date_raw
    z_Date = case_when(
      genre == "Other" & !is.na(date_raw) ~
        format(ymd(date_raw), "%Y-%m-%d"),
      TRUE ~ z_Date
    )
  )

# ---- CERTIFICATE --------------------------------------------------
# Item Type: Document
# Title: Marriage Certificate for John Smith and Jane Doe
# Name: State Board of Health Missouri
# Publication:
# Pages:
# Date:
# Archive:
# LocArchive:
# URL:
# Language: en_US
# Extra: Filename:

sources_fixed_5 <- sources_fixed_4 |>
  mutate(
    z_ItemType = case_when(
      genre == "Certificate" ~ "document",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Certificates
    # Reformulate title to "Marriage Certificate for John Smith and Jane Doe"
    # Extract certificate type (e.g., Marriage Certificate) from detected_filenames (last two words before file extension) and name from name, then combine with "for" to create z_Title
    # Capitalize first letter of each word in certificate type
    # Extract names from name and combine with "Certificate for" to create z_Title
    z_title = case_when(
      genre == "Certificate" ~ paste0(
        str_to_title(
          str_extract(
            detected_filenames,
            regex(
              "\\b\\w+\\s+\\w+\\b(?=\\.[^.]+$)", # Last 2 words before file extension, then capitalize first letter of each word
              ignore_case = TRUE
            )
          )
        ),
        " for ",
        name
      ),
      TRUE ~ z_title
    ),
    # Create z_name for Certificates
    # If detected_urls contains "sos.mo.gov" set to "State Board of Health Missouri"
    z_name = case_when(
      genre == "Certificate" &
        str_detect(
          detected_urls,
          regex("sos\\.mo\\.gov", ignore_case = TRUE)
        ) ~ "State Board of Health Missouri",
      TRUE ~ z_name
    ),
    # Create z_Language for Certificates
    z_Language = case_when(
      genre == "Certificate" ~ "en_US",
      TRUE ~ z_Language
    ),
    # Create z_Date for Certificates - use date_raw
    z_Date = case_when(
      genre == "Certificate" & !is.na(date_raw) ~
        format(ymd(date_raw), "%Y-%m-%d"),
      TRUE ~ z_Date
    ),
    # Create z_URL for Certificates
    z_URL = case_when(
      genre == "Certificate" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    )
  )

# ---- PARTE --------------------------------------------------
# Item Type: Document
# Title: Parte für Günter Putz
# Name:
# Publication:
# Pages:
# Date:
# Archive:
# LocArchive:
# URL:
# Language: de_AT
# Extra:

sources_fixed_6 <- sources_fixed_5 |>
  mutate(
    z_ItemType = case_when(
      genre == "Parte" ~ "document",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Parte
    # Reformulate title to "Parte für Günter Putz"
    # Extract name from name and combine with "Parte für" to create z_Title
    z_title = case_when(
      genre == "Parte" ~ paste0("Parte für ", name),
      TRUE ~ z_title
    ),
    # Create z_Language for Parte
    z_Language = case_when(
      genre == "Parte" ~ "de_AT",
      TRUE ~ z_Language
    ),
    # Create z_Date for Parte - use date_raw
    z_Date = case_when(
      genre == "Parte" & !is.na(date_raw) ~
        format(ymd(date_raw), "%Y-%m-%d"),
      TRUE ~ z_Date
    ),
    # Create z_URL for Parte
    z_URL = case_when(
      genre == "Parte" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    )
  )


# ---- GRAVE --------------------------------------------------
# Item Type: Document
# Title: Epitaph for John Smith, deceased 1950
# Name:
# Publication:
# Pages:
# Date:
# Archive: Find a Grave
# LocArchive:
# URL:
# Language: en_US
# Extra:

sources_fixed_7 <- sources_fixed_6 |>
  mutate(
    z_ItemType = case_when(
      genre == "Grave" ~ "document",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Grave
    # Reformulate title to "Epitaph for John Smith, deceased 1950"
    # Extract name from name and date from date_raw, then combine with "Epitaph for" to create z_Title
    z_title = case_when(
      genre == "Grave" ~ paste0(
        "Epitaph for ",
        name,
        if_else(
          !is.na(date_raw),
          paste0(", deceased ", str_extract(date_raw, "^\\d{4}")),
          ""
        )
      ),
      TRUE ~ z_title
    ),
    # Create z_Language for Grave
    z_Language = case_when(
      genre == "Grave" ~ "en_US",
      TRUE ~ z_Language
    ),
    # Create z_Date for Grave - use date_raw
    z_Date = case_when(
      genre == "Grave" & !is.na(date_raw) ~
        format(ymd(date_raw), "%Y-%m-%d"),
      TRUE ~ z_Date
    ),
    # Create z_Archive for Grave
    z_Archive = case_when(
      genre == "Grave" ~ "Find a Grave",
      TRUE ~ z_Archive
    ),
    # Create z_URL for Grave
    z_URL = case_when(
      genre == "Grave" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    )
  )

# ---- GRUNDBUCH --------------------------------------------------
# Item Type: Report
# Title: as is from title field
# Name:
# Report Number: as is from publication field
# Publication:
# Pages:
# Date:
# Archive:
# LocArchive:
# URL:
# Language: de_AT
# Extra:

sources_fixed_8 <- sources_fixed_7 |>
  mutate(
    z_ItemType = case_when(
      genre == "Grundbuch" ~ "report",
      TRUE ~ z_ItemType
    ),
    # Create z_Title for Grundbuch - use title field
    z_title = case_when(
      genre == "Grundbuch" & !is.na(title) ~ title,
      TRUE ~ z_title
    ),
    # Create z_ReportNumber for Grundbuch - use publication field
    z_ReportNumber = case_when(
      genre == "Grundbuch" & !is.na(publication) ~ publication,
      TRUE ~ NA_character_
    ),
    # Create z_Language for Grundbuch
    z_Language = case_when(
      genre == "Grundbuch" ~ "de_AT",
      TRUE ~ z_Language
    ),
    # Create z_Date for Grundbuch - use date_raw
    z_Date = case_when(
      genre == "Grundbuch" & !is.na(date_raw) ~
        format(ymd(date_raw), "%Y-%m-%d"),
      TRUE ~ z_Date
    ),
    # Create z_URL for Grundbuch
    z_URL = case_when(
      genre == "Grundbuch" & !is.na(detected_urls) ~ detected_urls,
      TRUE ~ z_URL
    )
  )


# ---- ADD MISSING FILE NAMES ---------------------------------
# Add missing detected_filenames for source_ids based on the following mapping:
# @96868972@	19550524 immigration petition.pdf
# @31864124@	18830214 Taufbuch.pdf
# @61798804@	18930915 Taufbuch.jpg
# @98612648@	History Jung-Rhyun Kim.pdf
# @23644032@	19880304 Partezettel.pdf
# @61166504@	Schick Josef und Theresia Totenbuchregister.jpg
# @93960074@	18830214 Taufbuch.pdf
# @8187572@	19380612 Trauungsbuch.jpg
# @78680360@	Ansan Kim Jokbo 1989.pdf
# @70523192@	0002 Putz Martin 1856.pdf

sources_fixed_9 <- sources_fixed_8 |>
  mutate(
    detected_filenames = case_when(
      source_id == "@96868972@" ~ "19550524 immigration petition.pdf",
      source_id == "@31864124@" ~ "18830214 Taufbuch.pdf",
      source_id == "@61798804@" ~ "18930915 Taufbuch.jpg",
      source_id == "@98612648@" ~ "History Jung-Rhyun Kim.pdf",
      source_id == "@23644032@" ~ "19880304 Partezettel.pdf",
      source_id ==
        "@61166504@" ~ "Schick Josef und Theresia Totenbuchregister.jpg",
      source_id == "@93960074@" ~ "18830214 Taufbuch.pdf",
      source_id == "@8187572@" ~ "19380612 Trauungsbuch.jpg",
      source_id == "@78680360@" ~ "Ansan Kim Jokbo 1989.pdf",
      source_id == "@70523192@" ~ "0002 Putz Martin 1856.pdf",
      TRUE ~ detected_filenames
    )
  )

# ---- ADD MISSING z_title -----------------------------------
# In all cases where z_title is missing add title field
sources_fixed_9 <- sources_fixed_9 |>
  mutate(
    z_title = case_when(
      is.na(z_title) & !is.na(title) ~ title,
      TRUE ~ z_title
    )
  )

# ---- UPDATE z_Extra ----------------------------------------
# Delete current content of z_Extra
# Add "GedcomID: <source_id>" to z_Extra for all records
sources_fixed_10 <- sources_fixed_9 |>
  mutate(
    z_Extra = paste0("GedcomID: ", source_id)
  )


# ---- SELECT AND REORDER COLUMNS -----------------------------
# select source_id, detected_filenames, and all columns starting with "z_"
sources_fixed_final <- sources_fixed_10 |>
  select(
    source_id,
    detected_filenames,
    starts_with("z_")
  )

# ---- SAVE ---------------------------------------------------
write_csv(sources_fixed_final, output_csv)
