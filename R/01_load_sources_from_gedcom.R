# ------------------------------------------------------------
# Step 1: Extract SOUR records from MacFamilyTree GEDCOM
# ------------------------------------------------------------

library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(readr)

# ---- CONFIG -------------------------------------------------

gedcom_file <- "data/Ahnentafel.ged" # adjust path

# ---- READ GEDCOM --------------------------------------------

lines <- readLines(gedcom_file, encoding = "UTF-8", warn = FALSE)

# ---- FIND SOUR RECORD STARTS --------------------------------

# Matches: 0 @12345@ SOUR
source_start_idx <- which(
  str_detect(lines, "^0 @[^@]+@ SOUR$")
)

stopifnot(length(source_start_idx) > 0)

# ---- FIND RECORD ENDS ---------------------------------------

source_end_idx <- c(
  source_start_idx[-1] - 1,
  length(lines)
)

source_blocks <- map2(
  source_start_idx,
  source_end_idx,
  ~ lines[.x:.y]
)

# ---- HELPER: extract level-1 fields (+ CONT/CONC) ----------

extract_l1 <- function(block, tag) {
  starts <- which(str_detect(block, paste0("^1 ", tag, "(?:\\b|$)")))
  if (length(starts) == 0) {
    return(NA_character_)
  }

  values <- map_chr(starts, function(start_idx) {
    next_l1 <- which(str_detect(block[(start_idx + 1):length(block)], "^1 "))
    end_idx <- if (length(next_l1) == 0) {
      length(block)
    } else {
      start_idx + next_l1[1] - 1
    }

    chunk <- block[start_idx:end_idx]
    first_line <- str_remove(chunk[1], paste0("^1 ", tag, " ?"))
    if (length(chunk) == 1) {
      return(first_line)
    }

    cont_lines <- chunk[-1]
    out <- first_line

    for (ln in cont_lines) {
      if (str_detect(ln, "^2 CONT(?:\\s|$)")) {
        out <- paste0(out, "\n", str_remove(ln, "^2 CONT ?"))
      } else if (str_detect(ln, "^2 CONC(?:\\s|$)")) {
        out <- paste0(out, str_remove(ln, "^2 CONC ?"))
      }
    }

    out
  })

  str_squish(paste(values, collapse = " | "))
}

# ---- PARSE SOUR RECORDS -------------------------------------

sources_df <- map_dfr(source_blocks, function(block) {
  tibble(
    source_id = str_match(block[1], "^0 (@[^@]+@)")[, 2],
    title = extract_l1(block, "TITL"),
    author = extract_l1(block, "AUTH"),
    place = extract_l1(block, "PLAC"),
    publication = extract_l1(block, "PUBL"),
    refn = extract_l1(block, "REFN"), # ← filename OR identifier
    text = extract_l1(block, "TEXT"), # ← often URL
    raw_block = paste(block, collapse = "\n")
  )
})

# ---- SAVE INTERMEDIATE RESULT -------------------------------

write_csv(sources_df, "data/sources_step01_raw.csv")

sources_df
