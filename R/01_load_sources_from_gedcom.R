# ------------------------------------------------------------
# Step 1: Extract SOUR records from MacFamilyTree GEDCOM
# ------------------------------------------------------------

library(stringr)
library(purrr)
library(tibble)
library(dplyr)

# ---- CONFIG -------------------------------------------------

gedcom_file <- "data/Ahnentafel.ged" # adjust path

# ---- READ GEDCOM --------------------------------------------

lines <- readLines(gedcom_file, encoding = "UTF-8", warn = FALSE)

# ---- FIND SOUR RECORD STARTS --------------------------------

# Matches: 0 @12345@ SOUR
source_start_idx <- which(
  str_detect(lines, "^0 @[^@]+@ SOUR$")
)

cat("Number of SOUR records found:", length(source_start_idx), "\n")

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

# ---- HELPER: extract level-1 fields -------------------------

extract_l1 <- function(block, tag) {
  vals <- block[str_detect(block, paste0("^1 ", tag, "\\b"))]
  if (length(vals) == 0) return(NA_character_)
  str_remove(vals, paste0("^1 ", tag, " "))
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

# ---- QUICK INSPECTION ---------------------------------------

print(sources_df %>% slice_head(n = 5))

# ---- SAVE INTERMEDIATE RESULT -------------------------------

write.csv(
  sources_df,
  "data/sources_step01_raw.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Saved", nrow(sources_df), "sources to data/sources_step01_raw.csv\n")
