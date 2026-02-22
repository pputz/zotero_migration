library(dplyr)
library(readr)
library(fs)
library(stringr)

local_root <- "/Users/pp/Docs/ancestors/Ahnentafel/Personen"
enriched_csv <- "data/sources_step01_enriched.csv"

# load
sources <- read_csv(enriched_csv, col_types = cols(.default = "c"))
all_files <- dir_ls(local_root, recurse = TRUE, type = "file")
file_map <- setNames(path_rel(all_files, local_root), path_file(all_files))

# derive collection_path when possible
if ("detected_filenames" %in% names(sources)) {
  sources <- sources %>% mutate(collection_path = path_dir(file_map[detected_filenames]))
} else {
  sources$collection_path <- NA_character_
}

# normalize
sources <- sources %>% mutate(collection_path = ifelse(is.na(collection_path), NA_character_, trimws(collection_path)))

# filesystem dirs
if (length(all_files) == 0) {
  cat("No files found under local_root.\n")
  quit(status = 0)
}

dir_paths <- unique(path_rel(path_dir(all_files), local_root))
dir_paths <- dir_paths[!is.na(dir_paths) & dir_paths != "."]
dir_paths <- trimws(dir_paths)
unique_paths <- unique(dir_paths)

cat("Total planned collection paths:", length(unique_paths), "\n\n")

# show first 50
if (length(unique_paths)>0) {
  cat("Sample planned paths (first 50):\n")
  for (p in head(unique_paths, 50)) cat(" -", p, "\n")
}

cat("\nLinie Kim child count:")
km <- unique_paths[startsWith(unique_paths, "Linie Kim")]
cat(length(km), "\n")
if (length(km)>0) {
  cat("Linie Kim subfolders:\n")
  for (p in km) cat(" -", p, "\n")
}

cat("\nFiltered 'Linie Kim' sample rows (up to 10):\n")
filtered <- sources %>% filter(!is.na(collection_path) & str_starts(collection_path, "Linie Kim"))
if (nrow(filtered)==0) cat("<none>\n") else print(utils::head(filtered %>% select(title, collection_path), 10))
