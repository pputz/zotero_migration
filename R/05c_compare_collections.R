#!/usr/bin/env Rscript
library(jsonlite)
library(dplyr)

plan <- fromJSON("data/liniekim_upload_plan.json", simplifyVector = FALSE)
cols <- read.csv("data/zotero_collections_familytree.csv", stringsAsFactors = FALSE)

make_desired_path <- function(file_path){
  dir <- dirname(file_path)
  subpath <- sub("^.*/Linie Kim/", "", dir)
  paste0("Familytree/Linie Kim/", subpath)
}

out <- lapply(plan, function(entry){
  file <- entry$files
  desired <- make_desired_path(file)
  matches <- cols %>% filter(path == desired)
  list(refn = entry$refn,
       title = entry$title,
       file = file,
       desired_path = desired,
       found = nrow(matches) > 0,
       collection_keys = if(nrow(matches)>0) paste(matches$key, collapse=",") else NA)
})

df <- bind_rows(out)
print(df)
write.csv(df, "data/liniekim_collections_match.csv", row.names = FALSE)

bad <- df %>% filter(!found)
if(nrow(bad)>0){
  cat(sprintf("\nMismatches (%d):\n", nrow(bad)))
  print(bad[c("refn","title","desired_path")])
} else {
  cat("\nAll plan targets found in collections.\n")
}
