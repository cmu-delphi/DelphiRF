# Define the directory for template files
template_dir <- "man-roxygen"

# Create the directory if it doesn't exist
if (!dir.exists(template_dir)) {
  dir.create(template_dir)
}

# Find all R script files in the "R" directory
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

# Extract all unique @template references
template_names <- unique(unlist(lapply(r_files, function(file) {
  lines <- readLines(file, warn = FALSE)
  matches <- grep("^#'\\s*@template\\s+", lines, value = TRUE)
  gsub("^#'\\s*@template\\s+", "", matches) # Extract template name
})))

# Generate empty template files if they don’t exist
for (template in template_names) {
  template_path <- file.path(template_dir, paste0(template, ".R"))
  if (!file.exists(template_path)) {
    writeLines(c(
      paste0("#' @param ", template, " [DESCRIPTION GOES HERE]"), ""
    ), template_path)
    message("Created template: ", template_path)
  }
}

message("Template generation complete!")
