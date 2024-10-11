#!/usr/bin/env Rscript

library(yaml)
library(here)
library(purrr)
library(readr)
library(stringr)

source(here("r-lib/utilities.R"))

# Variables
dir_in_base <- here()
dir_out <- file.path(getwd(), "c")
subdirs <- c("00-bilder", "01-daten")
yaml_content <- read_yaml("content.yml")

# Clear and make output directory
if (dir.exists(dir_out)) unlink(dir_out, recursive = TRUE)
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Process assignments
if (!is.null(yaml_content$assignments)) {
    assignments <- yaml_content$assignments

    # Loop over assignments
    idx <- 1
    for (entry in assignments$content) {
        #
        # Input directory and existence check
        dir_in <- file.path(dir_in_base, entry$root, "aufgaben")
        if (!dir.exists(dir_in)) stop("Folder ", dir_in, " does not exist")

        # Copy subdirectories
        walk(subdirs, \(dir) copy_dir(dir, dir_in, dir_out))

        # Assignment
        sink(file.path(dir_out, fname("aufgabenblatt-${idx}", idx)))
        print_header(assignments$title, entry$subtitle, idx)
        walk(entry$include, \(inc) cat_files(dir_in, inc, with_solution = FALSE))
        sink()

        # Solution
        if (assignments$with_solution) {
            sink(file.path(dir_out, fname(paste0("aufgabenblatt-${idx}-loesung-", entry$sol), idx)))
            print_header(paste0("Lösungen zu ", assignments$title), entry$subtitle, idx)
            walk(entry$include, \(inc) cat_files(dir_in, inc, with_solution = TRUE))
            sink()
        }

        # Loop
        idx <- idx + 1
    }
}
