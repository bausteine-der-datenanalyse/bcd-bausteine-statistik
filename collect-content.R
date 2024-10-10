#!/usr/bin/env Rscript

library(yaml)
library(here)
library(purrr)
library(readr)
library(stringr)


# -------------------------------------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------------------------------------


copy_folder <- function(dir_to_copy, dir_in, dir_out) {
    dir_data_in <- file.path(dir_in, dir_to_copy)
    dir_data_out <- file.path(dir_out, dir_to_copy)
    if (dir.exists(dir_data_in)) {
        if (!dir.exists((dir_data_out))) {
            dir.create((dir_data_out))
        }
        for (file in list.files(path = dir_data_in)) {
            file.copy(file.path(dir_data_in, file), file.path(dir_data_out, file))
        }
    }
}


print_header <- function(title, subtitle, idx) {
    cat("---\n")
    cat(paste0("title: \"", str_replace(title, "\\$idx", toString(idx)), "\"\n"))
    cat(paste0("subtitle: \"", subtitle, "\"\n"))
    cat("---\n")
}


cat_files <- function(dir, pattern, with_solution = FALSE) {
    for (file in list.files(path = dir, pattern = pattern, full.names = TRUE)) {
        is_solution <- str_ends(file, ".sol.qmd")
        have_solution <- file.exists(str_replace(file, ".qmd", ".sol.qmd"))

        if (!is_solution) {
            cat("\n")
            for (l in read_lines(file)) cat(l, "\n")

            if (with_solution) {
                cat("\n### Lösung {-}\n")
            }

            if (!have_solution) {
                cat("\n")
                cat("Keine Lösung\n")
            }
        } else if (with_solution) {
            cat("\n")
            for (l in read_lines(file)) cat(l, "\n")
        }



        # if (!with_solution || have_solution || is_solution) {
        #     if (!is_solution || with_solution) {
        #         for (l in read_lines(file)) cat(l, "\n")
        #     }
        # }
    }
}


fname <- function(basename, idx) {
    paste0(basename, "-", str_pad(idx, 2, side = "left", pad = "0"), ".qmd")
}


# -------------------------------------------------------------------------------------------------
# Process
# -------------------------------------------------------------------------------------------------

# Variables
dir_in_base <- here()
dir_out <- file.path(getwd(), "c")
subdirs <- c("00-bilder", "01-daten")
yaml_content <- read_yaml("content.yml")

# Make output directory
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
        walk(subdirs, \(dir) copy_folder(dir, dir_in, dir_out))

        # Assignment
        sink(file.path(dir_out, fname("aufgabenblatt", idx)))
        print_header(assignments$title, entry$subtitle, idx)
        walk(entry$include, \(inc) cat_files(dir_in, inc, with_solution = FALSE))
        sink()

        # Solution
        if (assignments$with_solution) {
            sink(file.path(dir_out, fname(paste0("aufgabenblatt-loesung", entry$sol), idx)))
            print_header(paste0("Lösungen zu ", assignments$title), entry$subtitle, idx)
            walk(entry$include, \(inc) cat_files(dir_in, inc, with_solution = TRUE))
            sink()
        }

        # Loop
        idx <- idx + 1
    }
}
