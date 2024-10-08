#!/usr/bin/env Rscript

library(yaml)
library(here)
library(readr)
library(stringr)

if (!file.exists("content.yml")) {
    stop("Folder must contain file `content.yml`", call. = FALSE)
}

idx <- 1
dir_out <- file.path(getwd(), "c")
yaml_content <- read_yaml("content.yml")

dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

assignments <- yaml_content$assignments

for (entry in assignments$content) {
    dir_in <- file.path(here(entry$root), "aufgaben")
    dir_data_in <- file.path(dir_in, "01-daten")
    dir_data_out <- file.path(dir_out, "01-daten")

    # Copy data folder
    if (dir.exists(dir_data_in)) {
        if (!dir.exists((dir_data_out))) {
            dir.create((dir_data_out))
        }
        for (file in list.files(path = dir_data_in)) {
            file.copy(file.path(dir_data_in, file), file.path(dir_data_out, file))
        }
    }

    # Redirect output
    sink(file.path(dir_out, paste0("aufgabenblatt-", str_pad(idx, 2, side = "left", pad = "0"), ".qmd")))

    # Header
    cat("---\n")
    cat(paste0("title: \"", str_replace(assignments$title, "\\$idx", toString(idx)), "\"\n"))
    cat(paste0("subtitle: \"", entry$subtitle, "\"\n"))
    cat("---\n")

    # Include files
    for (inc in entry$include) {
        for (file in list.files(path = dir_in, pattern = inc)) {
            cat("\n")
            for (l in read_lines(file.path(dir_in, file))) {
                cat(l, "\n")
            }
        }
    }

    # Finish
    sink()
    idx <- idx + 1
}
