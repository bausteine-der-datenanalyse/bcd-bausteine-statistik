#!/usr/bin/env Rscript

library(yaml)
library(here)
library(readr)
library(stringr)

if (!file.exists("content.yml")) {
    stop("Folder must contain file `content.yml`", call. = FALSE)
}

idx <- 1
od <- file.path(getwd(), "content")
yaml_content <- read_yaml("content.yml")

dir.create(od, recursive = TRUE, showWarnings = FALSE)

assignments <- yaml_content$assignments

for (entry in assignments$content) {
    ind <- file.path(here(entry$root), "aufgaben")
    aod <- file.path(od, paste0("aufgabenblatt-", str_pad(idx, 2, side = "left", pad = "0"), ".qmd"))

    sink(aod)

    cat("---\n")
    cat(paste0("title: \"", str_replace(assignments$title, "\\$idx", toString(idx)), "\"\n"))
    cat(paste0("subtitle: \"", entry$subtitle, "\"\n"))
    cat("---\n")

    for (inc in entry$include) {
        for (file in list.files(path = ind, pattern = inc)) {
            cat("\n")
            for (l in read_lines(file.path(ind, file))) {
                cat(l, "\n")
            }
        }
    }

    sink()
    idx <- idx + 1
}
