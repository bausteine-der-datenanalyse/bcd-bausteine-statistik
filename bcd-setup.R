library(gt)
library(readxl)
library(cowplot)
library(wbstats)
library(latex2exp)
library(lubridate)
library(patchwork)
library(tidyverse)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)

Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")

fill_box_color <- "lightblue"
fill_point_color <- "white"
outline_color <- "black"
annotation_fontsize <- 6
label_fontsize <- 4

update_geom_defaults(
  "bar",
  list(fill = fill_box_color, color = outline_color)
)
update_geom_defaults(
  "point",
  list(fill = fill_point_color, color = outline_color)
)
update_geom_defaults(
  "boxplot",
  list(fill = fill_box_color, color = outline_color)
)
update_geom_defaults(
  "dotplot",
  list(fill = fill_box_color, color = outline_color)
)

# Optionen fuer LaTeX-Ausgabe
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(
    fig.width = 9,
    fig.asp = 0.618,
    fig.align = "center",
    out.width = "85%",
    echo = FALSE
  )
  theme_set(
    theme_bw(base_size = 16)
  )
  update_theme(
    plot.title = element_text(size = 16)
  )
  update_geom_defaults(
    "text",
    list(size = 5)
  )
}

# Optionen fuer HTML-Ausgabe
if (knitr::is_html_output()) {
  knitr::opts_chunk$set(
    fig.width = 9,
    fig.asp = 0.618,
    fig.align = "center",
    out.width = "85%",
    echo = FALSE
  )
  theme_set(
    theme_gray(base_size = 16)
  )
}


# Transponieren mit Index als Merkmal
transpose_df <- function(d, start = 1, col = NULL) {
  if (is.null(col)) {
    d |>
      mutate(n = start:(nrow(d) + start - 1)) |>
      pivot_longer(cols = !n) |>
      pivot_wider(names_from = n)
  } else {
    d |>
      pivot_longer(cols = !starts_with(col)) |>
      pivot_wider(names_from = all_of(col))
  }
}
