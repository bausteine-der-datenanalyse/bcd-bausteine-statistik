library(readxl)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(kableExtra)

fill_box_color <- "lightblue"
fill_point_color <- "white"
outline_color <- "black"

update_geom_defaults("bar", list(fill = fill_box_color, color = outline_color))
update_geom_defaults("point", list(fill = fill_point_color, color = outline_color))
update_geom_defaults("boxplot", list(fill = fill_box_color, color = outline_color))
update_geom_defaults("dotplot", list(fill = fill_box_color, color = outline_color))

knitr::opts_chunk$set(
    fig.width = 9, fig.asp = 0.618, fig.aligh = "center", out.width = "85%", echo = FALSE
)

if (knitr::is_latex_output()) {
    theme_set(theme_bw(base_size = 16))
}

if (knitr::is_html_output()) {
    theme_set(theme_gray(base_size = 16))
}
