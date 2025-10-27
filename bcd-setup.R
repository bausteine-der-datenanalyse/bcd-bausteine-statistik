library(gt)
library(readxl)
library(cowplot)
library(wbstats)
library(latex2exp)
library(patchwork)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)

Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")

fill_box_color <- "lightblue"
fill_point_color <- "white"
outline_color <- "black"
annotation_fontsize <- 6

update_geom_defaults("bar", list(fill = fill_box_color, color = outline_color))
update_geom_defaults("point", list(fill = fill_point_color, color = outline_color))
update_geom_defaults("boxplot", list(fill = fill_box_color, color = outline_color))
update_geom_defaults("dotplot", list(fill = fill_box_color, color = outline_color))

if (knitr::is_latex_output()) {
    knitr::opts_chunk$set(
        fig.width = 9, fig.asp = 0.618, fig.align = "center", out.width = "85%", echo = FALSE
    )
    theme_set(theme_bw(base_size = 16))
}

if (knitr::is_html_output()) {
    knitr::opts_chunk$set(
        fig.width = 9, fig.asp = 0.618, fig.align = "center", out.width = "85%", echo = FALSE
    )
    theme_set(theme_gray(base_size = 16))
}
