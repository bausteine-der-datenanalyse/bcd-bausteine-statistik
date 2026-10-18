library(readxl)
library(tidyverse)


# -------------------------------------------------------------------------------------------------
# Beispieldaten Energieträger
# -------------------------------------------------------------------------------------------------

import_energietrager <- function(range) {
  read_excel(
    "01-daten/energiedaten-gesamt-xls.xlsx",
    sheet = "4",
    range = range
  ) |>
    mutate(
      Energieträger = replace_values(
        Energieträger,
        "andere Erneuerbare 2)" ~ "Andere Erneuerbare",
        "Wasser- und Windkraft 1) 3)" ~ "Wasser- und Windkraft",
        "Sonstige 4)" ~ "Sonstige"
      ),
      Energieträger = factor(Energieträger, levels = Energieträger)
    ) |>
    filter(
      Energieträger != "Außenhandelssaldo Strom"
    ) |>
    pivot_longer(
      cols = !Energieträger,
      names_to = "Jahr",
      values_to = "Wert"
    )
}


c_energietraeger <- c(
  "#107aa1",
  "#686868",
  "#a63910",
  "#fbd941",
  "#dd326e",
  "#98dedd",
  "#5eab3c",
  "#98bade"
)


# -------------------------------------------------------------------------------------------------
# Beispieldaten Klausur
# -------------------------------------------------------------------------------------------------

d_klausur <- read_excel(
  "01-daten/klausur-mathematik.xls"
) |>
  mutate(
    note = round(prozente_g),
    bestanden = note >= 50
  ) |>
  select(prozent = prozente_g, note, bestanden)


# -------------------------------------------------------------------------------------------------
# Histogram plotten
# -------------------------------------------------------------------------------------------------

p_histogram <- function(x, q = 1) {
  d <- tibble(x = x) |> filter(x < quantile(x, q))
  ggplot(d) +
    geom_histogram(mapping = aes(x = x), bins = 40) +
    theme_void()
}


# -------------------------------------------------------------------------------------------------
# Empirische Verteilungsfunktion plotten (nur waagerechte Linien, Punkt am Anfang, Kreis am Ende)
# -------------------------------------------------------------------------------------------------

p_ecdf_exakt <- function(x, linewidth = 1, size = 2, pad = 0.05) {
  xs <- sort(unique(x))
  fs <- ecdf(x)(xs)
  n <- length(xs)
  dx <- pad * max(diff(range(xs)), 1)

  d_linien <- tibble(x = c(xs[1] - dx, xs), xend = c(xs, xs[n] + dx), y = c(0, fs))
  d_punkte <- tibble(x = xs, y = fs)
  d_kreise <- tibble(x = xs, y = c(0, fs[-n]))

  ggplot() +
    geom_segment(data = d_linien, mapping = aes(x = x, xend = xend, y = y, yend = y), linewidth = linewidth) +
    geom_point(data = d_kreise, mapping = aes(x = x, y = y), shape = 21, fill = "white", size = size) +
    geom_point(data = d_punkte, mapping = aes(x = x, y = y), size = size)
}
