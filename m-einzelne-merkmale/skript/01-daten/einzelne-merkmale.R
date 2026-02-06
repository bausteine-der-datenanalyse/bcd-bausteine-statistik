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
            Energieträger = case_match(
                Energieträger,
                "andere Erneuerbare 2)" ~ "Andere Erneuerbare",
                "Wasser- und Windkraft 1) 3)" ~ "Wasser- und Windkraft",
                "Sonstige 4)" ~ "Sonstige",
                .default = Energieträger
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
