library(readxl)
library(tidyverse)

# Daten Energieträger

import_energietrager <- function(range) {
    read_excel("01-daten/energiedaten-gesamt-xls.xlsx", sheet = "4", range = range) |>
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
        filter(Energieträger != "Außenhandelssaldo Strom") |>
        pivot_longer(cols = !Energieträger, names_to = "Jahr", values_to = "Wert")
}

d_energietraeger_pj <- import_energietrager("A8:AC17")
d_energietraeger_anteil <- import_energietrager("A21:AC30")
d_energietraeger_anteil_2017 <- d_energietraeger_anteil |> filter(Jahr == 2017)

c_energietraeger <- c(
    "#107aa1", "#686868", "#a63910", "#fbd941",
    "#dd326e", "#98dedd", "#5eab3c", "#98bade"
)


# Daten Kreisdiagramm/Balkendiagramm

d_pob_1 <- tibble(N = rev(c("A", "B", "C", "D")), W = c(16, 17.5, 19, 21.5))
d_pob_2 <- tibble(N = rev(c("A", "B", "C", "D")), W = c(19, 17.5, 21.5, 16))
d_pob_3 <- tibble(N = rev(c("A", "B", "C", "D")), W = rev(c(16, 17.5, 19, 21.5)))

plot_pob_pie <- function(d) {
    pos <- cumsum(d$W) - d$W / 2
    ggplot(data = d) +
        geom_col(mapping = aes(x = factor(1), y = W, fill = N), show.legend = FALSE) +
        geom_text(aes(x = factor(1), y = pos, label = N)) +
        coord_polar(theta = "y") +
        scale_fill_brewer("Paired") +
        theme_void()
}

plot_pob_bar <- function(d) {
    ggplot(data = d) +
        geom_col(mapping = aes(x = N, y = W, fill = N), show.legend = FALSE) +
        scale_y_continuous(labels = NULL, breaks = NULL) +
        scale_fill_brewer("Paired") +
        labs(x = NULL, y = NULL)
}


# Daten Beispiel Klausur

d_klausur <- read_excel("Klausurergebnis_Mathe2_SoSe2018.xls") |>
    mutate(prozente_g = round(prozente_g, 2))

d_klausur_bestanden <- mutate(d_klausur,
    prozente_g = round(prozente_g),
    b = prozente_g >= 50
)


# Plot charakterisierung von Verteilungen

plot_verteilung <- function(x, q = 1) {
    tibble(X = x) |>
        filter(X < quantile(X, q)) |>
        ggplot() +
        geom_histogram(mapping = aes(x = X), bins = 40) +
        theme_void()
}


# Daten Beispiel Häufigkeitsverteilung

d_hfkv_a <- 0.8
d_hfkv_b <- 4.3
d_hfkv_1 <- tibble(Ausprägung = c(1.1, 1.3, 1.9, 2.5, 2.1, 1.3, 4)) |>
    group_by(Ausprägung) |>
    summarise(Häufigkeit = n())
d_hfkv_2 <- d_hfkv_1 |>
    rename(Xs = Ausprägung, n = Häufigkeit) |>
    mutate(Xe = lead(Xs, default = d_hfkv_b), S = cumsum(n)) |>
    add_row(Xs = d_hfkv_a, n = 0, S = 0, Xe = 1.1, .before = 1) |>
    select(Xs, Xe, S)
