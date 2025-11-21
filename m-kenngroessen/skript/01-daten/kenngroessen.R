# -------------------------------------------------------------------------------------------------
# Hilfsfunktionen
# -------------------------------------------------------------------------------------------------

# Vektoren a, b zu a1, b1, a2, b2, ... kombinieren
mix <- function(a, b) as.vector(rbind(a, b))

# Modus
# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


# -------------------------------------------------------------------------------------------------
# Funktionen zum Plotten
# -------------------------------------------------------------------------------------------------

# Mittelwert
p_mittelwert <- function(d, m) {
  x_bar <- mean(pull(d, {{ m }}))
  ggplot(data = d) +
    geom_dotplot(
      mapping = aes(x = {{ m }}),
      method = "histodot",
      dotsize = 0.75,
      binwidth = 1,
      stackratio = 1.25
    ) +
    geom_vline(xintercept = x_bar, color = 'red', linewidth = 1.15) +
    scale_x_continuous(
      breaks = c(1, 10, 20),
      minor_breaks = 1:21,
      limits = c(1, 21)
    ) +
    scale_y_continuous(breaks = NULL, minor_breaks = NULL, limits = c(0, 1)) +
    annotate(
      "text",
      x = x_bar + 0.3,
      y = 0.5,
      label = "Arithmetisches Mittel",
      hjust = "left",
      vjust = "center",
      size = 6
    ) +
    labs(x = NULL, y = NULL)
}

# Lageregeln
p_lageregeln <- function(d, var) {
  values <- pull(d, a)
  frequencies <- pull(d, {{ var }})
  measures <- 
    tibble(X = rep(values, frequencies)) |> 
    summarise(
      Mittelwert = mean(X),
      Median = median(X),
      Modus = modes(X),
    ) |>
    pivot_longer(cols = everything()) |>
    mutate(name = factor(name, levels = c("Mittelwert", "Median", "Modus")))

  ggplot() +
    geom_col(
      data = d, mapping = aes(x = a, y = {{ var }})
    ) +
    geom_vline(
      data = measures,
      mapping = aes(xintercept = value, color = name),
      linewidth = 1.15
    ) +
    scale_x_continuous(
      breaks = values, minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = c(0, 5, 10), minor_breaks = NULL
    ) +
    ggtitle(
      paste("Stichprobe", rlang::as_name(rlang::enquo(var)))
    ) +
    theme(
      plot.title = element_text(size = 20)
    ) +
    labs(
      x = NULL, y = NULL, color = NULL
    )
}

# Stichprobe auf Zahlenstrahl
p_stichprobe <- function(d = d_random_sample) {
  ggplot(data = d) +
    geom_hline(
      yintercept = -0.1
    ) +
    geom_point(
      mapping = aes(x = X, y = -0.1),
      size = 3,
      shape = 21,
      fill = 'orange'
    ) +
    scale_y_continuous(
      limits = c(-0.225, 0.15)
    ) +
    theme_void()
}

p_stichprobe_marker <- function(p, v, l, py = 0.15, hjust = -0.2) {
  p +
    geom_vline(xintercept = v) +
    annotate(
      "label",
      x = v,
      y = py,
      label = l,
      hjust = hjust,
      size = annotation_fontsize,
      fill = "white",
      border.color = "white",
      parse = TRUE
    )
}

# Fünf Punkte auf Zahlenstrahle
p_fuenfpunkt_zusammenfassung <- function(d) {
  x <- pull(d, X)

  p_stichprobe(d) |>
    p_stichprobe_marker(min(x), "x[min]") |>
    p_stichprobe_marker(
      quantile(x, 0.25, type = 2),
      "x[0.25]",
      hjust = 1.05
    ) |>
    p_stichprobe_marker(
      quantile(x, 0.50, type = 2),
      "x[med]",
      hjust = 0.5
    ) |>
    p_stichprobe_marker(quantile(x, 0.75, type = 2), "x[0.75]") |>
    p_stichprobe_marker(max(x), "x[max]", hjust = 1.05)
}

# Lorenzkurve
p_lorenzkurve <- function(d, v, t = v, mb = NULL) {
  x <- sort(c(0, pull(d, v)))
  l <- tibble(
    u = seq(0, 1, length.out = length(x)),
    v = cumsum(x) / sum(x)
  )

  ggplot(data = l, mapping = aes(x = u, y = v)) +
    geom_ribbon(
      mapping = aes(ymin = u, ymax = v),
      fill = "steel blue",
      alpha = 0.1
    ) +
    geom_line(linewidth = 1.05) +
    geom_line(mapping = aes(y = u)) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = seq(0, 1, by = 1 / nrow(d)),
      minor_breaks = NULL,
      labels = c("0", rep("", nrow(d) - 1), "1")
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 1 / nrow(d)),
      minor_breaks = mb,
      labels = c("0", rep("", nrow(d) - 1), "1")
    ) +
    labs(title = t)
}


# -------------------------------------------------------------------------------------------------
# Beispieldaten Lagemaße
# -------------------------------------------------------------------------------------------------

# Arithmetisches Mittel
d_bsp_mittelwert <- tibble(
  X = c(1, 2, 6, 2, 5, 2, 4, 3, 1, 3, 2, 1, 2, 3, 4, 1, 2),
  Y = c(1, 2, 6, 2, 5, 2, 4, 3, 1, 3, 2, 1, 21, 3, 4, 1, 2)
)

# Median
d_bsp_median <-
  tibble(
    Urliste = c("A", "A", "C", "A", "B", "B", "D", "B", "C", "A", "B", "D")
  ) |>
  mutate(
    `Geordnete Urliste` = sort(Urliste)
  )

# Lageregeln
d_bsp_lageregeln <- tibble(
  a = 1:9,
  I = c(8, 10, 8, 6, 5, 4, 2, 2, 1),
  II = c(1, 2, 4, 8, 10, 8, 4, 2, 1),
  III = rev(c(8, 10, 8, 6, 5, 4, 2, 2, 1))
)

# Wasserstand für geometrisches Mittel
d_bsp_gm_w <- tibble(
  Tag = 0:6,
  Wasserstand = c(1, 2, 1.5, 4.5, 2.25, 4.5, 5)
)

# Wachstumsfaktoren
d_bsp_gm_f <-
  d_bsp_gm_w |>
  mutate(
    Wachstumsfaktor = Wasserstand / lag(Wasserstand)
  ) |>
  rename(Zeitraum = Tag) |>
  select(-Wasserstand) |>
  drop_na()

W0 <- pluck(d_bsp_gm_w, "Wasserstand", 1)
x_geom <- exp(mean(log(pull(d_bsp_gm_f, Wachstumsfaktor))))

# Mittlere Wachstumsfaktoren
d_bsp_gm_fm <- d_bsp_gm_f |>
  mutate(
    Wachstumsfaktor = x_geom
  )

# Wasserstand mit mittlerem Wachstum
d_bsp_gm_wm <- d_bsp_gm_w |>
  mutate(
    Wasserstand = W0 * x_geom^Tag
  )

rm(W0, x_geom)


# -------------------------------------------------------------------------------------------------
# Beispieldaten Maße der Varibilität
# -------------------------------------------------------------------------------------------------

# Niederschläge Bochum
d_ns_bo <- read_delim(
  "01-daten/produkt_nieder_tag_19310101_20171231_00555.txt",
  trim_ws = TRUE,
  delim = ";",
  locale = locale(decimal_mark = ".")
) |>
  mutate(
    Datum = ymd(MESS_DATUM),
    Jahr = year(Datum),
    Monat = month(Datum, label = TRUE),
    Niederschlag = na_if(RS, -999)
  ) |>
  select(
    Datum,
    Jahr,
    Monat,
    Niederschlag
  )

d_ns_bo_2008_2010 <- d_ns_bo |>
  filter(
    Jahr == 2008 | Jahr == 2010
  ) |>
  mutate(
    Jahr = factor(Jahr)
  )

d_ns_bo_2008_2010_monat <- d_ns_bo_2008_2010 |>
  group_by(
    Jahr,
    Monat
  ) |>
  summarise(
    Niederschlag = sum(Niederschlag)
  ) |>
  ungroup()

d_ns_bo_2008_monat <- d_ns_bo_2008_2010_monat |>
  filter(Jahr == 2008)

d_niederschlag_kenngroessen <- d_ns_bo_2008_2010_monat |>
  group_by(
    Jahr
  ) |>
  summarize(
    Mittelwert = mean(Niederschlag),
    Min = min(Niederschlag),
    Max = max(Niederschlag),
    Spannweite = max(Niederschlag) - min(Niederschlag)
  ) |>
  ungroup()

d_niederschlag_am <- d_ns_bo_2008_2010_monat |>
  group_by(
    Jahr
  ) |>
  mutate(
    Niederschlag = Niederschlag - mean(Niederschlag)
  ) |>
  ungroup()


# -------------------------------------------------------------------------------------------------
# Beispieldaten Quantile und Box-Plots
# -------------------------------------------------------------------------------------------------

set.seed(18)
d_random_sample <- tibble(X = sample(100, 20))


# -------------------------------------------------------------------------------------------------
# Beispieldaten Konzentrationsmaß: Der Gini-Koeffizient
# -------------------------------------------------------------------------------------------------

# Niederschläge Nha Trang
d_ns_nt_2015_monat <-
  read_excel(
    "01-daten/E01.08.xlsx",
    skip = 3,
    na = c("..", "..."),
    col_names = c("Jahr", "Ort", "1":"12"),
    col_types = c("text", "text", rep("numeric", 12))
  ) |>
  fill(
    Jahr
  ) |>
  pivot_longer(
    cols = '1':'12',
    names_to = "Monat",
    values_to = "Niederschlag",
    names_transform = as.integer
  ) |>
  mutate(
    Monat = month(Monat, label = TRUE)
  ) |>
  filter(
    Ort == "Nha Trang",
    Jahr == 2015
  )

d_ns_bo_nt <-
  d_ns_bo_2008_monat |>
  mutate(
    Ort = "Bochum",
  ) |>
  bind_rows(d_ns_nt_2015_monat)


# Weltkarte mit Gini-Koeffiziennt
d_bsp_gini <-
  left_join(
    ne_countries(scale = "medium", returnclass = "sf") |>
      filter(sovereignt != "Antarctica"),
    wb_data(
      indicator = "SI.POV.GINI"
    ) |>
      select(
        date,
        iso3c,
        country,
        gini = SI.POV.GINI
      ) |>
      drop_na() |>
      group_by(
        country
      ) |>
      filter(
        date == max(date)
      ) |>
      ungroup(),
    by = join_by(adm0_a3 == iso3c)
  )

d_bsp_lorenzkurve <- tibble(
  A = rep(4, 5),
  B = c(rep(1, 4), 16),
  C = 2:6
)

d_bsp_gini_grenzfaelle <- tibble(
  A = rep(1, 5),
  B = c(rep(0, 4), 1),
)

d_bsp_gini_reduktion <- tibble(
  A = c(9, 9, 9, 9, 9, 81, 81, 81, 81, 81),
  B = c(25, 25, 25, 25, 25, 25, 25, 25, 25, 225)
)
