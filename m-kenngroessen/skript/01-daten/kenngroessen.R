# -------------------------------------------------------------------------------------------------
# Hilfsfunktionen
# -------------------------------------------------------------------------------------------------

# Vektoren a, b zu a1, b1, a2, b2, ... kombinieren
mix <- function(a, b) as.vector(rbind(a, b))

# Geometrisches Mittel
mean_geom <- function(x) exp(mean(log(x)))

# Modus
# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

tab_niederschlag <- function(d) {
  # d |>
  #   pivot_wider(names_from = Monat, values_from = Niederschlag) |>
  #   gt(rowname_col = "Jahr") |>
  #   tab_stubhead("Jahr") |>
  #   tab_options(table.width = pct(90))
  d
}

tab_niederschlag_sortiert <- function(d) {
  d |>
    select(-Jahr, -Monat) |>
    pivot_wider(names_from = "Ort", values_from = "Niederschlag") |>
    transpose_df(col = "Rang") |>
    gt(rowname_col = "name") |>
    cols_label_with(fn = \(x) md(paste0("$x_{(", x, ")}$"))) |>
    tab_options(table.width = pct(75))
}


# -------------------------------------------------------------------------------------------------
# Lagemaße
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

# Wasserstand Reservoir für geometrisches Mittel
d_reservoir_w <- tibble(
  Tag = 0:6,
  Wasserstand = c(1, 2, 1.5, 4.5, 2.25, 4.5, 5)
)

d_reservoir_x <-
  d_reservoir_w |>
  mutate(
    Wachstumsfaktor = Wasserstand / lag(Wasserstand)
  ) |>
  rename(Zeitraum = Tag) |>
  select(-Wasserstand) |>
  drop_na()

# Funktionen zum Plotten
p_mittelwert <- function(d, m) {
  x_bar <- mean(pull(d, {{ m }}))
  ggplot(data = d) +
    geom_dotplot(
      mapping = aes(x = {{ m }}),
      method = "histodot",
      dotsize = 0.5,
      binwidth = 1,
      stackratio = 1.25
    ) +
    geom_vline(xintercept = x_bar, color = "red", linewidth = 1.15) +
    scale_x_continuous(
      breaks = c(1, 10, 20),
      minor_breaks = 1:21,
      limits = c(1, 21)
    ) +
    scale_y_continuous(breaks = NULL, minor_breaks = NULL, limits = c(0, 1)) +
    annotate(
      "text",
      x = x_bar + 0.3,
      y = 1,
      label = "Arithmetisches Mittel",
      hjust = "left",
      vjust = "top"
    ) +
    labs(x = NULL, y = NULL)
}

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
      data = d,
      mapping = aes(x = a, y = {{ var }})
    ) +
    geom_vline(
      data = measures,
      mapping = aes(xintercept = value, color = name),
      linewidth = 1.15
    ) +
    scale_x_continuous(
      breaks = values,
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = c(0, 5, 10),
      minor_breaks = NULL
    ) +
    ggtitle(
      paste("Stichprobe", rlang::as_name(rlang::enquo(var)))
    ) +
    theme(
      plot.title = element_text(size = 20)
    ) +
    labs(
      x = NULL,
      y = NULL,
      color = NULL
    )
}


# -------------------------------------------------------------------------------------------------
# Maße der Varibilität
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
# Quantile und Box-Plots
# -------------------------------------------------------------------------------------------------

set.seed(18)
d_random_sample <- tibble(X = sample(100, 20))

p_stichprobe_raw <- function(d = d_random_sample) {
  ggplot(data = d) +
    geom_hline(
      yintercept = -0.1
    ) +
    geom_point(
      mapping = aes(x = X, y = -0.1),
      size = 3,
      shape = 21,
      fill = "orange"
    )
}

p_stichprobe <- function(d = d_random_sample) {
  p_stichprobe_raw(d) +
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

fig_boxplot <- function() {
  d <- tibble(
    X = c(-15, -14, -6, -5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 2, 3, 4, 7, 12, 15, 16)
  )

  x <- pull(d, X)
  x_min <- min(x)
  x_25 <- quantile(x, 0.25, type = 2)
  x_50 <- quantile(x, 0.50, type = 2)
  x_75 <- quantile(x, 0.75, type = 2)
  x_max <- max(x)
  d_q <- x_75 - x_25
  normal_min <- x_25 - 1.5 * d_q
  normal_max <- x_75 + 1.5 * d_q
  by <- 0.02
  bh <- 0.03
  ay <- -0.225

  whisker <- d |>
    filter(normal_min <= X & X <= normal_max) |>
    summarise(
      min = min(X),
      max = max(X)
    )

  outliers <- d |>
    filter(X < normal_min | X > normal_max)

  p_fuenfpunkt_zusammenfassung(d) +
    geom_vline(
      xintercept = c(normal_min, normal_max)
    ) +
    geom_segment(
      data = whisker,
      mapping = aes(x = min, xend = max, y = by, yend = by),
      linewidth = 1.0
    ) +
    geom_point(
      data = outliers,
      mapping = aes(x = X, y = by),
      size = 3
    ) +
    annotate(
      "rect",
      xmin = x_25,
      xmax = x_75,
      ymin = by - bh,
      ymax = by + bh,
      color = "black",
      fill = fill_box_color,
      linewidth = 1.0
    ) +
    annotate(
      "segment",
      x = x_50,
      xend = x_50,
      y = by - bh,
      yend = by + bh,
      linewidth = 1.15
    ) +
    annotate(
      "segment",
      x = c(normal_min, x_25, x_75),
      xend = c(x_25, x_75, normal_max),
      y = ay,
      yend = ay,
      arrow = arrow(length = unit(0.5, "lines"), ends = "both")
    ) +
    annotate(
      "label",
      x = c(
        (2 * x_25 - 1.5 * d_q) / 2,
        (x_25 + x_75) / 2,
        (2 * x_75 + 1.5 * d_q) / 2
      ),
      y = ay,
      label = c(
        TeX("$1.5 \\cdot d_q$", output = "character"),
        TeX("$d_q$", output = "character"),
        TeX("$1.5 \\cdot d_q$", output = "character")
      ),
      fill = "white",
      border.color = "white",
      label.padding = unit(1, "lines"),
      parse = TRUE,
      size = annotation_fontsize
    )
}


# -------------------------------------------------------------------------------------------------
# Gini-Koeffizient
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
    cols = "1":"12",
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
