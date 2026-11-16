# -------------------------------------------------------------------------------------------------
# Hilfsfunktionen
# -------------------------------------------------------------------------------------------------

quadrant <- Vectorize(function(x, y) {
  if (x > 0) {
    if (y > 0) {
      "I"
    } else {
      "II"
    }
  } else {
    if (y > 0) {
      "IV"
    } else {
      "III"
    }
  }
})

# Bestimmtheitsmaß R^2
rsquare <- function(x, y) {
  xm <- mean(x)
  ym <- mean(y)
  beta <- sum((x - xm) * (y - ym)) / sum((x - xm)^2)
  alpha <- ym - beta * xm
  yh <- alpha + beta * x
  sum((yh - ym)^2) / sum((y - ym)^2)
}


# -------------------------------------------------------------------------------------------------
# Funktionen zum Plotten
# -------------------------------------------------------------------------------------------------

plot_rsquare_example <- function(y) {
  ggplot(data = d_rsquare_example, mapping = aes(x = x, y = {{ y }})) +
    geom_abline(slope = 1, color = "red", linewidth = 0.75) +
    geom_point(shape = 21, fill = "light blue")
}

plot_anscombe <- function(d, ylims) {
  plotit <- function(n) {
    ggplot(data = d, mapping = aes(x = .data[[paste0("x", n)]], y = .data[[paste0("y", n)]])) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        fullrange = TRUE,
        color = "red",
        linewidth = 0.75
      ) +
      geom_point(shape = 21, size = 2.5, fill = "light blue") +
      scale_x_continuous(
        limits = c(3, 21),
        breaks = seq(0, 21, by = 3),
        minor_breaks = NULL
      ) +
      scale_y_continuous(
        limits = ylims,
        breaks = seq(-6, 20, by = 3),
        minor_breaks = NULL
      ) +
      labs(title = n, x = NULL, y = NULL) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(
          margin = margin(
            t = 5,
            b = -30,
            l = 10
          )
        )
      )
  }
  plotit(1) + plotit(2) + plotit(3) + plotit(4) + plot_layout(axes = "collect")
}

plot_linear_regression <- function(d, xdat, ydat, xlab, ylab, n, log = FALSE) {
  x <- pull(d, {{ xdat }})
  y <- pull(d, {{ ydat }})
  if (log) {
    R2 <- rsquare(log10(x), y) # nolint
  } else {
    R2 <- rsquare(x, y) # nolint
  }
  r <- sqrt(R2)

  # Plot
  p <- ggplot(data = d, mapping = aes(x = {{ xdat }}, y = {{ ydat }})) +
    geom_point(shape = 21, fill = "light blue") +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = "red",
      linewidth = 0.75,
      se = FALSE
    ) +
    labs(
      x = xlab,
      y = ylab,
      title = TeX(paste(
        "$r =",
        format(r, digits = 4),
        "\\;\\;  R^2 =",
        format(R2, digits = 4)
      ))
    )

  # Logscale
  if (log) {
    p +
      scale_x_log10(
        labels = function(x) {
          paste0(x / 1000, "K")
        }
      )
  } else {
    p
  }
}

plot_life_expectancy <- function(xvar, xlabel, xlim, xlog = FALSE) {
  special_countries <- c("United States", "Sierra Leone", "Germany", "Sri Lanka")

  d <- d_wb_latest |>
    mutate(special = country %in% special_countries)

  p <- ggplot(mapping = aes(x = {{ xvar }}, y = le)) +
    geom_point(
      data = d,
      mapping = aes(fill = special),
      shape = 21,
      show.legend = FALSE
    ) +
    geom_label(
      data = filter(d, special),
      mapping = aes(label = country),
      hjust = 0.8,
      nudge_y = 2,
      size = label_fontsize,
      alpha = 0.5
    ) +
    scale_fill_manual(values = c("light blue", "red")) +
    labs(x = xlabel, y = "Mittlere Lebenserwartung")

  if (xlog) {
    f <- function(x) {
      paste0(x / 1000, "K")
    }
    p + scale_x_log10(limits = xlim, labels = f)
  } else {
    p + scale_x_continuous(limits = xlim)
  }
}

plot_correlation_example <- function(x, y, t, farbig = TRUE) {
  x <- x - mean(x)
  y <- y - mean(y)
  d <- tibble(X = x, Y = y, Quadrant = factor(quadrant(x, y)))

  p <- ggplot(data = d) +
    geom_vline(xintercept = 0, color = "gray60") +
    geom_hline(yintercept = 0, color = "gray60")

  if (farbig) {
    p <- p +
      geom_point(mapping = aes(x = X, y = Y, fill = Quadrant), color = "black", shape = 21, size = 2.5) +
      scale_fill_manual(values = c("#2F6BC6", "#D33F49", "#EBC63F", "#43A047"))
  } else {
    p <- p + geom_point(mapping = aes(x = X, y = Y), fill = "light blue", color = "black", shape = 21, size = 2.5)
  }

  p + labs(title = t)
}


# -------------------------------------------------------------------------------------------------
# Funktionen Abbildungen
# -------------------------------------------------------------------------------------------------

tab_wirtschaftswachstum <- function(s, e) {
  d_wirtschaftswachstum |>
    filter(Jahr >= s, Jahr < e) |>
    select(-Jahr) |>
    transpose_df(start = s) |>
    gt(rowname_col = "name") |>
    tab_options(
      quarto.disable_processing = TRUE,
      table.width = pct(90)
    )
}


# -------------------------------------------------------------------------------------------------
# Funktionen Abbildungen
# -------------------------------------------------------------------------------------------------

p_correlation_idea <- function() {
  d <-
    tibble(x = c(-1, -0.7, 0.7, 1), y = c(1.5, -0.7, 0.6, -0.4)) |>
    mutate(
      x = x - mean(x),
      y = y - mean(y),
      x1 = x + sign(x) * abs(y),
      y1 = y + sign(y) * abs(x),
      c = factor(if_else(x * y > 0, 1, 2))
    )
  xi <- pluck(d, "x", 4)
  yi <- pluck(d, "y", 4)

  # Globales Koordinatensystem links und unten
  x0 <- min(d$x1) - 0.3
  y0 <- min(d$y1) - 0.3
  x_end <- max(d$x1) + 0.3
  y_end <- max(d$y1) + 0.3

  ggplot(data = d) +
    annotate(
      geom = "segment",
      x = x0, y = y0, xend = x_end, yend = y0,
      arrow = arrow(length = unit(0.025, "npc"), angle = 20)
    ) +
    annotate(
      geom = "segment",
      x = x0, y = y0, xend = x0, yend = y_end,
      arrow = arrow(length = unit(0.025, "npc"), angle = 20)
    ) +
    annotate(geom = "text", x = x_end, y = y0, label = "x", fontface = "italic", hjust = 0.5, vjust = 1.8) +
    annotate(geom = "text", x = x0, y = y_end, label = "y", fontface = "italic", hjust = 1.8, vjust = 0.5) +
    annotate(
      geom = "text", x = 0, y = y0, label = TeX("$\\bar{x}$", output = "character"),
      hjust = 0.5, vjust = 1.5, parse = TRUE
    ) +
    annotate(
      geom = "text", x = x0, y = 0, label = TeX("$\\bar{y}$", output = "character"),
      hjust = 1.5, vjust = 0.5, parse = TRUE
    ) +
    geom_rect(
      mapping = aes(
        xmin = 0,
        ymin = 0,
        xmax = x,
        ymax = y,
        fill = c
      ),
      alpha = 0.5
    ) +
    geom_rect(
      mapping = aes(
        xmin = x,
        ymin = 0,
        xmax = x1,
        ymax = y,
        fill = factor(3)
      ),
      alpha = 0.25,
      linewidth = 0.25,
      color = "black"
    ) +
    geom_rect(
      mapping = aes(
        xmin = 0,
        ymin = y,
        xmax = x,
        ymax = y1,
        fill = factor(4)
      ),
      alpha = 0.25,
      linewidth = 0.25,
      color = "black"
    ) +
    annotate(geom = "segment", x = 0, y = y0, xend = 0, yend = y_end) +
    annotate(geom = "segment", x = x0, y = 0, xend = x_end, yend = 0) +
    geom_segment(
      mapping = aes(
        x = x,
        y = 0,
        yend = y,
        color = factor(y < 0)
      ),
      arrow = arrow(length = unit(0.025, "npc"), angle = 20),
      linewidth = 0.75
    ) +
    geom_segment(
      mapping = aes(
        x = 0,
        xend = x,
        y = y,
        color = factor(x < 0)
      ),
      arrow = arrow(length = unit(0.025, "npc"), angle = 20),
      linewidth = 0.75
    ) +
    geom_point(
      mapping = aes(x = x, y = y),
      fill = "lightblue",
      color = "black",
      shape = 21,
      size = 2
    ) +
    scale_fill_manual(values = c("#2F6BC6", "#D33F49", "#EBC63F", "#43A047")) +
    scale_color_manual(values = c("#2F6BC6", "#D33F49")) +
    annotate(
      geom = "text",
      x = xi,
      y = yi,
      label = TeX("$(x_i, y_i)$", output = "character"),
      hjust = -0.2,
      vjust = 1.2,
      parse = TRUE
    ) +
    annotate(
      geom = "text",
      x = xi,
      y = yi / 2,
      label = TeX("$y_i - \\bar{y}$", output = "character"),
      hjust = -0.2,
      vjust = 0.5,
      parse = TRUE
    ) +
    annotate(
      geom = "text",
      x = xi / 2,
      y = yi,
      label = TeX("$x_i - \\bar{x}$", output = "character"),
      hjust = 0.5,
      vjust = 1.5,
      parse = TRUE
    ) +
    annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = TeX("$(\\bar{x}, \\bar{y})$", output = "character"),
      hjust = -0.2,
      vjust = 1.2,
      parse = TRUE
    ) +
    annotate(
      geom = "point",
      x = 0,
      y = 0,
      size = 2.5
    ) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
}

p_sachverstaendigenrat <- function() {
  ggplot(data = d_wirtschaftswachstum, mapping = aes(x = Prognose, y = Wachstum)) +
    geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 0.75) +
    geom_point(shape = 21, fill = "light blue", size = 2.5) +
    geom_rug()
}

p_lebenserwartung <- function(ncol = 2) {
  p1 <- plot_life_expectancy(he, "Ausgaben Gesundheitswesen (% BIP)", c(0, 17))
  p2 <- plot_life_expectancy(edu, "Ausgaben Bildung (% BIP)", c(0, 10.5))
  p3 <- plot_life_expectancy(gini, "Gini-Koeffizient", c(10, 70))
  p4 <- plot_life_expectancy(gdppc, "BIP pro Kopf (USD)", c(110, NA), TRUE)

  p1 + p2 + p3 + p4 + plot_layout(axes = "collect", ncol = ncol)
}

p_blasendiagramm <- function() {
  ggplot(data = d_wb_latest) +
    geom_point(
      mapping = aes(x = he, y = le, fill = gdppc, size = gdppc),
      shape = 21, show.legend = FALSE, alpha = 0.8
    ) +
    scale_size(range = c(0.1, 10)) +
    scale_fill_distiller(palette = "Reds", direction = 1) +
    labs(x = "Ausgaben Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung")
}

p_histogramm_dichte_2d <- function() {
  p0 <- ggplot(data = d_wb_latest, mapping = aes(x = he, y = le))

  p1 <- p0 +
    geom_bin_2d(binwidth = c(1.25, 2.5), color = "black", linewidth = 0.25) +
    scale_fill_distiller(palette = "Blues", direction = 1)

  p2 <- p0 +
    geom_density_2d_filled(color = "black", linewidth = 0.25) +
    scale_fill_brewer(palette = "Blues")

  p1 + p2 + plot_layout(axes = "collect") &
    xlim(0, 20) & ylim(45, 85) &
    labs(x = "Ausgaben Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung", fill = "Anzahl") &
    theme(legend.position = "none")
}

p_korrelation_beispiele <- function(farbig = TRUE) {
  withr::with_seed(6, {
    n <- 200
    x <- runif(n, min = -1, max = 1)
    p1 <- plot_correlation_example(x, x + rnorm(n, sd = 0.3), TeX("A: $r > 0$"), farbig)
    p2 <- plot_correlation_example(x, -x + rnorm(n, sd = 0.3), TeX("B: $r < 0$"), farbig)
    p3 <- plot_correlation_example(x, rnorm(n, sd = 0.3), TeX("C: $r \\approx 0$"), farbig)
    p4 <- plot_correlation_example(0.4 * x, rnorm(n, sd = 1), TeX("D: $r \\approx 0$"), farbig)
    p5 <- plot_correlation_example(x, -1 + 2 * x^2 + rnorm(n, sd = 0.3), TeX("E: $r \\approx 0$"), farbig)
    p6 <- plot_correlation_example(x, 1 - 2 * x^2 + rnorm(n, sd = 0.3), TeX("F: $r \\approx 0$"), farbig)
  })

  p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(guides = "collect") &
    theme_void() & xlim(-1, 1) & ylim(-2, 2)
}

p_pisa <- function() {
  countries <- c("Singapore", "Finland", "Brazil", "Switzerland")
  d <- mutate(d_pisa, sel = country %in% countries)

  ggplot(data = d, mapping = aes(x = read, y = math)) +
    geom_point(mapping = aes(fill = sel), shape = 21, show.legend = FALSE) +
    geom_label(
      data = filter(d, country %in% countries),
      mapping = aes(label = country),
      hjust = 1, nudge_x = -3, nudge_y = 5, size = label_fontsize, alpha = 0.5
    ) +
    scale_fill_manual(values = c("light blue", "red")) +
    labs(x = "Lesekompetenz", y = "Mathematische Kompetenz")
}

p_pisa_regression <- function() {
  ggplot(data = d_pisa, mapping = aes(x = read, y = math)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linewidth = 0.75) +
    geom_point(shape = 21, fill = "light blue", size = 2.5) +
    labs(x = "Lesekompetenz", y = "Mathematische Kompetenz")
}

p_anscombe_residuen <- function() {
  anscombe |>
    mutate(
      y1 = y1 - 3 - 0.5 * x1,
      y2 = y2 - 3 - 0.5 * x2,
      y3 = y3 - 3 - 0.5 * x3,
      y4 = y4 - 3 - 0.5 * x4,
    ) |>
    plot_anscombe(c(-4, 4))
}

p_bestimmtheitsmass_beispiel <- function() {
  plot_rsquare_example(y1) + plot_rsquare_example(y2) + plot_layout(axes = "collect") &
    scale_x_continuous(breaks = 0:1, limits = c(0, 1)) &
    scale_y_continuous(breaks = 0:1, limits = c(0, 1)) &
    labs(x = NULL, y = NULL)
}

p_loess <- function() {
  d <- withr::with_seed(1, tibble(x = 2 * pi * runif(300), y = sin(x) + 0.2 * rnorm(length(x))))

  ggplot(data = d, mapping = aes(x = x, y = y)) +
    geom_point(shape = 21, fill = "light blue", size = 2.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "red", linewidth = 0.75) +
    labs(x = NULL, y = NULL)
}

p_regression_lebenserwartung <- function(ncol = 2) {
  ylab <- "Mittlere Lebenserwartung"
  p1 <- plot_linear_regression(d_wb_latest, he, le, "Ausgaben Gesundheitswesen (% BIP)", ylab)
  p2 <- plot_linear_regression(d_wb_latest, edu, le, "Ausgaben Bildungswesen (% BIP)", ylab)
  p3 <- plot_linear_regression(d_wb_latest, gini, le, "Gini-Koeffizient", ylab)
  p4 <- plot_linear_regression(d_wb_latest, gdppc, le, "BIP pro Kopf", ylab, log = TRUE)

  p1 + p2 + p3 + p4 + plot_layout(axes = "collect", ncol = ncol)
}


# -------------------------------------------------------------------------------------------------
# Beispieldaten
# -------------------------------------------------------------------------------------------------

set.seed(103)

load("01-daten/pisa.Rdata")
load("01-daten/data-wb.Rdata")

d_wirtschaftswachstum <- read_csv("01-daten/wirtschaftswachstum.csv", col_types = "idd")

d_wb_latest <- d_wb_latest |> ungroup()

d_rsquare_example <-
  tibble(
    x = runif(250),
    y1 = x + 0.05 * rnorm(length(x)),
    y2 = x + 0.15 * rnorm(length(x))
  ) |>
  filter(x >= 0, x <= 1, y1 >= 0, y1 <= 1, y2 >= 0, y2 <= 1)
