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

plot_correlation_example <- function(x, y, t) {
  x <- x - mean(x)
  y <- y - mean(y)
  d <- tibble(X = x, Y = y, Quadrant = factor(quadrant(x, y)))

  ggplot(data = d) +
    geom_vline(xintercept = 0, color = "gray60") +
    geom_hline(yintercept = 0, color = "gray60") +
    geom_point(
      mapping = aes(x = X, y = Y, fill = Quadrant),
      color = "black",
      shape = 21,
      size = 2.5
    ) +
    labs(title = t)
}


# -------------------------------------------------------------------------------------------------
# Funktionen Abbildungen
# -------------------------------------------------------------------------------------------------

tbl_wirtschaftswachstum <- function(s, e) {
  d_wirtschaftswachstum |>
    filter(Jahr >= s, Jahr < e) |>
    select(-Jahr) |>
    transpose_df(start = s) |>
    gt(rowname_col = "name") |>
    tab_options(table.width = pct(75))
}


# -------------------------------------------------------------------------------------------------
# Funktionen Abbildungen
# -------------------------------------------------------------------------------------------------

fig_correlation_idea <- function() {
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

  ggplot(data = d) +
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
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
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
    scale_fill_manual(values = c("blue", "red", "yellow", "green")) +
    scale_color_manual(values = c("blue", "red")) +
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
