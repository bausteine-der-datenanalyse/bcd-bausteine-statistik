d_unistrasse <- read_excel(
  "01-daten/unistrasse-2017.xlsx",
  sheet = "raw(T)",
  range = "B2:H20712"
)

sample_in <- function(f, n, a, b) {
  r <- rep(0, n)
  i <- 1
  while (i <= n) {
    x <- f(1)
    if (a <= x && x <= b) {
      r[i] = x
      i = i + 1
    }
  }
  r
}

draw_coordinate_system <- function(
  xo,
  yo,
  xe,
  ye,
  xs = xo,
  ys = yo,
  arrowlength = 0.03,
  linewidth = 0.75
) {
  geom_path(
    data = tibble(x = c(xo, xo, NA, xs, xe), y = c(ye, ys, NA, yo, yo)),
    mapping = aes(x = x, y = y),
    arrow = arrow(angle = 20, length = unit(arrowlength, "npc"), end = "both"),
    linewidth = linewidth
  )
}

d_nq_examples <- function() {
  set.seed(22421)
  n <- 1500
  w <- 10

  tibble(
    Normalverteilt = w / 2 + sample_in(rnorm, n, -w / 2, w / 2),
    Gleichverteilt = w * runif(n),
    Linkssteil = sample_in(rlnorm, n, 0, w),
    Rechtssteil = w - sample_in(rlnorm, n, 0, w)
  ) |>
    pivot_longer(
      cols = everything(),
      names_to = "verteilung",
      values_to = "wert"
    ) |>
    mutate(
      verteilung = factor(
        verteilung,
        levels = c(
          "Normalverteilt",
          "Gleichverteilt",
          "Linkssteil",
          "Rechtssteil"
        )
      )
    )
}

fig_unistrasse <- function() {
  ggplot(data = d_unistrasse) +
    geom_histogram(
      mapping = aes(x = Geschwindigkeit, y = after_stat(density)),
      binwidth = 2,
      boundary = 1,
      closed = "left"
    ) +
    scale_x_continuous(breaks = c(25, 50, 75, 100)) +
    ylab("Relative Häufigkeit")
}

fig_dichtekurve <- function() {
  set.seed(105)

  # Data
  a <- 0.75
  b <- 1.5
  d_data <- tibble(x = 1.3 * rnorm(5000))

  # Plot histogram and density curve
  p <- ggplot(data = d_data, mapping = aes(x = x)) +
    geom_histogram(
      mapping = aes(y = after_stat(density)),
      binwidth = 0.75,
      boundary = 0,
      fill = "lightgray"
    ) +
    geom_density(
      bw = 1 / 2,
      fill = "lightblue",
      alpha = 1 / 2
    )

  # Get data from density curve
  # See https://stackoverflow.com/questions/31215748
  d_density <- get_layer_data(ggplot_build(p), i = 2) |>
    select(x, y) |>
    filter(x >= a, x <= b)

  # Plot
  p +
    geom_area(
      data = d_density,
      mapping = aes(x = x, y = y),
      fill = "dark violet",
      color = "black",
      alpha = 1 / 4
    ) +
    scale_x_continuous(
      breaks = c(0.75, 1.5),
      labels = c("a", "b"),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = NULL
    ) +
    draw_coordinate_system(-6, 0, 7.5, 0.3) +
    labs(x = NULL, y = NULL)
}

fig_quantile <- function(xp, lp) {
  #
  # Function
  f <- function(x) sin(x / 2) * sin(x)

  # Plot
  ggplot() +
    geom_ribbon(
      mapping = aes(x = after_stat(x), ymin = 0, ymax = after_stat(y)),
      stat = "function",
      fun = f,
      xlim = c(0, xp),
      color = "black",
      fill = "light blue"
    ) +
    geom_ribbon(
      mapping = aes(x = after_stat(x), ymin = 0, ymax = after_stat(y)),
      stat = "function",
      fun = f,
      xlim = c(xp, pi),
      color = "black",
      fill = "dark violet",
      alpha = 1 / 3
    ) +
    geom_segment(mapping = aes(x = xp, y = 0, yend = f(xp)), color = "black") +
    draw_coordinate_system(-0.3, 0, 3.6, 0.8) +
    scale_x_continuous(breaks = c(xp), labels = c(lp)) +
    scale_y_continuous(breaks = NULL) +
    labs(x = NULL, y = NULL)
}

fig_normalverteilung <- function() {
  mu <- 2

  s1 <- 0.75
  x1 <- mu + c(-s1, s1)
  f1 <- \(x) dnorm(x, mean = 2, sd = s1)

  s2 <- 1.5
  x2 <- mu + c(-s2, s2)
  f2 <- \(x) dnorm(x, mean = 2, sd = s2)

  ggplot() +
    geom_function(
      fun = f1,
      color = "hotpink",
      linewidth = 1,
      xlim = c(-2, 6)
    ) +
    annotate(
      "point",
      x = x1,
      y = f1(x1),
      shape = 21,
      fill = "hotpink",
      size = 2
    ) +
    annotate(
      "text",
      x = 3,
      y = 0.5,
      label = TeX("$\\sigma = 0.75$", output = "character"),
      parse = T
    ) +
    geom_function(
      fun = f2,
      color = "blue",
      linewidth = 1,
      xlim = c(-2, 6)
    ) +
    annotate("point", x = x2, y = f2(x2), shape = 21, fill = "blue", size = 2) +
    annotate(
      "text",
      x = 4.8,
      y = 0.1,
      label = TeX("$\\sigma = 1.5$", output = "character"),
      parse = T
    ) +
    annotate(
      "text",
      x = mu,
      y = 0.35,
      label = TeX("$\\mu = 2$", output = "character"),
      parse = T
    ) +
    draw_coordinate_system(0, 0, 6.5, 0.575, xs = -2.1, ys = -0.01) +
    scale_x_continuous(
      breaks = c(-2, 0.5, 1.25, 2, 2.75, 3.5, 6),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = c(f1(mu), f2(mu)),
      minor_breaks = NULL,
      labels = \(x) sprintf("%.2f", x)
    ) +
    labs(x = NULL, y = NULL)
}

fig_normalverteilung_prozentregel <- function() {
  ggplot() +
    stat_function(
      fun = dnorm,
      geom = "area",
      fill = "blue",
      alpha = 1 / 3,
      xlim = c(-1, 1)
    ) +
    stat_function(
      fun = dnorm,
      geom = "area",
      fill = "red",
      alpha = 1 / 3,
      xlim = c(-2, 2)
    ) +
    stat_function(
      fun = dnorm,
      geom = "area",
      fill = "yellow",
      alpha = 1 / 3,
      xlim = c(-3, 3)
    ) +
    annotate(
      "segment",
      x = c(-3, -2, -1, 1, 2, 3),
      y = 0,
      yend = dnorm(c(-3, -2, -1, 1, 2, 3)),
      linewidth = 0.25
    ) +
    geom_function(fun = dnorm, xlim = c(-3.5, 3.5), linewidth = 1) +
    scale_x_continuous(
      breaks = -3:3,
      labels = TeX(c(
        "$-3\\sigma$",
        "$-2\\sigma",
        "$-\\sigma$",
        "$\\mu$",
        "\\sigma",
        "2\\sigma",
        "3\\sigma"
      )),
      minor_breaks = NULL
    ) +
    scale_y_continuous(breaks = NULL) +
    labs(x = NULL, y = NULL)
}

fig_standardnormalverteilung <- function() {
  zp <- -0.3
  w2 <- 2.5

  p1 <- ggplot() +
    stat_function(
      fun = dnorm,
      geom = "area",
      xlim = c(-w2, zp),
      fill = "lightblue"
    ) +
    geom_function(
      fun = dnorm,
      linewidth = 1
    ) +
    draw_coordinate_system(
      0,
      0,
      w2,
      0.43,
      -w2
    ) +
    annotate(
      "point",
      x = c(-1, 1),
      y = dnorm(c(-1, 1))
    ) +
    annotate(
      "segment",
      x = zp,
      y = 0,
      yend = dnorm(zp),
      linewidth = 0.25
    ) +
    annotate(
      "text",
      label = TeX("$p = \\Phi(z_p$)$", output = "character"),
      parse = TRUE,
      x = zp,
      y = 0.03,
      hjust = 1.1
    ) +
    annotate(
      "text",
      label = TeX("$\\phi(z$)$", output = "character"),
      parse = TRUE,
      x = 0.5,
      y = 0.37,
      hjust = -0.2
    ) +
    scale_x_continuous(
      limits = c(-w2, w2),
      breaks = c(-2, -1, zp, 0, 1, 2),
      labels = c("-2", "-1", TeX("$z_p"), "0", "1", "2"),
      minor_breaks = NULL
    )

  p2 <- ggplot() +
    geom_function(
      fun = pnorm,
      linewidth = 1
    ) +
    draw_coordinate_system(
      0,
      0,
      w2,
      1,
      -w2
    ) +
    annotate(
      "text",
      label = TeX("$\\Phi(z$)$", output = "character"),
      parse = TRUE,
      x = 0.5,
      y = 0.93,
      hjust = 0.2
    ) +
    scale_x_continuous(
      limits = c(-w2, w2),
      breaks = c(-2, -1, zp, 0, 1, 2),
      labels = c(-2, -1, TeX("$z_p"), 0, 1, 2),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, pnorm(zp), 0.5, 0.75, 1),
      labels = c(0, 0.25, TeX("$p$"), 0.5, 0.75, 1),
      minor_breaks = seq(0, 1, by = 0.125)
    )

  p1 + p2 + plot_layout(axes = "collect") & labs(x = NULL, y = NULL)
}

fig_nq_plot <- function() {
  set.seed(29)

  # Parameters
  n <- 20
  dims <- list(w = 4, h = 4, d = 0.5, p = 1)
  xp <- cumsum(c(-dims$w / 2, dims$w, dims$d, dims$p))
  yp <- cumsum(c(-dims$h / 2, dims$h, dims$d, dims$p))
  dd <- 0.15
  ns <- n + 1
  dy <- 1 / ns

  # Data
  d <-
    tibble(
      sample = sort(sample_in(rnorm, n, -0.5 * dims$h, 0.5 * dims$h))
    ) |>
    mutate(
      quantiles = ppoints(n),
      theoretical = qnorm(quantiles)
    )

  d_qsample <- bind_rows(
    tibble(sample = yp[1], quantiles = 0),
    select(d, sample, quantiles),
    tibble(sample = yp[2], quantiles = pluck(d, "quantiles", -1))
  )

  d_qtheoretical <- tibble(
    x = seq(xp[1], xp[2], length.out = 50),
    y = pnorm(x)
  )

  # Plot
  ggplot(data = d) +
    # Rectangle in background
    annotate(
      "tile",
      x = 0,
      y = 0,
      width = dims$w,
      height = dims$h,
      fill = NA,
      color = "black",
      linewidth = 0.25
    ) +
    # NQ line
    annotate(
      "segment",
      x = xp[1],
      y = yp[1],
      xend = xp[2],
      yend = yp[2],
      linewidth = 0.25
    ) +
    # Theoretical
    draw_coordinate_system(
      0,
      yp[3],
      xp[2] + dd,
      yp[4] + dd,
      xs = xp[1],
      arrowlength = 0.015,
      linewidth = 0.35
    ) +
    geom_line(
      data = d_qtheoretical,
      mapping = aes(x = x, y = y + yp[3])
    ) +
    annotate(
      "text",
      x = xp[1],
      y = yp[4],
      label = "Theoretische \nVerteilungsfunktion",
      hjust = 0,
      vjust = 1.1
    ) +
    annotate(
      "text",
      x = xp[2] + dd,
      y = yp[3],
      label = "z",
      hjust = 0,
      vjust = 1.3
    ) +
    annotate(
      "text",
      x = xp[2] / 5,
      y = yp[3] + 1,
      label = TeX("$\\Phi(z)$", output = "character"),
      hjust = 0,
      vjust = 1.1,
      parse = TRUE
    ) +

    # Empirical
    draw_coordinate_system(
      # samples
      xp[3],
      yp[1],
      xp[4] + dd,
      yp[2] + dd,
      arrowlength = 0.015,
      linewidth = 0.35
    ) +
    geom_step(
      data = d_qsample,
      mapping = aes(x = quantiles + xp[3], y = sample),
      direction = 'vh'
    ) +
    annotate(
      "text",
      x = xp[4] + dd,
      y = yp[1] + dd,
      label = "Empirische \nVerteilungsfkt.",
      hjust = 1,
      vjust = 0
    ) +
    annotate(
      "text",
      x = xp[3],
      y = yp[2] + dd,
      label = "x",
      hjust = 2,
      vjust = 0
    ) +
    annotate(
      "text",
      x = xp[3] + 0.6,
      y = 0,
      label = TeX("$F(x)$", output = "character"),
      hjust = 0,
      vjust = 1,
      parse = TRUE
    ) +

    # Connecting lines
    geom_segment(
      # horizontal from qq
      mapping = aes(x = theoretical, xend = xp[3] + quantiles, y = sample),
      linewidth = 0.125,
      color = 'red'
    ) +
    geom_segment(
      # vertical from qq
      mapping = aes(x = theoretical, y = sample, yend = quantiles + yp[3]),
      linewidth = 0.125,
      color = 'red'
    ) +
    geom_segment(
      # vertical from ecdf
      mapping = aes(x = quantiles + xp[3], y = sample, yend = yp[3]),
      linewidth = 0.125,
      color = 'red'
    ) +
    geom_segment(
      # horizontal from tcdf
      mapping = aes(x = theoretical, xend = xp[3], y = quantiles + yp[3]),
      linewidth = 0.125,
      color = 'red'
    ) +
    geom_curve(
      mapping = aes(
        x = quantiles + xp[3],
        y = yp[3],
        xend = xp[3],
        yend = quantiles + yp[3]
      ),
      linewidth = 0.125,
      color = 'red'
    ) +

    # Points
    geom_point(
      mapping = aes(x = xp[3], y = sample),
      shape = 21,
      size = 2,
      fill = 'hotpink'
    ) +
    geom_point(
      mapping = aes(x = theoretical, y = yp[3]),
      shape = 21,
      size = 2,
      fill = 'lightblue'
    ) +
    geom_qq(aes(sample = sample)) +

    # Adjustments
    scale_x_continuous(
      breaks = c(xp[3], xp[4]),
      labels = c(0, 1),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = c(xp[3], xp[3] + 0.5, xp[4]),
      labels = c(0, "1/2", 1),
      minor_breaks = NULL
    ) +
    labs(x = NULL, y = NULL) +
    coord_fixed()
}

fig_kerndichteschaetzer <- function() {
  set.seed(10)

  n <- 35
  h <- 3 / 4
  xs <- -1.73

  d <- tibble(
    x = sample_in(rnorm, n, -2.5, 2.5)
  )

  K <- \(u) if_else(abs(u) <= 1, 0.75 * (1 - u^2), 0)
  Ks <- \(x) 1 / h * K((x - xs) / h)
  fhat <- Vectorize(\(x) 1 / (n * h) * sum(K((x - pull(d, x)) / h)))

  d2 <- d |>
    mutate(y = Ks(x)) |>
    filter(y > 0)

  ggplot(data = d, aes(x = x)) +
    draw_coordinate_system(
      -3,
      0,
      3,
      1.05,
      arrowlength = 0.02,
      linewidth = 0.55
    ) +
    geom_histogram(
      mapping = aes(y = after_stat(density)),
      binwidth = 0.4,
      center = 0,
      fill = 'wheat'
    ) +
    geom_segment(
      data = d2,
      mapping = aes(x = x, y = 0, yend = y),
      color = 'chartreuse3'
    ) +
    annotate(
      geom = 'segment',
      x = xs,
      y = 0,
      yend = fhat(xs),
      color = 'red',
      linewidth = 1
    ) +
    annotate(
      geom = 'segment',
      x = xs,
      y = 0,
      yend = Ks(xs),
      color = 'red',
      linewidth = 0.25
    ) +
    geom_function(
      fun = Ks,
      color = 'steelblue',
      linewidth = 1,
      xlim = c(-3, 2.5),
      n = 500
    ) +
    geom_function(
      fun = fhat,
      color = 'red',
      linewidth = 1,
      xlim = c(-3, 2.5),
      n = 500
    ) +
    geom_point(
      mapping = aes(y = 0),
      size = 2.5
    ) +
    annotate(
      geom = 'text',
      x = xs,
      y = 0,
      label = "x",
      parse = TRUE,
      vjust = 1.5
    ) +
    annotate(
      geom = 'text',
      x = 1.5,
      y = fhat(1.5),
      label = TeX(r"($\hat{f}(x)$)"),
      parse = TRUE,
      vjust = -1.3
    ) +
    annotate(
      geom = 'text',
      x = -1.4,
      y = 0.9,
      label = "Kernfunktion K für die Stelle x",
      hjust = 0,
    ) +
    theme_void()
}
