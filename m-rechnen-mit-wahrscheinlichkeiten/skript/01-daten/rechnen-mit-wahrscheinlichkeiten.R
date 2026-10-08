library(ggforce)

# -------------------------------------------------------------------------------------------------
# Dartscheibe
# -------------------------------------------------------------------------------------------------

# Radien, Winkel und Werte
r <- 170
r_i <- c(6.35, 15.9, 99, 107, 162, 170)
p_i <- seq(from = 0, to = 2 * pi, length.out = 21) - pi / 20
v_i <- c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10)

# Funktion um N Pfeile zu werfen
throw_darts <- function(N) {
  set.seed(2)
  x_i <- rep(0, N)
  y_i <- rep(0, N)

  # N Wuerfe
  for (i in 1:N) {
    x <- r
    y <- r
    # So lange werfen, bis Scheibe getroffen
    repeat {
      x <- runif(n = 1, min = -r, max = r)
      y <- runif(n = 1, min = -r, max = r)
      if (sqrt(x^2 + y^2) <= r) {
        break
      }
    }
    x_i[i] <- x
    y_i[i] <- y
  }
  tibble(x = x_i, y = y_i)
}

# Felder
fields <- tibble(
  r0 = rep(r_i[-1][-5], times = rep(20, 4)),
  r = rep(r_i[-(1:2)], times = rep(20, 4)),
  start = rep(p_i[-21], 4),
  end = rep(p_i[-1], 4),
  fill = factor(rep(c(rep(c(1, 2), 10), rep(c(3, 4), 10)), 2))
) |>
  arrange(-r0, start)

# Bull's Eye
eye <- tibble(
  r = rev(r_i[1:2]),
  fill = factor(c(3, 4))
)

# Text
dt <- tibble(
  x = 1.08 * r * cos((p_i[-21] + p_i[-1]) / 2),
  y = 1.08 * r * sin((p_i[-21] + p_i[-1]) / 2),
  text = as.character(v_i)
)

# Plot Scheibe
plot_dartscheibe <- function() {
  ggplot() +
    geom_arc_bar(
      data = fields,
      mapping = aes(x0 = 0, y0 = 0, r0 = r0, r = r, start = start, end = end, fill = fill)
    ) +
    geom_circle(data = eye, mapping = aes(x0 = 0, y0 = 0, r = r, fill = fill)) +
    geom_text(data = dt, mapping = aes(x = x, y = y, label = text)) +
    scale_fill_manual(
      values = c("2" = "#9D97A5", "1" = "#F3D8B7", "4" = "#B91718", "3" = "#2D9144"), guide = "none"
    ) +
    theme_void() +
    coord_equal(ratio = 1)
}
