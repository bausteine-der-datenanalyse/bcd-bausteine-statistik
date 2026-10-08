# -------------------------------------------------------------------------------------------------
# Studierende in Fachbereichen
# -------------------------------------------------------------------------------------------------

p_stichprobe <- function(d, nn) {
  ggplot(data = d) +
    geom_bar(
      mapping = aes(x = fb, y = after_stat(count / sum(count)), fill = fb),
      color = "black", show.legend = FALSE
    ) +
    labs(x = NULL, y = NULL, title = paste0("Umfrage ", nn)) +
    scale_y_continuous(labels = scales::percent)
}

to_table <- function(dfs, names) {
  map2(dfs, names, \(d, name) {
    d |>
      group_by(fb) |>
      summarise(p = 100 * n() / nrow(d)) |>
      mutate(name = name)
  }) |>
    bind_rows() |>
    pivot_wider(names_from = fb, values_from = p) |>
    gt(rowname_col = "name") |>
    fmt_number(decimals = 1)
}

set.seed(1)
d_fb <- tibble(
  fb = c(rep("A", 600), rep("B", 1575), rep("E", 1175), rep("G", 400), rep("M", 850), rep("W", 2300))
)
d_fb1 <- tibble(fb = sample(d_fb$fb, size = 250))
d_fb2 <- tibble(fb = sample(d_fb$fb, size = 250))
d_fb3 <- tibble(fb = sample(d_fb$fb, size = 250))


# -------------------------------------------------------------------------------------------------
# Mittelwert-Experiment
# -------------------------------------------------------------------------------------------------

mu <- 2
sigma <- 2

experiment <- function(nn, s) {
  d <- rep(0, nn)
  for (i in 1:nn) {
    d[i] <- mean(rnorm(n = s, mean = mu, sd = sigma))
  }
  d
}
