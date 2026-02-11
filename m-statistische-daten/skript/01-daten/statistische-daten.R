# -------------------------------------------------------------------------------------------------
# Abfluss Donau
# -------------------------------------------------------------------------------------------------

d_donau <- read_delim(
  "01-daten/6342800.day",
  delim = ";",
  locale = locale(decimal_mark = "."),
  skip = 40,
  trim_ws = TRUE
) |>
  select(
    Datum = "YYYY-MM-DD",
    Abfluss = Original
  )


# -------------------------------------------------------------------------------------------------
# Tidy Data
# -------------------------------------------------------------------------------------------------

tab_tidydata <- function(d) gt(d) |> tab_options(table.width = pct(75))


d_tidydata_1 <- tribble(
  ~"Land"     , ~"Jahr" , ~"Typ"        , ~"Anzahl"  , # nolint: comma_linter
  "Italien"   ,    1999 , "Fälle"       ,        745 , # nolint: comma_linter
  "Italien"   ,    1999 , "Bevölkerung" ,   19987071 , # nolint: comma_linter
  "Italien"   ,    2000 , "Fälle"       ,       2666 , # nolint: comma_linter
  "Italien"   ,    2000 , "Bevölkerung" ,   20595360 , # nolint: comma_linter
  "Brasilien" ,    1999 , "Fälle"       ,      37737 , # nolint: comma_linter
  "Brasilien" ,    1999 , "Bevölkerung" ,  172006362 , # nolint: comma_linter
  "Brasilien" ,    2000 , "Fälle"       ,      80488 , # nolint: comma_linter
  "Brasilien" ,    2000 , "Bevölkerung" ,  174504898 , # nolint: comma_linter
  "China"     ,    1999 , "Fälle"       ,     212258 , # nolint: comma_linter
  "China"     ,    1999 , "Bevölkerung" , 1272915272 , # nolint: comma_linter
  "China"     ,    2000 , "Fälle"       ,     213766 , # nolint: comma_linter
  "China"     ,    2000 , "Bevölkerung" , 1280428583 , # nolint: comma_linter
)

d_tidydata_2 <- tribble(
  ~"Land"     , ~"Jahr" , ~"Fälle" , ~"Bevölkerung" , # nolint: comma_linter
  "Italien"   ,    1999 ,      745 ,       19987071 , # nolint: comma_linter
  "Italien"   ,    2000 ,     2666 ,       20595360 , # nolint: comma_linter
  "Brasilien" ,    1999 ,    37737 ,      172006362 , # nolint: comma_linter
  "Brasilien" ,    2000 ,    80488 ,      174504898 , # nolint: comma_linter
  "China"     ,    1999 ,   212258 ,     1272915272 , # nolint: comma_linter
  "China"     ,    2000 ,   213766 ,     1280428583 , # nolint: comma_linter
)

d_tidydata_3 <- tribble(
  ~"Land"     , ~"Jahr" , ~"Anteile"          , # nolint: comma_linter
  "Italien"   ,    1999 , "745/19987071"      , # nolint: comma_linter
  "Italien"   ,    2000 , "2666/20595360"     , # nolint: comma_linter
  "Brasilien" ,    1999 , "37737/172006362"   , # nolint: comma_linter
  "Brasilien" ,    2000 , "80488/174504898"   , # nolint: comma_linter
  "China"     ,    1999 , "212258/1272915272" , # nolint: comma_linter
  "China"     ,    2000 , "213766/1280428583" , # nolint: comma_linter
)

d_tidydata_41 <- tribble(
  ~"Land"     , ~"1999" , ~"2000" , # nolint: comma_linter
  "Italien"   ,     745 ,    2666 , # nolint: comma_linter
  "Brasilien" ,   37737 ,   80488 , # nolint: comma_linter
  "China"     ,  212258 ,  213766 , # nolint: comma_linter
)

d_tidydata_42 <- tribble(
  ~"Land"     , ~"1999"    , ~"2000"    , # nolint: comma_linter
  "Italien"   ,   19987071 ,   20595360 , # nolint: comma_linter
  "Brasilien" ,  172006362 ,  174504898 , # nolint: comma_linter
  "China"     , 1272915272 , 1280428583 , # nolint: comma_linter
)


# -------------------------------------------------------------------------------------------------
# Stichprobe
# -------------------------------------------------------------------------------------------------

fig_stichprobe <- function() {
  n <- 13.0
  set.seed(22202)

  find_sample <- Vectorize(
    function(x, y) {
      sx = pull(samples, x)
      sy = pull(samples, y)
      ss = pull(samples, s)
      for (i in 2:nrow(samples)) {
        bb = c(
          x >= sx[i] - ss[i] / 2,
          x <= sx[i] + ss[i] / 2,
          y >= sy[i] - ss[i] / 2,
          y <= sy[i] + ss[i] / 2
        )
        if (all(bb)) {
          return(i - 1)
        }
      }
      NA
    }
  )

  plot_bars <- function(df, title) {
    ggplot(data = df) +
      geom_bar(mapping = aes(x = c, fill = c), show.legend = FALSE) +
      scale_x_discrete(breaks = NULL) +
      scale_y_discrete(breaks = NULL) +
      labs(x = NULL, y = NULL, title = title) +
      theme_void()
  }

  samples <- tibble(
    x = c(n / 2 + 0.5, 5.5, 11.0),
    y = c(n / 2 + 0.5, 8.5, 4.0),
    s = c(n, 6.0, 3.0)
  )

  df <-
    tibble(
      x = rep(1:n, times = n),
      y = rep(1:n, times = rep(n, n)),
      c = sample(x = factor(1:4), size = n * n, replace = TRUE)
    ) |>
    mutate(
      sample = find_sample(x, y)
    )

  p1 <- ggplot(data = df) +
    geom_tile(
      data = samples,
      mapping = aes(x = x, y = y, width = s, height = s),
      fill = "light gray",
      color = "black",
      linewidth = 0.75
    ) +
    geom_point(
      mapping = aes(x = x, y = y, color = c),
      size = 2.5,
      show.legend = FALSE
    ) +
    annotate(
      geom = "text",
      x = 0.5,
      y = n + 1.25,
      label = "Grundgesamtheit",
      hjust = "left"
    ) +
    annotate(
      geom = "text",
      x = 10,
      y = n + 1.25,
      label = "Stichprobe A",
      hjust = "left"
    ) +
    annotate(
      geom = "step",
      x = c(7.5, 7.5, 9.9),
      y = c(11.5, n + 1.25, n + 1.25)
    ) +
    annotate(
      geom = "text",
      x = 10,
      y = -0.25,
      label = "Stichprobe B",
      hjust = "right"
    ) +
    annotate(
      geom = "step",
      x = c(10.1, 10.5),
      y = c(-0.25, 2.5)
    ) +
    coord_fixed() +
    theme_void()

  p21 <- plot_bars(df, "Grundgesamtheit")
  p22 <- plot_bars(df |> filter(sample == 1), "Stichprobe A")
  p23 <- plot_bars(df |> filter(sample == 2), "Stichprobe B")

  p1 +
    plot_spacer() +
    (plot_spacer() /
       p21 /
       p22 /
       p23 /
       plot_spacer() +
       plot_layout(heights = c(0.5, 1, 1, 1, 0.25))) +
    plot_layout(widths = c(1, 0.1, 1))
}
