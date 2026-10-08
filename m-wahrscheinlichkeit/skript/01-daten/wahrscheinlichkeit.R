# -------------------------------------------------------------------------------------------------
# Reißzwecken: U = Spitze auf dem Boden, O = Spitze oben
# -------------------------------------------------------------------------------------------------

reisszwecken <- "UUUOOUOUOOOOUOOUOOOUUOOOOUOOOOOOOOOUUUUOOOOUUUUUOUOOOUUOOOOOOOOOOOUUUUOO"

d_reisszwecken <- tibble(
  wurf = seq_len(nchar(reisszwecken)),
  ergebnis = strsplit(reisszwecken, "")[[1]],
  f = cumsum(ergebnis == "U") / wurf
)

tab_reisszwecken <- function() {
  f <- c(d_reisszwecken$f, rep(NA, 8))
  matrix(f, ncol = 10, byrow = TRUE) |>
    as_tibble(.name_repair = ~ paste0("V", seq_along(.x))) |>
    gt() |>
    fmt_number(decimals = 3) |>
    sub_missing(missing_text = "") |>
    cols_label(everything() ~ "") |>
    tab_options(column_labels.hidden = TRUE)
}
