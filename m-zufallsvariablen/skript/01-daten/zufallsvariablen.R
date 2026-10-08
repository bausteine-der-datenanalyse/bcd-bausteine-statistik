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

# Basiswert
v <- function(p) {
  if (p < 0) {
    p <- p + 2 * pi
  }
  if (p > p_i[21]) {
    p <- p - 2 * pi
  }
  v_i[findInterval(p, p_i)]
}

# Multiplikator
m <- function(r) {
  if (r >= r_i[5]) {
    2
  } else if (r >= r_i[3] & r <= r_i[4]) {
    3
  } else {
    1
  }
}

# Punktzahl
Z <- Vectorize(
  function(x, y) {
    r <- sqrt(x^2 + y^2)
    if (r <= r_i[1]) {
      50
    } else if (r <= r_i[2]) {
      25
    } else {
      m(r) * v(atan2(y, x))
    }
  }
)

# Experiment
d_darts <- throw_darts(2500) |>
  mutate(r = sqrt(x^2 + y^2), z = Z(x, y))

# Theoretische Verteilung

# Wertemenge
b <- 1:20
w <- sort(unique(c(b, 2 * b, 3 * b, c(25, 50))))

a_i <- pi * (r_i^2 - c(0, r_i[-6])^2)
a_t <- pi * r^2

pe <- a_i[1] / a_t
pb <- a_i[2] / a_t
ps <- (a_i[3] + a_i[5]) / (20 * a_t)
pd <- a_i[6] / (20 * a_t)
pt <- a_i[4] / (20 * a_t)

f_darts <- Vectorize(
  function(z) {
    if (z == 50) {
      pe
    } else if (z == 25) {
      pb
    } else {
      (z <= 20) * ps + (z <= 40 & z %% 2 == 0) * pd + (z %% 3 == 0) * pt
    }
  }
)

d_darts_theorie <- tibble(z = w, p = f_darts(w))


# -------------------------------------------------------------------------------------------------
# Pegel Hofkirchen
# -------------------------------------------------------------------------------------------------

HQ10 <- 2700
d_hofkirchen <- read.csv("01-daten/hofkirchen-6342800.day", skip = 40, sep = ";", dec = ".") |>
  select(Date = YYYY.MM.DD, Q = Original) |>
  mutate(
    Date = ymd(Date),
    Year = year(Date)
  ) |>
  group_by(Year) |>
  summarise(Qmax = max(Q)) |>
  mutate(X = if_else(Qmax >= HQ10, 1, 0))
