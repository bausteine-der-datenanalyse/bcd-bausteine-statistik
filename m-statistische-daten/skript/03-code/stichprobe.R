library(tidyverse)

n <- 13

d <- tibble(
  x = rep(1:n, times = n),
  y = rep(1:n, times = rep(n, n)),
  c = sample(x = c("1", "2", "3", "4"), size = n*n, replace = TRUE)
)

ggplot(d) +
  geom_point(mapping = aes(x = x, y = y, color = c), size = 5, show.legend = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_void()

