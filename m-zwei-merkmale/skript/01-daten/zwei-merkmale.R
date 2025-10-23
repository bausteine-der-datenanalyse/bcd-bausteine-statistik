library(tidyverse)
library(latex2exp)


# Ausgleichsgeraden

load("01-daten/pisa.Rdata")

rs <- function(x, y) {
  xm <- mean(x)
  ym <- mean(y)
  beta <- sum((x-xm)*(y-ym))/sum((x-xm)^2)
  alpha <- ym - beta * xm
  yh <- alpha + beta * x
  sum((yh-ym)^2)/sum((y-ym)^2)
}



x <- d_pisa$math
y <- d_pisa$read
xm <- mean(x)
ym <- mean(y)
xm
ym
beta <- sum((x-xm)*(y-ym))/sum((x-xm)^2)
alpha <- ym - beta * xm
beta
alpha

lm(y ~ x)

rs(x, y)

ggplot(data = d_pisa, mapping = aes(x = read, y = math)) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.5) +
  geom_point(shape = 21, fill = "light blue") +
  labs(x = "Lesekompetenz", y = "Mathematische Kompetenz") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/pisa-2.svg", width = 4, height = 3.5)  

# Anscombe examples
d <- read.csv("01-daten/anscombe.csv", sep = "\t", dec = ",")
d

pa <- function(x, y, n) {
  
  xm <- mean(x)
  ym <- mean(y)
  beta <- sum((x-xm)*(y-ym))/sum((x-xm)^2)
  alpha <- ym - beta * xm
  yh <- alpha + beta * x
  
  print(alpha)
  print(beta)
  print(lm(y ~ x))
  
  d <- tibble(X = x, Y = y)
  p <- ggplot(data = d, mapping = aes(x = X, y = Y)) +
    geom_abline(intercept = alpha, slope = beta, color = "red", size = 0.5) +
    geom_point(shape = 21, fill = "light blue") +
    labs(x = NULL, y = NULL) +
    lims(x = c(4, 20), y = c(4,14)) +
    theme(aspect.ratio = 1)
  ggsave(paste0("../skript/00-bilder/anscombe", n, ".svg"), width = 1.9, height = 1.9)  
  
  p <- ggplot(data = d, mapping = aes(x = X, y = Y-yh)) +
    geom_hline(color = "red", size = 0.5, yintercept = 0) +
    geom_point(shape = 21, fill = "light blue") +
    labs(x = NULL, y = NULL) +
    lims(x = c(4, 20), y = c(-5,5)) +
    theme(aspect.ratio = 1)
  ggsave(paste0("../skript/00-bilder/anscombe-r", n, ".svg"), width = 1.9, height = 1.9)  
  p
}

rs(d$x1, d$y1)
rs(d$x2, d$y2)
rs(d$x3, d$y3)
rs(d$x4, d$y4)

cor(d$x1, d$y1)
cor(d$x2, d$y2)
cor(d$x3, d$y3)
cor(d$x4, d$y4)

pa(d$x1, d$y1, "1")
pa(d$x2, d$y2, "2")
pa(d$x3, d$y3, "3")
pa(d$x4, d$y4, "4")

# Beispiel R^2
x <- runif(100)
y <- 0.3 * x + 0.01 * rnorm(100)
d <- tibble(X=x, Y=y)
ggplot(data = d, mapping = aes(x = X, y = Y)) +
  geom_point(shape = 21, fill = "light blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.5) +
  labs(x = NULL, y = NULL) +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/ausgleichsgerade-r2-1.svg", width = 3, height = 3)  
rs(x,y)

x <- runif(100)
y <- 0.3 * x + 0.05 * rnorm(100)
d <- tibble(X=x, Y=y)
ggplot(data = d, mapping = aes(x = X, y = Y)) +
  geom_point(shape = 21, fill = "light blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.5) +
  labs(x = NULL, y = NULL) +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/ausgleichsgerade-r2-2.svg", width = 3, height = 3)  
rs(x,y)

# LOESS
n <- 300
x <- 10*runif(n)
y <- sin(2*pi*x/10) + 0.2 * rnorm(n)
d <- tibble(X=x, Y=y)
ggplot(data = d, mapping = aes(x = X, y = Y)) +
  geom_point(shape = 21, fill = "light blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +
  labs(x = NULL, y = NULL) +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/loess.svg", width = 3, height = 2.5)  


# Korrelationskoeffizient

load("01-daten/data-wb.Rdata")


# Highlight und Treibhausgasemissionen pro Kopf
d_wb_latest -> d

# Korrelationskoeffizienten
cor(log10(d$gdp), d$le)
cor(d$he, d$le)
cor(d$hepc, d$le)
cor(d$gini, d$le)

# Beispiele - was wollen wir erkennen
quadrant <- Vectorize(function(x, y) {
  if(x > 0) {
    if(y > 0) {
      1
    }
    else {
      4
    }
  }
  else {
    if(y > 0) {
      2
    }
    else {
      3
    }
  }
})

kb <- function(x, y, n) {
  mx = mean(x)
  my = mean(y)
  
  a = min(min(x), min(y))
  b = max(max(x), max(y))
  
  d = tibble(X = x, Y  = y, Q = factor(quadrant(x-mx, y-my)));
  
  p <- ggplot(data = d) +
    geom_vline(xintercept = mx, color = "gray90") +
    geom_hline(yintercept = my, color = "gray90") +
    geom_point(mapping = aes(x = X, y = Y, fill = Q), show.legend = F, color = "black", shape = 21, size = 2.5) +
    coord_cartesian(xlim = c(a, b), ylim = c(a, b)) +
    theme_void() + theme(panel.border = element_rect(colour = "black", size = 1), aspect.ratio = 1) 
  ggsave(paste0("../skript/00-bilder/korrelation-beispiel-", n, ".svg"), width = 4, height = 4)
  
  p <- ggplot(data = d) +
    geom_vline(xintercept = mx, color = "gray90") +
    geom_hline(yintercept = my, color = "gray90") +
    geom_point(mapping = aes(x = X, y = Y), size = 2) +
    coord_cartesian(xlim = c(a, b), ylim = c(a, b)) +
    theme_void() + theme(panel.border = element_rect(colour = "black", size = 1), aspect.ratio = 1) 
  ggsave(paste0("../skript/00-bilder/korrelation-beispiel-bw-", n, ".svg"), width = 4, height = 4)
  
  p
}

set.seed(123)
x = runif(200)
kb(x, 0.1 +  0.7 * x + 0.1 * rnorm(200), "1")
kb(x, 0.5-0.7 * (x-0.5) + 0.1 * rnorm(200), "2")
kb(x, 0.5 + 0.1 * rnorm(200), "3")
kb( 0.35*rnorm(200), rnorm(200), "4")
kb(x, 3*(x-0.5)^2 + 0.1 + 0.1 * rnorm(200), "5")
set.seed(5)
kb(x, 1-(3*(x-0.5)^2 + 0.1 + 0.12 * rnorm(200)), "6")

# Beispiel Gini
m <- tibble(gini = mean(d$gini), le = mean(d$le))

# Alte Gruppierung entfernen (Fix)
d <- d %>% dplyr::ungroup()
m <- m %>% dplyr::ungroup()

ggplot(mapping = aes(x = gini, y = le)) +
  geom_point(data = d, shape = 21, fill = "light blue") +
  geom_point(data = m, shape = 21, fill = "red") +
  labs(x = "Gini-Koeffizient", y = "Mittlere Lebenserwartung") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/korrelationskoeffizient-1.svg", width = 4, height = 4, dpi = 600)


# Zahlenwerte
mcor <- function(x, y) {
  mx = mean(x)
  my = mean(y)
  sum((x-mx)*(y-my)) / (sqrt(sum((x-mx)^2))*sqrt(sum((y-my)^2)))
}

Prognose = c(2.0, 4.5, 4.5, 3.5, 3.75, 2.75, 0.5, 0.5, 1.0, 2.5, 3.0,  3.0,  2.0,  1.5,  2.5,  3.0,  3.5,  2.5,  0.0, 0.0, 3, 2, 2.5)
Wachstum = c(-3.6, 5.6, 2.4, 3.4, 4.4, 1.8, -0.3, -1.2, 1.2, 2.6, 2.5, 2.5, 1.7, 3.4, 4.0, 4.6, 3.4, 1.5, -1.9, 2.3, 1.9, 1.4, 2.2)
cor(Prognose, Wachstum)
mcor(Prognose, Wachstum)

cor(d$he, d$le)
cor(d$edu, d$le)
cor(d$gini, d$le)
cor(log10(d$gdp), d$le)


# Pisa

rm(list = ls())

load("01-daten/pisa.Rdata")

# Ausgewählte Länder
cnt = c("Singapore", "Finland", "Brazil", "Switzerland")
d <- mutate(d_pisa, sel = country %in% cnt)
head(d)

cor(d$math, d$read)

head(arrange(d, math/read))

ggplot(data = d, mapping = aes(x = read, y = math)) +
  geom_point(mapping = aes(fill = sel), shape = 21, show.legend = F) +
  geom_label(
    data = filter(d, country %in% cnt), 
    mapping = aes(label = country),
    hjust = 1, nudge_x = -3, nudge_y = 5, size = 2.5, alpha = 0.5
  ) +
  scale_fill_manual(values = c("light blue", "red")) +
  labs(x = "Lesekompetenz", y = "Mathematische Kompetenz") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/pisa-1.svg", width = 4, height = 3.5)  


# Sachverständigenrat

# Aus einem Buch
d1 <- tibble(
  Jahr = 1975:1997,
  Prognose = c(2.0, 4.5, 4.5, 3.5, 3.75, 2.75, 0.5, 0.5, 1.0, 2.5, 3.0,  3.0, 2.0, 1.5, 2.5, 3.0, 3.5, 2.5,  0.0, 0.0, 3.0, 2.0, 2.5),
  Wachstum = c(-3.6, 5.6, 2.4, 3.4, 4.4, 1.8, -0.3, -1.2, 1.2, 2.6, 2.5, 2.5, 1.7, 3.4, 4.0, 4.6, 3.4, 1.5, -1.9, 2.3, 1.9, 1.4, 2.2)
)

d2 <- d1 %>% gather(-Jahr, key = T, value = V)

ggplot(data = d2) + 
  geom_col(mapping = aes(x = Jahr, y = V, fill = T), position = "dodge", color = "black") + 
  scale_x_continuous(minor_breaks = (1974:1998) - 0.5) +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = NULL, y = "Werte in Prozent", legend = NULL, fill = NULL)
ggsave("../skript/00-bilder/sachverstaendigenrat-1.svg", width = 6, height = 2.4)


ggplot(data = d1, mapping = aes(x = Prognose, y = Wachstum)) + 
  geom_abline(intercept = 0, slope = 1, color = "gray") +
  geom_point(shape = 21, size = 2, fill = "orange") +
  geom_rug() +
  coord_fixed(xlim = c(-2, 6), ylim = c(-2, 6)) +
  theme_bw()
ggsave("../skript/00-bilder/sachverstaendigenrat-2.svg", width = 3, height = 3)

cor(d1$Prognose, d1$Wachstum)


# Weltbank

load("01-daten/data-wb.Rdata")

# Alte Gruppierung entfernen (Fix)
d_wb_latest <- d_wb_latest %>% dplyr::ungroup()

# Interessante Länder heraussuchen
arrange(d_wb_latest, le/he)
arrange(d_wb_latest, -le/he)
cl <- c("United States", "Sierra Leone", "Qatar", "Sri Lanka")

# Highlight und Treibhausgasemissionen pro Kopf
d_wb_latest %>%
  mutate(hl = country %in% cl, ggepc = gge/pop) %>%
  arrange(hl) -> d

# Ausgaben Gesundheitswesen in % BIP
ggplot(data = d, mapping = aes(x = he, y = le)) +
  geom_point(mapping = aes(fill = hl), shape = 21, show.legend = F) +
  geom_label(
    data = filter(d_wb_latest, country %in% cl), 
    mapping = aes(label = country),
    hjust = 0.8, nudge_y = 2, size = 2.5, alpha = 0.5
  ) +
  scale_x_continuous(limits = c(0, 17)) +
  scale_fill_manual(values = c("light blue", "red")) +
  labs(x = "Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/weltbank-1.svg", width = 3, height = 3)

# Bildung
ggplot(data = d, mapping = aes(x = edu, y = le)) +
  geom_point(mapping = aes(fill = hl), shape = 21, show.legend = F) +
  geom_label(
    data = filter(d, country %in% cl), 
    mapping = aes(label = country),
    hjust = 0.1, nudge_y = 2, size = 2.5, alpha = 0.5
  ) +
  scale_fill_manual(values = c("light blue", "red")) +
  labs(x = "Bildung (% BIP)", y = "Mittlere Lebenserwartung") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/weltbank-2.svg", width = 3, height = 3)

# Gini
ggplot(data = d, mapping = aes(x = gini, y = le)) +
  geom_point(mapping = aes(fill = hl), shape = 21, show.legend = F) +
  geom_label(
    data = filter(d_wb_latest, country %in% cl), 
    mapping = aes(label = country),
    hjust = 0, nudge_x = 1, size = 2.5, alpha = 0.5
  ) +
  scale_fill_manual(values = c("light blue", "red")) +
  labs(x = "Gini-Koeffizient", y = "Mittlere Lebenserwartung") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/weltbank-3.svg", width = 3, height = 3)

# BIP pro Kopf
ggplot(data = d, mapping = aes(x = gdppc, y = le)) +
  geom_point(mapping = aes(fill = hl), shape = 21, show.legend = F) +
  geom_label(
    data = filter(d_wb_latest, country %in% cl), 
    mapping = aes(label = country),
    hjust = 0.3, nudge_y = 2, size = 2.5, alpha = 0.5
  ) +
  scale_fill_manual(values = c("light blue", "red")) +
  scale_x_log10(limits = c(NA, 100000), labels = function(x) {paste0(x/1000, "K")}) +
  labs(x = "BIP pro Kopf (USD)", y = "Mittlere Lebenserwartung") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/weltbank-4.svg", width = 3, height = 3)

# Ausgaben Gesundheitswesen in % BIP mit BIP pro Kopf
ggplot(data = arrange(d, gdppc), mapping = aes(x = he, y = le)) +
  geom_point(mapping = aes(fill = gdppc, size = gdppc), shape = 21, show.legend = F, alpha = 0.8) +
  scale_x_continuous(limits = c(0, 17)) +
  scale_y_continuous(limits = c(45, 85)) +
  scale_size(range = c(0.1, 10)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(x = "Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung") +
  theme(aspect.ratio = 1)
ggsave("../skript/00-bilder/weltbank-5.svg", width = 3, height = 3)


# Histogramme etc.

ggplot(data = d) +
  stat_bin2d(mapping = aes(x = he, y = le), binwidth = c(2.5,5)/2, color = "black", size = 0.25) +
  labs(x = "Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung", fill = "Anzahl") +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend", breaks = seq(1, 9, by = 4)) +
  scale_x_continuous(breaks = seq(5, 15, by = 5), minor_breaks = seq(0, 20, by = 1.25)) +
  scale_y_continuous(breaks = seq(50, 80, by = 10), minor_breaks = seq(45, 85, by = 2.5)) +
  coord_cartesian(xlim = c(1.5, 17), ylim = c(45, 85)) +
  theme(aspect.ratio = 1, legend.position = "top")
ggsave("../skript/00-bilder/weltbank-2d-1.svg", width = 3, height = 3.5)


ggplot(data = d) +
  geom_hex(mapping = aes(x = he, y = le), binwidth = c(2.5,5) / 2, color = "black", size = 0.25) +
  labs(x = "Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung", fill = "Anzahl") +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend", breaks = seq(2, 10, by = 2)) +
  scale_x_continuous(breaks = seq(5, 15, by = 5), minor_breaks = seq(0, 20, by = 1.25)) +
  scale_y_continuous(breaks = seq(50, 80, by = 10), minor_breaks = seq(45, 85, by = 2.5)) +
  coord_cartesian(xlim = c(1.5, 17), ylim = c(45, 85)) +
  theme(aspect.ratio = 1, legend.position = "top")
ggsave("../skript/00-bilder/weltbank-2d-2.svg", width = 3, height = 3.5)


ggplot(data = d) +
  stat_density_2d(mapping = aes(x = he, y = le, fill = ..level..), color = "black", size = 0.25, geom = "polygon", show.legend = FALSE, alpha = 0.8, bins = 10) +
  labs(x = "Gesundheitswesen (% BIP)", y = "Mittlere Lebenserwartung", fill = "Anzahl") +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend") +
  scale_x_continuous(breaks = seq(0, 35, by = 5), minor_breaks = seq(0, 20, by = 1.25), limits = c(5-3*1.25, 15+2*1.25)) +
  scale_y_continuous(breaks = seq(50, 90, by = 10), minor_breaks = seq(45, 90, by = 2.5), limits = c(45, 90)) +
  coord_cartesian(xlim = c(1.5, 17), ylim = c(45, 85)) +
  theme(aspect.ratio = 1, legend.position = "top")
ggsave("../skript/00-bilder/weltbank-2d-3.svg", width = 3, height = 3)


# Beispiele

load("01-daten/data-wb.Rdata")

plotit <- function(d, xdat, ydat, xlab, ylab, n, log = F) {
  x <- d[[xdat]]
  y <- d[[ydat]]
  if(log) {
    x <- log10(x)
  }
  
  xm <- mean(x)
  ym <- mean(y)
  
  beta <- sum((x-xm)*(y-ym))/sum((x-xm)^2)
  alpha <- ym - beta * xm
  yh <- alpha + beta * x
  
  r <- sum((x - xm) * (y - ym)) / (sqrt(sum((x - xm)^2)*sum((y - ym)^2)))
  R2 <- sum((yh - ym)^2) / sum((y - ym)^2)
  r-cor(x, y)
  R2-cor(x, y)^2
  
  p <- ggplot(data = d, mapping = aes_string(x = xdat, y = ydat)) +
    geom_point(shape = 21, fill = "light blue") +
    geom_abline(intercept = alpha, slope = beta, color = "red", size = 0.5) +
    labs(x = xlab, y = ylab, title = TeX(paste("$r =", format(r, digits = 4), "\\;\\;  R^2 =", format(R2, digits = 4)))) +
    theme(aspect.ratio = 1)
  
  if(log) {
    p <- p + scale_x_log10()
  }
  
  ggsave(paste0("../skript/00-bilder/zwei-merkmale-beispiele-", n, ".svg"), width = 3, height = 3)
  p
}

# Sachverständigenrat
d1 <- tibble(
  Prognose = c(2.0, 4.5, 4.5, 3.5, 3.75, 2.75, 0.5, 0.5, 1.0, 2.5, 3.0,  3.0,  2.0,  1.5,  2.5,  3.0,  3.5,  2.5,  0.0, 0.0, 3, 2, 2.5),
  Wachstum = c(-3.6, 5.6, 2.4, 3.4, 4.4, 1.8, -0.3, -1.2, 1.2, 2.6, 2.5, 2.5, 1.7, 3.4, 4.0, 4.6, 3.4, 1.5, -1.9, 2.3, 1.9, 1.4, 2.2)
)
plotit(d1, "Prognose", "Wachstum", "Prognose", "Wachstum", "0")

# Alte Gruppierung entfernen (Fix)
d_wb_latest <- d_wb_latest %>% dplyr::ungroup()

# Weltbank
plotit(d_wb_latest, "he", "le", "Gesundheitswesen (% BIP)", "Mittlere Lebenswerwartung", "1")
plotit(d_wb_latest, "edu", "le", "Bildungswesen (% BIP)", "Mittlere Lebenswerwartung", "2")
plotit(d_wb_latest, "gini", "le", "Gini-Koeffizient", "Mittlere Lebenswerwartung", "3")
plotit(d_wb_latest, "gdppc", "le", "BIP pro Kopf", "Mittlere Lebenswerwartung", "4", log = T)


