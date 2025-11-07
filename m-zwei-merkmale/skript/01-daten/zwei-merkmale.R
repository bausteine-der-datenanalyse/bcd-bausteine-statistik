# -------------------------------------------------------------------------------------------------
# Daten laden
# -------------------------------------------------------------------------------------------------

load("01-daten/pisa.Rdata")
load("01-daten/data-wb.Rdata")

# -------------------------------------------------------------------------------------------------
# Hilfsfunktionen
# -------------------------------------------------------------------------------------------------

# Bestimmtheitsmaß R^2
rs <- function(x, y) {
  xm <- mean(x)
  ym <- mean(y)
  beta <- sum((x-xm)*(y-ym))/sum((x-xm)^2)
  alpha <- ym - beta * xm
  yh <- alpha + beta * x
  sum((yh-ym)^2)/sum((y-ym)^2)
}

  
# -------------------------------------------------------------------------------------------------
# Beispieldaten Sachverständigenrat
# -------------------------------------------------------------------------------------------------

d1 <- tibble(
  Jahr = 1975:1997,
  Prognose = c(2.0, 4.5, 4.5, 3.5, 3.75, 2.75, 0.5, 0.5, 1.0, 2.5, 3.0,  3.0, 2.0, 1.5, 2.5, 3.0, 3.5, 2.5,  0.0, 0.0, 3.0, 2.0, 2.5),
  Wachstum = c(-3.6, 5.6, 2.4, 3.4, 4.4, 1.8, -0.3, -1.2, 1.2, 2.6, 2.5, 2.5, 1.7, 3.4, 4.0, 4.6, 3.4, 1.5, -1.9, 2.3, 1.9, 1.4, 2.2)
)

d2 <- d1 %>% gather(-Jahr, key = T, value = V)


# -------------------------------------------------------------------------------------------------
# Beispieldaten Weltbank
# -------------------------------------------------------------------------------------------------

# Alte Gruppierung entfernen (Fix)
d_wb_latest <- d_wb_latest %>% dplyr::ungroup()

# Interessante Länder heraussuchen
arrange(d_wb_latest, le/he)
arrange(d_wb_latest, -le/he)
cl <- c("United States", "Sierra Leone", "Qatar", "Sri Lanka")

# Highlight und Treibhausgasemissionen pro Kopf
d_wb_latest %>%
  mutate(hl = country %in% cl, ggepc = gge/pop) %>%
  arrange(hl) -> d3

# -------------------------------------------------------------------------------------------------
# Beispieldaten Korrelation
# -------------------------------------------------------------------------------------------------

d_wb <- d_wb_latest

cor(log10(d_wb$gdp), d_wb$le)
cor(d_wb$he, d_wb$le)
cor(d_wb$hepc, d_wb$le)
cor(d_wb$gini, d_wb$le)

quadrant <- Vectorize(function(x, y) {
  if(x > 0) {
    if(y > 0) {1} else {4}
  } else {
    if(y > 0) {2} else {3}
  }
})

kb <- function(x, y, n) {
  mx = mean(x)
  my = mean(y)
  
  a = min(min(x), min(y))
  b = max(max(x), max(y))

d4 = tibble(X = x, Y  = y, Q = factor(quadrant(x-mx, y-my))) #xxx

ggplot(data = d4) +
  geom_vline(xintercept = mx, color = "gray90") +
  geom_hline(yintercept = my, color = "gray90") +
  geom_point(mapping = aes(x = X, y = Y, fill = Q), show.legend = F, color = "black", shape = 21, size = 2.5) +
  coord_cartesian(xlim = c(a, b), ylim = c(a, b)) +
  theme_void() + theme(panel.border = element_rect(colour = "black", size = 1), aspect.ratio = 1)
}

set.seed(123)
x = runif(200)

# -------------------------------------------------------------------------------------------------
# Beispieldaten Pisa
# -------------------------------------------------------------------------------------------------

# Ausgewählte Länder
cnt = c("Singapore", "Finland", "Brazil", "Switzerland")
d5 <- mutate(d_pisa, sel = country %in% cnt)

xpisa <- d_pisa$math
ypisa <- d_pisa$read

rs(xpisa, ypisa)

# -------------------------------------------------------------------------------------------------
# Beispieldaten Ausgleichsgerade
# -------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------
# Beispieldaten Anscombes Quartett
# -------------------------------------------------------------------------------------------------

d_ans <- read.csv("01-daten/anscombe.csv", sep = "\t", dec = ",")

xa1 <- d_ans$x1
xa2 <- d_ans$x2
xa3 <- d_ans$x3
xa4 <- d_ans$x4
ya1 <- d_ans$y1
ya2 <- d_ans$y2
ya3 <- d_ans$y3
ya4 <- d_ans$y4 

pa <- function(x, y, n) {
  
  xm <- mean(x)
  ym <- mean(y)
  beta <- sum((x-xm)*(y-ym))/sum((x-xm)^2)
  alpha <- ym - beta * xm
  yh <- alpha + beta * x
  
#d6 <- tibble(X = x, Y = y) #xxx

#ggplot(data = d_ans, mapping = aes(x = X, y = Y)) +
  geom_abline(intercept = alpha, slope = beta, color = "red", size = 1) +
  geom_point(shape = 21, fill = "light blue") +
  labs(x = NULL, y = NULL) +
  lims(x = c(4, 20), y = c(4,14)) +
  theme(aspect.ratio = 1)

  
ggplot(data = d_ans, mapping = aes(x = X, y = Y-yh)) +
  geom_hline(color = "red", size = 1, yintercept = 0) +
  geom_point(shape = 21, fill = "light blue") +
  labs(x = NULL, y = NULL) +
  lims(x = c(4, 20), y = c(-5,5)) +
  theme(aspect.ratio = 1)
}

rs(xa1, ya1)
rs(xa2, ya2)
rs(xa3, ya3)
rs(xa4, ya4)

cor(xa1, ya1)
cor(xa2, ya2)
cor(xa3, ya3)
cor(xa4, ya4)

# -------------------------------------------------------------------------------------------------
# Beispieldaten R^2
# -------------------------------------------------------------------------------------------------

xr1 <- runif(100)
yr1 <- 0.3 * xr1 + 0.01 * rnorm(100)
d7 <- tibble(X=xr1, Y=yr1)

xr2 <- runif(100)
yr2 <- 0.3 * xr2 + 0.05 * rnorm(100)
d8 <- tibble(X=xr2, Y=yr2)


# -------------------------------------------------------------------------------------------------
# Beispieldaten LOESS
# -------------------------------------------------------------------------------------------------

nl <- 300
xl <- 10*runif(nl)
yl <- sin(2*pi*xl/10) + 0.2 * rnorm(nl)
d9 <- tibble(X=xl, Y=yl)

# -------------------------------------------------------------------------------------------------
# Beispieldaten Beispiele
# -------------------------------------------------------------------------------------------------

#Plotit
plotit <- function(d, xdat, ydat, xlab, ylab, n, log = FALSE) {
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
       geom_abline(intercept = alpha, slope = beta, color = "red", size = 1) +
       labs(x = xlab, y = ylab, title = TeX(paste("$r =", format(r, digits = 4), "\\;\\;  R^2 =", format(R2, digits = 4)))) +
       theme(aspect.ratio = 1)
  
  if(log) {
    p <- p + scale_x_log10()
  }
  
return(p)

}
