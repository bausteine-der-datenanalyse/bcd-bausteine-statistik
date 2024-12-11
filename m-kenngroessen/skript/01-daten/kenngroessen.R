# Daten für Tabellen

tbl1 <- tribble (
  ~"Beobachtung", ~"1",~"2",~"3",~"4",~"5",~"6",~"7",~"8",~"9",~"10",~"11",~"12",~"13",~"14",~"15",~"16",~"17",
  "X",1, 2, 6, 2, 5, 2, 4, 3, 1, 3, 2, 1, 2, 3, 4, 1, 2,
  "Y", 1, 2, 6, 2, 5, 2, 4, 3, 1, 3, 2, 1, 21, 3, 4, 1, 2
)

tbl2 <- tribble(
  ~"Jahr",~"Jan",~"Feb",~"Mär",~"Apr",~"Mai",~"Jun",~"Jul",~"Aug",~"Sep",~"Okt",~"Nov",~"Dez",
  "2008",19.35,-23.55,3.85,-5.15,-26.65,-9.75,42.25,28.95,5.25,6.45,-12.45,-28.55,
  "2010",-24.45,-.05,-5.05,-61.65,-21.05,-50.45,5.65,114.35,-0.75,-21.85,53.55,11.75
)

tbl3 <- tribble (
  ~"Beobachtung", ~"1",~"2",~"3",~"4",~"5",~"6",~"7",~"8",~"9",~"10",~"11",~"12",
  "Urliste", "A", "A", "C", "A", "B", "B", "D", "B", "C", "A", "B", "D",
  "geordnete Urliste", "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "D", "D"
)

# Daten Arithmetisches Mittel

d_arith_a <- tibble(
  X = c(1, 2, 6, 2, 5, 2, 4, 3, 1, 3, 2, 1, 2, 3, 4, 1, 2),
)

mean_arith_a <- mean(d_arith_a$X)

d_arith_b <- tibble(
  X = c(1, 2, 6, 2, 5, 2, 4, 3, 1, 3, 2, 1, 21, 3, 4, 1, 2),
)

mean_arith_b <- mean(d_arith_b$X)

ar_arith <- 6

# Funktion für 3 Plots Lageregeln
 plot_lageregeln <- function(h, n) {
   v <- 1:9
   x <- c()
   for (i in v) {
     x <- c(x, rep(c(v[i]), h[i]))
   }
   d <- tibble(X = x)
   
   ggplot(data = d) +
     geom_bar(mapping = aes(x = X)) +
     scale_x_continuous(breaks = 1:9, minor_breaks = NULL) +
     scale_y_continuous(breaks = c(5, 10), minor_breaks = NULL) +
     labs(x = NULL, y = NULL) +
     ggtitle(paste("Stichprobe", n)) +
     theme(plot.title = element_text(size=20))
 }
 
# Daten für Plots geometrisches Mittel
b <- c(1, 2, 1.5, 4.5, 2.25, 4.5, 5)
n <- length(b) - 1
xx <- 1:n
 
for (i in 1:n) {
   xx[i] = b[i+1]/b[i] 
 }
 
xb = prod(xx)^(1/6)
 
d1_geo <- tibble(
   x = 0:n,
   B = b
 )
 
d2_geo <- tibble(
   x = 1:n - 0.5,
   w = xx,
   g = xx > 1
 )
 
d3_geo <- tibble(
   x = 0:n,
   B = seq(1, 5, length = 7)
 )
 
d4_geo <- tibble(
   x = 1:n - 0.5,
   w = rep(xb, n),
   g = rep(TRUE, n)
 )


# Daten für Maße der Variabilität/Niederschläge
 
d_nie <- read.csv(
   "01-daten/produkt_nieder_monat_18910101_20171231_00555.txt",
   sep = ";", dec = "."
 ) %>%
   select(MESS_DATUM_BEGINN, MESS_DATUM_ENDE, MO_RR) %>%
   mutate(
     MESS_DATUM_BEGINN = ymd(MESS_DATUM_BEGINN),
     MESS_DATUM_ENDE = ymd(MESS_DATUM_ENDE),
     JAHR = year(MESS_DATUM_BEGINN),
     MONAT = month(MESS_DATUM_BEGINN, label = TRUE),
     MO_RR = replace(MO_RR, MO_RR == -999, NA)
   )
 
 
d2_nie <- d_nie %>%
   group_by(JAHR) %>% 
   summarise(
     AM = mean(MO_RR),
     SD = sd(MO_RR),
     MN = min(MO_RR),
     MX = max(MO_RR)
    )
 
y1_nie <- 2008
d01_nie <- filter(d_nie, JAHR == y1_nie)
dd1_nie <- filter(d2_nie, JAHR == y1_nie)
m1_nie <- dd1_nie$AM
v1_nie <- d01_nie$MO_RR - m1_nie
y2_nie <- 2010
d02_nie <- filter(d_nie, JAHR == y2_nie)
dd2_nie <- filter(d2_nie, JAHR == y2_nie)
m2_nie <- dd2_nie$AM
v2_nie <- d02_nie$MO_RR - m2_nie
 
d4_nie <- filter(d_nie, JAHR == y1_nie | JAHR == y2_nie) %>% mutate(JAHR = as.factor(JAHR))
d5_nie <- filter(d2_nie, JAHR == y1_nie | JAHR == y2_nie) %>% mutate(JAHR = as.factor(JAHR))

# Daten erstellen Boxplot
 n_box <- 20
 set.seed(18)
 d_box <- tibble(X = sample(100, n_box))
 
 xmed_box <- median(d_box$X)
 
 pp_box_1 <- c(0.05, 0.33, 0.75)
 qs_box_1 <- quantile(d_box$X, probs = pp_box_1, type = 2)
 
 pp_box_2 <- c(0, 0.25, 0.5, 0.75, 1)
 qs_box_2 <- quantile(d_box$X, probs = pp_box_2, type = 2)
 
 xx_box <- ((c(qs_box_2, 0) + c(0, qs_box_2))/2)[2:5]
 
 l_box <- tibble(X = xx_box, labels = rep("25%", 4))
 
 pp <- 0.1
 
# Plots Quantile Boxplots
 
## plot erstellen
 p_box_1 <- ggplot(data = d_box) +
   geom_hline(yintercept = 0) +
   geom_point(mapping = aes(x = X, y = 0), size = 3, color = 'white') +
   geom_point(mapping = aes(x = X, y = 0), size = 3, shape = 21, fill = 'orange', alpha = 3/4) +
   scale_x_continuous(breaks = NULL, minor_breaks = NULL) +
   scale_y_continuous(breaks = NULL, minor_breaks = NULL, limits = c(-0.125, 0.25)) +
   theme(panel.border = element_blank()) +
   labs(x = "", y = "")
 
## Annotationsfunktion erstellen
 ap <- function(v, l, py = 0.15) {
   annotate("text", x = v, y = py, label = l, hjust = -0.2, parse = TRUE)
 }
 
## Boxplots 2
p_box_2 <- p_box_1 + 
   geom_vline(xintercept = qs_box_1) +
   ap(qs_box_1[1], "x[0.05]") +
   ap(qs_box_1[2], "x[0.33]") +
   ap(qs_box_1[3], "x[0.75]")
 
# Daten Niederschlag Boxplots
 
d_nie_jahr <- read.csv(
   "01-daten/produkt_nieder_tag_19310101_20171231_00555.txt",
   sep = ";", dec = "."
) %>%
   select(MESS_DATUM, RS) %>%
   mutate(
     MESS_DATUM = ymd(MESS_DATUM),
     JAHR = year(MESS_DATUM),
     MONAT = month(MESS_DATUM, label = TRUE),
     RS = replace(RS, RS == -999, NA)
)

# Niederschläge Vietnam
# https://www.gso.gov.vn/default_en.aspx?tabid=773
d_ns_vietnam_m <- read_excel("01-daten/E01.08.xlsx", skip = 2, na = "..") %>%
  rename(Year = ...1, City = ...2) %>%
  fill(Year) %>%
  gather(-City, -Year, key = "Month", value = "NS") %>%
  mutate(Date = dmy(paste(1, Month, Year)), Month = month(Date, label = T), NS = as.numeric(NS)) 

# Niederschläge Bochum
d_ns_bochum_m <- read.csv(
  "01-daten/produkt_nieder_monat_18910101_20171231_00555.txt",
  sep = ";", dec = "."
) %>%
  select(MESS_DATUM_BEGINN, MESS_DATUM_ENDE, MO_RR) %>%
  mutate(
    MESS_DATUM_BEGINN = ymd(MESS_DATUM_BEGINN),
    MESS_DATUM_ENDE = ymd(MESS_DATUM_ENDE),
    Year = year(MESS_DATUM_BEGINN),
    Month = month(MESS_DATUM_BEGINN, label = TRUE),
    NS = replace(MO_RR, MO_RR == -999, NA)
  )


# Gefilterte Daten
d1_vie <- filter(d_ns_vietnam_m, City == "Nha Trang", Year == 2015) %>% select(City, Month, NS)
d2_bo <- filter(d_ns_bochum_m, Year == 2008) %>% mutate(City = "Bochum") %>% select(City, Month, NS)
d_bind <- rbind(d1_vie, d2_bo)

# Lorenzkurven

lorenzkurve <- function(d, l) {
  d %>% 
    arrange(NS) %>% 
    mutate(u = row_number() / n(), v = cumsum(NS) / sum(NS)) %>% 
    select(u, v) %>%
    rbind(data.frame(u = 0, v = 0)) %>%
    arrange(u) -> l1
  
  n <- length(d$NS)
  
  ggplot(data = l1, mapping = aes(x = u, y = v)) +
    geom_ribbon(mapping = aes(ymin = u, ymax = v), fill = "steel blue", alpha = 0.1) +
    geom_line(size = 0.5) + geom_point(size = 2.5) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, size = 0.5) +
    labs(title = l, x = NULL, y = NULL) + 
    scale_x_continuous(breaks = seq(0, 1, by = 1/4), minor_breaks = seq(0, 1, by = 1/n)) +
    scale_y_continuous(breaks = seq(0, 1, by = 1/4), minor_breaks = seq(0, 1, by = 1/n)) +
    coord_equal()
}

lorenzkurve(d2_bo, "Bochum")
lorenzkurve(d1_vie, "Nha Trang")