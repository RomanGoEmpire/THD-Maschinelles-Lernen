# Setup ------------------------------------------------------------------------
# Task can be found in the script on page 45
WD = getwd()
setwd(WD)

data <-
  read.csv2(
    "Daten/autos.csv",
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = TRUE
  )

data[, "Herkunft"] <- as.factor(data[, "Herkunft"])

summary(data)

# Aufgabe 2 --------------------------------------------------------------------
verbrauch <- data[, "Verbrauch"]
zylinder <- data[, "Zylinder"]
hubraum <- data[, "Hubraum"]

mean(verbrauch)
median(verbrauch)
sd(verbrauch)

mean(zylinder)
median(zylinder)
sd(zylinder)

mean(hubraum)
median(hubraum)
sd(hubraum)

# 1 (=Amerika), 2 (=Europa) und 3 (=Asien)
table(data$Herkunft)

# Aufgabe 3 --------------------------------------------------------------------
x <- data[, "Leistung"]
y <- data[, "Verbrauch"]
plot(
  x,
  y,
  type = "p",
  main = "Beziehung zwischen Leistung und Verbrauch",
  xlab = "Leistung",
  ylab = "Verbrauch",
  xlim = c(46, 230),
  ylim = c(5, 26.1),
  pch = 16,
  col = "#3f88c5",
  cex = 0.7,
  
)

# Aufgabe 4 --------------------------------------------------------------------
verbrauch <- data[, "Verbrauch"]
mean_by_continent <-
  aggregate(verbrauch ~ data[, "Herkunft"], data = data, mean)
names(mean_by_continent) <- c("Herkunft", "Verbrauch")
mean_by_continent # 1 (=Amerika), 2 (=Europa) und 3 (=Asien)

# Aufgabe 5 --------------------------------------------------------------------
usa <- subset(data, Herkunft == "1")
europe <- subset(data, Herkunft == "2")
asia <- subset(data, Herkunft == "3")

x_usa_leistung <- usa[, "Leistung"]
y_usa_verbrauch <- usa[, "Verbrauch"]

plot(
  x_usa_leistung,
  y_usa_verbrauch,
  main = "Leistung und Verbauch",
  type = "p",
  xlab = "Leistung",
  ylab = "Verbauch",
  xlim = c(46, 230),
  ylim = c(5, 26.1),
  pch = 16,
  cex = 0.7,
  col = "#3f88c5",
  bty = "n",
  
)

x_europe_leistung <- europe[, "Leistung"]
y_europe_verbrauch <- europe[, "Verbrauch"]
points(
  x_europe_leistung,
  y_europe_verbrauch,
  pch = 16,
  cex = 0.7,
  col = "#ffba08"
)

x_asia_leistung <- asia[, "Leistung"]
y_asia_verbrauch <- asia[, "Verbrauch"]
points(
  x_asia_leistung,
  y_asia_verbrauch,
  pch = 16,
  cex = 0.7,
  col = "#d00000"
)
# Aufgabe 6 --------------------------------------------------------------------

usa <- subset(data, Herkunft == "1")
europe <- subset(data, Herkunft == "2")
asia <- subset(data, Herkunft == "3")
usa_verbrauch <- usa[, "Verbrauch"]
europe_verbrauch <- europe[, "Verbrauch"]
asia_verbrauch <- asia[, "Verbrauch"]
boxplot(
  usa_verbrauch,
  europe_verbrauch,
  asia_verbrauch,
  ylab = "Verbrauch",
  main = "Vergleich Verbrauch anhand Herkunft",
  names = c("Usa", "Europe", "Asia"),
  col = c("#3f88c5", "#ffba08", "#d00000")
)
# Aufgabe 7 --------------------------------------------------------------------

verbrauch <- data[, "Verbrauch"]
leistung <- data[, "Leistung"]
gewicht <- data[, "Gewicht"]

cor(leistung, verbrauch)
cor(verbrauch, gewicht)
