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

verbrauch <- data[, "Verbrauch"]
zylinder <- data[, "Zylinder"]
hubraum <- data[, "Hubraum"]

summary(data)



# Aufgabe 2

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



# Aufgabe 3

x <- data[, "Leistung"]
y <- verbrauch
plot(
  x,
  y,
  xlab = "Leistung",
  ylab = "Verbrauch",
  pch = 16,
  col = "blue",
  cex = 0.7
)
