# Setup ------------------------------------------------------------------------
# Task can be found in the script on page 71
WD = getwd()
setwd(WD)

data <-
  read.csv2(
    "Daten/ENB2012_data.csv",
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = TRUE
  )

data <- subset(data[0:10])

names(data) <- c(
  "RelativeCompactness",
  "SurfaceArea",
  "WallArea",
  "RoofArea",
  "OverallHeight",
  "Orientation",
  "GlazingArea",
  "GlazingAreaDistribution",
  "HeatingLoad",
  "CoolingLoad"
)

data[1:5, ]

summary(data)

# Task 1 -----------------------------------------------------------------------

data$Orientation <- as.factor(data$Orientation)
data$OverallHeight <- as.factor(data$OverallHeight)
data$GlazingArea <- as.factor(data$GlazingArea)
data$GlazingAreaDistribution <-
  as.factor(data$GlazingAreaDistribution)

summary(data)

# Task 2 -----------------------------------------------------------------------

par(mfrow = c(3, 3))

for (name in colnames(data)) {
  plot(
    data[, name],
    data$CoolingLoad,
    main = paste("Plot", name),
    xlab = name,
    ylab = "CoolingLoad"
  )
}

# Task 3 -----------------------------------------------------------------------

print(min(data$CoolingLoad))
print(max(data$CoolingLoad))


model <-
  lm(
    CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
      Orientation + GlazingArea + GlazingAreaDistribution ,
    data = data
  )
model

y <- data$CoolingLoad
prognosis <- model$fitted.values
Error <- mean(abs(y - prognosis))
Error
# ------------------------------------------------------------------------------
library(tree)

Baum <-
  tree(
    CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
      Orientation + GlazingArea + GlazingAreaDistribution,
    data = data
  )
plot(Baum)
text(Baum)

tuning <- cv.tree(Baum, K = 5)

tuning

plot(tuning)

t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]

model <- prune.tree(Baum, best = Anzahl.Endknoten)
plot(model)
text(model)

# ------------------------------------------------------------------------------
n <- length(data[, 1])
index <- sample(1:n, n, replace = FALSE)
data <- data[index, ]

data.train <- data[1:537, ]
data.test <- data[538:768, ]

# Berechnung des Modells auf den Trainingsdaten:
# Achtung: im Befehl 'cv.tree' steht bei 'data' nun Daten.train !!!

Baum <-
  tree(
    CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
      Orientation + GlazingArea + GlazingAreaDistribution,
    data = data.train
  )
tuning <- cv.tree(Baum, K = 5)
t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]

model <- prune.tree(Baum, best = Anzahl.Endknoten)
plot(model)
text(model)


# Berechnung der Prognoseergebnisse auf den Testdaten:

X.test <-
  data.test[, c(
    "RelativeCompactness",
    "WallArea",
    "RoofArea",
    "OverallHeight",
    "Orientation",
    "GlazingArea",
    "GlazingAreaDistribution"
  )]
prognosen <- predict(model, X.test)

# Berechnung des mittleren Prognosefehlers (MAD)

y.test <- data.test[, "CoolingLoad"]
mean(abs(y.test - prognosen))


# Task 4 -----------------------------------------------------------------------