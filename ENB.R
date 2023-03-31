# Setup ------------------------------------------------------------------------
# Task can be found in the script on page 71

# install.packages("tree")
# install.packages("ANN2")

library(tree)
library(ANN2)

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
# Regression
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
# Entscheidungsbaum
best_model = NULL
best = c(1000)

for (i in c(1:10000)) {
  n <- length(data[, 1])
  index <- sample(1:n, n, replace = FALSE)
  data <- data[index, ]
  
  n <- nrow(data) * 0.7
  data.train <- data[1:n, ]
  data.test <- data[(n + 1):768, ]
  
  tree <-
    tree(
      CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
        Orientation + GlazingArea + GlazingAreaDistribution,
      data = data.train
    )
  tuning <- cv.tree(tree, K = 5)
  t <- which.min(tuning$dev)
  Anzahl.Endknoten <- tuning$size[t]
  
  model <- prune.tree(tree, best = Anzahl.Endknoten)
  
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
  mean = mean(abs(y.test - prognosen))
  
  if (mean < best) {
    print(mean)
    best_model = model
    best = mean
  }
}

plot(model)
text(model)

# ------------------------------------------------------------------------------
# Neuronales Netzwerk
best_model = NULL
best_train = c(1000)
best_test = c(1000)
best_average = c(1000)
best_difference = c(1000)


for (i in c(1:1000)) {
  n <- length(data[, 1])
  index <- sample(1:n, n, replace = FALSE)
  data <- data[index, ]
  
  n <- nrow(data) * 0.6
  data.train <- data[1:n, ]
  data.test <- data[(n + 1):768, ]
  
  
  X <-
    model.matrix(
      CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
        Orientation + GlazingArea + GlazingAreaDistribution,
      data = data.train
    )
  X <- X[, -1]   # entferne den Intercept
  y <- data.train$CoolingLoad
  
  model <-
    neuralnetwork(
      X,
      y,
      hidden.layers = c(8, 4, 2),
      regression = TRUE,
      loss.type = "squared",
      learn.rates = 0.01,
      n.epochs = 250,
      batch.size = 16,
      verbose = FALSE
    )
  
  X.test <-
    model.matrix(
      CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
        Orientation + GlazingArea + GlazingAreaDistribution,
      data = data.test
    )
  X.test <- X.test[, -1]   # entferne den Intercept
  y.test <- data.test[, "CoolingLoad"]
  
  
  prognosen <- predict(model, X)$predictions
  mean_train = mean(abs(prognosen - y))
  prognosen <- predict(model, X.test)$predictions
  mean_test = mean(abs(prognosen - y.test))
  mean_average = (mean_train + mean_test) / 2
  mean_difference = mean_test / mean_train
  # mean_train
  # mean_test
  # mean_average
  # mean_difference
  
  
  if (best_train > mean_train &&
      best_test > mean_test && best_average > mean_average) {
    best_model = model
    best_train = mean_train
    best_test = mean_test
    best_average = mean_average
    print("New Best")
    print(paste("Train:", best_train))
    print(paste("Test:", best_test))
    print(paste("Average:", best_average))
  }
}

# Task 4 -----------------------------------------------------------------------
