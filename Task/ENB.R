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

shuffle_data <- function(data) {
  n <- length(data[, 1])
  index <- sample(1:n, n, replace = FALSE)
  data <- data[index,]
  return(data)
}

data <- shuffle_data(data)

# Task 2 -----------------------------------------------------------------------


# Boxplots aller Variablen


# Histogramme aller Variablen



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

## ---- Regression ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

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
min <- min(data$CoolingLoad)
max <- max(data$CoolingLoad)
range <- max - min

## ---- Entscheidungsbaum ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

train_test_divider <- function(data, percentage) {
  n <- nrow(data) * percentage
  return (list(train = data[1:n,], test = data[(n + 1):nrow(data),]))
}


data <- shuffle_data(data)

devided_data <- train_test_divider(data, 0.7)
train <- devided_data$train
test <- devided_data$test


tree <-
  tree(
    CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
      Orientation + GlazingArea + GlazingAreaDistribution,
    data = train
  )
tuning <- cv.tree(tree, K = 5)
t <- which.min(tuning$dev)
Anzahl.Endknoten <- tuning$size[t]

model <- prune.tree(tree, best = Anzahl.Endknoten)

# Berechnung der Prognoseergebnisse auf den Testdaten:

X.test <-
  test[, c(
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

y.test <- test[, "CoolingLoad"]
mean = mean(abs(y.test - prognosen))

plot(model)
text(model)

## ---- Neuronales Netzwerk ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

shuffle_data <- function(data) {
  n <- length(data[, 1])
  index <- sample(1:n, n, replace = FALSE)
  data <- data[index,]
  return(data)
}

train_test_divider <- function(data, percentage) {
  n <- round(nrow(data) * percentage)
  return (list(train = data[1:n,], test = data[(n + 1):nrow(data),]))
}

create_model_matrix <- function(target_and_predictors, data) {
  matrix <- model.matrix(target_and_predictors, data = data)
  matrix [, 1] # Remove Intercept
  return(matrix)
}

calculate_mean <- function(model, X, y) {
  prediction <- predict(model, X)$predictions
  mean = mean(abs(prediction - y))
  return (mean)
}

data <- shuffle_data(data)

devided_data <- train_test_divider(data, 0.5)
train <- devided_data$train
test <- devided_data$test

target_and_predictors <-
  CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
  Orientation + GlazingArea + GlazingAreaDistribution

# Training Matrix
X <- create_model_matrix(target_and_predictors, train)
y <- train$CoolingLoad
# Test Matrix
X_test <- create_model_matrix(target_and_predictors, test)
y_test <- test$CoolingLoad

model <-
  neuralnetwork(
    X,
    y,
    hidden.layers = c(8, 4, 2),
    loss.type = "absolute",
    learn.rates = 0.01,
    n.epochs =  250,
    batch.size = 8,
    regression = TRUE,
    verbose = FALSE
  )

mean_train <- calculate_mean(model, X, y)
mean_test <- calculate_mean(model, X_test, y_test)

mean_train
mean_test

# ---- LAB SECTION ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

## Try out one configuration for multiple times to find a good seed

tries = 1000

best_model = NULL
best_train = 10000
best_test = 10000
best_average = 10000
best_difference = 10000


results <- integer(tries)


for (i in c(1:tries)) {
  model <-
    neuralnetwork(
      X,
      y,
      hidden.layers = c(8, 4, 3, 2),
      loss.type = "squared",
      learn.rates = 0.01,
      n.epochs =  500,
      batch.size = 8,
      regression = TRUE,
      verbose = FALSE
    )
  mean_train <- calculate_mean(model, X, y)
  mean_test <- calculate_mean(model, X_test, y_test)
  mean_average = (mean_train + mean_test) / 2
  
  results[i] = mean_average
  
  if (best_train > mean_train &&
      best_test > mean_test && best_average > mean_average) {
    best_model = model
    best_train = mean_train
    best_test = mean_test
    best_average = mean_average
    print(paste("Try:", i))
    print(paste("Train:", best_train))
    print(paste("Test:", best_test))
    print(paste("Average:", best_average))
  }
}

hist(results)

# ------------------------------------------------------------------------------

## Trying out different settings and storing them into "neural_network.csv"

vector_to_string <- function(my_vector) {
  my_string <- paste(my_vector, collapse = "-")
  return(my_string)
}


hidden_layers <- list(c(4, 4),
                      c(5, 2),
                      c(6, 3, 2),
                      c(8, 4, 2),
                      c(7, 4, 3, 2),
                      c(9, 6, 4, 3),
                      c(10, 5, 3, 2))

loss_types <- list("absolute", "squared", "huber", "pseudo-huber")
learning_rates <- list(0.001, 0.003, 0.01, 0.03)
epochs <- list(100, 250, 500)
batch_sizes <- list(2, 4, 8)


# create dataframe
empty_matrix <- matrix(nrow = 0, ncol = 7)
neural_network_results <- data.frame(empty_matrix)


combinations <-
  expand.grid(hidden_layers, loss_types, learning_rates, epochs, batch_sizes)

names(combinations) <-
  c("hidden_layers",
    "loss_types",
    "learning_rates",
    "epochs",
    "batch_sizes")

for (i in (c(1:1008))) {
  print(i)
  current_combination <- combinations[i, ]
  hidden_layer <- unlist(current_combination$hidden_layers)
  loss_type <- unlist(current_combination$loss_types)
  learning_rate <- unlist(current_combination$learning_rates)
  epoch <- unlist(current_combination$epochs)
  batch_size <- unlist(current_combination$batch_sizes)
  
  tries = 1
  
  best_model = NULL
  best_train = 10000
  best_test = 10000
  best_average = 10000
  best_difference = 10000
  
  
  for (i in c(1:tries)) {
    model <-
      neuralnetwork(
        X,
        y,
        hidden.layers = hidden_layer,
        regression = TRUE,
        loss.type = loss_type,
        learn.rates = learning_rate,
        n.epochs = epoch,
        batch.size = batch_size,
        verbose = FALSE
      )
    
    mean_train <- calculate_mean(model, X, y)
    mean_test <- calculate_mean(model, X_test, y_test)
    mean_average = (mean_train + mean_test) / 2
    
    if (best_train > mean_train &&
        best_test > mean_test && best_average > mean_average) {
      best_model = model
      best_train = mean_train
      best_test = mean_test
      best_average = mean_average
    }
  }
  
  # add new row
  new_row <- data.frame(t(
    c(
      vector_to_string(hidden_layer),
      loss_type,
      learning_rate,
      epoch,
      batch_size,
      best_train,
      best_test
    )
  ))
  neural_network_results <- rbind(neural_network_results, new_row)
}

# Headers

colnames(neural_network_results) <-
  c(
    "hidden_layers",
    "loss_types",
    "learning_rates",
    "epochs",
    "batch_sizes",
    "train",
    "test"
  )

# save csv

write.csv(
  neural_network_results,
  "neural_network.csv",
  row.names = FALSE,
  quote = FALSE
)


nn_stats <-
  read.csv(
    "neural_network.csv",
    header = TRUE,
    sep = ",",
    fill = TRUE,
    stringsAsFactors = TRUE
  )

nn_stats

## Visualize

for (name in colnames(nn_stats)) {
  plot(
    nn_stats[, name],
    nn_stats$test,
    main = paste("Plot", name),
    xlab = name,
    ylab = "Test"
  )
}

# Regression on results of nn_stats

model <-
  lm(test ~ hidden_layers + loss_types + learning_rates + epochs + batch_sizes,
     data = nn_stats)
model

y <- nn_stats$test
prognosis <- model$fitted.values
Error <- mean(abs(y - prognosis))
Error
min <- min(data$CoolingLoad)
max <- max(data$CoolingLoad)
range <- max - min


string_to_vector <- function(my_vector) {
  my_string <- strsplit(my_string, "-")[[1]]
  return(my_string)
}

# Task 4 -----------------------------------------------------------------------
