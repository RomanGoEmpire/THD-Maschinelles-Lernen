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

data[1:5,]

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
## ---- Regression ----
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


## ---- Entscheidungsbaum ------------------------------------------------------
best_model = NULL
best = c(1000)

for (i in c(1:10000)) {
  n <- length(data[, 1])
  index <- sample(1:n, n, replace = FALSE)
  data <- data[index,]
  
  n <- nrow(data) * 0.7
  data.train <- data[1:n,]
  data.test <- data[(n + 1):768,]
  
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

# ---- Neuronales Netzwerk -----------------------------------------------------

# shuffle
n <- length(data[, 1])
index <- sample(1:n, n, replace = FALSE)
data <- data[index,]
# split data into test and training data
n <- nrow(data) * 0.5
data.train <- data[1:n,]
data.test <- data[(n + 1):768,]


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

X <-
  model.matrix(
    CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
      Orientation + GlazingArea + GlazingAreaDistribution,
    data = data.train
  )

X <- X[, -1]   # entferne den Intercept

y <- data.train$CoolingLoad

X.test <-
  model.matrix(
    CoolingLoad ~ RelativeCompactness + WallArea + RoofArea + OverallHeight +
      Orientation + GlazingArea + GlazingAreaDistribution,
    data = data.test
  )

X.test <- X.test[, -1]   # entferne den Intercept

y.test <- data.test[, "CoolingLoad"]


vector_to_string <- function(my_vector) {
  my_string <- paste(my_vector, collapse = "-")
  return(my_string)
}

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
  current_combination <- combinations[i,]
  hidden_layer <- unlist(current_combination$hidden_layers)
  loss_type <- unlist(current_combination$loss_types)
  learning_rate <- unlist(current_combination$learning_rates)
  epoch <- unlist(current_combination$epochs)
  batch_size <- unlist(current_combination$batch_sizes)
  
  tries = 100
  
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
        hidden.layers = hidden_layer,
        regression = TRUE,
        loss.type = loss_type,
        learn.rates = learning_rate,
        n.epochs = epoch,
        batch.size = batch_size,
        verbose = FALSE
      )
    
    prognosen <- predict(model, X)$predictions
    mean_train = mean(abs(prognosen - y))
    
    prognosen <- predict(model, X.test)$predictions
    mean_test = mean(abs(prognosen - y.test))
    
    mean_average = (mean_train + mean_test) / 2
    mean_difference = mean_test / mean_train
    
    results[i] = mean_average
    
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

string_to_vector <- function(my_vector) {
  my_string <- strsplit(my_string, "-")[[1]]
  return(my_string)
}

# Task 4 -----------------------------------------------------------------------
