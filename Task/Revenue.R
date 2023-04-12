# Setup ------------------------------------------------------------------------
# Task can be found in the script on page 71
WD = getwd()
setwd(WD)

data <-
  read.csv(
    "Daten/online_shoppers_intention.csv",
    header = TRUE,
    sep = ",",
    fill = TRUE,
    stringsAsFactors = TRUE
  )


data$Administrative <- as.factor(data$Administrative)
data$Informational <- as.factor(data$Informational)
data$SpecialDay <- as.factor(data$SpecialDay)
data$OperatingSystems <- as.factor(data$OperatingSystems)
data$Browser <- as.factor(data$Browser)
data$TrafficType <- as.factor(data$TrafficType)
data$Region <- as.factor(data$Region)

data$Weekend <- ifelse(data$Weekend == TRUE,1,0)
data$Weekend <- as.factor(data$Weekend)
data$Revenue <- ifelse(data$Revenue == TRUE,1,0)
data$Revenue <- as.factor(data$Revenue)

summary(data)

# Analysis ---------------------------------------------------------------------

boxplot(ExitRates ~ Revenue,data)


