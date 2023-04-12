# Setup ------------------------------------------------------------------------
# Task can be found in the script on page 76
WD = getwd()
setwd(WD)

data <-
  read.csv(
    "Daten/Teilebedarfe.csv",
    header = TRUE,
    sep = ",",
    fill = TRUE,
    stringsAsFactors = TRUE
  )

names(data) <- c("Tag","Bedarf")

y <- data$Bedarf

min(y)
max(y)
     

# Alternative 1 -----------------------------------------------------------------

mean <- mean(y)
standard <- sd(y)

mean
standard

result_1 <- mean + 2 * standard
result_1

mean(ifelse(result_1 >= y, 1,0))


# Alternative 2 -----------------------------------------------------------------

result_2 <- quantile(y,0.975)
result_2

mean(ifelse(result_2 >= y, 1,0))

