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

data[1:5,]

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

summary(data)

# Task 1 -----------------------------------------------------------------------

data$Orientation <- as.factor(data$Orientation)
data$OverallHeight <- as.factor(data$OverallHeight)
data$GlazingArea <- as.factor(data$GlazingArea)
data$GlazingAreaDistribution <- as.factor(data$GlazingAreaDistribution)

summary(data)

# Task 2 -----------------------------------------------------------------------

# Task 3 -----------------------------------------------------------------------

# Task 4   -----------------------------------------------------------------------