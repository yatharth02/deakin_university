library(dplyr)
library(moments)
library(GGally)

set.seed(224207854) # using studentId as seed for consistent samples

######################
# T1. Understand the data

## (i) Download the txt file (ENB_2023.txt) from CloudDeakin and save it to
##  your R working directory.
setwd("~/DeakinUniversity/Real Word Analytics/Mid Term Assessment")
list.files()
##

## (ii) Assign the data to a matrix
the.data <- as.matrix(read.table("ENB_2023.txt"))
##

## (iii) The variable of interest is Y (Appliances). To investigate Y, generate
## a subset of 340 with numerical data
my.data <- the.data[sample(1:671, 340), c(1:6)]
dim(my.data) # Verifying shape of data set
print(my.data)

## Renaming colmnames for provided dataset
my.data <- my.data %>%
  as.data.frame %>%
  rename(
    'X1' = 'V1',
    'X2' = 'V2',
    'X3' = 'V3',
    'X4' = 'V4',
    'X5' = 'V5',
    'Y' = 'V6'
  ) %>%
  as.matrix

print(my.data)
summary(my.data)
##

## (iv) Use scatter plots and histograms to understand the relationship between
## each of the variables X1, X2, X3, X4, X5 and your variable of interest Y.

### Scatter plots(5), for each X variable against variable of interest Y.
plot(
  x = my.data[, 1],
  y = my.data[, 6],
  xlab = "Temperature in kitchen area(X1)",
  ylab = "Appliances(Y)",
  main = "Temperature in kitchen area vs Appliances"
)
plot(
  x = my.data[, 2],
  y = my.data[, 6],
  xlab = "Humidity in kitchen area(X2)",
  ylab = "Appliances(Y)",
  main = "Humidity in kitchen area vs Appliances"
)
plot(
  x = my.data[, 3],
  y = my.data[, 6],
  xlab = "Temperature outside(X3)",
  ylab = "Appliances(Y)",
  main = "Temperature outside vs Appliances"
)
plot(
  x = my.data[, 4],
  y = my.data[, 6],
  xlab = " Humidity outside(X4)",
  ylab = "Appliances(Y)",
  main = "Humidity outside vs Appliances"
)
plot(
  x = my.data[, 5],
  y = my.data[, 6],
  xlab = "Visibility(X5)",
  ylab = "Appliances(Y)",
  main = "Visibility vs Appliances"
)
###

### Histogram plots(6) for each X and Y variable.
# Function to plot a histogram
create_histogram <-
  function(x,
           xlab = "",
           pos = "topright",
           rounding = 2) {
    mean <- round(mean(x), rounding)
    meanlabel <- paste("Mean   (", mean, ")")
    median <- round(median(x), rounding)
    medianlabel <- paste("Median (", median, ")")
    min <- round(min(x), rounding)
    minlabel <- paste("Min    (", min, ")")
    max <- round(max(x), rounding)
    maxlabel <- paste("Max    (", max, ")")
    
    title = paste("Histogram of ", xlab)
    
    hist(x, xlab = xlab, main = title)
    abline(v = mean, col = 2, lwd = 3)
    abline(v = median, col = 7, lwd = 3)
    abline(v = min, col = 4, lwd = 3)
    abline(v = max, col = 4, lwd = 3)
    legend(
      x = pos,
      # Position
      cex = 0.75,
      # Size of text
      legend = c(meanlabel, medianlabel, minlabel, maxlabel),
      # Legend texts
      lty = c(1),
      # Line types
      col = c(2, 7, 4, 4),
      # Line colors
      lwd = 3
    )                    # Line width
  }

## Plotting histogram for all params
create_histogram(
  my.data[, 1],
  "Temperature in kitchen area, in Celsius(X1)"
)
create_histogram(
  my.data[, 2],
  "Humidity in kitchen area, given as a percentage(X2)"
)
create_histogram(
  my.data[, 3],
  "Temperature outside (from weather station), in Celsius(X3)"
)
create_histogram(
  my.data[, 4],
  "Humidity outside (from weather station), given as a percentage(X4)"
)
create_histogram(
  my.data[, 5],
  "Visibility (from weather station), in km(X5)"
)
create_histogram(
  my.data[, 6],
  "Appliances, energy use, in Wh(Y)"
)

######################

######################
# T2. Transform the data
colSums(is.na(my.data)) # Checking missing values

dim(unique(my.data)) # Verifying provided data is unique via shape comparison.

# Checking skewness for all variables
skewness(my.data[, 1]) # 0.3852449
skewness(my.data[, 2]) # 0.4896909
skewness(my.data[, 3]) # 0.107878
skewness(my.data[, 4]) # -0.3207455
skewness(my.data[, 5]) # 0.740524
skewness(my.data[, 6]) # 1.728178

## Calculating correlation coefficients
cor(my.data[, 1], my.data[, 6]) # 0.4229276
cor(my.data[, 2], my.data[, 6]) # 0.1314382
cor(my.data[, 3], my.data[, 6]) # 0.5266902
cor(my.data[, 4], my.data[, 6]) # 0.07758634
cor(my.data[, 5], my.data[, 6]) # 0.2708228

## Plotting correlation graph
ggcorr(my.data, label = TRUE, label_round = 2)

# As per the above checks, will use 4 variables i.e. X1, X2, X3, X5
my.new.data <- my.data[, c(1, 2, 3, 5, 6)]
dim(my.new.data)

## Plotting correlation graph for choosen params
ggcorr(my.new.data, label = TRUE, label_round = 2)


## Plotting boxplot for all params
my.new.data %>% as.data.frame %>% boxplot(main = 'Distribution')


## Outlier function to fetch all outliers
find_outliers <- function(x) {
  # Find the Inter Quantile Range, denoted by iqr
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  
  # Define the lower and upper bounds of the dataset
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Using the upper and lower bounds identified thus, 
  # divide the dataset into outliers and filtered sets.
  # Save the values identified as outliers and the filtered values
  outliers <- x[x <= lower_bound | x >= upper_bound]
  x_filtered <- x[x >= lower_bound & x <= upper_bound]
  
  # return the set of both the outliers and the filtered dataset for analysis
  return(list(length(outliers), length(x_filtered)))
}

## Checking outliers over dataset
find_outliers(my.new.data[, 1])
find_outliers(my.new.data[, 2])
find_outliers(my.new.data[, 3])
find_outliers(my.new.data[, 4])
find_outliers(my.new.data[, 5])


p = 1 / 2 # Value for polynomial transformation

## Z-score scaling
unit.z <- function(x) {
  0.15 * ((x - mean(x)) / sd(x)) + 0.5
}

## Defining mean/standard deviation for all params
mean_X1 <- mean(my.new.data[, 1] ^ p)
sd_X1 <- sd(my.new.data[, 1] ^ p)

mean_X2 <- mean(log(my.new.data[, 2]))
sd_X2 <- sd(log(my.new.data[, 2]))

mean_X3 <- mean(my.new.data[, 3] ^ p)
sd_X3 <- sd(my.new.data[, 3] ^ p)

mean_X4 <- mean(log(my.new.data[, 4]))
sd_X4 <- sd(log(my.new.data[, 4]))

mean_Y <- mean(log(my.new.data[, 5]))
sd_Y <- sd(log(my.new.data[, 5]))

## Checking outliers over scaled/transformed data
find_outliers(unit.z(my.new.data[, 1] ^ p))
find_outliers(unit.z(log(my.new.data[, 2])))
find_outliers(unit.z(my.new.data[, 3] ^ p))
find_outliers(unit.z(log(my.new.data[, 4])))
find_outliers(unit.z(log(my.new.data[, 5])))

## Updating my.new.data variable with data scaling/transforming
my.new.data[, 1] <- unit.z(my.new.data[, 1] ^ p)
my.new.data[, 2] <- unit.z(log(my.new.data[, 2]))
my.new.data[, 3] <- unit.z(my.new.data[, 3] ^ p)
my.new.data[, 4] <- unit.z(log(my.new.data[, 4]))
my.new.data[, 5] <- unit.z(log(my.new.data[, 5]))

## Saving it to a txt file
write.table(my.new.data, "yatharth-transformed.txt")

######################

######################
# T3. Build models and investigate the importance of each variable

source("AggWaFit718.R")

my.data.transformed <-
  as.matrix(read.table("yatharth-transformed.txt"))

dim(my.data.transformed)

## a. Weighted arithmetic mean (WAM)
fit.QAM(
  my.data.transformed,
  output.1 = "wam_output1.txt",
  stats.1 = "wam_stats1.txt"
)

## b. Weighted power means (WPM) with p = 0.5
fit.QAM(
  my.data.transformed,
  output.1 = "p0.5_output1.txt",
  stats.1 = "p0.5_stats1.txt",
  g = PM05,
  g.inv = invPM05
)

## c. Weighted power means (WPM) with p = 2
fit.QAM(
  my.data.transformed,
  output.1 = "p2_output1.txt",
  stats.1 = "p2_stats1.txt",
  g = QM,
  g.inv = invQM
)

## d. An ordered weighted averaging function (OWA)
fit.OWA(
  my.data.transformed,
  output.1 = "owa_output1.txt",
  stats.1 = "owa_stats1.txt"
)

## e. The Choquet integral
fit.choquet(
  my.data.transformed,
  output.1 = "chq_output1.txt",
  stats.1 = "chq_stats1.txt"
)

######################

######################

# T4. Use your model for prediction.

X1 <- 22
X2 <- 38
X3 <- 4
X4 <- 88.2
X5 <- 34

new.input.to.transform <- c(X1, X2, X3, X5)
new.input.to.transform

## z-score function for new inputs
new.unit.z <- function(x, mean, sd) {
  0.15 * ((x - mean) / sd) + 0.5
}

## Preprocessing new inputs
new.input.to.transform[1] <-
  new.unit.z(new.input.to.transform[1] ^ p, mean_X1, sd_X1)
new.input.to.transform[2] <-
  new.unit.z(log(new.input.to.transform[2]), mean_X2, sd_X2)
new.input.to.transform[3] <-
  new.unit.z(new.input.to.transform[3] ^ p, mean_X3, sd_X3)
new.input.to.transform[4] <-
  new.unit.z(log(new.input.to.transform[4]), mean_X4, sd_X4)

## Weights from WAM function
w <-
  c(0.371771434283025,
    0.0269788834609033,
    0.428218127468992,
    0.17303155478708)

new.input.to.transform

## Using best fitting model from T3, i.e. WAM
y.scaled.pred <- QAM(new.input.to.transform, w)

## Performing inverse transformation over predicted scaled Y.
y.pred <-
  exp((sd_Y * (y.scaled.pred - 0.5) / 0.15) + mean_Y) # 95.10746

round(y.pred, digits = 0) # 95
y.actual <- 100

# Model seems to be good as difference between pred_y(95) and actual_y(100) is
# 5 unit gap, we can use different functionalities also to decrease this gap.

######################