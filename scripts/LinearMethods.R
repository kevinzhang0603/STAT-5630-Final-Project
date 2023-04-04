library(tidyverse)
library(glmnet)
library(MLmetrics)

set.seed(4133) # For reproducibility of the analysis
theme_set(theme_bw(base_family = "Poppins")) # Setting a graphic style for the analysis
data = read_csv("https://raw.githubusercontent.com/bbwieland/STAT-5630-Final-Project/main/data/insurance.csv")

## Train-Test Split ----

# We will use 70/30 train/test split size for our data

train.indices = sample(1:nrow(data), 0.7 * nrow(data))
train = data[train.indices,]
test = data[-train.indices,]

## Exploring a Response Variable Transformation ----

# Visualizing our response variable's distribution with a histogram
ggplot(data, aes(x = charges)) +
  geom_histogram(bins = sqrt(nrow(data)), color = "black", fill = "grey") +
  labs(x = "Medical Charges (dollars)", y = "Count", title = "Distribution of Response Variable")

# Our linear methods for classification may work better with a more normally distributed response variable.
# So we explore a logarithmic transformation.
ggplot(data, aes(x = log(charges))) +
  geom_histogram(bins = sqrt(nrow(data)), color = "black", fill = "grey") +
  labs(x = "Medical Charges (dollars)", y = "Count", title = "Distribution of Response Variable")

# This makes our data approximately normal, which is a desirable behavior.
# We will model the log-transformed variable. 
train$charges.log = log(train$charges)
test$charges.log = log(test$charges)

## EDA of Predictor Relationships with Response Variable ----

# Let us analyze each variable individually. 

# First our numeric predictors:

# Age
ggplot(train, aes(x = age, y = charges.log)) +
  geom_point() +
  labs(x = "Age", y = "log(Charges)", title = "Relationship of Age and Medical Charges")

# BMI
ggplot(train, aes(x = bmi, y = charges.log)) +
  geom_point() +
  labs(x = "BMI", y = "log(Charges)", title = "Relationship of BMI and Medical Charges")

# Children (semi-categorical, but we can treat potentially as numeric)
ggplot(train, aes(x = children, y = charges.log, group = children)) +
  geom_boxplot() +
  labs(x = "Children", y = "log(Charges)", title = "Relationship of Number of Children and Medical Charges")

# Now our categorical predictors:

# Sex
ggplot(train, aes(x = sex, y = charges.log, group = sex)) +
  geom_boxplot() +
  labs(x = "Sex", y = "log(Charges)", title = "Relationship of Sex and Medical Charges")

# Smoker
ggplot(train, aes(x = smoker, y = charges.log, group = smoker)) +
  geom_boxplot() +
  labs(x = "Smoking Status", y = "log(Charges)", title = "Relationship of Smoking Status and Medical Charges")

# Region
ggplot(train, aes(x = region, y = charges.log, group = region)) +
  geom_boxplot() +
  labs(x = "Region", y = "log(Charges)", title = "Relationship of Residence Region and Medical Charges")

## Fitting a Simple Baseline Model for Comparison

# Let us consider the sample mean of the logarithm of charges as our sample
baseline.pred = exp(mean(train$charges.log))

# RMSE in and out of sample
baseline.RMSE.in = RMSE(baseline.pred, train$charges)
baseline.RMSE.out = RMSE(baseline.pred, test$charges)

baseline.RMSE.in
baseline.RMSE.out
