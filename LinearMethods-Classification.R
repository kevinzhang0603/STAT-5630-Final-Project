library(tidyverse)
library(caret)
library(MASS)

set.seed(4133) # For reproducibility of the analysis
theme_set(theme_bw(base_family = "Poppins")) # Setting a graphic style for the analysis
data = read_csv("https://raw.githubusercontent.com/bbwieland/STAT-5630-Final-Project/main/data/insurance.csv")

data$response = factor(data$smoker, levels = c("no","yes"))

## Train-Test Split ----

# We will use 70/30 train/test split size for our data

train.samples = createDataPartition(data$smoker, p = 0.7, list = F)
train = data[train.samples,]
test = data[-train.samples,]

## Linear Discriminant Analysis ----

# We need to transform our data — specifically, center and scale it — for optimal LDA performance.

sampling.method = trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down") # note the use of downsampling to address class imbalance

lda.model = caret::train(response ~ age + bmi + children + region + sex + charges,
                              data = train,
                              method = "lda",
                              preProcess = c("scale", "center"),
                              trControl = sampling.method)

### In-Sample Metrics ----
lda.predictions.train = predict(lda.model, train)

table(lda.predictions.train)
table(train$response)

# accuracy
mean(lda.predictions.train == train$response)

### Out-of-Sample Metrics ----
lda.predictions.test = predict(lda.model, test)

table(lda.predictions.test)
table(test$response)

# accuracy
mean(lda.predictions.test == test$response)

# compute ROC curve
roc.curve(test$response, lda.predictions.test)
