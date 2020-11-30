library(readr)
dataset <- read.csv("data.csv")
head(dataset)

#checking uniqueness
unique(dataset$age)

#unique entries
length(unique(dataset$age))

#counting
sort(table(dataset$player), decreasing = TRUE)

#filtering
datasett <- dataset[dataset$age == "25", ]
dim(datasett)


library(purrr)
library(dplyr)
dataset %>%
  select_if(is.numeric) %>%
  map_dbl(mean, na.rm = TRUE)


library(GGally)
dataset %>%
  select(ast, fg, trb) %>%
  ggpairs()

library(cluster)
set.seed(1)
column <- function(col){
  sum(is.na(col)) == 0 && is.numeric(col)
}
newcolumn <- sapply(dataset, column)
clusters <- kmeans(dataset[,newcolumn], centers=3)
labels <- clusters$cluster

dataset1 <- prcomp(dataset[,newcolumn], center=TRUE)
twocolumns <- dataset1$x[,1:2]
clusplot(twoColumns, labels)


# Random sample indexes
sample_size <- floor(0.8 * nrow(dataset))
set.seed(1)
trainIndex <- sample(1:nrow(dataset), sample_size)
train <- dataset[trainIndex,]
test <- dataset[-trainIndex,]

#linear regression
linear <- lm(ast ~ fg, data=train)
predictions <- predict(linear, test)
summary(linear)

#logistic regression
logistic <- glm(ast ~ fg, data=train)
predicted= predict(logistic,test)
summary(logistic)

#decision trees
library(rpart)
#tree
fit <- rpart(ast ~ fg, data=train, method="class")
predicted= predict(fit,test)
summary(fit)

#gradient boost
library(caret)
fit <- train(ast ~ fg, data=train, method = "gbm")
predicted= predict(fit,test,type= "prob")[,2]
summary(fit)

#xgb boost
library(xgboost)
# Transform the two data sets into xgb.Matrix





