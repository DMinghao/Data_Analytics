##### Dependencies #####

R.Version()

if (!require(tree))
  install.packages('tree')
library(tree)

if (!require(randomForest))
  install.packages("randomForest")
library(randomForest)

if (!require(randomForestExplainer))
  install.packages("randomForestExplainer")
library(randomForestExplainer)

##### Pre-process Data #####

data <-
  read.csv('ObesityDataSet.csv',
           na.strings = '?',
           stringsAsFactors = T)
data <- na.omit(data)

# rename columns
colnames(data)[6] <- 'eats_high_calor_food'
colnames(data)[7] <- 'eats_veggies'
colnames(data)[8] <- 'num_meals'
colnames(data)[9] <- 'eats_snacks'
colnames(data)[11] <- 'drinks_water'
colnames(data)[12] <- 'counts_calories'
colnames(data)[13] <- 'exercises_often'
colnames(data)[14] <- 'time_using_tech'
colnames(data)[15] <- 'drinks_alcohol'
colnames(data)[16] <- 'method_trans'
colnames(data)[17] <- 'weight_category'
names(data)

head(data)

set.seed(1)

# split sample and test
train <- sample(1:nrow(data), nrow(data) / 2)
test <- data[-train,]

##### Decision Tree #####

# plant a tree
data.tree <- tree(weight_category ~ ., data = data)
data.tree
summary(data.tree)
plot(data.tree)
text(data.tree, pretty = 0)


data.tree <- tree(weight_category ~ ., data = data, subset = train)
data.tree.pred = predict(data.tree, test, type = "class")

table(data.tree.pred, test$weight_category)
mean(data.tree.pred == test$weight_category)


cv.data <- cv.tree(data.tree, FUN = prune.misclass)
names(cv.data)
plot(cv.data)
cv.data


prune.data <- prune.misclass(data.tree, best = 9)
plot(prune.data)
text(prune.data, pretty = 0)

prune.data.pred <- predict(prune.data, test, type = "class")
table(prune.data.pred, test$weight_category)
mean(prune.data.pred == test$weight_category)



data.forest <- randomForest(
  weight_category ~ .,
  data = data,
  subset = train,
  ntree = 5000,
  importance = T,
  localImp = T
)
data.forest
plot(data.forest)

data.forest <- randomForest(
  weight_category ~ .,
  data = data,
  subset = train,
  ntree = 5000,
  mtry = ncol(data) - 1,
  importance = T,
  localImp = T
)
data.forest
plot(data.forest)

importance(data.forest)
varImpPlot(data.forest)

data.forest.pred <- predict(data.forest, test, typr = "class")
table(data.forest.pred, test$weight_category)
mean(data.forest.pred == test$weight_category)

mtry.list <- c()
data.forest.final <- NULL
i <- 1
max <- 0
for (i in 1:ncol(data) - 1) {
  data.forest.dummy <- randomForest(
    weight_category ~ .,
    data = data,
    subset = train,
    ntree = 5000,
    mtry = i,
    importance = T,
    localImp = T
  )
  val <- predict(data.forest.dummy, test, typr = "class")
  score <- mean(val == test$weight_category)
  if (score >= max) {
    data.forest.final <- data.forest.dummy
    max <- score
  }
  mtry.list[i] <- score
}

mtry.list
plot(mtry.list)

data.forest.final
plot(data.forest.final)

importance(data.forest.final)
varImpPlot(data.forest.final)

data.forest.final.pred <-
  predict(data.forest.final, test, typr = "class")
table(data.forest.final.pred, test$weight_category)
mean(data.forest.final.pred == test$weight_category)

explain_forest(data.forest.final, interactions = TRUE, data = data)
