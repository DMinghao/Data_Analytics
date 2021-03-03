if(!require(tree))
  install.packages('tree')
library(tree)

if (!require(randomForest))
  install.packages("randomForest")
library(randomForest)

data <-
  read.csv('ObesityDataSet.csv',
           na.strings = '?',
           stringsAsFactors = T)
data <- na.omit(data)
View(data)

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

data[1,]
# data$weight_category <- unclass(data$weight_category) - 2
# data$family_history_with_overweight <- unclass(data$family_history_with_overweight) - 1
# data$eats_high_calor_food <- unclass(data$eats_high_calor_food) - 1
# data$SMOKE <- unclass(data$SMOKE) - 1
# data$counts_calories <- unclass(data$counts_calories) - 1
# data$drinks_alcohol <- unclass(data$drinks_alcohol) - 1
# data$eats_snacks <- unclass(data$eats_snacks) - 1
#
# data[1, ]

# data$normal <- ifelse(data$weight_category=='Normal_Weight','Yes','No')
# data[1, ]

data.tree <- tree(weight_category ~ ., data = data)
data.tree
summary(data.tree)
plot(data.tree)
text(data.tree, pretty = 0)


set.seed(2)

train <- sample(1:nrow(data), nrow(data) / 2)
test <- data[-train,]

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
  importance = T
)
data.forest
plot(data.forest)

data.forest <- randomForest(
  weight_category ~ .,
  data = data,
  subset = train,
  ntree = 5000,
  mtry = ncol(data) - 1,
  importance = T
)
data.forest
plot(data.forest)

importance(data.forest)
varImpPlot(data.forest)

data.forest.pred <- predict(data.forest, test, typr = "class")
table(data.forest.pred, test$weight_category)
mean(data.forest.pred == test$weight_category)

mtry.list <- c()
i <- 1
for (i in 1:ncol(data) - 1) {
  data.forest.dummy <- randomForest(
    weight_category ~ .,
    data = data,
    subset = train,
    ntree = 5000,
    mtry = i,
    importance = T
  )
  val <- predict(data.forest.dummy, test, typr = "class")
  mtry.list[i] <- mean(val == test$weight_category)
}

mtry.list
plot(mtry.list)

data.forest.final <- randomForest(
  weight_category ~ .,
  data = data,
  subset = train,
  ntree = 5000,
  mtry = which.max(mtry.list),
  importance = T
)
data.forest.final
plot(data.forest.final)

importance(data.forest.final)
varImpPlot(data.forest.final)

data.forest.final.pred <- predict(data.forest.final, test, typr = "class")
table(data.forest.final.pred, test$weight_category)
mean(data.forest.final.pred == test$weight_category)


# rf.cv <- rfcv(data, data$weight_category, cv.fold=10)
#
# with(rf.cv, plot(n.var, error.cv))
