library(tidyverse)
library(class) #knn 
library(MASS)
library(glmnet) #lasso
library(gbm) #boosted trees

# load in data -----------------------------------------------------------------
load("ADProj.Rdata")

# pull out given training and test set -----------------------------------------
train_set <- ADProj$X_train
train_outcome <- ADProj$y_train
train_set <- cbind(train_outcome, train_set)
test_set <- ADProj$X_test

# break training set into training and validation set --------------------------
set.seed(546)
train_sample <- sample(nrow(train_set), 200) #50%
training_set <- train_set[train_sample, ]
val_set <- train_set[-train_sample, ]

# first method: K nearest neighbors --------------------------------------------

#vectors to store training and test set accuracy
acc_vec_train <- rep(0, 50)
acc_vec_test <- rep(0, 50)

#run for 50 different ks
for(i in 1:50){
  knn.test.pred <- knn(train = training_set[, -1],
                       test = val_set[, -1],
                       cl = training_set$Outcome, k = i)
  knn.train.pred <- knn(train = training_set[, -1],
                        test = training_set[, -1],
                        cl = training_set$Outcome, k = i)
  
  acc_vec_train[i] <- mean(knn.train.pred == training_set$Outcome)
  acc_vec_test[i] <- mean(knn.test.pred == val_set$Outcome)
}

num <- c(1:50)
accuracy <- as.data.frame(cbind(num, acc_vec_train, acc_vec_test))
#k = 20 appears to be our best option

#figure 1: training and test set accuracy
accuracy %>%
  ggplot((aes(x = num, y = acc_vec_train*100))) +
  geom_point(aes(col = "Data1")) +
  geom_point(aes(y = acc_vec_test*100, col = "Data2")) +
  scale_color_manual(labels = c("Training set", "Validation set"), 
                     values = c("blue","red")) +
  ylim(0,100) +
  labs(x = "Choice of k", y = "% Accurate", col = "Dataset") +
  geom_vline(xintercept = 20, col = "black", linetype = "dashed")

#get predictions on test set
knn.test.pred <- knn(train = training_set[,-1],
                     test = test_set,
                     cl = training_set$Outcome, k = 20)
predictions <- as.data.frame(knn.test.pred)

# second method: lasso regression -----------------------------------------------

set.seed(10)

#organize data in format cv.glmnet function expects
newX <- model.matrix(~ . -Outcome, data = train_set)

#fit cross-validated lasso mod
lasso.mod <- cv.glmnet(newX[, -1],
                       train_set[, 1],
                       alpha = 1,
                       nfolds = 5,
                       family = "binomial",
                       type.measure = "auc")
lasso.mod$lambda.min

#fit using best lambda
fit_min <- glmnet(newX[, -1], 
                  train_set[, 1], 
                  family = "binomial", 
                  alpha = 1, 
                  lambda = lasso.mod$lambda.min)

#predictions and accuracy on training set
predictions_min <- predict(fit_min, 
                           newx = as.matrix(train_set[,-1]), 
                           type = "response")
label_min <- ifelse(predictions_min > 0.5, "AD", "C")
acc_min <- mean(label_min == train_set$Outcome)

#get predictions on test set
predictions_min <- predict(fit_min, 
                           newx = as.matrix(test_set),
                           type = "response")
label_min <- ifelse(predictions_min > 0.5, "AD", "C")
predictions2 <- as.data.frame(label_min)

# third method: boosted trees --------------------------------------------------

#make training set outcome binary
train_set$Outcome_bin <- ifelse(train_set$Outcome == "AD", 1, 0)
train_set <- train_set[, -1]

#run boosted tree model
set.seed(10)
boost_mod <-gbm(Outcome_bin ~ ., 
                data = train_set, 
                distribution = "bernoulli", 
                n.trees = 500,
                interaction.depth = 2,
                shrinkage = 0.01)

#get training set predictions and accuracy
boosted_train_pred <- predict(boost_mod, newdata = train_set, type = "response")
boosted_train_pred <- ifelse(boosted_train_pred > 0.5, 1, 0)
boosted_train_acc <- mean(boosted_train_pred == train_set$Outcome_bin)

#get test set predictions
boosted_test_pred <- predict(boost_mod, newdata = test_set, type = "response")
boosted_test_pred <- ifelse(boosted_test_pred > 0.5, "AD", "C")
predictions3 <- as.data.frame(boosted_test_pred)
