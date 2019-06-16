library(randomForest)
library(AUC)
library(RSNNS)

X_train <- read.csv("C:\\Users\\Goko\\Desktop\\hw7\\training_data.csv", header = TRUE)
X_test <- read.csv("C:\\Users\\Goko\\Desktop\\hw7\\test_data.csv", header = TRUE)

y_load <- read.csv("C:\\Users\\Goko\\Desktop\\hw7\\training_labels.csv", header = FALSE)
y_train <- as.factor(y_load[,1])

smp_size  <- 75000

train_ind <- sample(seq_len(nrow(X_train)), size = smp_size)

X_new_validation <- X_train[train_ind, ]
X_new_train <- X_train[-train_ind, ]
y_new_train <- as.factor(y_load[-train_ind ,1])
y_new_validation <- as.factor(y_load[train_ind ,1])


model <- randomForest(y~., data = cbind(X_new_train, y = y_new_train), ntree=200)
randomforest_classifier <- model

y_pred_lr <- predict(model, X_new_validation, type= "prob")

roc_curve <- roc(predictions = y_pred_lr[,2], labels = y_new_validation)
auc(roc_curve)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)

test_scores <- predict(randomforest_classifier, X_test, type= "prob")
write.table(test_scores[,2], file = "C:\\Users\\Goko\\Desktop\\hw7\\test_predictions.csv", row.names = FALSE, col.names = FALSE)
 