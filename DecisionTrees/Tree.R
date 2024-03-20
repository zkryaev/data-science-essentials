data <- read.csv("~/survey.csv", header=TRUE, sep=",")

train_data <- data[1:600, ]
test_data <- data[601:750, ]

#a
decision_tree_inform <- rpart(MYDEPV ~ Price + Income + Age, data = train_data, method = "class", parms = list(split = "information"), xval = 3)

printcp(decision_tree_inform)

rpart.plot(decision_tree_inform, type = 0)

#b
dtc_predictions <- predict(decision_tree_inform, newdata = test_data, type = "class")

conf_matrix <- table(dtc_predictions, test_data$MYDEPV)
print(conf_matrix)

#c
resubstitution_error_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Коэффициент ошибки переобучения:", resubstitution_error_rate, "\n", 
    "Т.е примерно", round(resubstitution_error_rate, digits = 4)*100, "% были неверно классифицированы моделью \n")

#d ROCR library!!!
dtc_probabilities <- as.matrix(predict(decision_tree_inform, newdata = test_data, type = "prob"))
roc_pred <- prediction(dtc_probabilities[, 2], test_data$MYDEPV)
roc_perf <- performance(roc_pred, "tpr", "fpr")

plot(roc_perf, main = "Кривая ROC", col = "blue", lwd = 2, 
     xlab = "Ложноположительная оценка", ylab = "Истинноположительная оценка")

auc_value <- performance(roc_pred, "auc")@y.values[[1]]
cat("Площадь под кривой ROC (AUC):", auc_value, "\n")

#e
accuracy <- sum(dtc_predictions == test_data$MYDEPV) / length(test_data$MYDEPV)
cat("Accuracy:", round(accuracy, digits = 4) * 100, "%\n")

#f
decision_tree_gini <- rpart(MYDEPV ~ Price + Income + Age, data = train_data, method = "class", parms = list(split = "gini"), xval = 3)
printcp(decision_tree_gini)
rpart.plot(decision_tree_gini, type = 0)
dt_gini_predictions <- predict(decision_tree_gini, newdata = test_data, type = "class")
conf_matrix_gini <- table(dt_gini_predictions, test_data$MYDEPV)
print(conf_matrix_gini)
resubstitution_error_rate_gini <- 1 - sum(diag(conf_matrix_gini)) / sum(conf_matrix_gini)
cat("Коэффициент ошибки переобучения:", resubstitution_error_rate_gini, "\n", 
    "Т.е примерно", round(resubstitution_error_rate_gini, digits = 4)*100, "% были неверно классифицированы моделью \n")
dt_gini_predictions <- as.matrix(predict(decision_tree_gini, newdata = test_data, type = "prob"))
roc_pred_gini <- prediction(dt_gini_predictions[, 2], test_data$MYDEPV)
roc_perf_gini <- performance(roc_pred_gini, "tpr", "fpr")
plot(roc_perf_gini, main = "Кривая ROC", col = "blue", lwd = 2, 
     xlab = "Ложноположительная оценка", ylab = "Истинноположительная оценка")
auc_value_gini <- performance(roc_pred_gini, "auc")@y.values[[1]]
cat("Площадь под кривой ROC (AUC):", auc_value_gini, "\n")


#g
pruned_tree <- prune(decision_tree_inform, cp = decision_tree_inform$cptable[which.min(decision_tree_inform$cptable[, "xerror"]), "CP"])
rpart.plot(pruned_tree, type = 0)
pruned_predictions <- predict(pruned_tree, newdata = test_data, type = "class")
conf_matrix_prun <- table(pruned_predictions, test_data$MYDEPV)
print(conf_matrix_prun)
resubstitution_error_rate_prun <- 1 - sum(diag(conf_matrix_prun)) / sum(conf_matrix_prun)
cat("Коэффициент ошибки переобучения:", resubstitution_error_rate_prun, "\n", 
    "Т.е примерно", round(resubstitution_error_rate_prun, digits = 4)*100, "% были неверно классифицированы моделью \n")
