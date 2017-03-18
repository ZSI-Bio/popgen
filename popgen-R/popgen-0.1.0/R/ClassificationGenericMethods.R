# # Popgen. Classification functions (both tuning and prediction)
# # * Decision Trees is only for classification of super-populations groups
#
# setTrainError <-function(theObject, error){
#             theObject@train.error <- as.numeric(error)
#             return (theObject)}
#
# setTestError <- function(theObject, error){
#             theObject@test.error <- as.numeric(error)
#             return (theObject)}
#
# setPrediction <- function(theObject, prediction){
#             theObject@predicted <- as.numeric(prediction)
#             return(theObject) }
#
# setGeneric(name = "macroEval",
#            def = function(obj, isTrain = T) {
#              standardGeneric("macroEval")})
#
# setMethod(f = "macroEval",
#           signature = "ClassificationMetrics",
#           definition = function(obj, isTrain = T){
#             eval.metr <- c("Precision", "Recall", "F1-Score", "Accuracy", "Kappa")
#             eval.tbl <- matrix(nrow = 1, ncol = length(eval.metr))
#             colnames(eval.tbl) <- eval.metr
#             rownames(eval.tbl) <- class(obj)[1]
#             if (isTrain == T){
#               precision <- obj@Precision.train
#               recall <- obj@Recall.train
#               conf.matr <- obj@confusionMatrix.train
#               error <- obj@train.error
#             } else{
#               precision <- obj@Precision.test
#               recall <- obj@Recall.test
#               conf.matr <- obj@confusionMatrix.test
#               error <- obj@test.error
#             }
#
#             eval.tbl[1, "Precision"] <-  round(mean(precision), 4) * 100
#             eval.tbl[1, "Recall"] <-  round(mean(recall), 4) * 100
#             eval.tbl[1, "F1-Score"] <-  round(mean(2 * precision * recall / (precision + recall)), 4) * 100
#             eval.tbl[1, "Accuracy"] <-  round(100 - error, 2)
#
#             num.instaces.total <- sum(conf.matr)
#             num.istances.per.class <- apply(conf.matr, 1, sum)
#             num.pred.per.class <- apply(conf.matr, 2, sum)
#             p <- num.istances.per.class / num.instaces.total # distribution of instances over the actual classes
#             q <- num.pred.per.class / num.instaces.total
#             exp.accuracy <- sum(p * q) * 100
#             eval.tbl[1, "Kappa"] <- round((eval.tbl[1, "Accuracy"] - exp.accuracy)  / (100 - exp.accuracy), 2)
#
#             return(eval.tbl)
#           }
# )
#
#
# # --- knn methods --- #
# setGeneric(name = "knn.k.estimation",
#            def = function(theObject, train.data.set, labels = c("Region"), categorical.var = c("SampleId"), cv = "loo"){
#              standardGeneric("knn.k.estimation")})
#
# # --- Function for estimation the "k" parameter --- #
#
# setMethod(f = "knn.k.estimation",
#           signature = "knnObj",
#           definition = function(theObject, train.data.set, labels = c("Region"), categorical.var = c("SampleId"), cv = "loo"){
#             neighbours <- 1:10
#             errors <- matrix(nrow = length(neighbours), ncol = 2)
#             colnames(errors) <- c("Nr of neighbours", "Error")
#
#             train.data <- train.data.set[, !names(train.data.set) %in% labels]
#             train.data <- train.data[, !names(train.data) %in% categorical.var]
#             train.class <- train.data.set[, names(train.data.set) %in% labels]
#
#             for (K in neighbours)
#             {
#               error <- 0
#               switch(cv,
#
#                      "loo" = { # knn with LOOCV
#                        prediction <- knn.cv(train.data, train.class, k = K, prob = F)
#                        confusion.matrix <- table(prediction, train.class)
#                        error <- 100 - sum(diag(confusion.matrix)) / sum(confusion.matrix) * 100
#                      },
#
#                      "k-fold" = {
#                        n = 10
#                        for (population in levels(train.data$Region))
#                        {
#                          train.data.temp <- train.data[train.data$Region != population,]
#                          validation.data <- train.data[train.data$Region == population,]
#                          validation.class <- validation.data$Region
#                          prediction <- knn(train.data.temp[, -1], validation.data[, -1], train.data.temp$Region, k = K, prob = F)
#                          confusion.matrix <- table(prediction, validation.class)
#                          error <- error + 100 - sum(diag(confusion.matrix)) / sum(confusion.matrix) * 100 / length(levels(train.data$Region))
#                        }
#                      }
#               )
#               errors[K, "Nr of neighbours"] <- neighbours[K]
#               errors[K, "Error"] <- error
#             }
#             theObject@confusionMatrix.train <- confusion.matrix
#             theObject@k.estimation.table <- errors
#             theObject@k <- as.numeric(errors[errors[, "Error"] == (min(errors[, "Error"])), "Nr of neighbours"][1])
#             theObject <- setTrainError(theObject, errors[errors[,"Nr of neighbours"] == theObject@k, "Error"])
#             theObject@Precision.train <- diag(theObject@confusionMatrix.train) / rowSums(theObject@confusionMatrix.train)
#             theObject@Recall.train <- diag(theObject@confusionMatrix.train) / colSums(theObject@confusionMatrix.train)
#             theObject@Macro.eval.train <- macroEval(theObject, isTrain = T)
#             return(theObject)
#           }
# )
#
#
# setGeneric(name = "knn.prediction",
#            def = function(theObject, train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId")){
#              standardGeneric("knn.prediction")})
#
# setMethod(f = "knn.prediction",
#           signature = "knnObj",
#           definition = function(theObject, train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId")){
#             train.data <- train.data.set[, !names(train.data.set) %in% labels]
#             train.data <- train.data[, !names(train.data) %in% categorical.var]
#             train.class <- train.data.set[, names(train.data.set) %in% labels]
#
#             test.data <- test.data.set[, !names(test.data.set) %in% labels]
#             test.data <- train.data[, !names(test.data) %in% categorical.var]
#             test.class <- test.data.set[, names(test.data.set) %in% labels]
#
#             prediction <- knn(train.data, test.data, train.class, k = theObject@k, prob = F)
#
#             confusion.matrix <- table(prediction, test.class)
#             error <- 100 - sum(diag(confusion.matrix)) / sum(confusion.matrix) * 100
#
#             theObject@confusionMatrix.test <- confusion.matrix
#             theObject <- setTestError(theObject, error)
#             theObject <- setPrediction(theObject, prediction)
#             theObject@Precision.test <- diag(theObject@confusionMatrix.test) / rowSums(theObject@confusionMatrix.test)
#             theObject@Recall.test <- diag(theObject@confusionMatrix.test) / colSums(theObject@confusionMatrix.test)
#
#             .auc <- multiclass.roc(test.class, as.numeric(prediction))
#             theObject@auc <- .auc$auc[1]
#
#             theObject@Macro.eval.test <- macroEval(theObject, isTrain = F)
#
#             return(theObject) })
#
# # --- Naive Bayes --- #
#
# setGeneric(name = "nb.prediction",
#            def = function(theObject, train.data.set, test.data.set)
#            {
#              standardGeneric("nb.prediction")
#            })
#
# setMethod(f = "nb.prediction",
#           signature = "nb",
#           definition = function(theObject, train.data.set, test.data.set)
#           {
#             train.data <- subset(train.data.set, select = - Region)
#             train.class <- train.data.set$Region# "Region" is a target class
#
#             test.data <- subset(test.data.set, select = - Region)
#             test.class <- test.data.set$Region# "Region" is a target class
#
#             model <- naiveBayes(Region~ ., data = train.data.set)
#             # --- Training set --- #
#             prediction <- predict(model, train.data)
#             confusion.matrix <- table(train.class, prediction)
#             error <- 100 - sum(diag(confusion.matrix)) / sum(confusion.matrix) * 100
#
#             theObject@confusionMatrix.train <- confusion.matrix
#             theObject <- setTrainError(theObject, error)
#
#             # --- Test set --- #
#             prediction <- predict(model, newdata = test.data)
#             confusion.matrix <- table(test.class, prediction)
#             error <- 100 - sum(diag(confusion.matrix)) / sum(confusion.matrix) * 100
#
#             theObject@confusionMatrix.test <- confusion.matrix
#             theObject <- setTestError(theObject, error)
#             theObject <- setPrediction(theObject, prediction)
#
#             .auc <- multiclass.roc(test.class, as.numeric(prediction))
#             theObject@auc <- .auc$auc[1]
#
#             theObject@Precision.train <- diag(theObject@confusionMatrix.train) / rowSums(theObject@confusionMatrix.train)
#             theObject@Recall.train <- diag(theObject@confusionMatrix.train) / colSums(theObject@confusionMatrix.train)
#             theObject@Precision.test <- diag(theObject@confusionMatrix.test) / rowSums(theObject@confusionMatrix.test)
#             theObject@Recall.test <- diag(theObject@confusionMatrix.test) / colSums(theObject@confusionMatrix.test)
#
#             theObject@Macro.eval.test <- macroEval(theObject, isTrain = F)
#             theObject@Macro.eval.train <- macroEval(theObject, isTrain = T)
#
#             return(theObject)
#           }
# )
#
# setGeneric(name = "dt.prediction",
#            def = function(theObject, train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId")) {
#              standardGeneric("dt.prediction") })
#
# setMethod(f = "dt.prediction",
#           signature = "dtObj",
#           definition = function(theObject, train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId")) {
#             train.data <- train.data.set[, !names(train.data.set) %in% labels]
#             train.data <- train.data[, !names(train.data) %in% categorical.var]
#             train.class <- train.data.set[, names(train.data.set) %in% labels]
#
#             test.data <- test.data.set[, !names(test.data.set) %in% labels]
#             test.data <- train.data[, !names(test.data) %in% categorical.var]
#             test.class <- test.data.set[, names(test.data.set) %in% labels]
#
#             model = rpart(Region~ ., data = train.data.set, parms = list(split='gini'),
#                           control = rpart.control(cp = 0.001, xval = 10, maxdepth = 15))
#
#             # Prunining the tree
#             par(mfrow = c(1, 2))
#             plot(model$cptable[, 2], model$cptable[, 3], col = "purple", type = "o", xlab = "nsplits", ylab = "error")
#             lines(model$cptable[, 2], model$cptable[, 4], col = "blue", type = "o")
#             grid(20, 20, lwd = 1)
#             legend("topleft", c("R(T) training", "R(T) cv"), col = c("purple", "blue"), lty = 1, cex = 0.8, bty = "n", text.width = 6, seg.len = 0.5)
#
#             plotcp(model)
#
#             theObject@plot.cptable <- recordPlot()
#             theObject@model.cptable <- model$cptable
#
#             alfa <- model$cptable[which.min(model$cptable[, 4]), 1]
#             model.tree.prune <- prune(model, cp = alfa)
#
#             # theObject@model.tree <- model.tree.prune
#
#             par(mfrow = c(1,1))
#             fancyRpartPlot(model.tree.prune, cex = 0.7)
#             theObject@plot.tree <- recordPlot()
#
#             # --- Training data --- #
#             set.seed(8934)
#
#             pred <- predict(model.tree.prune, newdata = train.data)
#             prediction <- NULL
#             prediction[pred[, 1] >= 0.5] = "AFR"
#             prediction[pred[, 2] >= 0.5] = "AMR"
#             prediction[pred[, 3] >= 0.5] = "EAS"
#             prediction[pred[, 4] >= 0.5] = "EUR"
#             prediction[pred[, 5] >= 0.5] = "SAS"
#
#             theObject@confusionMatrix.train <- table(train.class, prediction)
#             theObject@train.error <- 100 - sum(diag(theObject@confusionMatrix.train)) / sum(theObject@confusionMatrix.train) * 100
#
#             # --- Test data --- #
#             pred <- predict(model.tree.prune, newdata = test.data)
#             prediction <- NULL
#             prediction[pred[, 1] >= 0.5] = "AFR"
#             prediction[pred[, 2] >= 0.5] = "AMR"
#             prediction[pred[, 3] >= 0.5] = "EAS"
#             prediction[pred[, 4] >= 0.5] = "EUR"
#             prediction[pred[, 5] >= 0.5] = "SAS"
#
#             theObject@confusionMatrix.test <- table(test.class, prediction)
#             theObject@test.error <- 100 - sum(diag(theObject@confusionMatrix.test)) / sum(theObject@confusionMatrix.test) * 100
#
#             theObject@Precision.train <- diag(theObject@confusionMatrix.train) / rowSums(theObject@confusionMatrix.train)
#             theObject@Recall.train <- diag(theObject@confusionMatrix.train) / colSums(theObject@confusionMatrix.train)
#             theObject@Precision.test <- diag(theObject@confusionMatrix.test) / rowSums(theObject@confusionMatrix.test)
#             theObject@Recall.test <- diag(theObject@confusionMatrix.test) / colSums(theObject@confusionMatrix.test)
#
#             theObject@Macro.eval.test <- macroEval(theObject, isTrain = F)
#             theObject@Macro.eval.train <- macroEval(theObject, isTrain = T)
#
#             return(theObject)
#           }
# )
#
# setGeneric(name = "rf.prediction",
#            def = function(theObject, train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId")) {
#              standardGeneric("rf.prediction") })
#
# setMethod(f = "rf.prediction",
#           signature = "rfObj",
#           definition = function(theObject, train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId")){
#             train.data <- train.data.set[, !names(train.data.set) %in% labels]
#             train.data <- train.data[, !names(train.data) %in% categorical.var]
#             train.class <- train.data.set[, names(train.data.set) %in% labels]
#
#             test.data <- test.data.set[, !names(test.data.set) %in% labels]
#             test.data <- train.data[, !names(test.data) %in% categorical.var]
#             test.class <- test.data.set[, names(test.data.set) %in% labels]
#
#             model <- randomForest(Region~ ., train.data.set, xtest = test.data, ytest = test.class)
#             print(model)
#
#             # --- Training data --- #
#             theObject@confusionMatrix.train <- as.table(model$confusion)
#             theObject@train.error <- 100 - sum(diag(theObject@confusionMatrix.train)) / sum(theObject@confusionMatrix.train) * 100
#
#             # --- Test data --- #
#             theObject@confusionMatrix.test <- as.table(model$test$confusion)
#             theObject@predicted <- as.numeric(model$test$predicted)
#             theObject@test.error <- 100 - sum(diag(theObject@confusionMatrix.test)) / sum(theObject@confusionMatrix.test) * 100
#
#             .auc <- multiclass.roc(test.class, as.numeric(model$test$predicted))
#             theObject@auc <- .auc$auc[1]
#
#             # --- Plot --- #
#             plot(model)
#             theObject@plot.model <- recordPlot()
#
#             plot(importance(model), lty = 2, pch = 16)
#             grid(20, 20, lty = 1, lwd = 1)
#             lines(importance(model))
#             text(importance(model), rownames(importance(model)), pos = 4, cex = 0.8)
#             theObject@plot.importance <- recordPlot()
#
#             # ROC curve
#             # the positive prediction on the test samples
#             #             par(mfrow= c(2, 2))
#             #             for (i in 1:4)
#             #             {
#             #               pred <- prediction(model$test$votes[, i], test.class)
#             #               plot(performance(pred, 'tpr', 'fpr'), main = "ROC curve")
#             #               abline(0, 1, col="red")
#             #             }
#             #             theObject@plot.roc.curve <- recordPlot()
#
#             theObject@Precision.train <- diag(theObject@confusionMatrix.train) / rowSums(theObject@confusionMatrix.train)
#             recall <- diag(theObject@confusionMatrix.train) / colSums(theObject@confusionMatrix.train)
#             theObject@Recall.train <- recall[-length(recall)]
#             theObject@Precision.test <- diag(theObject@confusionMatrix.test) / rowSums(theObject@confusionMatrix.test)
#             recall <- diag(theObject@confusionMatrix.test) / colSums(theObject@confusionMatrix.test)
#             theObject@Recall.test <- recall[-length(recall)]
#
#             theObject@Macro.eval.test <- macroEval(theObject, isTrain = F)
#             theObject@Macro.eval.train <- macroEval(theObject, isTrain = T)
#
#             return(theObject)
#           }
# )
#
# setGeneric(name = "lda.qda.prediction",
#            def = function(theObject, train.data.set, test.data.set, discriminant.analysis)
#            {
#              standardGeneric("lda.qda.prediction")
#            })
#
# setMethod(f = "lda.qda.prediction",
#           signature = "lda.qda",
#           definition = function(theObject, train.data.set, test.data.set, discriminant.analysis)
#           {
#             train.data <- subset(train.data.set, select = - Region)
#             train.class <- train.data.set$Region# "Region" is a target class
#
#             test.data <- subset(test.data.set, select = - Region)
#             test.class <- test.data.set$Region# "Region" is a target class
#
#             switch(discriminant.analysis,
#                    LDA = {model <- lda(train.data, train.class)},
#                    QDA = {model <- qda(train.data, train.class)},
#                    stop("Enter one of 'LDA', 'QDA'")
#             )
#
#             model.cv <- update(model, CV = T)
#
#             # --- Training data --- #
#             theObject@confusionMatrix.train <- table(train.class, model.cv$class)
#             theObject@train.error <- 100 - sum(diag(theObject@confusionMatrix.train)) / sum(theObject@confusionMatrix.train) * 100
#
#             # --- Test data --- #
#             prediction <- predict(model, test.data)
#             theObject@confusionMatrix.test <- table(test.class, prediction$class)
#             theObject@test.error <- 100 - sum(diag(theObject@confusionMatrix.test)) / sum(theObject@confusionMatrix.test) * 100
#
#             theObject <- setPrediction(theObject, prediction$class)
#
#             if (discriminant.analysis == "LDA")
#               theObject@x <- prediction$x
#
#             theObject@Precision.train <- diag(theObject@confusionMatrix.train) / rowSums(theObject@confusionMatrix.train)
#             theObject@Recall.train <- diag(theObject@confusionMatrix.train) / colSums(theObject@confusionMatrix.train)
#             theObject@Precision.test <- diag(theObject@confusionMatrix.test) / rowSums(theObject@confusionMatrix.test)
#             theObject@Recall.test <- diag(theObject@confusionMatrix.test) / colSums(theObject@confusionMatrix.test)
#
#             .auc <- multiclass.roc(test.class, as.numeric(prediction$class))
#             theObject@auc <- .auc$auc[1]
#             theObject@Macro.eval.test <- macroEval(theObject, isTrain = F)
#             theObject@Macro.eval.train <- macroEval(theObject, isTrain = T)
#
#             return(theObject)
#           }
# )
#
# # --- Plot the prediction --- #
#
# plot.prediction <- function(predicted, data.set, var.x = "PC.1", var.y = "PC.2", legend = "", title.name = "")
# {
#   colors <- c("red", "green", "blue", "yellow")
#   mycolors <- colors[as.numeric(predicted)]
#
#   save.par <- par(mfrow= c(1, 1))
#   pred.plot <- plot(data.set[, c(var.x, var.y)] * 2, xlab = var.x, ylab = var.y, type = "n")
#   grid(20, 20, lty = 1, lwd = 1)
#   points(data.set[, c(var.x, var.y)], col = mycolors, pch = "*", cex = 2)
#
#   for (i in 1:4)
#   {
#     m1 <- mean(subset(data.set[, var.x], predicted == i))
#     m2 <- mean(subset(data.set[, var.y], predicted == i))
#     points(m1, m2, pch = 16, cex = 1,col = i)
#   }
#
#   legend("bottom", legend,  col = colors, pch = "*", lty = 2, cex = 0.75, horiz = T, bty = "n")
#   title(title.name)
#
#   return(recordPlot())
# }
#
