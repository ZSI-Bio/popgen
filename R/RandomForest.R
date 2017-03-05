#' Random Forest Classifier
#' @export
rfPrediction <-  function(train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId"), ntree = 500){
  theObject <- rfObj()

  train.data <- splitData(train.data.set, labels, categorical.var)
  train.class <- train.data.set[, names(train.data.set) %in% labels]

  test.data <- splitData(test.data.set, labels, categorical.var)
  test.class <- test.data.set[, names(test.data.set) %in% labels]

  model <- randomForest(x = train.data, y = train.class, xtest = test.data, ytest = test.class, ntree = ntree)
  print(model)

  theObject@model <- model
  theObject@metrics.train <- setClassMetrics(theObject@metrics.train, model$predicted, train.class)
  theObject@metrics.test <- setClassMetrics(theObject@metrics.test, model$test$predicted, test.class)

  # Plot
  plot(model)
  theObject@plot.model <- recordPlot()

  plot(importance(model), lty = 2, pch = 16)
  grid(20, 20, lty = 1, lwd = 1)
  lines(importance(model))
  text(importance(model), rownames(importance(model)), pos = 4, cex = 0.8)
  theObject@plot.importance <- recordPlot()

  # ROC curve # only for binary classification
  # the positive prediction on the test samples
  #             par(mfrow= c(2, 2))
  #             for (i in 1:4)
  #             {
  #               pred <- prediction(model$test$votes[, i], test.class)
  #               plot(performance(pred, 'tpr', 'fpr'), main = "ROC curve")
  #               abline(0, 1, col="red")
  #             }
  #             theObject@plot.roc.curve <- recordPlot()

  return ( theObject )
}
