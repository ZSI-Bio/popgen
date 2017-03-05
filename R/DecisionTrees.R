#' Decision Trees Classifier
#' @export
dtPrediction <- function(train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId"), cp = 0.001) {
  theObject <- dtObj()

  train.data <- splitData(train.data.set, labels, categorical.var)
  train.class <- train.data.set[, names(train.data.set) %in% labels]

  test.data <- splitData(test.data.set, labels, categorical.var)
  test.class <- test.data.set[, names(test.data.set) %in% labels]

  if(categorical.var == ""){
    data <- train.data.set
  }  else data <- train.data.set[, !names(train.data.set) %in% categorical.var]

  model = rpart(Region ~ ., data = data, parms = list(split = 'gini'), control = rpart.control(cp = cp, xval = 10, maxdepth = 10))
  theObject@model.tree <- model
  # Prunining the tree
  par(mfrow = c(1, 2))
  plot(model$cptable[, 2], model$cptable[, 3], col = "purple", type = "o", xlab = "nsplits", ylab = "error")
  lines(model$cptable[, 2], model$cptable[, 4], col = "blue", type = "o")
  grid(20, 20, lwd = 1)
  legend("topleft", c("R(T) training", "R(T) cv"), col = c("purple", "blue"), lty = 1, cex = 0.8, bty = "n", text.width = 6, seg.len = 0.5)

  plotcp(model)

  theObject@plot.cptable <- recordPlot()
  theObject@model.cptable <- model$cptable

  alfa <- model$cptable[which.min(model$cptable[, 4]), 1]
  model.tree.prune <- prune(model, cp = alfa)

  par(mfrow = c(1,1))
  fancyRpartPlot(model.tree.prune, cex = 0.7)
  theObject@plot.tree <- recordPlot()

  # Training data
  set.seed(8934)

  pred <- predict(model.tree.prune, newdata = train.data)
  prediction <- colnames(pred)[max.col(pred, ties.method = "first")]
  theObject@metrics.train <- setClassMetrics(theObject@metrics.train, prediction, train.class)

  # Test data
  pred <- predict(model.tree.prune, newdata = test.data)
  prediction <- colnames(pred)[max.col(pred, ties.method = "first")]
  theObject@metrics.test <- setClassMetrics(theObject@metrics.test, prediction, test.class)

  return ( theObject )
}
