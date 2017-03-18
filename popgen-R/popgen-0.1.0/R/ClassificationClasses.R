Metrics <- setClass(Class = "Metrics",
                    slots = c(confusion.matrix = "table",
                              error = "numeric",
                              precision = "numeric",
                              recall = "numeric",
                              macro.eval = "matrix",
                              auc = "numeric",
                              prediction = "character"))

#' @export
ClassificationMetrics <- setClass(Class = "ClassificationMetrics",
                                 slots = c(metrics.train = "Metrics",
                                           metrics.test = "Metrics"))

#' @export
knnObj <- setClass(Class = "knnObj",
                  slots = c(k.estimation.table = "matrix",
                            k = "numeric"),
                  prototype = list(k = 1),
                  contains = "ClassificationMetrics")

#' @export
svmObj <- setClass(Class = "svmObj",
                  slots = c(
                    model = "svm",
                    cross.errors = "matrix",
                    nSV = "matrix",
                    cost = "numeric",
                    sigma = "numeric"),
                  prototype = list(cost = 100,
                                   sigma = 0.1),
                  contains = "ClassificationMetrics")

#' @export
nb <- setClass(Class = "nb", # Naive Bayes
               contains = "ClassificationMetrics")

#' @export
dtObj <- setClass(Class = "dtObj", # Decision trees
               slots = c(
                 model.tree = "rpart",
                 model.cptable = "matrix",
                 plot.cptable = "recordedplot",
                 plot.tree = "recordedplot"),
               contains = "ClassificationMetrics")

#' @export
lda.qda <- setClass(Class = "lda.qda",
                    slot = c( x = "matrix"),
                    contains = "ClassificationMetrics")

#' @export
rfObj <- setClass(Class = "rfObj", # Random Forest
               slots = c(
                 model = "randomForest",
                 plot.model = "recordedplot",
                 plot.importance = "recordedplot",
                 plot.roc.curve = "recordedplot" ),
               contains = "ClassificationMetrics")
