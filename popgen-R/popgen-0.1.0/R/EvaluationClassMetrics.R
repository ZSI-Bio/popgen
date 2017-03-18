splitData <-  function(data, labels, categorical.var){
  t.data <- data[, !names(data) %in% labels]
  t.data <- t.data[, !names(t.data) %in% categorical.var]
  return ( t.data )}


setGeneric(name = "macroEval",
           def = function(obj) {
             standardGeneric("macroEval")})

setMethod(f = "macroEval",
          signature = "Metrics",
          definition = function(obj){
            eval.metr <- c("Precision", "Recall", "F1-Score", "Accuracy", "Kappa")
            eval.tbl <- matrix(nrow = 1, ncol = length(eval.metr))
            colnames(eval.tbl) <- eval.metr
            rownames(eval.tbl) <- class(obj)[1]

            precision <- obj@precision
            recall <- obj@recall
            conf.matr <- obj@confusion.matrix
            error <- obj@error

            eval.tbl[1, "Precision"] <-  round(mean(precision), 2)
            eval.tbl[1, "Recall"] <-  round(mean(recall), 2)
            eval.tbl[1, "F1-Score"] <-  round(mean(2 * precision * recall / (precision + recall)), 2)
            eval.tbl[1, "Accuracy"] <-  round(100 - error, 2)

            num.instaces.total <- sum(conf.matr)
            num.istances.per.class <- apply(conf.matr, 1, sum)
            num.pred.per.class <- apply(conf.matr, 2, sum)
            p <- num.istances.per.class / num.instaces.total # distribution of instances over the actual classes
            q <- num.pred.per.class / num.instaces.total
            exp.accuracy <- sum(p * q) * 100
            eval.tbl[1, "Kappa"] <- round((eval.tbl[1, "Accuracy"] - exp.accuracy)  / (100 - exp.accuracy), 2)

            return(eval.tbl)})


setClassMetrics <- function(obj, prediction, labels){
  obj@prediction <- as.character(prediction)
  obj@confusion.matrix <- table(labels, prediction)
  obj@error <- round((1 - sum(diag(obj@confusion.matrix)) / sum(obj@confusion.matrix)) * 100, 2)
  obj@precision <- round(diag(obj@confusion.matrix) / rowSums(obj@confusion.matrix) * 100, 2)
  obj@recall <- round(diag(obj@confusion.matrix) / colSums(obj@confusion.matrix) * 100, 2)
  obj@macro.eval <- macroEval(obj)
  .auc <- multiclass.roc(as.numeric(as.factor(labels)), as.numeric(as.factor(prediction)))
  obj@auc <- .auc$auc[1]

  return ( obj )
}
