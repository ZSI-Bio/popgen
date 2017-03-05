setGeneric("purity", function(obj) { standardGeneric("purity") })

setMethod(f = "purity",
          signature = "ClusteringMetrics",
          definition = function(obj){
            conf.matr <- obj@conf.matr
            corr <- 0
            for(i in 1:ncol(conf.matr))
              corr <- corr + max(conf.matr[, i])
            pur <- round(corr / sum(conf.matr) * 100, 2)
            return (pur)
          })

getClusterMetrics <- function(object, features, target, prediction){
  conf.matr <- table(target, prediction)
  object@conf.matr <- conf.matr
  object@prediction <- prediction
  object@purity <- purity(object)
  object@ari <- round(randIndex(prediction, target, correct = FALSE) * 100, 2)
  object@ch <- round(calinhara(features, as.integer(prediction)), 2) # CH index - from fpc
  object@dunn <- round(intCriteria(as.matrix(features), as.integer(prediction), c("Dunn"))$dunn, 2)
  return (object)
}

setGeneric("HClustF", function(object, data, k) { standardGeneric("HClustF") })

setMethod(f = "HClustF",
          signature = "hclustObj",
          definition = function(object, data, k)
          {
            features <- select(data, -c(SampleId, Region))
            target <- data$Region

            set.seed(1234)
            distM <- dist(features, method = "euclidean")
            hc <- hclust(distM, method = "ward.D")
            hc.groups <- cutree(hc, k)

            prediction <- as.factor(hc.groups)
            object <- getClusterMetrics(object, features, target, prediction)
            object@cdg <- aggregate(as.data.frame(features), list(hc.groups), mean)[, -1]
            object@k <- k

            return(object)
          }
)

setGeneric(name = "kmeansClustF",
           def = function(object, data, k, cdg)
           {
             standardGeneric("kmeansClustF")
           })


setMethod(f = "kmeansClustF",
          signature = "kmeansObj",
          definition = function(object, data, k, cdg)
          {
            features <- select(data, -c(SampleId, Region))
            target <- data$Region

            set.seed(1234)
            k.means <- kmeans(features, cdg)
            prediction <- as.factor(k.means$cluster)
            conf.matr <- table(target, prediction)

            object <- getClusterMetrics(object, features, target, prediction)
            object@k <- k

            return(object)
          }
)

setGeneric(name = "emcClustF",
           def = function(object, data, k)
           {
             standardGeneric("emcClustF")
           })


setMethod(f = "emcClustF",
          signature = "emcObj",
          definition = function(object, data, k)
          {
            features <- select(data, -c(SampleId, Region))
            target <- data$Region

            set.seed(1234)
            emc <- Mclust(features, k)
            prediction <- as.factor(emc$classification)
            conf.matr <- table(target, prediction)

            object@emc <- emc
            object <- getClusterMetrics(object, features, target, prediction)
            object@k <- k

            return(object)
          }
)
