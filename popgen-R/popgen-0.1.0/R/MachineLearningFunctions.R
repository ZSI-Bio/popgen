
#' Hierarchical clustering
#'
#' Hierarchical clustering
#' @export
HClust <- function(data, k){
  HClustF(hclustObj(), data, k)
}

#' Kmeams clustering
#'
#' Kmeams clustering
#' @export
kmeansClust <- function(data, k){
  h.cl <- HClustF(hclustObj(), data, k)
  kmeansClustF(kmeansObj(), data, k, h.cl@cdg)
}

#' E-M clustering
#'
#' E-M clustering
#' @export
emcClust <- function(data, k){
  emcClustF(emcObj(), data, k)
}

#' # ----------------------
#' # Classification
#'
#' #' knn k estimation
#' #'
#' #' knn k estimation
#' #' @export
#' knnEstimate <- function(data){
#'   knn.k.estimation(knnObj(), data, cv = "loo")
#' }
#'
#' #' knn classifier
#' #'
#' #' knn classifier
#' #' @export
#' knnPredict <- function(train.data, test.data, k){
#'   knn.prediction(knnObj(), train.data, test.data)
#' }

