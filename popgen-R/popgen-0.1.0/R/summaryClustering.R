#' Summary for clustering evaluation metrics
#' @export
summaryClustering <- function(obj.hclust, obj.kmeans, obj.emc){
  cross.eval.cluster <- matrix(nrow = 3, ncol = 5)
  colnames(cross.eval.cluster) <- c("Purity", "RI", "ARI", "CH", "Dunn")
  rownames(cross.eval.cluster) <- c("Hierarchical", "K-means", "E-M")

  cross.eval.cluster["Hierarchical", "RI"] <-obj.hclust@ri
  cross.eval.cluster["K-means", "RI"] <-obj.kmeans@ri
  cross.eval.cluster["E-M", "RI"] <-obj.emc@ri

  cross.eval.cluster["Hierarchical", "ARI"] <-obj.hclust@ari
  cross.eval.cluster["K-means", "ARI"] <-obj.kmeans@ari
  cross.eval.cluster["E-M", "ARI"] <-obj.emc@ari

  cross.eval.cluster["Hierarchical", "Purity"] <-obj.hclust@purity
  cross.eval.cluster["K-means", "Purity"] <-obj.kmeans@purity
  cross.eval.cluster["E-M", "Purity"] <-obj.emc@purity

  cross.eval.cluster["Hierarchical", "CH"] <-obj.hclust@ch
  cross.eval.cluster["K-means", "CH"] <-obj.kmeans@ch
  cross.eval.cluster["E-M", "CH"] <-obj.emc@ch

  cross.eval.cluster["Hierarchical", "Dunn"] <-obj.hclust@dunn
  cross.eval.cluster["K-means", "Dunn"] <-obj.kmeans@dunn
  cross.eval.cluster["E-M", "Dunn"] <-obj.emc@dunn

  return (cross.eval.cluster)
}
