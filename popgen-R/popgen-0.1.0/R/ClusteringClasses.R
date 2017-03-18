#' @export
setClass(Class = "ClusteringMetrics",
                             slot = c(
                               k = "numeric",
                               conf.matr = "table",
                               prediction = "numeric",
                               purity = "numeric",
                               ri = "numeric",
                               ari = "numeric",
                               ch = "numeric",
                               dunn = "numeric"))

#' @export
kmeansObj <- setClass(Class = "kmeansObj",
                      contains = "ClusteringMetrics")

#' @export
hclustObj <- setClass(Class = "hclustObj",
                      slot = c(
                        cdg = "list"),
                      contains = "ClusteringMetrics")
#' @export
emcObj <- setClass(Class = "emcObj",
                   slot = c(emc = "Mclust"),
                   contains = "ClusteringMetrics")
