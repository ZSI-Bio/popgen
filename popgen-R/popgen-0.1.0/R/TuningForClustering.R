# Popgen. Parameters tuning for clustering

# -------------------
#' Tuning MAF for clusterig
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld linkage-disequilibrium threshold
#' @param n.pc number of principal components (PCA)
#' @param n.clusters number of clusters
#' @export
#'
tuningMAF <- function(genofile, pop.df, inf.maf.set, sup.maf.set, ld = 0.2, missing.rate = 0.0, n.pc = 4, n.clusters = 5){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  cross.freq <- matrix(0, nrow = length(freq.names), ncol = 3)
  colnames(cross.freq) <- c("Hierarchical", "K-means", "E-M")
  rownames(cross.freq) <- freq.names

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  best.model.ari <- 0.0
  best.model <- NULL
  best.min.maf <- inf.maf.set[1]
  best.max.maf <- sup.maf.set[1]

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
    snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)

    genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)

    pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt = n.pc, num.thread=4, missing.rate = missing.rate)
    pca.df <- getPCAdf(pca, n.pc, pop.df)

    obj.hclust <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
    obj.kmeans <- kmeansClustF( kmeansObj(), data = pca.df, k = n.clusters, obj.hclust@cdg)
    obj.emc <- emcClustF( emcObj(), data = pca.df, k = n.clusters)

    cross.freq[toString(freq.names[i]), "Hierarchical"] <- obj.hclust@ari
    cross.freq[toString(freq.names[i]), "K-means"] <- obj.kmeans@ari
    cross.freq[toString(freq.names[i]), "E-M"] <- obj.emc@ari

    if(best.model.ari < max(cross.freq[i,])){
      best.model.ari <- max(cross.freq[i,])
      best.min.maf <- min.maf
      best.max.maf <- max.maf
      best.model <- ifelse(colnames(cross.freq[max(cross.freq[i,])]) == "Hierarchical", obj.hclust,
                           ifelse(colnames(cross.freq[max(cross.freq[i,])]) == "K-means", obj.kmeans, obj.emc))
    }
    snpgdsClose(genofile.filtered)
  }

  list(crossTbl = cross.freq, bestModel = best.model, minMAF = best.min.maf, maxMAF = best.max.maf)
}

# -------------------
#' Tuning LD for clustering
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param min.maf infimum maf for filtering SNPs
#' @param max.maf supremum maf for filtering SNPs
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc number of principal components (PCA)
#' @param n.clusters number of clusters
#' @export
#'
tuningLD <- function(genofile, pop.df, min.maf, max.maf, ld.set = ld.set, missing.rate = 0.0, n.pc = 4, n.clusters = 5){
  cross.ld <- matrix(0, nrow = length(ld.set), ncol = 3)
  colnames(cross.ld) <- c("Hierarchical", "K-means", "E-M")
  rownames(cross.ld) <- ld.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  best.model.ari <- 0.0
  best.model <- NULL
  best.ld <- ld.set[1]

  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))

  for (ld in ld.set){
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)

    pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=4, missing.rate = missing.rate)
    pca.df <- getPCAdf(pca, n.pc, pop.df)

    obj.hclust <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
    obj.kmeans <- kmeansClustF( kmeansObj(), data = pca.df, k = n.clusters, obj.hclust@cdg)
    obj.emc <- emcClustF( emcObj(), data = pca.df, k = n.clusters)

    cross.ld[toString(ld), "Hierarchical"] <- obj.hclust@ari
    cross.ld[toString(ld), "K-means"] <- obj.kmeans@ari
    cross.ld[toString(ld), "E-M"] <- obj.emc@ari

    if(best.model.ari < max(cross.ld[toString(ld),])){
      best.model.ari <- max(cross.ld[toString(ld),])
      best.ld <- ld
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTbl = cross.ld, bestLD = best.ld)
}

# -------------------
#' Tuning nPC for clustering
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param min.maf infimum maf for filtering SNPs
#' @param max.maf supremum maf for filtering SNPs
#' @param ld linkage-disequilibrium threshold
#' @param n.pc set of numbers of principal components to tune
#' @param n.clusters number of clusters
#' @export
#'
tuningNumPC <- function(genofile, pop.df, min.maf, max.maf, ld, n.pc.set, missing.rate = 0.0, n.clusters = 5){
  cross.npc <- matrix(0, nrow = length(n.pc.set), ncol = 3)
  colnames(cross.npc) <- c("Hierarchical", "K-means", "E-M")
  rownames(cross.npc) <- n.pc.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  best.model.ari <- 0.0
  best.model <- NULL
  best.n.pc <- n.pc.set[1]

  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
  snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
  snp.id.set <- unlist(snpset)

  for (n.pc in n.pc.set){
    pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=8, missing.rate = missing.rate)
    pca.df <- getPCAdf(pca, n.pc, pop.df)

    obj.hclust <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
    obj.kmeans <- kmeansClustF( kmeansObj(), data = pca.df, k = n.clusters, obj.hclust@cdg)
    obj.emc <- emcClustF( emcObj(), data = pca.df, k = n.clusters)

    cross.npc[toString(n.pc), "Hierarchical"] <- obj.hclust@ari
    cross.npc[toString(n.pc), "K-means"] <- obj.kmeans@ari
    cross.npc[toString(n.pc), "E-M"] <- obj.emc@ari

    if(best.model.ari < max(cross.npc[toString(n.pc),])){
      best.model.ari <- max(cross.npc[toString(n.pc),])
      best.n.pc <- n.pc
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTbl = cross.npc, nPC = best.n.pc)
}

# -------------------
#' Tuning MAF vs  LD for clustering
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc number of principal components (PCA)
#' @param n.clusters number of clusters
#' @param method clustering method
#' @details Three option for "method" = {'hierarchical', 'kmeans', 'em'}
#' @export
#'
tuningLdMaf <- function(genofile, pop.df, inf.maf.set, sup.maf.set, ld.set = ld.set, missing.rate = 0.0, n.pc = 4, n.clusters = 5, method = "hierarchical"){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  cross.tbl <- matrix(0, nrow = length(freq.names), ncol = length(ld.set))
  rownames(cross.tbl) <- freq.names
  colnames(cross.tbl) <- ld.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  best.model.ari <- 0.0
  best.ld <- ld.set[1]
  best.min.maf <- inf.maf.set[1]
  best.max.maf <- sup.maf.set[1]

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
    snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
    genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))

    for (ld in ld.set){
      snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
      snp.id.set <- unlist(snpset)

      pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=4, missing.rate = missing.rate)
      pca.df <- getPCAdf(pca, n.pc, pop.df)

      if (method == "hierarchical")
        obj <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
      else if (method == "kmeans"){
        obj.hclust <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
        obj <- kmeansClustF( kmeansObj(), data = pca.df, k = n.clusters, obj.hclust@cdg)
      }
      else if (method == "em")
        obj <- emcClustF( emcObj(), data = pca.df, k = n.clusters)

      cross.tbl[toString(freq.names[i]), toString(ld)] <- obj@ari

      if(best.model.ari < obj@ari){
        best.model.ari <- obj@ari
        best.ld <- ld
        best.min.maf <- inf.maf.set[i]
        best.max.maf <- sup.maf.set[i]
      }
    }
    snpgdsClose(genofile.filtered)
  }
  list(crossTbl = cross.tbl, ld = best.ld, min.maf = best.min.maf, max.maf = best.max.maf)
}

# -------------------
#' Tuning MAF vs  nPC for clustering
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld linkage-disequilibrium threshold
#' @param n.pc set of numbers of principal components to tune
#' @param n.clusters number of clusters
#' @param method clustering method
#' @details Three option for "method" = {'hierarchical', 'kmeans', 'em'}
#' @export
#'
tuningMAFPC <- function(genofile, pop.df, inf.maf.set, sup.maf.set, n.pc.set,  ld = 0.2, missing.rate = 0.0, n.clusters = 5, method = "hierarchical"){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  cross.tbl <- matrix(nrow = length(freq.names), ncol = length(n.pc.set))
  rownames(cross.tbl) <- freq.names
  colnames(cross.tbl) <- n.pc.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  best.model.ari <- 0.0
  best.npc <- n.pc.set[1]
  best.min.maf <- inf.maf.set[1]
  best.max.maf <- sup.maf.set[1]

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
    snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
    genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)

    for (n.pc in n.pc.set){
      pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=4, missing.rate = missing.rate)
      pca.df <- getPCAdf(pca, n.pc, pop.df)

      if (method == "hierarchical")
        obj <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
      else if (method == "kmeans"){
        obj.hclust <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
        obj <- kmeansClustF( kmeansObj(), data = pca.df, k = n.clusters, obj.hclust@cdg)
      }
      else if (method == "em")
        obj <- emcClustF( emcObj(), data = pca.df, k = n.clusters)

      cross.tbl[toString(freq.names[i]), toString(n.pc)] <- obj@ari

      if(best.model.ari < obj@ari){
        best.model.ari <- obj@ari
        best.npc <- n.pc
        best.min.maf <- inf.maf.set[i]
        best.max.maf <- sup.maf.set[i]
      }
    }
    snpgdsClose(genofile.filtered)
  }
  list(crossTbl = cross.tbl, npc = best.npc, min.maf = best.min.maf, max.maf = best.max.maf)
}

# -------------------
#' Tuning LD vs  nPC for clustering
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param min.maf infimum maf for filtering SNPs
#' @param max.maf supremum maf for filtering SNPs
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc set of numbers of principal components to tune
#' @param n.clusters number of clusters
#' @param method clustering method
#' @details Three option for "method" = {'hierarchical', 'kmeans', 'em'}
#' @export
#'
tuningLDnPC <- function(genofile, pop.df, min.maf, max.maf, ld.set, n.pc.set, missing.rate = 0.0, n.clusters = 5, method = "hierarchical"){
  cross.tbl <- matrix(nrow = length(ld.set), ncol = length(n.pc.set))
  rownames(cross.tbl) <- ld.set
  colnames(cross.tbl) <- n.pc.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  best.model.ari <- 0.0
  best.npc <- n.pc.set[1]
  best.ld <- ld.set[1]

  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))

  for (ld in ld.set){
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)

    for (n.pc in n.pc.set){
      pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=4, missing.rate = missing.rate)
      pca.df <- getPCAdf(pca, n.pc, pop.df)

      if (method == "hierarchical")
        obj <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
      else if (method == "kmeans"){
        obj.hclust <- HClustF( hclustObj(), data = pca.df, k = n.clusters)
        obj <- kmeansClustF( kmeansObj(), data = pca.df, k = n.clusters, obj.hclust@cdg)
      }
      else if (method == "em")
        obj <- emcClustF( emcObj(), data = pca.df, k = n.clusters)

      cross.tbl[toString(ld), toString(n.pc)] <- obj@ari

      if(best.model.ari < obj@ari){
        best.model.ari <- obj@ari
        best.npc <- n.pc
        best.ld <- ld
      }
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTbl = cross.tbl, npc = best.npc, ld = best.ld, ari = best.model.ari)
}

# -------------------
#' Tuning MAF vs LD vs  nPC for clustering
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc set of numbers of principal components to tune
#' @param n.clusters number of clusters
#' @param method clustering method
#' @details Three option for "method" = {'hierarchical', 'kmeans', 'em'}
#' @export
#'
tuningMAFLDnPC <- function(genofile, pop.df, inf.maf.set, sup.maf.set, ld.set, n.pc.set, missing.rate = 0.0, n.clusters = 5, method = "hierarchical"){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  best.min.maf <- inf.maf.set[1];  best.max.maf <- sup.maf.set[1]
  best.ld <- ld.set[1]
  best.npc <- n.pc.set[1]
  max.ari <- 0

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    tun.res <- tuningLDnPC(genofile, pop.df, min.maf, max.maf, ld.set, n.pc.set, missing.rate, n.clusters, method)

    if(max.ari < tun.res$ari){
      max.ari <- tun.res$ari
      best.n.pc <- tun.res$nPC
      best.ld <- tun.res$ld
      best.min.maf <- inf.maf.set[i]
      best.max.maf <- sup.maf.set[i]
    }
  }
  list(min.maf = best.min.maf, max.maf = best.max.maf, ld = best.ld, nPC = best.npc)
}
