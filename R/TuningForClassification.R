# Popgen. Parameters tuning for classifcation

fillCrossTbl <- function(cross.tbl, where, metrics.svm.l, metrics.svm.q, metrics.svm.r, metrics.dt, metrics.rf){
  cross.tbl[toString(where), "SVM linear"]     <- metrics.svm.l@error
  cross.tbl[toString(where), "SVM quadratic"]  <- metrics.svm.q@error
  cross.tbl[toString(where), "SVM radial"]     <- metrics.svm.r@error
  cross.tbl[toString(where), "Decision Trees"] <- metrics.dt@error
  cross.tbl[toString(where), "Random Forest"]  <- metrics.rf@error
  return ( cross.tbl )
}

getTrainTestErr <- function(df, split.coef, labels, categorical.var, cross.tbl.train, cross.tbl.test, where, cost = 10, sigma = 1, cp = 0.001, ntree = 500){
  n.samples <- nrow(df)
  train.data <- df[1:round(split.coef * n.samples), ]
  test.data <- df[-(1:round(split.coef * n.samples)), ]

  obj.svm.l <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "linear", cost = cost, sigma = sigma)
  obj.svm.q <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "quadratic", cost = cost, sigma = sigma)
  obj.svm.r <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "RBF", cost = cost, sigma = sigma)
  obj.dt <- dtPrediction(train.data, test.data, labels, categorical.var, cp = cp)
  obj.rf <- rfPrediction(train.data, test.data, labels, categorical.var, ntree)

  tbl.train <- fillCrossTbl(cross.tbl.train, where, obj.svm.l@metrics.train, obj.svm.q@metrics.train, obj.svm.r@metrics.train, obj.dt@metrics.train, obj.rf@metrics.train)
  tbl.test <- fillCrossTbl(cross.tbl.test, where, obj.svm.l@metrics.test, obj.svm.q@metrics.test, obj.svm.r@metrics.test, obj.dt@metrics.test, obj.rf@metrics.test)

  return ( list(train.tbl = tbl.train, test.tbl = tbl.test) )
}

# -------------------
#' Tuning MAF for classification
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @export
#'
tuningMAFclass <- function(genofile, pop.df, inf.maf.set, sup.maf.set, ld = 0.2, missing.rate = 0.0, n.pc = 5,
                           labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                           cost = 10, sigma = 1, cp = 0.001, ntree = 500){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  cross.freq.test <- matrix(0, nrow = length(freq.names), ncol = 5)
  colnames(cross.freq.test) <- c("SVM linear", "SVM quadratic", "SVM radial", "Decision Trees", "Random Forest")
  rownames(cross.freq.test) <- freq.names

  cross.freq.train <- matrix(0, nrow = length(freq.names), ncol = 5)
  colnames(cross.freq.train) <- c("SVM linear", "SVM quadratic", "SVM radial", "Decision Trees", "Random Forest")
  rownames(cross.freq.train) <- freq.names

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  min.train.err <- 0.0; min.test.err <- 0.0
  best.min.maf <- inf.maf.set[1]
  best.max.maf <- sup.maf.set[1]

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]
    pca.df <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld, missing.rate, n.pc, F, F)

    train.test.tbl <- getTrainTestErr(pca.df, split.coef, labels, categorical.var, cross.freq.train, cross.freq.test, freq.names[i], cost, sigma, cp, ntree)
    cross.freq.train <- train.test.tbl$train.tbl
    cross.freq.test <- train.test.tbl$test.tbl

    train.err <- min(cross.freq.train[i,])
    test.err <- min(cross.freq.test[i,])

    if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  && (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.min.maf <- min.maf; best.max.maf <- max.maf
    }
  }

  list(crossTrainTbl = cross.freq.train, crossTestTbl = cross.freq.test, minMAF = best.min.maf, maxMAF = best.max.maf)
}

# -------------------
#' Tuning LD for classification
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @export
#'
tuningLDclass <- function(genofile,  pop.df, min.maf, max.maf, ld.set, missing.rate = 0.0, n.pc = 5,
                     labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                     cost = 10, sigma = 1, cp = 0.001, ntree = 500){
  cross.tbl.test <- matrix(0, nrow = length(ld.set), ncol = 5)
  colnames(cross.tbl.test) <- c("SVM linear", "SVM quadratic", "SVM radial", "Decision Trees", "Random Forest")
  rownames(cross.tbl.test) <- ld.set

  cross.tbl.train <- matrix(0, nrow = length(ld.set), ncol = 5)
  colnames(cross.tbl.train) <- c("SVM linear", "SVM quadratic", "SVM radial", "Decision Trees", "Random Forest")
  rownames(cross.tbl.train) <- ld.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  min.train.err <- 0.0; min.test.err <- 0.0
  best.ld <- ld.set[1]

  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))

  for (ld in ld.set){
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)
    pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread = 4, missing.rate = missing.rate)
    pca.df <- getPCAdf(pca, n.pc, pop.df)

    train.test.tbl <- getTrainTestErr(pca.df, split.coef, labels, categorical.var, cross.tbl.train, cross.tbl.test, ld, cost, sigma, cp, ntree)
    cross.tbl.train <- train.test.tbl$train.tbl
    cross.tbl.test <- train.test.tbl$test.tbl

    train.err <- min(cross.tbl.train[toString(ld),])
    test.err <- min(cross.tbl.test[toString(ld),])

    if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  && (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.ld <- ld
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, bestLD = best.ld)
}

# -------------------
#' Tuning nPC for classification
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @export
#'
tuningNumPCclass <- function(genofile,  pop.df, min.maf, max.maf, ld, n.pc.set, missing.rate = 0.0,
                        labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                        cost = 10, sigma = 1, cp = 0.001, ntree = 500){
  cross.tbl.test <- matrix(0, nrow = length(n.pc.set), ncol = 5)
  colnames(cross.tbl.test) <- c("SVM linear", "SVM quadratic", "SVM radial", "Decision Trees", "Random Forest")
  rownames(cross.tbl.test) <- n.pc.set

  cross.tbl.train <- matrix(0, nrow = length(n.pc.set), ncol = 5)
  colnames(cross.tbl.train) <- c("SVM linear", "SVM quadratic", "SVM radial", "Decision Trees", "Random Forest")
  rownames(cross.tbl.train) <- n.pc.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  min.train.err <- 0.0; min.test.err <- 0.0
  best.n.pc <- n.pc.set[1]

  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
  snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
  snp.id.set <- unlist(snpset)

  for (n.pc in n.pc.set){
    pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=8, missing.rate = missing.rate)
    pca.df <- getPCAdf(pca, n.pc, pop.df)

    train.test.tbl <- getTrainTestErr(pca.df, split.coef, labels, categorical.var, cross.tbl.train, cross.tbl.test, n.pc, cost, sigma, cp, ntree)
    cross.tbl.train <- train.test.tbl$train.tbl
    cross.tbl.test <- train.test.tbl$test.tbl

    train.err <- min(cross.tbl.train[toString(n.pc),])
    test.err <- min(cross.tbl.test[toString(n.pc),])

    if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  && (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.n.pc <- n.pc
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, nPC = best.n.pc)
}
