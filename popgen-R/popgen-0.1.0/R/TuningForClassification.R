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
  train.data <- df[sample(1:n.samples, round(split.coef * n.samples)), ]
  test.data <- df[-(sample(1:n.samples, round(split.coef * n.samples))), ]

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
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld linkage-disequilibrium threshold
#' @param n.pc number of principal components (PCA)
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
  min.train.err <- 200.0; min.test.err <- 100.0
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

    if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  & (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.min.maf <- min.maf; best.max.maf <- max.maf
    }
  }

  list(crossTrainTbl = cross.freq.train, crossTestTbl = cross.freq.test, minMAF = best.min.maf, maxMAF = best.max.maf)
}

# -------------------
#' Tuning LD for classification
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param min.maf infimum maf for filtering SNPs
#' @param max.maf supremum maf for filtering SNPs
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc number of principal components (PCA)
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
  min.train.err <- 200.0; min.test.err <- 100.0
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

    if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  & (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.ld <- ld
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, bestLD = best.ld)
}

# -------------------
#' Tuning nPC for classification
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param min.maf infimum maf for filtering SNPs
#' @param max.maf supremum maf for filtering SNPs
#' @param ld linkage-disequilibrium threshold
#' @param n.pc set of numbers of principal components to tune
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
  min.train.err <- 200.0; min.test.err <- 100.0
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

    if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  & (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.n.pc <- n.pc
    }
  }
  snpgdsClose(genofile.filtered)
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, nPC = best.n.pc)
}

# -------------------
#' Tuning LD vs nPC for classification
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param min.maf infimum maf for filtering SNPs
#' @param max.maf supremum maf for filtering SNPs
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc set of numbers of principal components to tune
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @param method classification method
#' @details With method = 'svml' function tunes parameters for SVM with linear kernel, 'svmq' - for SVM with quadratic kernel, 'svmr' - for SVM with radial kernel, 'dt' - for Decision Trees, 'rfs' - for Random Forest
#' @export
#'
tuningLDnPCclass <- function(genofile,  pop.df, min.maf, max.maf, ld.set, n.pc.set, missing.rate = 0.0,
                             labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                             cost = 10, sigma = 1, cp = 0.001, ntree = 500, method = "svml"){
  cross.tbl.test <- matrix(nrow = length(ld.set), ncol = length(n.pc.set))
  rownames(cross.tbl.test) <- ld.set
  colnames(cross.tbl.test) <- n.pc.set

  cross.tbl.train <- matrix(nrow = length(ld.set), ncol = length(n.pc.set))
  rownames(cross.tbl.train) <- ld.set
  colnames(cross.tbl.train) <- n.pc.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  min.train.err <- 200.0; min.test.err <- 100.0
  best.n.pc <- n.pc.set[1]
  best.ld <- ld.set[1]

  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))

  for (ld in ld.set){
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)
    for (n.pc in n.pc.set){
      pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=8, missing.rate = missing.rate)
      df <- getPCAdf(pca, n.pc, pop.df)

      n.samples <- nrow(df)
      train.data <- df[sample(1:n.samples, round(split.coef * n.samples)), ]
      test.data <- df[-(sample(1:n.samples, round(split.coef * n.samples))), ]

      model <- NULL

      switch(method,
             svml = {model <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "linear", cost = cost, sigma = sigma)},
             svmq = {model <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "quadratic", cost = cost, sigma = sigma)},
             svmr = {model <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "RBF", cost = cost, sigma = sigma)},
             dt = {model <- dtPrediction(train.data, test.data, labels, categorical.var, cp = cp)},
             rf = {model <- rfPrediction(train.data, test.data, labels, categorical.var, ntree)},
             stop("Enter one of 'svml', 'svvmq', 'svmr', 'dt', 'rf'"))

      cross.tbl.train[toString(ld), toString(n.pc)] <- model@metrics.train@error
      cross.tbl.test[toString(ld), toString(n.pc)] <- model@metrics.test@error

      train.err <- cross.tbl.train[toString(ld), toString(n.pc)]
      test.err <-  cross.tbl.test[toString(ld), toString(n.pc)]

      if(abs(train.err - test.err) <= abs(min.train.err - min.test.err)  & (min.train.err > train.err || min.test.err > test.err)){
        min.train.err <- train.err; min.test.err <- test.err
        best.n.pc <- n.pc
        best.ld <- ld
      }
    }
  }

  snpgdsClose(genofile.filtered)
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, nPC = best.n.pc, ld = best.ld, minTrainErr = min.train.err, minTestErr = min.test.err)
}


# -------------------
#' Tuning MAF vs nPC for classification
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld linkage-disequilibrium threshold
#' @param n.pc set of numbers of principal components to tune
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @param method classification method
#' @details With method = 'svml' function tunes parameters for SVM with linear kernel, 'svmq' - for SVM with quadratic kernel, 'svmr' - for SVM with radial kernel, 'dt' - for Decision Trees, 'rfs' - for Random Forest
#' @export
#'
tuningMAFnPCclass <- function(genofile,  pop.df, inf.maf.set, sup.maf.set, ld, n.pc.set, missing.rate = 0.0,
                             labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                             cost = 10, sigma = 1, cp = 0.001, ntree = 500, method = "svml"){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  cross.tbl.test <- matrix(nrow = length(freq.names), ncol = length(n.pc.set))
  rownames(cross.tbl.test) <-  freq.names
  colnames(cross.tbl.test) <- n.pc.set

  cross.tbl.train <- matrix(nrow = length(freq.names), ncol = length(n.pc.set))
  rownames(cross.tbl.train) <-  freq.names
  colnames(cross.tbl.train) <- n.pc.set

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  min.train.err <- 100.0; min.test.err <- 100.0
  best.n.pc <- n.pc.set[1]
  best.ld <- ld.set[1]
  best.min.maf <- inf.maf.set[1];  best.max.maf <- sup.maf.set[1]

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
    snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
    genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
    snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
    snp.id.set <- unlist(snpset)

    for (n.pc in n.pc.set){
      pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=n.pc, num.thread=8, missing.rate = missing.rate)
      df <- getPCAdf(pca, n.pc, pop.df)

      n.samples <- nrow(df)
      train.data <- df[sample(1:n.samples, round(split.coef * n.samples)), ]
      test.data <- df[-(sample(1:n.samples, round(split.coef * n.samples))), ]

      model <- NULL

      switch(method,
             svml = {model <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "linear", cost = cost, sigma = sigma)},
             svmq = {model <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "quadratic", cost = cost, sigma = sigma)},
             svmr = {model <- svmPrediction(train.data, test.data, labels, categorical.var, which.kernel = "RBF", cost = cost, sigma = sigma)},
             dt = {model <- dtPrediction(train.data, test.data, labels, categorical.var, cp = cp)},
             rf = {model <- rfPrediction(train.data, test.data, labels, categorical.var, ntree)},
             stop("Enter one of 'svml', 'svvmq', 'svmr', 'dt', 'rf'"))

      train.err <-  model@metrics.train@error
      test.err  <-  model@metrics.test@error

      cross.tbl.train[toString(freq.names[i]), toString(n.pc)] <- train.err
      cross.tbl.test[toString(freq.names[i]), toString(n.pc)]  <- test.err

      if((abs(train.err - test.err) <= abs(min.train.err - min.test.err))  & (min.train.err > train.err || min.test.err > test.err)){
        min.train.err <- train.err; min.test.err <- test.err
        best.n.pc <- n.pc
        best.min.maf <- inf.maf.set[i]
        best.max.maf <- sup.maf.set[i]
      }
    }
    snpgdsClose(genofile.filtered)
  }
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, nPC = best.n.pc, min.maf = best.min.maf, max.maf = best.max.maf)
}

# -------------------
#' Tuning MAF vs LD for classification
#' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc number of principal components (PCA)
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @param method classification method
#' @details With method = 'svml' function tunes parameters for SVM with linear kernel, 'svmq' - for SVM with quadratic kernel, 'svmr' - for SVM with radial kernel, 'dt' - for Decision Trees, 'rfs' - for Random Forest
#' @export
#'
tuningMAFLDclass <- function(genofile,  pop.df, inf.maf.set, sup.maf.set, ld.set, n.pc, missing.rate = 0.0,
                              labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                              cost = 10, sigma = 1, cp = 0.001, ntree = 500, method = "svml"){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  cross.tbl.test <- NULL
  cross.tbl.train <- NULL

  freq <- snpgdsSNPRateFreq(genofile, with.id=T, sample.id = pop.df$SampleId)
  min.train.err <- 200.0; min.test.err <- 100.0
  best.ld <- ld.set[1]
  best.min.maf <- inf.maf.set[1];  best.max.maf <- sup.maf.set[1]

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    tun.res <- tuningLDnPCclass(genofile,  pop.df, min.maf, max.maf, ld.set, n.pc, missing.rate, labels, categorical.var, split.coef, cost, sigma, cp, ntree, method)

    cross.tbl.train <- rbind(cross.tbl.train, t(tun.res$crossTrainTbl))
    cross.tbl.test <- rbind(cross.tbl.test, t(tun.res$crossTestTbl))

    train.err <- tun.res$minTrainErr
    test.err <- tun.res$minTestErr
    if((abs(train.err - test.err) <= abs(min.train.err - min.test.err))  & (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.ld <- tun.res$ld
      best.min.maf <- inf.maf.set[i];   best.max.maf <- sup.maf.set[i]
    }

  }
  rownames(cross.tbl.test) <-  freq.names
  rownames(cross.tbl.train) <-  freq.names
  list(crossTrainTbl = cross.tbl.train, crossTestTbl = cross.tbl.test, ld = best.ld, min.maf = best.min.maf, max.maf = best.max.maf)
}

# -------------------
#' Tuning MAF vs LD vs nPC for classification
#' #' @param genofile gds file
#' @param pop.df panel file that contains labels, e.g. Region
#' @param inf.maf.set range of infimum maf for tuning
#' @param sup.maf.set range of supremum maf for tuning
#' @param ld.set linkage-disequilibrium range for tuning
#' @param n.pc set of numbers of principal components to tune
#' @param split split df into train and test
#' @param cost for svm
#' @param sigma for all kernels if svm apart of linear one
#' @param cp control parameter for decision trees
#' @param ntree number of trees for random forest
#' @param method classification method
#' @details With method = 'svml' function tunes parameters for SVM with linear kernel, 'svmq' - for SVM with quadratic kernel, 'svmr' - for SVM with radial kernel, 'dt' - for Decision Trees, 'rfs' - for Random Forest
#' @export
#'
tuningMAFLDnPCclass <- function(genofile,  pop.df, inf.maf.set, sup.maf.set, ld.set, n.pc.set, missing.rate = 0.0,
                             labels = c("Region"), categorical.var = c("SampleId"), split.coef = 0.67,
                             cost = 10, sigma = 1, cp = 0.001, ntree = 500, method = "svml"){
  freq.names <- paste0("(", inf.maf.set, "; ", sup.maf.set, ")")
  best.min.maf <- inf.maf.set[1];  best.max.maf <- sup.maf.set[1]
  best.ld <- ld.set[1]
  best.npc <- n.pc.set[1]
  min.train.err <- 200.0; min.test.err <- 100.0

  for (i in 1:length(inf.maf.set)){
    min.maf <- inf.maf.set[i]; max.maf <- sup.maf.set[i]

    tun.res <- tuningLDnPCclass(genofile, pop.df, min.maf, max.maf, ld.set, n.pc.set, missing.rate, labels, categorical.var, split.coef, cost, sigma, cp, ntree, method)

    train.err <- tun.res$minTrainErr
    test.err <- tun.res$minTestErr

    if((abs(train.err - test.err) <= abs(min.train.err - min.test.err))  & (min.train.err > train.err || min.test.err > test.err)){
      min.train.err <- train.err; min.test.err <- test.err
      best.n.pc <- tun.res$nPC
      best.ld <- tun.res$ld
      best.min.maf <- inf.maf.set[i]
      best.max.maf <- sup.maf.set[i]
    }
  }
  list(min.maf = best.min.maf, max.maf = best.max.maf, ld = best.ld, nPC = best.npc)
}



