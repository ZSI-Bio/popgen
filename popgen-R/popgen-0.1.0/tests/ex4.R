# Popgen
# Example 4. Classification. Tuning the classifier parameters
# Example for super-populations.
# Analysis of sub-population is the same
# Please note that not all performed steps are included in this example

genofile           <- chr22load() # Load chr 22 (Original repo 1000 Genomes)
panel              <- panelLoad() # Load panel file
panel$`Sub-region` <- as.character(panel$`Sub-region`)
pop.df             <- panel[, c(1, 3)] # Choose SampleId and Region
colnames(pop.df)   <- c("SampleId", "Region")
n.clusters         <- length(unique((pop.df$Region))) # perform clustering for n = number of population groups

min.maf <- 0.005
max.maf <- 0.05
npc     <- 70
ld      <- 0.2

df  <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld=ld, npc = npc, with.eig.tbl = F) # QC analysis (breifly description in 'man')

# Split data into train and test
n.samples  <- nrow(df)
train.data <- df[sample(1:n.samples, round(0.67 * n.samples)), ]
test.data  <- df[-(sample(1:n.samples, round(0.67 * n.samples))), ]

# --------
# SVM with linear kernel
#
# Tune the cost parameter
costs.set    <- c(1, 10, 20, 30, 40, 50, 60, 70, 100) # range of cost parameters for tuning
cross.errors <-  matrix(0, nrow = length(costs.set), ncol = 2)

n <- 10 # repeat n times and take avg
for (i in 1:n){
  svm.est      <- svmEstimation(train.data.set = train.data, which.kernel = "linear", costs = costs.set, sigmas = 1)
  cross.errors <- cross.errors + svm.est@cross.errors
}

(cross.errors <- cross.errors / n) # look

# --------
# SVM with quadratic/radial kernel
#
# Tune the cost and sigma parameters
sigmas                        <- 10^(-2:1) # range of sigma parameters for tuning, cost para,eters are the same as for SV< linear
cross.errors.radial           <- matrix(0, nrow = length(sigmas), ncol = length(costs.set)) # create zeros matrix for errors
colnames(cross.errors.radial) <- costs.set
rownames(cross.errors.radial) <- sigmas

nSV.radial <- matrix(0, nrow = length(sigmas), ncol = length(costs.set)) # create zeros matrix for support vectors
colnames(nSV.radial) <- costs.set
rownames(nSV.radial) <- sigmas

for (i in 1:n){ # 'n' is defines upper (cee SVM with linear)
  svm.est <- svmEstimation(train.data.set = train.data, which.kernel = "RBF", costs = costs.set, sigmas = sigmas) # for quadratic kernel: 'RBF' -> 'quadratic'

  cross.errors.radial <- cross.errors.radial + svm.est@cross.errors
  nSV.radial          <- nSV.radial + svm.est@nSV
}

(cross.errors.radial <- cross.errors.radial / n)
(nSV.radial          <- nSV.radial / n)

plotTuning(t(cross.errors.radial), isNumeric = T)
plotTuning(t(nSV.radial), isNumeric = T)

# --------
# Decision Trees
#
set.seed(189)
dt.pred <- dtPrediction(train.data, test.data, cp = 0.01, split.cr = "gini")
dt.pred@plot.tree # plot tree
dt.pred@plot.cptable # plot complexity parameter table
dt.pred@model.tree # plot splits
dt.pred@model.cptable # complexity parameter table
dt.pred@metrics.train@error
dt.pred@metrics.train@precision # training precision for each class
dt.pred@metrics.train@recall #training recall for each class

dt.pred@metrics.train@macro.eval
dt.pred@metrics.test@macro.eval

dt.pred@metrics.train@confusion.matrix
dt.pred@metrics.test@confusion.matrix


# --------
# Random Forest
rf.pred <- rfPrediction(train.data, test.data, categorical.var = "SampleId", ntree = 500)
rf.pred@metrics.train@macro.eval
rf.pred@metrics.test@macro.eval
rf.pred@plot.importance

# plot only OOB error to determine th enumebt of trees
p <- plot(rf.pred@model$err.rate[,"OOB"], type="n",  xlab = "Number of trees",ylab = "Error")
lines(x = 1:150, rf.pred@model$err.rate[,"OOB"], type="l", pch=19, col = "red", lwd = 3)
axis(side = 1,  seq(0, 150, 25))
lines(x = seq(1, 150, 10), rf.pred@model$err.rate[,"OOB"], type="p", pch=19, col = "red", lwd = 0.01)


