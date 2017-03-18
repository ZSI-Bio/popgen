# Popgen
# Example 4. Classification. Prediction
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

pca.df  <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld=ld, npc = npc, with.eig.tbl = F) # QC analysis (breifly description in 'man')

# Split data into train and test
n.samples  <- nrow(pca.df)
train.data <- pca.df[sample(1:n.samples, round(0.67 * n.samples)), ]
test.data  <- pca.df[-(sample(1:n.samples, round(0.67 * n.samples))), ]
C          <- 20 # cost parameter
sigma      <- 10 # sigma parameter

# ----------------
# SVM
# example for quadratic kernel. For others, please fix which.kernel to euther 'linear' or 'RBF'

set.seed(47903)
svm.pred <- svmPrediction(train.data, test.data, which.kernel = "quadratic", cost = C, sigma = sigma)

svm.pred@metrics.train@macro.eval # macro training evaluation measures
svm.pred@metrics.test@macro.eval #  macro test evaluation measures

svm.pred@metrics.train@confusion.matrix # training confusion matrix
svm.pred@metrics.test@confusion.matrix # test confusion matrix

svm.pred@model # print the model

svm.pred@metrics.train@precision # train precision for each class
svm.pred@metrics.test@precision # test precision for each class
