genofile <- chr22load()
panel <- panelLoad()
# pop <- "EUR"
pop.df <- panel[, c(1, 3)]
# pop.df <- panel[panel$Region == pop, c(1, 2)]
colnames(pop.df) <- c("SampleId", "Region")
pop.df$Region <- as.factor(as.character(pop.df$Region))

min.maf <- 0.01
max.maf <- 0.05
ld <- 0.2
# qcDF <- getQCdf(genofile, pop.df, min.maf, max.maf, ld)
# df <- qcDF[, c(1:10, ncol(qcDF))]

# snp.panel <- getSNPpanel(genofile, pop.df, min.maf, max.maf, ld, "~/1000genomes/snps_panel.csv")
npc <- 5
pca.info <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, ld, npc = npc, with.eig.tbl = T)
df <- pca.info$df

n.samples <- nrow(df)
train.data <- df[1:round(0.67 * n.samples), ]
test.data <- df[-(1:round(0.67 * n.samples)), ]

# --------
# SVM
svm.est <- svmEstimation(train.data.set = train.data, which.kernel = "quadratic") #, costs = c(1), sigmas = c(10))
svm.est@cross.errors
svm.pred <- svmPrediction(train.data, test.data, which.kernel = "linear", cost = 10, sigma = 1)
svm.pred@metrics.train@macro.eval
svm.pred@metrics.test@macro.eval

# --------
# Decision Trees
dt.pred <- dtPrediction(train.data, test.data)
dt.pred@metrics.train@macro.eval
dt.pred@metrics.test@macro.eval
dt.pred@metrics.train@confusion.matrix
dt.pred@model.tree

# --------
# Random Forest
rf.pred <- rfPrediction(train.data, test.data, categorical.var = "SampleId")
rf.pred@metrics.train@macro.eval
rf.pred@metrics.test@macro.eval

data <- test.data[, -1]
data["prediction"] <- (rf.pred@metrics.test@prediction)
data$Region <- data$Region
attach(data)
plotMap2(data, PC3, PC2)
detach(data)



# ------------------------
# MAF tuning
min.maf.set <- c(0.001, 0.0025, 0.0025, 0.005, 0.005, 0.005, 0.01, 0.01)
max.maf.set <- c(0.1, 0.1, 0.05, 0.01, 0.05, 0.1, 0.05, 0.5)

freq.tuning.result <- tuningMAFclass(genofile, pop.df, min.maf.set, max.maf.set, n.pc = n.pc)

cross.tbl.train <- as.data.frame(freq.tuning.result$crossTrainTbl)
toLatex(xtable(cross.tbl.train))

cross.tbl.test <- as.data.frame(freq.tuning.result$crossTestTbl)
toLatex(xtable(cross.tbl.test))


plotTuning(cross.tbl.train)
plotTuning(cross.tbl.test)

# ------------------------
# LD tuning

set.seed(1234)
min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF
ld.set <- c(0.2, 0.3) #, 0.4, 0.5, 0.7)
ld.tuning.result <- tuningLDclass(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = n.pc)

cross.ld.tbl.train <- as.data.frame(ld.tuning.result$crossTrainTbl)
toLatex(xtable(cross.ld.tbl.train))

cross.ld.tbl.test <- as.data.frame(ld.tuning.result$crossTestTbl)
toLatex(xtable(cross.ld.tbl.test))

plotTuning(cross.ld.tbl.train)
plotTuning(cross.ld.tbl.test)

ld.tuning.result$bestLD

# ------------------------
# nPC tuning

min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF
ld <- ld.tuning.result$bestLD
npc.set <-c(2, 3, 4)#, 5, 10, 15, 20, 30, 40)#, 45, 50, 60)

npc.tuning.result <- tuningNumPCclass(genofile, pop.df, min.maf, max.maf, ld, n.pc.set = npc.set)

cross.npc.tbl.train <- as.data.frame(npc.tuning.result$crossTrainTbl)
toLatex(xtable(cross.npc.tbl.train))

cross.npc.tbl.test <- as.data.frame(npc.tuning.result$crossTestTbl)
toLatex(xtable(cross.npc.tbl.test))

plotTuning(cross.npc.tbl.train)
plotTuning(cross.npc.tbl.test)

npc.tuning.result$nPC
