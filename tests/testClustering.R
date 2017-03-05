genofile <- chr22load()
panel <- panelLoad()
pop <- "SAS"
pop.df <- panel[panel$Region == pop, c(1, 2)]
colnames(pop.df) <- c("SampleId", "Region")

# ------------------------
# MAF tuning

min.maf.set <- c(0.001, 0.0025, 0.0025, 0.005, 0.005, 0.005, 0.01, 0.01)
max.maf.set <- c(0.1, 0.1, 0.05, 0.01, 0.05, 0.1, 0.05, 0.5)
n.pc <- 10
missing.rate = 0.0

freq.tuning.result <- tuningMAF(genofile, pop.df, min.maf.set, max.maf.set, n.pc = n.pc, n.clusters = length(unique((pop.df$Region))))

cross.tbl <- as.data.frame(freq.tuning.result$crossTbl)
toLatex(xtable(cross.tbl))

plotTuning(freq.tuning.result$crossTbl)

# ------------------------
# LD tuning
set.seed(1234)
min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF
ld.set <- c(0.2, 0.3, 0.4, 0.5, 0.7)
missing.rate <- 0.0
ld.tuning.result <- tuningLD(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = n.pc, n.clusters = length(unique((pop.df$Region))))

plotTuning(ld.tuning.result$crossTbl)
toLatex(xtable(ld.tuning.result$crossTbl))
ld.tuning.result$bestLD

# ------------------------
# nPC tuning

min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF
ld <- ld.tuning.result$bestLD
missing.rate <- 0.0
npc.set <-c(2, 3, 4, 5, 10, 15, 20, 30, 40)#, 45, 50, 60)

npc.tuning.result <- tuningNumPC(genofile, pop.df, min.maf, max.maf, ld, n.pc.set = npc.set, n.clusters = length(unique((pop.df$Region))))
plotTuning(npc.tuning.result$crossTbl, isNumeric = T)
toLatex(xtable(npc.tuning.result$crossTbl))
npc.tuning.result$nPC

# ------------------------
# LD MAF tuning

min.maf <- c(0.001, 0.0025, 0.005, 0.005, 0.01, 0.05)
max.maf <-  c(0.1, 0.1, 0.05, 0.1, 0.05, 0.5)
ld.set <- c(0.2, 0.3, 0.5, 0.7, 0.8)
n.pc <- npc.tuning.result$nPC
missing.rate <- 0.0

n.rep <- 1
cross.tbl <- matrix(nrow = length(min.maf), ncol = length(ld.set), 0)
tun.ld <- 0.0
tun.min.maf <- 0.0
tun.max.maf <- 0.0

for (i in 1:n.rep){
  ld.maf.tuning.result <- tuningLdMaf(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = n.pc, n.clusters = length(unique((pop.df$Region))), method = "kmeans")
  cross.tbl <- cross.tbl + ld.maf.tuning.result$crossTbl
  tun.ld <- tun.ld + ld.maf.tuning.result$ld
  tun.min.maf <- tun.min.maf + ld.maf.tuning.result$min.maf
  tun.max.maf <- tun.max.maf + ld.maf.tuning.result$max.maf
}

cross.tbl <- cross.tbl / n.rep


tun.ld <- tun.ld / n.rep
tun.min.maf <- tun.min.maf / n.rep
tun.max.maf <- tun.max.maf / n.rep

ld.maf.tuning.result$ld
ld.maf.tuning.result$min.maf
ld.maf.tuning.result$max.maf

plotTuning(cross.tbl)
toLatex(xtable(cross.tbl))

# ------------------------
# MAF nPC tuning
set.seed(4564)
min.maf.set <- c(0.0025, 0.005, 0.005, 0.01, 0.01)
max.maf.set <- c(0.1, 0.05, 0.1, 0.05, 0.5)
npc.set <- c(2, 3, 4, 5, 10, 15, 20)

npc.tuning.result <-tuningMAFPC (genofile, pop.df, min.maf.set, max.maf.set, n.pc.set = npc.set, ld = ld, n.clusters = length(unique((pop.df$Region))), method = "em")
plotTuning(t(npc.tuning.result$crossTbl), isNumeric = T)
toLatex(xtable(npc.tuning.result$crossTbl))
npc.tuning.result$npc
npc.tuning.result$min.maf
npc.tuning.result$max.maf

# ------------------------
# LD nPC tuning
set.seed(1234)
min.maf <- 0.005
max.maf <- 0.1
npc.set <- c(2, 3, 4, 5, 10, 15, 20, 30, 40)
ld.set <- c(0.2, 0.3, 0.5, 0.7, 0.8)

npc.ld.tuning.result <-tuningLDnPC (genofile, pop.df, min.maf, max.maf, ld.set = ld.set, n.pc.set = npc.set, n.clusters = length(unique((pop.df$Region))), method = "kmeans")
plotTuning(t(npc.ld.tuning.result$crossTbl), isNumeric = T)
toLatex(xtable(t(npc.ld.tuning.result$crossTbl)))




tun.npc <- npc.tuning.result$npc
tun.min.maf <- ld.maf.tuning.result$min.maf
tun.max.maf <- ld.maf.tuning.result$max.maf
tun.ld <- 0.2 #npc.ld.tuning.result$ld
n.clusters <- length(unique((pop.df$Region)))

pca.info <- getQCPCAdf(genofile, pop.df, 0.005, 0.5, 0.7, npc = 10, with.eig.tbl = T)
pca.info <- getQCPCAdf(genofile, pop.df, min.maf, max.maf, 0.7, npc = 10, with.eig.tbl = T)
df <- pca.info$df
toLatex(xtable(pca.info$eigenvalues))
obj.hclust <- HClust( data = df, k = n.clusters)
obj.kmeans <- kmeansClust( data = df, k = n.clusters)
obj.emc <- emcClust( data = df, k = n.clusters)

toLatex(xtable(summaryClustering(obj.hclust, obj.kmeans, obj.emc)))

prediction <- obj.hclust@prediction
prediction.df <- cbind(df, prediction)
attach(prediction.df)
plotMap2(prediction.df, PC1, PC2)
detach(prediction.df)

toLatex(xtable(obj.emc@conf.matr))
