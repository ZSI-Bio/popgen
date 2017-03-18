# Popgen
# Example 3. Classification. Tuning QC parameters
# Example for super-populations.
# Analysis of sub-population is the same
# Please note that not all performed steps are included in this example


genofile           <- chr22load() # Load chr 22 (Original repo 1000 Genomes)
panel              <- panelLoad() # Load panel file
panel$`Sub-region` <- as.character(panel$`Sub-region`)
pop.df             <- panel[, c(1, 3)] # Choose SampleId and Region
colnames(pop.df)   <- c("SampleId", "Region")
n.clusters         <- length(unique((pop.df$Region))) # perform clustering for n = number of population groups

# ------------------------
# Set parameters
#
min.maf.set  <- c(0.001, 0.0025, 0.0025, 0.005, 0.005, 0.005, 0.01, 0.01)
max.maf.set  <- c(0.1, 0.1, 0.05, 0.01, 0.05, 0.1, 0.05, 0.5)
npc.set      <- c(3, 4, 5, 10, 15, 20, 30, 40, 45, 50)
ld.set       <- c(0.2, 0.3, 0.5, 0.7)

# ------------------------
# MAF tuning
#
npc <- 20
freq.tuning.result <- tuningMAFclass(genofile, pop.df, min.maf.set, max.maf.set, n.pc = npc)

(cross.tbl.train <- as.data.frame(freq.tuning.result$crossTrainTbl)) # Training errors for different nPC
(cross.tbl.test  <- as.data.frame(freq.tuning.result$crossTestTbl))  # Test errors for different nPC

plotTuning(cross.tbl.train) # plot training error for different nPC
plotTuning(cross.tbl.test)  # plot test error for different nPC
plotTuning(abs(cross.tbl.train - cross.tbl.test)) # plot difference between training and test errors for different nPC

# ------------------------
# LD tuning
#
min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF

ld.tuning.result <- tuningLDclass(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = npc)

(cross.ld.tbl.train <- as.data.frame(ld.tuning.result$crossTrainTbl))
(cross.ld.tbl.test <- as.data.frame(ld.tuning.result$crossTestTbl))

plotTuning(cross.ld.tbl.train)
plotTuning(cross.ld.tbl.test)
plotTuning(abs(cross.ld.tbl.train - cross.ld.tbl.test))

# ------------------------
# nPC tuning

min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF
ld      <- ld.tuning.result$bestLD

npc.tuning.result <- tuningNumPCclass(genofile, pop.df, min.maf, max.maf, ld, n.pc.set = npc.set)

(cross.npc.tbl.train <- as.data.frame(npc.tuning.result$crossTrainTbl))
(cross.npc.tbl.test <- as.data.frame(npc.tuning.result$crossTestTbl))

plotTuning(cross.npc.tbl.train, isNumeric = T)
plotTuning(cross.npc.tbl.test, isNumeric = T)
plotTuning(abs(cross.npc.tbl.train - cross.npc.tbl.test), isNumeric = T)

# ------------------------
# nPC vs LD tuning

min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF

npc.ld.tun.result <- tuningLDnPCclass(genofile,  pop.df, min.maf, max.maf, ld.set, npc.set)

npc.ld.tun.result$crossTrainTbl
npc.ld.tun.result$crossTestTbl
npc.ld.tun.result$nPC
npc.ld.tun.result$ld


# ------------------------
# MAF vs nPC tuning
ld  <- ld.tuning.result$bestLD

maf.npc.tun.result <- tuningMAFnPCclass(genofile,  pop.df, min.maf, max.maf, ld = ld, npc.set)

maf.npc.tun.result$crossTrainTbl
maf.npc.tun.result$crossTestTbl
maf.npc.tun.result$nPC
maf.npc.tun.result$min.maf
maf.npc.tun.result$max.maf

# ------------------------
# MAF vs LD tuning

npc <- npc.tuning.result$nPC

maf.ld.tun.result <- tuningMAFLDclass(genofile,  pop.df, min.maf, max.maf, ld.set, n.pc = npc)

maf.ld.tun.result$crossTrainTbl
maf.ld.tun.result$crossTestTbl
maf.ld.tun.result$ld
maf.ld.tun.result$min.maf
maf.ld.tun.result$max.maf

# ------------------------
# MAF vs LD vs nPC tuning

maf.ld.npc.tun.result <- tuningMAFLDnPCclass(genofile,  pop.df, min.maf, max.maf, ld.set, npc.set)

maf.ld.npc.tun.result$min.maf
maf.ld.npc.tun.result$max.maf
maf.ld.npc.tun.result$ld
maf.ld.npc.tun.result$nPC
