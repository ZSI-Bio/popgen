# Popgen
# Example 2. Clustering. Tuning parameters
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
min.maf.set  <- c(0.001, 0.0025, 0.0025, 0.005, 0.005, 0.005, 0.01, 0.01)
max.maf.set  <- c(0.1, 0.1, 0.05, 0.01, 0.05, 0.1, 0.05, 0.5)
npc.set      <- c(3, 4, 5, 10, 15, 20, 30, 40, 45, 50)
ld.set       <- c(0.2, 0.3, 0.5, 0.7)

# ------------------------
# MAF tuning
n.pc         <- 4
ld           <- 0.2
missing.rate <- 0.0

freq.tuning.result <- tuningMAF(genofile, pop.df, min.maf.set, max.maf.set, ld = ld, n.pc = n.pc, n.clusters = length(unique((pop.df$Region)))) # see at man for fucntion descriprion

freq.tuning.result$crossTbl  # tuned parameters
plotTuning(freq.tuning.result$crossTbl) # plot

# ------------------------
# LD tuning
#
min.maf       <- freq.tuning.result$minMAF # obtained optimal values from previous step
max.maf       <- freq.tuning.result$maxMAF
missing.rate  <- 0.0

ld.tuning.result <- tuningLD(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = n.pc, n.clusters = length(unique((pop.df$Region))))

ld.tuning.result$crossTbl # tuned parameters
plotTuning(ld.tuning.result$crossTbl)

# ------------------------
# nPC tuning

min.maf <- freq.tuning.result$minMAF
max.maf <- freq.tuning.result$maxMAF
ld      <- ld.tuning.result$bestLD

npc.tuning.result <- tuningNumPC(genofile, pop.df, min.maf, max.maf, ld, n.pc.set = npc.set, n.clusters = length(unique((pop.df$Region))))

npc.tuning.result$crossTbl
plotTuning(npc.tuning.result$crossTbl, isNumeric = T)

# ------------------------
# LD MAF tuning

n.pc <- npc.tuning.result$nPC

ld.maf.tuning.result <- tuningLdMaf(genofile, pop.df, min.maf, max.maf, ld.set, n.pc = n.pc, n.clusters = length(unique((pop.df$Region))), method = "hierarchical")

ld.maf.tuning.result$crossTbl
plotTuning(ld.maf.tuning.result$crossTbl)

# ------------------------
# MAF nPC tuning
#
ld  <- ld.maf.tuning.result$ld

npc.tuning.result <-tuningMAFPC (genofile, pop.df, min.maf.set, max.maf.set, n.pc.set = npc.set, ld = ld, n.clusters = length(unique((pop.df$Region))), method = "hierarchical")

npc.tuning.result$crossTbl
plotTuning(t(npc.tuning.result$crossTbl), isNumeric = T)

npc.tuning.result$npc
npc.tuning.result$min.maf
npc.tuning.result$max.maf

# ------------------------
# LD nPC tuning

min.maf <- freq.tuning.result$minMAF # or npc.tuning.result$min.maf
max.maf <- freq.tuning.result$maxMAF # or npc.tuning.result$max.maf

npc.ld.tuning.result <-tuningLDnPC (genofile, pop.df, min.maf, max.maf, ld.set = ld.set, n.pc.set = npc.set, n.clusters = length(unique((pop.df$Region))), method = "hierarchical")

npc.ld.tuning.result$crossTbl
plotTuning(t(npc.ld.tuning.result$crossTbl), isNumeric = T)

# ------------------------
# MAF vs LD vs nPC tuning

maf.ld.npc.tun.result <- tuningMAFLDnPC(genofile,  pop.df, min.maf, max.maf, ld.set, npc.set)

maf.ld.npc.tun.result$min.maf
maf.ld.npc.tun.result$max.maf
maf.ld.npc.tun.result$ld
maf.ld.npc.tun.result$nPC

