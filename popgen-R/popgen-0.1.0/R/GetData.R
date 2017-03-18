#' Explode GDS
#'
#' Explode the gds X times
#' Use function snpgdsCombineGeno from SNPRelate
#' @param gds input gds
#' @param output.gds.fn output gds
#' @param times times to expolode
#' @export
explodeGDS <- function(gds, output.gds.gn, times){
  snpgdsClose(gds)
  gds.fn.set <- rep(gds$filename, times)
  snpgdsCombineGeno(gds.fn.set, output.gds.gn)
}

#' Get gds after QC
#'
#' @return The gds object after QC
#' @export
getQCgds <- function(genofile, pop.df,
                     min.maf = 0.005,
                     max.maf = 0.05,
                     ld = 0.2,
                     missing.rate = 0.0){

  snp.panel <- getSNPpanel2(genofile, pop.df, min.maf, max.maf, ld)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
  snpgdsCreateGenoSet(genofile.filtered$filename, "qc.gds", snp.id = snp.panel$id)
  snpgdsClose(genofile.filtered)
  return (snpgdsOpen("qc.gds"))
}


# not exported
getSNPpanel2 <- function(genofile,
                        pop.df,
                        min.maf = 0.005,
                        max.maf = 0.05,
                        ld = 0.2){

  freq <- snpgdsSNPRateFreq(genofile, sample.id = pop.df$SampleId, with.id = T)
  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id = snp.ids)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
  snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
  selected.snp.id.set <- unlist(snpset)

  snp.positions.set <- read.gdsn(index.gdsn(genofile, "snp.position"))
  snp.chr.set <- read.gdsn(index.gdsn(genofile, "snp.chromosome"))
  chr.snp.id.set <- read.gdsn(index.gdsn(genofile, "snp.id"))

  snp.panel <- cbind(as.data.frame(snp.chr.set), as.data.frame(snp.positions.set), as.data.frame(chr.snp.id.set))
  colnames(snp.panel) <- c("contigName", "start", "id")
  snp.panel <- snp.panel[selected.snp.id.set, c("contigName", "start", "id")]
  snpgdsClose(genofile.filtered)

  return (snp.panel)
}

#' Get SNP panel
#'
#' Takes a genofile and return a list of variants with frequencies from the given frames
#' @param genofile the .gds file
#' @param min.maf the inf limit of minor allele frequencies
#' @param max.maf the sup limit of minor allele frequencies
#' @param ld the linkage disequlibrium threshold
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' @return The list of selected SNPs (chromosome number and the start position)
#' @export

getSNPpanel <- function(genofile,
                        pop.df,
                        min.maf = 0.005,
                        max.maf = 0.05,
                        ld = 0.2,
                        file = ""){

  snp.panel <- getSNPpanel2(genofile, pop.df, min.maf, max.maf, ld)
  write.table(snp.panel, file, quote = FALSE, row.names = FALSE, sep = ",", col.names = T)

  return (snp.panel)
}

#' Get DataFrame
#'
#' Perform quality control analysis of the given genotype and return DataFrame to be fed to ML models
#'
#' @param genofile the .gds file
#' @param pop.df the samples panel
#' @param min.maf the inf limit of minor allele frequencies
#' @param max.maf the sup limit of minor allele frequencies
#' @param ld the linkage disequlibrium threshold
#' @param missing.rate a missiang rate threshold
#' @return a DataFrame after PCA
#' @export

getQCdf <- function(genofile, pop.df,
                       min.maf = 0.005,
                       max.maf = 0.05,
                       ld = 0.2,
                       missing.rate = 0.0){

  snp.panel <- getSNPpanel2(genofile, pop.df, min.maf, max.maf, ld)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
  df <-as.data.frame(snpgdsGetGeno(genofile.filtered, snp.id = snp.panel$id))
  snpgdsClose(genofile.filtered)

  snp.panel <- snp.panel %>% mutate(id = paste0(contigName, ":", start))
  colnames(df) <- snp.panel$id
  df["Region"] <- pop.df$Region
  return (df)
}


#' Get reduced DataFrame
#'
#' Perform quality control analysis and dimensionality reduction of the given genotype and return DataFrame to be fed to ML models
#'
#' @param genofile the .gds file
#' @param pop.df the samples panel
#' @param min.maf the inf limit of minor allele frequencies
#' @param max.maf the sup limit of minor allele frequencies
#' @param ld the linkage disequlibrium threshold
#' @param missing.rate a missiang rate threshold
#' @param npc the number of principal components
#' @param with.scree.plot if TRUE return the scree plot of the first 15th PCs, default = FALSE
#' @param with.eig.tbl if TRUE return the table with eigenvalues and variances of obtained  PCs, default = FALSE
#' @return a DataFrame after PCA
#' @export

getQCPCAdf <- function(genofile, pop.df,
                       min.maf = 0.005,
                       max.maf = 0.05,
                       ld = 0.2,
                       missing.rate = 0.0,
                       npc = 4,
                       with.scree.plot = F,
                       with.eig.tbl = F){

  freq <- snpgdsSNPRateFreq(genofile, with.id = T, sample.id = pop.df$SampleId)
  snp.ids <- freq$snp.id[which(freq$MinorFreq > min.maf & freq$MinorFreq < max.maf)]
  snpgdsCreateGenoSet(genofile$filename, paste0(genofile$filename, "filtered", ".gds"), snp.id=snp.ids, sample.id = pop.df$SampleId)
  genofile.filtered <- snpgdsOpen(paste0(genofile$filename, "filtered", ".gds"))
  snpset <- snpgdsLDpruning(genofile.filtered, ld.threshold = ld)
  snp.id.set <- unlist(snpset)
  pca <- snpgdsPCA(genofile.filtered, snp.id=snp.id.set,  bayesian = FALSE, eigen.cnt=npc, num.thread=4, missing.rate = missing.rate)
  pca.df <- getPCAdf(pca, npc, pop.df)
  snpgdsClose(genofile.filtered)

  if (with.eig.tbl == T){
    eigenvalues <- na.omit(pca$eigenval)
    # variance <- eigenvalues / sum(eigenvalues)
    selected.npc <- ifelse(npc < 50, npc, 50)
    p <- plot(eigenvalues, type="n",  main = "Scree plot",  xlab = "Principal Components",ylab = "Eigenvalues")
    lines(x = 1:length(eigenvalues[1:selected.npc]), eigenvalues[1:selected.npc], type="b", pch=19, col = "red")
    variance <- as.numeric(na.omit(pca$varprop))
    cum.variance <- cumsum(variance)
    eig.table <- as.data.frame(cbind(paste0(rep("PC", selected.npc), 1:selected.npc), round(eigenvalues, 2), round(variance * 100, 2), round(cum.variance * 100, 2)))
    colnames(eig.table) <- c("", "Eigenvalue", "Percentage of variance", "Cumulative percentage of variance")

    return (list(df = pca.df, eigenvalues = eig.table))
  }

  return (pca.df)
}

#' Get reduced DataFrame
#'
#' Combine DF from PCA with samples panel DF. Not exported
#'
#' @param pca a result PCA DataFrame
#' @param n.pc a number of PCs
#' @param pop.df a samples panel
#' @return a DataFrame after PCA

getPCAdf <- function(pca, n.pc, pop.df){
  pca.df <- as.data.frame(pca$eigenvect)
  pc.vector <- rep("PC", ncol(pca.df))
  col.names <- paste0(pc.vector, 1:ncol(pca.df))
  colnames(pca.df) <- col.names

  pca.df <- data.frame(SampleId = pca$sample.id,
                       Region = pop.df$Region,
                       pca.df)
  pca.df <- pca.df[, 1:(n.pc + 2)]
  return (pca.df)
}
