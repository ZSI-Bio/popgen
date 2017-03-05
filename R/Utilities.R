#' Load an example data
#'
#' Load the chr22 from the 1000 Genomes Project
#' @return The chr22 in .gds format
#' @export
chr22load <- function()
{
  snpgdsOpen(system.file("extdata", "chr22filtered.gds", package="popgen"))
}

#' Load a list of individuals for an example data
#'
#' Load a sample panel from 1000 Genomes Project
#' @return The a list of individuals
#' @export

panelLoad <- function()
{
  panel <- read.table(system.file("extdata", "panel.panel", package="popgen"), header = T)
  colnames(panel) <- c("SampleId", "Sub-region", "Region", "Gender")
  panel
}
