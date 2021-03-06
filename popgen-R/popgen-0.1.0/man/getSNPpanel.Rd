% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetData.R
\name{getSNPpanel}
\alias{getSNPpanel}
\title{Get SNP panel}
\usage{
getSNPpanel(genofile, pop.df, min.maf = 0.005, max.maf = 0.05, ld = 0.2,
  file = "")
}
\arguments{
\item{genofile}{the .gds file}

\item{min.maf}{the inf limit of minor allele frequencies}

\item{max.maf}{the sup limit of minor allele frequencies}

\item{ld}{the linkage disequlibrium threshold}

\item{file}{either a character string naming a file or a connection open for writing. "" indicates output to the console.}
}
\value{
The list of selected SNPs (chromosome number and the start position)
}
\description{
Takes a genofile and return a list of variants with frequencies from the given frames
}
