\name{prep.conop}
\alias{prep.conop}
\alias{prep.eventfile}
\alias{prep.weight}

\title{
Prepares CONOP9 input file.
}
\description{
Functions to prepare CONOP input files referred to as \code{eventfile} and \code{loadfile}. Function \code{prep.weight} allow the user to modify \code{loadfile} to include weights (to deal with outliers).
}
\usage{
prep.conop(Input, lofo, relaxed = FALSE, paired = TRUE)
prep.eventfile(Input, taxa_list)
prep.weight(Input, method = "badspecies", bad, weight.bad = "0.50", 
            inconsistent, weight.inconsistent = "0.50", 
            paclist, abmat, mbsf, pacman.sample.ratio = 0.1, weight.pacman = "0.50")
}
\arguments{
  \item{Input}{
Object of class CONOPinput to modify.
}
  \item{lofo}{
Output of function \code{\link{lofo}}.
}
  \item{relaxed}{
If \code{FALSE}, FO will be able to more down a section but not up and LO inversely. If \code{TRUE}, they will both be able to more up or down a section.
}
  \item{paired}{
If \code{TRUE}, FO and LO are paired events.
}
  \item{taxa_list}{
Vector of taxa names to be used in the analysis (for non-bioevents, it will need to be modified later manually or with other functions such as \code{prep.magn})
}
  \item{method}{
\code{prep.weight} should be used one method at a time. The methods are as follow: \code{badspecies, badevent, inconsistent} and \code{pacman}.
For \code{inconsistent} and \code{badspecies}, the function weights down species that are thought to be diachronic (\code{bad} and \code{weight.bad} needed) or with inconsistent ranges(\code{inconsistent} and \code{weight.inconsistant} needed). For \code{badevent}, the function weights down an a priori diachronic event (\code{bad} and \code{weight.bad} needed). For \code{pacman} (\code{paclist}, \code{}), the function weights down a sample according to a pacman profiling (Lazarus et al. 2012).

}
  \item{bad}{
For \code{badspecies} method, a vector of species name to weight down. For \code{badevent} method, a two-column matrix: first column is the name of the species for which the FO will be weight down, second column is the name of the species for which the LO is desired to be weighted down.
}
  \item{weight.bad}{
Weight (from 0 to 1) for \code{badspecies} and \code{badevent} method.
}
  \item{inconsistent}{
For \code{inconstistent}, a vector of species names to be weighted down.
}
  \item{weight.inconsistent}{
Weight (from 0 to 1) for \code{inconsitent} method.
}
  \item{paclist}{
Output of function \code{pacman}.
}
  \item{abmat}{
For \code{pacman} method, same matrix as used to create \code{paclist}
}
  \item{mbsf}{
Vector of sample depth (same order as the samples in \code{abmat}).
}
  \item{pacman.sample.ratio}{
Ratio of anomalous occurrences per sample above which the function is desired to weight down the sample (numeric value from 0 to 1).
}
  \item{weight.pacman}{
Weight (from 0 to 1) for \code{pacman} method.
}

}

\references{
Lazarus et al. 2012. Pacman profiling: a simple procedure to identify stratigraphic outliers in high-density deep-sea microfossil data. Paleobiology, 38(1): 858-875.
Sadler, 2007. CONOP version 7.43
}
\author{
Johan Renaudie.
}

\seealso{
\code{\link{CONOPinput-class}} and \code{\link{read.config}}.
}
\examples{
	#Here is an example routine to prepare files for CONOP9
    example <- new("CONOPinput")					#Create an empty container
	setwd("~/path/to/datasets/")					#Set the wd to the folder containing the datasets to process
	sites <- c("Morgan Creek","Potomac",...)		#vector of section names
	files <- c("morgan.csv","potomac.csv",...)	    #vector of dataset filenames
	sectfile <- data.frame(seq_along(sites), substring(sites,1,3), seq_along(sites), 		#Create the SectFile
						as.character(sites), rep(1,length(sites)), stringsAsFactors=FALSE)
	colnames(sectfile) <- 1:5
	example@Sectfile <- sectfile
	example@Config <- read.config("/path/to/conop9.cfg")		#No function to create the config file yet, so need to write it on your own.
	l <- list() 					#will contains the LOFO table of each section
	tax <- c()						#will contains the unique, sorted taxa names
	for(i in seq_along(files)){
		temp <- read.table(files[i], sep=",", header=TRUE, check.names=FALSE, row.names=1)
		l[[i]] <- lofo(temp[,-1],temp[,1])		#Assuming the datasets are samples (rows) vs taxa (columns) with column 1 being the depth of the samples.
		tax <- sort(unique(c(rownames(l[[i]]),tax)))
		}
	example <- prep.eventfile(example,tax)
	example <- prep.conop(example,l,relaxed=FALSE,paired=TRUE)
	write(example)
}
\keyword{IO}