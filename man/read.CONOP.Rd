\name{read.CONOP}
\alias{read.CONOP}
\alias{read.config}
\alias{read.cpchrt}
\title{
Read the output of CONOP9.
}
\description{
\code{read.CONOP} reads all output of CONOP9 and create an object of class \code{CONOP}.  
\code{read.config} is a function to read the configuration file (both input and output).  
\code{read.cpchrt} is a function to read the CONOP9 additional output file \code{cpchrt}. Needed for \code{\link[Graphs]{culling}}.
}
\usage{
read.CONOP(dir)
read.config(file = file.choose())
read.cpchrt(file = file.choose())
}
\arguments{
  \item{dir}{
Directory of the CONOP9 run to input.
}
  \item{file}{
For \code{read.config}, configuration file (usually CONOP9.CFG); for \code{read.copchrt}, composite ranges file (usually cpcht.txt)
}
}

\value{
An object of class \code{CONOP} for \code{read.CONOP}, a list for \code{read.config} and a matrix, equivalent to that found on the file for \code{read.cpchrt}.
}
\references{
Sadler, 2007. CONOP version 7.43.
}
\author{
Johan Renaudie.
}

\seealso{
\code{\link{CONOP-class}}.
}
\examples{
	download.file("http://geology.usf.edu/faculty/data/harries/CD_2007.zip", "CONOP9.zip")
	unzip("CONOP9.zip")
	riley1 <- read.CONOP("CD_2007/SAMPLES/RILEY/Augmented")
	
	#read.CONOP includes read.config. Here is an example for read.config alone.
	
	riley.config <- read.config("CD_2007/SAMPLES/RILEY/Augmented/CONOP9.CFG")
}
\keyword{IO}
