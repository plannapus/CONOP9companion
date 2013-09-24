\name{lofo}
\alias{lofo}
\title{
Bioevents
}

\description{
Find the FO and LO for each species of the dataset on the studied site.
}

\usage{
lofo(site, mbsf, remove.na = TRUE)
}

\arguments{
 \item{site}{
Sample by Taxa matrix (samples as rows, taxa as columns)
}
  \item{mbsf}{
Vector of sample depth (or height, or age if known).
}
  \item{remove.na}{
If the species is contained in the last sample or in the first sample, remove.na=TRUE will output the depth of the last or the first sample as its LO or FO.
}
}

\value{
Output is a matrix with one row for each taxon concerned.  
Two columns: LO (Last Occurrence) and FO (First Occurrence).
}

\author{
Johan Renaudie.
}
\keyword{misc}
