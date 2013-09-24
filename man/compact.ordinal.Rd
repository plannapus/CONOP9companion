\name{compact.ordinal}
\alias{compact.ordinal}

\title{
Compact the ordinal composite sequence.
}
\description{
Take the ordinal composite sequence of events and compact it so that events that are not differentiable temporally are projected to the same level.
}
\usage{
compact.ordinal(CONOP)
}

\arguments{
  \item{CONOP}{An object of class \code{CONOP}, typically the output of function \code{\link{read.CONOP}}.}
}

\value{
The modified object of class \code{CONOP}.
}

\author{
Johan Renaudie.
}

\keyword{misc}