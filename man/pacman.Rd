\name{pacman}
\alias{pacman}

\title{Pacman profiling and trimming}
\description{Flag and trim anomalous occurrences given a certain percentage on an abundance matrix}
\usage{pacman(neptune, perc_top = 5, perc_bottom = 3)}

\arguments{
  \item{ab_mat}{An abundance matrix with samples as rows and taxa as columns.}
  \item{perc_top}{Percentage to trim at the top of the ranges, default to 5\%}
  \item{perc_bottom}{Percentage to trim at the bottom of the ranges, default to 3\%}
}

\value{
The returned value is a list:
\item{Trimming}{Trimmed Matrix}
\item{Profiling}{Vector: number of outliers in each sample.}
}
\references{
Lazarus et al. 2012. Pacman profiling: a simple procedure to identify stratigraphic outliers in high-density deep-sea microfossil data. Paleobiology, 38(1): 858-875.
}
\author{
Lazarus et al. 2012 for the algorithm, Johan Renaudie for the code.
}
\keyword{misc}
