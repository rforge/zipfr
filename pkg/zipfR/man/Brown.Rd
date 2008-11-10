\name{Brown}
\docType{data}
\alias{Brown}
\alias{Brown.tfl}
\alias{Brown.spc}
\alias{Brown.emp.vgc}

\title{Brown Corpus Frequency Data (zipfR)}

\description{

  \code{Brown.tfl}, \code{Brown.spc} and \code{Brown.emp.vgc} are
  \code{\link{zipfR}} objects of classes \code{\link{tfl}},
  \code{\link{spc}} and \code{\link{vgc}}, respectively.

  These data were extracted from the Brown corpus (see Kucera and
  Francis 1967).
}


\examples{

  data(Brown.tfl)
  summary(Brown.tfl)

  data(Brown.spc)
  summary(Brown.spc)

  data(Brown.emp.vgc)
  summary(Brown.emp.vgc)

}

\details{

  \code{Brown.emp.vgc} is the \emph{empirical} vocabulary growth
  curve, reflecting the \code{V} and \code{V(1)} development in the
  non-randomized corpus.

  We removed numbers and other forms of non-linguistic material before
  collecting word counts from the Brown.

}

\seealso{

  The datasets documented in \code{\link{BrownSubsets}} pertain to
  various subsets of the Brown (e.g., informative prose, adjectives
  only, etc.)
  
}

\references{

  Kucera, H. and Francis, W.N. (1967). \emph{Computational analysis of
  present-day American English}. Brown University Press, Providence.
  
}


%%\examples{
%%}

\keyword{datasets}


