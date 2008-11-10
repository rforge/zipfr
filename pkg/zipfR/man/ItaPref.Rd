\name{ItaPref}
\docType{data}
\alias{ItaPref}
\alias{ItaRi}
\alias{ItaRi.spc}
\alias{ItaRi.emp.vgc}
\alias{ItaUltra}
\alias{ItaUltra.spc}
\alias{ItaUltra.emp.vgc}

\title{Italian Ri- and Ultra- Prefix Frequency Data (zipfR)}

\description{

  \code{ItaRi.spc} and \code{ItaRi.emp.vgc} are \code{\link{zipfR}}
  objects of classes \code{\link{tfl}}, \code{\link{spc}} and
  \code{\link{vgc}}, respectively. They contain frequency data for all
  verbal lemmas with the prefix ri- (similar to English re-) in the
  Italian la Repubblica corpus.

  \code{ItaUltra.spc} and \code{ItaUltra.emp.vgc} contain the same
  kinds of data for the adjectival prefix ultra-.

}


\examples{

  data(ItaRi.spc)
  summary(ItaRi.spc)

  data(ItaRi.emp.vgc)
  summary(ItaRi.emp.vgc)

  data(ItaUltra.spc)
  summary(ItaUltra.spc)

  data(ItaUltra.emp.vgc)
  summary(ItaUltra.emp.vgc)

}

\details{

  \code{ItaRi.emp.vgc} and \code{ItaUltra.emp.vgc} are
  \emph{empirical} vocabulary growth curves, reflecting the \code{V}
  and \code{V(1)} development in the non-randomized corpus.

  The data were manually checked, as described for ri- in Baroni (to
  appear).

}

\references{

	Baroni, M. (to appear) I sensi di ri-: Un'indagine
	preliminare. In Maschi, R., Penello, N. and Rizzolatti,
	P. (eds.), \emph{Miscellanea di studi linguistici offerti a
	Laura Vanelli}. Udine, Forum.

	la Repubblica corpus: \url{http://sslmit.unibo.it/repubblica/}
  
}


\keyword{datasets}


