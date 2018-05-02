\name{EvertLuedeling2001}
\docType{data}
\alias{EvertLuedeling2001}
\encoding{UTF-8}

\title{Samples of German Word Formation Affixes (zipfR)}

\description{

  Corpus data for measuring the productivity of German word formation affixes
  \emph{-bar}, \emph{-lich}, \emph{-sam}, \emph{-ös}, \emph{-tum},
  \emph{Klein-}, \emph{-chen} and \emph{-lein} (Evert & Lüdeling 2001).
  Data were extracted from two volumes of the German daily newspaper
  \emph{Stuttgarter Zeitung}, then manually cleaned and normalized.

}

\usage{

EvertLuedeling2001

}

\format{

  A list of 8 character vectors for the different affixes, with names
  \code{klein} (\emph{Klein-}), \code{bar} (\emph{-bar}),
  \code{chen} (\emph{-chen}), \code{lein} (\emph{-lein}),
  \code{lich} (\emph{-lich}), \code{oes} (\emph{-ös}),
  \code{sam} (\emph{-sam}), \code{tum} (\emph{-tum}).
  
  Each vector contains all relevant tokens from the corpus in their
  original (chronological) ordering, so vocabulary growth curves can
  be determined from the vectors in addition to type frequency lists
  and frequency spectra.
  
}

\references{

  Evert, Stefan and Lüdeling, Anke (2001).
  Measuring morphological productivity: Is automatic preprocessing sufficient?
  In \emph{Proceedings of the Corpus Linguistics 2001 Conference}, pages 167--175, Lancaster, UK.
  
}

\examples{

str(EvertLuedeling2001)

# tokens and type counts for the different affixes
sapply(EvertLuedeling2001, function (x) {
  y <- vec2tfl(x)
  c(N=N(y), V=V(y))
})

}

\keyword{datasets}
