\name{Tiger}
\docType{data}
\alias{Tiger}
\alias{TigerNP}
\alias{TigerNP.tfl}
\alias{TigerNP.spc}
\alias{TigerNP.emp.vgc}
\alias{TigerPP}
\alias{TigerPP.tfl}
\alias{TigerPP.spc}
\alias{TigerPP.emp.vgc}



\title{Tiger NP and PP expansions (zipfR)}

\description{

  Objects of classes \code{\link{tfl}}, \code{\link{spc}} and
  \code{\link{vgc}} that contain frequency data for the syntactic
  expansions of Noun Phrases (NP) and Prepositional Phrases (PP) in
  the Tiger German treebank.

}


\examples{

  data(TigerNP.tfl)
  summary(TigerNP.tfl)

  data(TigerNP.spc)
  summary(TigerNP.spc)

  data(TigerNP.emp.vgc)
  summary(TigerNP.emp.vgc)

  data(TigerPP.tfl)
  summary(TigerPP.tfl)

  data(TigerPP.spc)
  summary(TigerPP.spc)

  data(TigerPP.emp.vgc)
  summary(TigerPP.emp.vgc)

}

\details{

  In this dataset, types are not words, but syntactic expansions,
  i.e., sequences of syntactic categories that form NPs (in
  \code{TigerNP}) or PPs (in \code{TigerPP}), according to the Tiger
  annotation scheme for German. Thus, for example, among the expansion
  types in the \code{TigerNP} dataset, we find \code{ART_NN} and
  \code{ART_ADJA_NN}, whereas among the PP expansions in
  \code{TigerPP} we find \code{APPR_ART_NN} and \code{APPR_NN}
  (\code{APPR} is the tag for prepositions in the Tiger tagset).

  The Tiger treebank contains about 900,000 tokens (50,000 sentences)
  of German newspaper text from the Frankfurter Rundschau. The token
  frequencies of the expansion types are taken from this corpus.

  \code{TigerNP.tfl} and \code{TigerPP.tfl} are the type frequency
  lists.  \code{TigerNP.spc} and \code{TigerPP.spc} are frequency
  spectra. \code{TigerNP.emp.vgc} and \code{TigerPP.emp.vgc} are the
  corresponding observed vocabulary growth curves (tracking the
  development of \code{V} and \code{V(1)} in the original order of
  occurrence of the expansion tokens in the source corpus).

}

\references{

	Tiger Project:
	\url{http://www.ims.uni-stuttgart.de/projekte/TIGER}
  
}

\keyword{datasets}
