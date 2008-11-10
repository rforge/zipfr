\name{BrownSubsets}
\docType{data}
\alias{BrownSubsets}
\alias{BrownAdj}
\alias{BrownAdj.spc}
\alias{BrownAdj.emp.vgc}
\alias{BrownNoun}
\alias{BrownNoun.spc}
\alias{BrownNoun.emp.vgc}
\alias{BrownVer}
\alias{BrownVer.spc}
\alias{BrownVer.emp.vgc}
\alias{BrownImag}
\alias{BrownImag.spc}
\alias{BrownImag.emp.vgc}
\alias{BrownInform}
\alias{BrownInform.spc}
\alias{BrownInform.emp.vgc}
\alias{Brown100k}
\alias{Brown100k.spc}


\title{Brown Corpus Subset Frequency Data (zipfR)}

\description{

  Objects of classes \code{\link{spc}} and \code{\link{vgc}} that
  contain frequency data for various subsets of words from the Brown
  corpus (see Kucera and Francis 1967).

}


\examples{

  data(BrownAdj.spc)
  summary(BrownAdj.spc)

  data(BrownAdj.emp.vgc)
  summary(BrownAdj.emp.vgc)

  data(BrownInform.spc)
  summary(BrownInform.spc)

  data(BrownInform.emp.vgc)
  summary(BrownInform.emp.vgc)

  data(Brown100k.spc)
  summary(Brown100k.spc)

}

\details{

  \code{BrownAdj.spc}, \code{BrownNoun.spc} and \code{BrownVer.spc}
  are frequency spectra of all the Brown corpus words tagged as
  adjectives, nouns and verbs, respectively. \code{BrownAdj.emp.vgc},
  \code{BrownNoun.emp.vgc} and \code{BrownVer.emp.vgc} are the
  corresponding observed vocabulary growth curves (tracking the
  development of \code{V} and \code{V(1)}, like all the files with
  suffix \code{.emp.vgc} below).

  \code{BrownImag.spc} and \code{BrownInform.spc} are frequency
  spectra of the Brown corpus words subdivided into the two main
  stylistic partitions of the corpus, i.e., imaginative and
  informative prose, respectively. \code{BrownImag.emp.vgc} and
  \code{BrownInform.emp.vgc} are the corresponding observed vocabulary
  growth curves.

  \code{Brown100k.spc} is the spectrum of the first 100,000 tokens in
  the Brown (useful, e.g., for extrapolation experiments in which we
  want to estimate a \code{lnre} model on a subset of the data
  available). The corresponding observed growth curve can be easily
  obtained from the one for the whole Brown (\code{Brown.emp.vgc}).

  Notice that we removed numbers and other forms of non-linguistic
  material before collecting any data from the Brown.

}


\seealso{

  The data described in \code{\link{Brown}} pertain to the Brown as a
  whole.
  
}

\references{

  Kucera, H. and Francis, W.N. (1967). \emph{Computational analysis of
  present-day American English}. Brown University Press, Providence.
  
}

%%\examples{
%%}

\keyword{datasets}
