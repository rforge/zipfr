\name{Baayen2001}
\docType{data}
\alias{Baayen2001}
\encoding{UTF-8}

\title{Frequency Spectra from Baayen (2001) (zipfR)}

\description{

  Frequency spectra included as examples in Baayen (2001).
  
}

\usage{

Baayen2001

}

\format{

  A list of 23 frequency spectra, i.e. objects of class \code{\link{spc}}.
  List elements are named according to the original files, but without the extension \code{.spc}.
  See Baayen (2001, pp. 249-277) for details.

  In particular, the following spectra are included:

  \describe{
    \item{\code{alice}:}{Lewis Carroll, \emph{Alice's Adventures in Wonderland}}
    \item{\code{through}:}{Lewis Carroll, \emph{Through the Looking-Glass and What Alice Found There}}
    \item{\code{war}:}{H. G. Wells, \emph{War of the Worlds}}
    \item{\code{hound}:}{Arthur Conan-Doyle, \emph{Hound of the Baskervilles}}
    \item{\code{havelaar}:}{E. Douwes Dekker, \emph{Max Havelaar}}
    \item{\code{turkish}:}{An archeology text (Turkish)}
    \item{\code{estonian}:}{A. H. Tammsaare, \emph{Truth and Justice} (Estonian)}
    \item{\code{bnc}:}{The context-governed subcorpus of the British National Corpus (BNC)}
    \item{\code{in1}:}{Sample of 1 million tokens from \emph{The Independent}}
    \item{\code{in8}:}{Sample of 8 million tokens from \emph{The Independent}}
    \item{\code{heid}:}{Nouns in \emph{-heid} in the CELEX database (Dutch)}
    \item{\code{iteit}:}{Nouns in \emph{-iteit} in the CELEX database (Dutch)}
    \item{\code{ster}:}{Nouns in \emph{-ster} in the CELEX database (Dutch)}
    \item{\code{in}:}{Nouns in \emph{-in} in the CELEX database (Dutch)}
    \item{\code{nouns}:}{Simplex nouns in the CELEX database (Dutch)}
    \item{\code{sing}:}{Singular nouns in M. Innes, \emph{The Bloody Wood}}
    \item{\code{plur}:}{Plural nouns in M. Innes, \emph{The Bloody Wood}}
    \item{\code{nessw}:}{Nouns in \emph{-ness} in the written subcorpus of the BNC}
    \item{\code{nesscg}:}{Nouns in \emph{-ness} in the context-governed subcorpus of the BNC}
    \item{\code{nessd}:}{Nouns in \emph{-ness} in the demographic subcorpus of the BNC}
    \item{\code{filarial}:}{Counts of filarial worms in mites on rats}
    \item{\code{cv}:}{Context-vowel patterns in the TIMIT speech database}
    \item{\code{pairs}:}{Word pairs in E. Douwes Dekker, \emph{Max Havelaar}}
  }
}

\references{

  Baayen, R. Harald (2001). \emph{Word Frequency Distributions.} Kluwer,
  Dordrecht.
  
}

\examples{

Baayen2001$alice

}

\keyword{datasets}
