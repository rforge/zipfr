\name{Dickens}
\docType{data}
\alias{Dickens}
\alias{Dickens.spc}
\alias{Dickens.emp.vgc}
\alias{DickensOliverTwist}
\alias{DickensOliverTwist.spc}
\alias{DickensOliverTwist.emp.vgc}
\alias{DickensGreatExpectations}
\alias{DickensGreatExpectations.spc}
\alias{DickensGreatExpectations.emp.vgc}
\alias{DickensOurMutualFriend}
\alias{DickensOurMutualFriend.spc}
\alias{DickensOurMutualFriend.emp.vgc}


\title{Dickens' Frequency Data (zipfR)}

\description{

  Objects of classes \code{\link{spc}} and \code{\link{vgc}} that
  contain frequency data for a collection of Dickens's works from
  Project Gutenberg, and for 3 novels (Oliver Twist, Great
  Expectations and Our Mutual Friends).

}


\examples{

  data(Dickens.spc)
  summary(Dickens.spc)

  data(Dickens.emp.vgc)
  summary(Dickens.emp.vgc)

  data(DickensOliverTwist.spc)
  summary(DickensOliverTwist.spc)

  data(DickensOliverTwist.emp.vgc)
  summary(DickensOliverTwist.emp.vgc)

}

\details{

  \code{Dickens.spc} has a frequency spectrum derived from a
  collection of Dickens' works downloaded from the Gutenberg archive
  (A Christmas Carol, David Copperfield, Dombey and Son, Great
  Expectations, Hard Times, Master Humphrey's Clock, Nicholas
  Nickleby, Oliver Twist, Our Mutual Friend, Sketches by BOZ, A Tale
  of Two Cities, The Old Curiosity Shop, The Pickwick Papers, Three
  Ghost Stories). \code{Dickens.emp.vgc} contains the corresponding
  observed vocabulary growth (\code{V} and \code{V(1)}).

  \code{DickensOliverTwist.spc} and \code{DickensOliverTwist.emp.vgc}
  contain spectrum and observed growth curve (\code{V} and \code{V(1)}
  of the early novel Oliver Twist (1837-1839).

  \code{DickensGreatExpectations.spc} and
  \code{DickensGreatExpectations.emp.vgc} contain spectrum and
  observed growth curve (\code{V} and \code{V(1)}) of the late novel
  Great Expectations (1860-1861).


  \code{DickensOurMutualFriend.spc} and
  \code{DickensOurMutualFriend.emp.vgc} contain spectrum and observed
  growth curve (\code{V} and \code{V(1)}) of Our Mutual Friend, the
  last novel completed by Dickens (1864-1865).

  Notice that we removed numbers and other forms of non-linguistic
  material before collecting the frequency data.

}


\references{

	Project Gutenberg: \url{http://www.gutenberg.org/}

	Charles Dickens on Wikipedia: \url{http://en.wikipedia.org/wiki/Charles_Dickens}

  
}

%%\examples{
%%}

\keyword{datasets}
