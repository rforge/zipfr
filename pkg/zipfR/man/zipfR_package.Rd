\name{zipfR-package}
\alias{zipfR-package}
\alias{zipfR}
\encoding{utf8}
\docType{package}
\title{

zipfR: lexical statistics in R

}
\description{

The zipfR package performs Large-Number-of-Rare-Events (LNRE) modeling
of (linguistic) type frequency distributions (Baayen 2001) and
provides utilities to run various forms of lexical statistics analysis
in R.

}
\details{

The best way to get started with zipfR is to read the tutorial, which
you can find as a package vignettte via the HTML documentation;
you can also download it from
\url{https://zipfr.r-forge.r-project.org/#start}

zipfR is released under the GNU General Public License
(\url{http://www.gnu.org/copyleft/gpl.html})

}


\seealso{

  The zipfR tutorial: available as a package vignette and online from
  \url{https://zipfr.r-forge.r-project.org/#start}.

  Some good entry points into the zipfR documentation are
  be \code{\link{spc}}, \code{\link{vgc}}, \code{\link{tfl}},
  \code{\link{read.spc}}, \code{\link{read.tfl}},
  \code{\link{read.vgc}}, \code{\link{lnre}},
  \code{\link{lnre.vgc}}, \code{\link{plot.spc}},
  \code{\link{plot.vgc}}

  Harald Baayen's LEXSTATS tools, which implement a wider range of LNRE models:
  \url{https://www.springer.com/de/book/9780792370178}


  Stefan Evert's UCS tools for collocation analysis, which include
  functions that have been integrated into \bold{zipfR}:
  \url{http://www.collocations.de/software.html}

}

\author{ Stefan Evert <\email{stefan.evert@fau.de}> and Marco Baroni
<\email{marco.baroni@unitn.it}>

Maintainer: Stefan Evert <\email{stefan.evert@fau.de}>
}

\references{

  zipfR Website: \url{https://zipfR.r-forge.r-project.org/}

  Baayen, R. Harald (2001). \emph{Word Frequency Distributions.} Kluwer,
  Dordrecht.

  Baroni, Marco (2008). Distributions in text. In: A. Lüdeling
  and M. Kytö (eds.), \emph{Corpus Linguistics. An
  International Handbook}, article 37. Mouton de Gruyter, Berlin.

  Evert, Stefan (2004). A simple LNRE model for random character
  sequences. \emph{Proceedings of JADT 2004}, 411-422.

  Evert, Stefan (2004b). \emph{The Statistics of Word Cooccurrences: Word
    Pairs and Collocations.} PhD Thesis, IMS, University of Stuttgart.
  URN urn:nbn:de:bsz:93-opus-23714
  \url{http://dx.doi.org/10.18419/opus-2556}
  
  Evert, Stefan and Baroni, Marco (2006). Testing the extrapolation
  quality of word frequency models. \emph{Proceedings of Corpus
  Linguistics 2005}.

  Evert, Stefan and Baroni, Marco (2006). The zipfR library: Words and
  other rare events in R. \emph{useR! 2006: The second R user
  conference}.

}

\keyword{ package }
%% ~~ optionally other keywords from "KEYWORDS.db", one per line ~~

\examples{
## load Oliver Twist and Great Expectations frequency spectra
data(DickensOliverTwist.spc)
data(DickensGreatExpectations.spc)

## check sample size and vocabulary and hapax counts
N(DickensOliverTwist.spc)
V(DickensOliverTwist.spc)
Vm(DickensOliverTwist.spc,1)
N(DickensGreatExpectations.spc)
V(DickensGreatExpectations.spc)
Vm(DickensGreatExpectations.spc,1)

## compute binomially interpolated growth curves
ot.vgc <- vgc.interp(DickensOliverTwist.spc,(1:100)*1570)
ge.vgc <- vgc.interp(DickensGreatExpectations.spc,(1:100)*1865)

## plot them
plot(ot.vgc,ge.vgc,legend=c("Oliver Twist","Great Expectations"))

## load Dickens' works frequency spectrum
data(Dickens.spc)

## compute Zipf-Mandelbrot model from Dickens data
## and look at model summary
zm <- lnre("zm",Dickens.spc)
zm

## plot observed and expected spectrum
zm.spc <- lnre.spc(zm,N(Dickens.spc))
plot(Dickens.spc,zm.spc)

## obtain expected V and V1 values at arbitrary sample sizes
EV(zm,1e+8)
EVm(zm,1,1e+8)

## generate expected V and V1 growth curves up to a sample size
## of 10 million tokens and plot them, with vertical line at 
## estimation size
ext.vgc <- lnre.vgc(zm,(1:100)*1e+5,m.max=1)
plot(ext.vgc,N0=N(zm),add.m=1)


}
