\name{lnre.bootstrap}
\alias{lnre.bootstrap}
\title{Parametric bootstrapping for LNRE models (zipfR)}

\description{

  This function implements parametric bootstrapping for LNRE models, i.e. it draws a specified number of random samples from the population described by a given \code{lnre} object.  For each sample, two callback functions are applied to perform transformations and/or extract statistics.  In an important application (bootstrapped confidence intervals for model parameters), the first callback estimates a new LNRE model and the second callback extracts the relevant parameters from this model.  See \sQuote{Use Cases} and \sQuote{Examples} below for other use cases.
  
}

\usage{
lnre.bootstrap(model, N, ESTIMATOR, STATISTIC, 
               replicates=100, sample=c("spc", "tfl", "tokens"),
               simplify=TRUE, verbose=TRUE, parallel=1L, seed=NULL, \dots)
}

\arguments{

  \item{model}{a trained LNRE model, i.e. an object belonging to a subclass of \code{lnre}.  The model must provide a \code{\link{rlnre}} method to generate random samples from the underlying frequency distribution.}

  \item{N}{a single positive integer, specifying the size \eqn{N} (i.e. token count) of the individual bootstrap samples}

  \item{ESTIMATOR}{
    a callback function, normally used for estimating LNRE models in the bootstrap procedure.  It is called once for each bootstrap sample with the sample as first argument (in the form determined by \code{sample}).  Additional arguments (\code{\dots}) are passed on to the callback, so it is possible to use \code{ESTIMATOR=lnre} with appropriate settings.  If this step is not needed, set \code{ESTIMATOR=identity} to pass samples through to the \code{STATISTIC} callback.
  }

  \item{STATISTIC}{
    a callback function, normally used to extract model parameters and other relevant statistics from the bootstrapped LNRE models.  It is called once for each  bootstrap sample, with the value returned by \code{ESTIMATOR} as its single argument.  The return values are automatically aggregated across all bootstrap samples (see \sQuote{Value} below).  If this step is not needed, set \code{STATISTIC=identity} in order to pass through the results of the \code{ESTIMATOR} callback.  Note that \code{STATISTIC} \bold{must not} return \code{NULL}, which is used internally to signal errors.
  }

  \item{replicates}{a single positive integer, specifying the number of bootstrap samples to be generated}
  
  \item{sample}{
    the form in which each sample is passed to \code{ESTIMATOR}: as a frequency spectrum (\code{spc}, the default), as a type-frequency list (\code{tfl}) or as a factor vector representing the token sequence (\code{tokens}). \bold{Warning:} The latter can be computationally expensive for large \code{N}.
    
    Alternatively, a callback function that will be invoked with arguments \code{model} and \code{replicates} and must return a random sample in the format expected by \code{ESTIMATOR}.  See \sQuote{Use Cases} below for typical applications.
  }

  \item{simplify}{if \code{TRUE}, use \code{rbind()} to combine list of results into a single data structure. In this case, the estimator should return either a vector of fixed length or a single-row data frame or matrix.  No validation is carried out before attempting the simplification.}

  \item{verbose}{if \code{TRUE}, show progress bar in R console during the bootstrapping process (which can take a long time). The progress bar may be updated quite infrequently if parallel processing is enabled.}

  \item{parallel}{whether to enable parallel processing. Either an integer specifying the number of worker processes to be forked, or a pre-initialised \pkg{snow} cluster created with \code{\link{makeCluster}}; see \sQuote{Details} below.}

  \item{seed}{a single integer value used to initialize the RNG in order to generate reproducible results}
  
  \item{\dots}{any further arguments are passed through to the \code{ESTIMATOR} callback function}

}

\value{

  If \code{simplify=FALSE}, a list of length \code{replicates} containing the statistics obtained from each individual bootstrap sample.  In addition, the following attributes are set:
  
  \itemize{
    \item \code{N} = sample size of the bootstrap replicates
    \item \code{model} = the LNRE model from which samples were generated
    \item \code{errors} = number of samples for which either the \code{ESTIMATOR} or the \code{STATISTIC} callback produced an error
  }
  
  If \code{simplify=TRUE}, the statistics are combined with \code{rbind()}.  This is performed unconditionally, so make sure that \code{STATISTIC} returns a suitable value for all samples, typically vectors of the same length or single-row data frames with the same columns.
  The return value is usually a matrix or data frame with \code{replicates} rows.  No additional attributes are set.

}

\details{
  
  The parametric bootstrapping procedure works as follows:
  
  \enumerate{
    \item \code{replicates} random samples of \code{N} tokens each are drawn from the population described by the LNRE model \code{model} (possibly using a callback function provided in argument \code{sample})
    
    \item Each sample is passed to the callback function \code{ESTIMATOR} in the form determined by \code{sample} (a frequency spectrum, type-frequency list, or factor vector of tokens). If \code{ESTIMATOR} fails, it is re-run with a different sample, otherwise the return value is passed on to \code{STATISTIC}.  Use \code{ESTIMATOR=identity} to pass the original sample through to \code{STATISTIC}.
    
    \item The callback function \code{STATISTIC} is used to extract relevant information for each sample. If \code{STATISTIC} fails, the procedure is repeated from step 2 with a different sample.  The callback will typically return a vector of fixed length or a single-row data frame, and the results for all bootstrap samples are combined into a matrix or data frame if \code{simplify=TRUE}.
  }
  
  \bold{Warning:} Keep in mind that sampling a token vector can be slow and consume large amounts of memory for very large \code{N} (several million tokens). If possible, use \code{sample="spc"} or \code{sample="tfl"}, which can be generated more efficiently.

  \bold{Parallelisation}
  
  Since bootstrapping is a computationally expensive procedure, it is usually desirable to use parallel processing.  \code{lnre.bootstrap} supports two types of parallelisation, based on the \pkg{parallel} package:
  \itemize{
    \item On Unix platforms, you can set \code{parallel} to an integer number in order to fork the specified number of worker processes, utilising multiple cores on the same machine.  The \code{\link{detectCores}} function shows how many cores are available, but due to hyperthreading and memory contention, it is often better to set \code{parallel} to a smaller value.  Note that forking may be unstable especially in a GUI environment, as explained on the \code{mcfork} manpage.
    
    \item On all platforms, you can pass a pre-initialised \pkg{snow} cluster in the \code{argument}, which consists of worker processes on the same machine or on different machines.  A suitable cluster can be created with \code{\link{makeCluster}}; see the \pkg{parallel} package documentation for further information.  It is your responsibility to set up the cluster so that all required data sets, packages and custom functions are available on the worker processes; \code{lnre.bootstrap} will only ensure that the \pkg{zipfR} package itself is loaded.
  }

  Note that parallel processing is not enabled by default and will only be used if \code{parallel} is set accordingly.

}

\section{Use cases}{
  \describe{
  
    \item{Bootstrapped \bold{confidence intervals for model parameters}:}{
      The \code{\link[=confint.lnre]{confint}} method for LNRE models uses bootstrapping to estimate confidence intervals for the model parameters.
      
      For this application, \code{ESTIMATOR=lnre} re-estimates the LNRE model from each bootstrap sample. Configuration options such as the model type, cost function, etc. are passed as additional arguments in \code{\dots}, and the sample must be provided in the form of a frequency spectrum. The return values are successfully estimated LNRE models.
      
      \code{STATISTIC} extracts the model parameters and other coefficients of interest (such as the population diversity \code{S}) from each model and returns them as a named vector or single-row data frame.  The results are combined with \code{simplify=TRUE}, then empirical confidence intervals are determined for each column.
    }
    
    \item{Empirical \bold{sampling distribution of productivity measures}:}{
      For some of the more complex measures of productivity and lexical richness (see \code{\link{productivity.measures}}), it is difficult to estimate the sampling distribution mathematically.  In these cases, an empirical approximation can be obtained by parametric bootstrapping.
      
      The most convenient approach is to set \code{ESTIMATOR=productivity.measures}, so the desired measures can be passed as an additional argument \code{measures=} to \code{lnre.bootstrap}. The default \code{sample="spc"} is appropriate for most measures and is efficient enough to carry out the procedure for multiple sample sizes.
      
      Since the estimator already returns the required statistics for each sample in a suitable format, set \code{STATISTIC=identity} and \code{simplify=TRUE}.
    }
    
    \item{Empirical \bold{prediction intervals for vocabulary growth curves}:}{
      Vocabulary growth curves can only be generated from token vectors, so set \code{sample="tokens"} and keep \code{N} reasonably small.
      
      \code{ESTIMATOR=vec2vgc} compiles \code{vgc} objects for the samples. Pass \code{steps} or \code{stepsize} as desired and set \code{m.max} if growth curves for \eqn{V_1, V_2, \ldots} are desired.
      
      \emph{Either} use \code{STATISTIC=identity} and \code{simplify=FALSE} to return a list of \code{vgc} objects, which can be plotted or processed further with \code{sapply()}. This strategy is particulary useful if one or more \eqn{V_m} are desired in addition to \eqn{V}.
      
      \emph{Or} use \code{STATISTIC=function (x) x$V} to extract y-coordinates for the growth curve and combine them into a matrix with \code{simplify=TRUE}, so that prediction intervals can be computed directly.  Note that the corresponding x-coordinates are not returned and have to be inferred from \code{N} and \code{stepsize}.
    }
  
    \item{Simulating \bold{non-randomness} and \bold{mixture distributions}:}{
      More complex populations and non-random samples can be simulated by providing a user callback function in the \code{sample} argument.  This callback is invoked with parameters \code{model} and \code{n} and has to return a sample of size \code{n} in the format expected by \code{ESTIMATOR}.
      
      For simulating non-randomness, the callback will typically use \code{rlnre} to generate a random sample and then apply some transformation.
      
      For simulating mixture distributions, it will typically generate multiple samples from different populations and merge them; the proportion of tokens from each population should be determined by a multinomial random variable.  Individual populations might consist of LNRE models, or a finite number of \dQuote{lexicalised} types.  Note that only a single LNRE model will be passed to the callback; any other parameters have to be injected as bound variables in a local function definition.
    }
  
  }
}

\seealso{

  \code{\link{lnre}} for more information about LNRE models.  The high-level estimator function \code{\link{lnre}} uses \code{lnre.bootstrap} to collect data for approximate confidence intervals.
  
}

\keyword{ models }
\keyword{ distribution }
  
\examples{

## TODO

}

