\name{txtboxplot}
\alias{txtboxplot}
\alias{boxcore}
\title{
  Text based boxplot
}
\description{
  Produces rudimentary ascii boxplots. The boxplot statistics are produced
  using the \code{boxplot.stats} function.
}
\usage{
txtboxplot(..., range = 1.5, legend = NULL, xlab = NULL,
           width = round(options()$width * 0.8))
}
\arguments{
  \item{\dots}{
    Numeric vectors for which a boxplot should be produced
  }
  \item{range}{
    This determines how far the plot whiskers extend out from the
    box. See \code{\link{boxplot.stats}} and the \code{coef} function
    for details.
  }
  \item{legend}{
    Logical determining whether a legend should be drawn.  If
    \code{legend} is NULL there will be a legend drawn in case there is
    more than one boxplot.
  }
  \item{xlab}{
    label for x-axis of boxplot, if NULL no x-label will be plotted
  }
  \item{width}{
    Width of the plot
  }
}
\seealso{
  \code{\link{txtplot}}
}
\author{
  Bjoern Bornkamp
}
\note{
  Due to rounding to a relatively crude grid results can only be
  approximate. E.g. the equally spaced axis ticks may be non-equally
  spaced in the plot.
}
\examples{
  rand1 <- rnorm(100, 1, 2)
  rand2 <- rnorm(50, 2, 2)
  rand3 <- rnorm(50, 2, 5)
  txtboxplot(rand1)
  txtboxplot(rand1, rand2, rand3)
}
\keyword{ hplot }
