\name{txtplot}
\alias{txtplot}
\alias{txtdensity}
\alias{txtcurve}
\alias{txtacf}
\alias{txtbarchart}
\alias{getRng}
\alias{getTicks}
\alias{insEOL}
\alias{drawLegend}
\alias{checkNA}
\title{
  Ascii graphics
}
\description{
  Provides a function to produce rudimentary ascii graphics
  directly in the terminal window. 
  
  \code{txtplot} provides the basic plotting function of two numeric
  vectors. All other functions below are based on this.\cr
  \code{txtcurve} is a text based equivalent of the \code{curve} function\cr
  \code{txtdensity} is a text based density estimation function based on the
  \code{density} function\cr
  \code{txtacf} is a text based equivalent of the \code{acf} function
  and based on the \code{acf} function.\cr
  \code{txtbarchart} is a text based barplot and plots the
  relative frequences of the occurences of the different levels of a
  factor (in percent)
}
\usage{
txtplot(x, y = NULL, pch = "*", width = round(options()$width*0.8),
        height = round(0.25*width), xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = NULL)

txtcurve(expr, from = NULL, to = NULL, n = 101,
         pch = "*", width = round(options()$width*0.8),
         height = round(0.25*width), xlab = NULL, ylab = NULL)

txtdensity(x, pch = "*", width = round(options()$width*0.8),
        height = round(0.25*width), xlab = NULL, ylab = NULL)

txtacf(x, pch = "*", lag.max = 20, type = c("correlation", "covariance", "partial"),
       na.action = na.fail, demean = TRUE, width = round(options()$width*0.8),
       height = round(0.25*width), xlab = NULL, ylab = NULL)

txtbarchart(x, pch = "*", width = round(options()$width*0.8),
            height = round(0.25*width), ylab = NULL)
}
\arguments{
  \item{x}{
    numeric containing the x-values to plot (for \code{txtbarchart} this
    needs to be of class factor). NA, NaN are removed for
    plotting. Infinities cause an error
  }
  \item{y}{
    numeric containing the x-values to plot (needs to be of the same
    length as x). If NULL the numeric \code{x} is plotted against
    \code{1:length(x)}. NA, NaN are removed for plotting. Infinities
    cause an error
  }
  \item{pch}{
    Plotting symbol
}
  \item{width, height}{
    Width and height of the plots in points
}
  \item{xlab, ylab}{
    labels for x and y axis
  }
  \item{xlim, ylim}{
    limits for x and y axis in plot, if equal to NULL automatically
    determined from x and y.
  }
  \item{expr}{
    An expression to plot (containing x)
  }
  \item{from,to}{
    Defines boundaries of plotting region for expr in txtcurve
  }
  \item{n}{
    integer specifying the number of x values between from and to
  }
  \item{lag.max, type, na.action, demean}{
    arguments for call of \code{acf} function, see ?acf for details
  }
}
\seealso{
  \code{\link{txtboxplot}}
}
  \note{
  Due to rounding to a relatively crude grid results can only be
  approximate! The equally spaced axis ticks, for example, may be
  non-equally spaced in the plot.

  Due to the crude grid also there might be several points per pixel.
  The function uses the same plotting symbol no matter how many points
  coincide on one pixel 
}
\author{
  Bjoern Bornkamp
}

\examples{
  ## basic plotting function
  require(stats)
  txtplot(cars[,1], cars[,2])
  ## can include axis labels when desired
  txtplot(cars[,1], cars[,2], xlab = "speed", ylab = "distance")

  ## text based density plot
  txtdensity(rnorm(500))

  ## text based plotting of functions
  txtcurve(x/(x+1), 0, 4, xlab = "Emax model")

  ## text based acf
  txtacf(rnorm(100))

  ## text based barchart
  x <- factor(c("orange", "orange", "red", "green", "green", "red",
               "yellow", "purple", "purple", "orange"))
  txtbarchart(x)

  ## text based boxplots
  rand1 <- rnorm(100, 1, 2)
  rand2 <- rnorm(50, 2, 2)
  rand3 <- rnorm(50, 2, 5)
  txtboxplot(rand1)
  txtboxplot(rand1, rand2, rand3)

}
\keyword{ hplot }
