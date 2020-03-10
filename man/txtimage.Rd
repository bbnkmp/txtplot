% vi:ts=2 et:
\encoding{UTF-8}
\name{txtimage}
\alias{txtimage}
\title{Display a Text Image of a Matrix}
\description{
  Print a rudimentary image of a matrix on the R console using a
  user-supplied alphabet as a \dQuote{palette} of sorts.
}
\usage{
  txtimage(z, width, height, yaxis, image.transpose, na.char, alphabet)
}
\arguments{
  \item{z}{
    numeric matrix containing values to be plotted (NAs are allowed)
  }
  \item{width}{
    desired width in characters, defaults to full screen or number of
    columns in \code{z}, whichever is less
  }
  \item{height}{
    desired height in characters, defaults to 25/80 of width or number
    of rows in \code{z}, whichever is less
  }
  \item{yaxis}{
    direction of the Y axis, defaults to 'up' like in \code{\link{image}}
  }
  \item{image.transpose}{
    whether to draw rows of the matrix horizontally, like \code{\link{image}} does
  }
  \item{na.char}{
    character to substitute for NAs (if any)
  }
  \item{alphabet}{
    symbols to compose the plot of, linear scale. Either a single
    multi-character string or a vector of single-character strings.
    NAs are separately substituted for \code{na.char}.
  }
}
\details{
  By default, \code{\link{txtplot}} mimics the behaviour of
  \code{\link{image}}, drawing the rows of the matrix along the x axis
  and making the y values grow from bottom to top of the plot. The
  function can be made to mimic matrix \code{\link{print}} instead (rows
  arranged vertically from top to bottom) by specifying \code{yaxis}
  and \code{image.transpose} arguments.

  If requested \code{width} or \code{height} is less than dimensions of
  the matrix, it is resampled using \code{\link{fft}} by decimating the
  unnecessary high frequencies in the frequency domain and performing
  the inverse transformation.
}
\value{
  The function is called for its side effect of printing the textual
  plot on the R console using \code{\link{cat}}, but it also invisibly
  returns the resulting character matrix.
}
\author{
  Ivan Krylov
}
\seealso{
  \code{\link{image}}
}
\examples{
  txtimage(datasets::volcano)
  \dontshow{ # FIXME: LaTeX vs Unicode
    txtimage(datasets::volcano, alphabet = ' ░▒▓█')
  }
}
\keyword{hplot}
\keyword{print}
