% vi:ts=2 et:
\name{txtimage}
\alias{txtimage}
\title{Display a Text Image of a Matrix}
\description{
%% TODO: ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  txtimage(x, width, height, alphabet, yaxis, image.transpose)
}
\arguments{
  \item{x}{numeric matrix containing values to be plotted (NAs are allowed)}
  \item{width}{desired width in characters, defaults to full screen}
  \item{height}{desired height in characters, defaults to .25 of width}
  \item{alphabet}{
    symbols to compose the plot of, linear scale. NAs become spaces.
    Either a single multi-character string or a vector of single-character strings.
  }
  \item{yaxis}{
    direction of the Y axis, defaults to 'up' like in \code{\link{image}}
  }
  \item{image.transpose}{
    whether to draw rows of the matrix horizontally, like \code{\link{image}} does
  }
}
\details{
  %TODO: make sure to mention fft
}
\value{
  The function is called for its side effect of printing the textual
  plot on the R standard output using \code{\link{cat}}, but it also
  invisibly returns the resulting character matrix.
}
\author{
  Ivan Krylov
}
\seealso{
  \code{\link{image}}, \code{\link{loess}}
}
\examples{
  txtimage(datasets::volcano)
}
\keyword{hplot}
\keyword{print}
