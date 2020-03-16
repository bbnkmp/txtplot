% vi:ts=2 et:
\name{txtimage}
\alias{txtimage}
\title{Display a Text Image of a Matrix}
\description{
  Print a rudimentary image of a matrix on the R console using a
  user-supplied alphabet as a palette of sorts.
}
\usage{
  txtimage(
    z, width, height, yaxis = c('up', 'down'), transpose = TRUE,
    na.char = ' ', alphabet = c(0:9, letters, LETTERS), Lanczos = 3
  )
}
\arguments{
  \item{z}{
    Numeric matrix containing values to be plotted. \code{NA} and
    \code{NaN} are allowed, but infinities, being impossible to scale,
    cause an error.
  }
  \item{width}{
    Desired width in characters. Defaults to full screen (by means of
    \code{getOption('width')}) or number of columns (rows if transposed)
    in \code{z}, whichever is less.
  }
  \item{height}{
    Desired height in characters. Defaults to \eqn{25/80} of \code{width}
    or number of rows (columns if transposed) in \code{z}, whichever
    is less.
  }
  \item{yaxis}{
    Direction of the Y axis, \code{"up"} or \code{"down"}. Defaults to
    'up' like in \code{image}.
  }
  \item{transpose}{
    Whether to arrange rows by the X axis, like \code{image}
    does. Defaults to \code{TRUE}.
  }
  \item{na.char}{
    Character to substitute for values satisfying \code{is.na}. A warning
    is produced if \code{na.char} is found in the \code{alphabet} in
    presence of \code{NA} in \code{z}.
  }
  \item{alphabet}{
    Symbols to compose the plot of, linear scale. Either a single
    multi-character string or a vector of single-character strings.
    Defaults to \code{c(0:9, letters, LETTERS)}.
  }
  \item{Lanczos}{
    Positive integer defining the size of the Lanczos filter kernel. Given
    a value of \eqn{a}, the windowed sinc kernel will have \eqn{2a-1}
    lobes.
  }
}
\details{
  By default, \code{txtimage} mimics the behaviour of \code{image},
  drawing the rows of the matrix along the X axis and making the Y axis
  grow from bottom to the top of the plot. The function can be made to
  mimic matrix \code{print} instead (rows arranged vertically from top to
  bottom) by specifying \code{yaxis} and \code{image.transpose} arguments.

  If requested \code{width} or \code{height} is different from dimensions
  of the matrix, it is resampled using the Lanczos filter:

  \deqn{%
    L(x) = \mathrm{sinc}(x) \, \mathrm{sinc}(x/a) \, | x < a |
  }{%
    L(x) = sinc(x) sinc(x/a) |x < a|
  }
  \deqn{%
    S_{ij} = \sum_{k, l} s_{kl} L \left( i - \frac{k}{r} \right) L \left(j - \frac{l}{r} \right)
  }{%
    S[i, j] = sum(s[k, l] * tcrossprod(L(i - k/r), L(j - l/r)))
  }

  When resampling, the rows and columns are assumed to correspond to a
  uniform linear grid.
}
\value{
  The function is called for its side effect of printing the textual
  plot on the R console using \code{cat}, but it also invisibly
  returns the resulting character matrix.
}
\author{
  Ivan Krylov
}
\seealso{
  \code{image}
}
\examples{
  txtimage(datasets::volcano)
  \dontrun{
    # try this if your terminal supports shade/block characters
    txtimage(datasets::volcano, alphabet = " \u2591\u2592\u2593\u2588")
  }
}
\keyword{hplot}
