# vi:ts=2 et:

.Lanczos <- function(x, a) {
  # sinc(x) * sinc(x/a) clamped at +/- a
  ifelse(
    x == 0, 1,
    (abs(x) < a) * sin(pi * x) * sin(pi * x / a) / (pi^2 * x^2 / a)
  )
}

.resample <- function(z, width, height, a) {
  # L(x,y) = L(x) * L(y) for every combination of pixels
  kernels <- Matrix::Matrix(vapply(
    seq(1, width),
    # the Lanczos kernel argument is in terms of new coordinates
    function(j) .Lanczos(j - 1 - (seq(1, ncol(z)) - 1) * (width - 1) / (ncol(z) - 1), a),
    numeric(ncol(z))
  ), sparse = TRUE) %x% Matrix::Matrix(vapply(
    seq(1, height),
    function(i) .Lanczos(i - 1 - (seq(1, nrow(z)) - 1) * (height - 1) / (nrow(z) - 1), a),
    numeric(nrow(z))
  ), sparse = TRUE)
  ret <- matrix(
    replace(as.vector(z), is.na(z), 0) %*% kernels /
      # normalise to alpha channel to avoid darkening
      Matrix::colSums(kernels),
    height, width
  )
  # anything touched by a missing pixel with a nonzero weight must be missing
  is.na(ret) <- as.vector(0 < as.vector(is.na(z)) %*% (kernels != 0))
  ret
}

txtimage <- function(
  z, width, height, yaxis = c('up', 'down'), transpose = TRUE,
  legend = TRUE, na.char = ' ', alphabet = 0:9, Lanczos = 3
) {
  # check for +/- Inf before performing any computations involving range()
  stopifnot(!is.infinite(z))
  # the kernel parameter of must be a nonnegative integer
  stopifnot(length(Lanczos) == 1, Lanczos > 0, Lanczos == round(Lanczos))

  # alphabet could be either a multi-character string or a vector of characters
  if (length(alphabet) == 1) alphabet <- strsplit(alphabet, NULL)[[1]]
  stopifnot(nchar(alphabet) == 1)

  yaxis <- match.arg(yaxis)

  if (transpose) z <- t(z)
  if (missing(width)) width <- min(getOption('width'), ncol(z))
  stopifnot(width <= ncol(z))
  if (missing(height)) height <- min(getOption('width') * 25 / 80, nrow(z))
  stopifnot(height <= nrow(z))

  zrange <- diff(range(z, na.rm = TRUE))

  if (width != ncol(z) || height != nrow(z)) { # must resample z to specified size
    z <- .resample(z, width, height, Lanczos)
    # Downsampling should not increase the range of z values, but when
    # resampling nearly constant matrices, rounding noise may increase it.
    # Solution: save the original zrange and use the updated range only if
    # it becomes narrower.
    if ((rzrange <- diff(range(z, na.rm = TRUE))) <= zrange)
      zrange <- rzrange
  }

  if (yaxis == 'up') z <- z[height:1,]

  indices <- if (zrange == 0) { # handle z = const
    rep(ceiling(length(alphabet) / 2), length(z))
  } else {
    ceiling((z - min(z, na.rm = TRUE))/zrange * length(alphabet))
  }
  # NB: we have rescaled to [0; length(alphabet)], but the only zeroes correspond to
  # points exactly equal to min(z). Let's manually reassign them to the lowest alphabet character.
  indices[indices == 0] <- 1

  if (na.char %in% alphabet && any(is.na(indices)))
    warning("NAs indistinguishable from values in the plot")

  txt <- structure(alphabet[indices], dim = dim(z))
  txt[is.na(txt)] <- na.char

  cat(t(cbind(txt, '\n')), sep = '')

  if (legend) {
    cuts <- seq(min(z, na.rm = TRUE), max(z, na.rm = TRUE), length.out = length(alphabet)+1)
    attr(txt, 'cuts') <- cuts
    attr(txt, 'alphabet') <- alphabet
    # Round cuts to just enough decimal places to see a difference between the values
    if (cuts[2] > cuts[1]) cuts <- round(cuts, 1-floor(log10(diff(cuts[1:2]))))
    cat('\n', cuts[1], paste(sQuote(alphabet), cuts[-1]), '\n')
  }

  invisible(txt) # in case you need it for something
}
