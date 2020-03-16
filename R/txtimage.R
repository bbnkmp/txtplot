# vi:ts=2 et:

.Lanczos <- function(x, a) {
  # sinc(x) * sinc(x/a) clamped at +/- a
  ifelse(
    x == 0, 1,
    (abs(x) < a) * sin(pi * x) * sin(pi * x / a) / (pi^2 * x^2 / a)
  )
}

.resample <- function(z, width, height, a) {
  ret <- matrix(NA, height, width)
  for (i in 1:height) for (j in 1:width) {
    kernel <- tcrossprod(
      # L(x,y) = L(x) * L(y)
      # the Lanczos kernel argument is in terms of new coordinates
      .Lanczos(i - 1 - (1:nrow(z) - 1) * (height - 1) / (nrow(z) - 1), a),
      .Lanczos(j - 1 - (1:ncol(z) - 1) * (width - 1) / (ncol(z) - 1), a)
    )
    # normalise to alpha channel to avoid darkening
    ret[i,j] <- sum(kernel[kernel != 0] * z[kernel != 0]) / sum(kernel)
  }
  ret
}

txtimage <- function(
  z, width, height, yaxis = c('up', 'down'), transpose = TRUE,
  na.char = ' ', alphabet = 0:9, Lanczos = 3
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

  if (width != ncol(z) || height != nrow(z)) # must resample z to specified size
    z <- round(
      .resample(z, width, height, Lanczos),
      digits = ceiling(log10(length(alphabet)))
    )

  if (yaxis == 'up') z <- z[height:1,]

  zrange <- diff(range(z, na.rm = TRUE))
  indices <- if (zrange != 0) {
    ceiling((z - min(z, na.rm = TRUE))/zrange * length(alphabet))
  } else { # handle z = const
    rep(ceiling(length(alphabet) / 2), length(z))
  }
  # NB: we have rescaled to [0; length(alphabet)], but the only zeroes correspond to
  # points exactly equal to min(z). Let's manually reassign them to the lowest alphabet character.
  indices[indices == 0] <- 1

  if (na.char %in% alphabet && any(is.na(indices)))
    warning("NAs indistinguishable from values in the plot")

  txt <- structure(alphabet[indices], dim = dim(z))
  txt[is.na(txt)] <- na.char

  cat(t(cbind(txt, '\n')), sep = '')
  invisible(txt) # in case you need it for something
}
