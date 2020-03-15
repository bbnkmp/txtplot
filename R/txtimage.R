# vi:ts=2 et:

.Lanczos <- function(x, a) {
  # sinc(x) * sinc(x/a) clamped at +/- a
  ifelse(x == 0, 1, (abs(x) < a) * sin(pi * x) * sin(pi * x / a) / (pi^2 * x^2 / a))
}

.resample <- function(z, width, height, a) {
  # S(x) = \sum_{i = \floor{x} - a + 1}^{\floor{x} + a} s[i] L(x - i)
  ret <- matrix(NA, height, width)
  for (i in 1:height) for (j in 1:width) {
    # get coordinates of new points in terms of original image indices
    x <- (i - 1) / (height - 1) * (nrow(z) - 1) + 1
    y <- (j - 1) / (width - 1) * (ncol(z) - 1) + 1
    # original image indices to sum the Lanczos kernel over
    # (make sure to clamp values outside the image)
    ii <- max(1, floor(x) - a + 1):min(nrow(z), floor(x) + a)
    jj <- max(1, floor(y) - a + 1):min(ncol(z), floor(y) + a)
    # NB: L(x,y) = L(x) * L(y)
    ret[i,j] <- sum(
      tcrossprod(.Lanczos(x - ii, a), .Lanczos(y - jj, a)) * z[ii, jj]
    )
  }
  ret
}

txtimage <- function(
  z, width, height, yaxis = c('up', 'down'), transpose = TRUE, na.char = ' ',
  alphabet = c(0:9, letters, LETTERS), Lanczos = 3
) {
  stopifnot(!is.infinite(z)) # check for +/- Inf before performing any computations involving range()
  stopifnot(Lanczos > 0, Lanczos == round(Lanczos)) # the parameter of the kernel must be nonnegative integer

  # alphabet could be either a multi-character string or a vector of characters
  if (length(alphabet) == 1) alphabet <- strsplit(alphabet, NULL)[[1]]
  stopifnot(nchar(alphabet) == 1)

  if (transpose) z <- t(z)
  if (missing(width)) width <- min(getOption('width'), ncol(z))
  if (missing(height)) height <- min(getOption('width') * 25 / 80, nrow(z))

  if (width != ncol(z) || height != nrow(z)) # must resample z to specified size
    z <- .resample(z, width, height, Lanczos)

  if (match.arg(yaxis) == 'up') z <- z[height:1,]

  indices <- ceiling((z - min(z, na.rm = TRUE))/diff(range(z, na.rm = TRUE)) * length(alphabet))
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
