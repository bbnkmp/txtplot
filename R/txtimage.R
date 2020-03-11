# vi:ts=2 et:
txtimage <- function(
  z, width, height, yaxis = c('up', 'down'), transpose = TRUE, na.char = ' ',
  alphabet = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
) {
  stopifnot(!is.infinite(z)) # check for +/- Inf before performing any computations involving range()

  # alphabet could be either a multi-character string or a vector of characters
  if (length(alphabet) == 1) alphabet <- strsplit(alphabet, NULL)[[1]]
  stopifnot(nchar(alphabet) == 1)

  if (transpose) z <- t(z)
  if (missing(width)) width <- min(getOption('width'), ncol(z))
  if (missing(height)) height <- min(getOption('width') * 25 / 80, nrow(z))

  if (width != ncol(z) || height != nrow(z)) { # must resample z to specified size
    z <- Mod(fft(fft(z)[1:height, 1:width], inverse = TRUE))
  }

  if (match.arg(yaxis) == 'up') z <- z[height:1,]

  indices <- (z - min(z, na.rm = TRUE))/diff(range(z, na.rm = TRUE)) # \in [0;1]
  indices <- 1 + indices * (length(alphabet) - 1) # \in [1; length(alphabet)]

  if (na.char %in% alphabet && any(is.na(indices)))
    warning("NAs indistinguishable from values in the plot")

  txt <- structure(alphabet[indices], dim = dim(z))
  txt[is.na(txt)] <- na.char

  cat(t(cbind(txt, '\n')), sep = '')
  invisible(txt) # in case you need it for something
}
