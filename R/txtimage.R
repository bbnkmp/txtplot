# vi:ts=2 et:
txtimage <- function(
  x, width, height,
  alphabet = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
  yaxis = c('up', 'down'), image.transpose = T
) {
  if (image.transpose) x <- t(x)
  if (missing(width)) width <- min(getOption('width'), ncol(x))
  if (missing(height)) height <- min(width * .25, ncol(x))
  if (width != ncol(x) || height != nrow(x)) { # must resample x to specified size
    x <- Mod(fft(fft(x)[1:height, 1:width], inverse = T))
  }
  if (match.arg(yaxis) == 'up') x <- x[height:1,]

  # alphabet could be either a multi-character string or a vector of characters
  if (length(alphabet) == 1) alphabet <- strsplit(alphabet, NULL)[[1]]
  stopifnot(nchar(alphabet) == 1)

  indices <- (x - min(x, na.rm = T))/diff(range(x, na.rm = T)) # \in [0;1]
  indices <- 1 + indices * (length(alphabet) - 1) # \in [1; length(alphabet)]
  indices[!is.finite(indices)] <- NA # in case we got NaNs

  txt <- structure(alphabet[indices], dim = dim(x))
  txt[is.na(txt)] <- ' ' # space is a good substitute for NAs in this context

  cat(t(cbind(txt, '\n')), sep = '')
  invisible(txt) # in case you need it for something
}
