#-------------------------------------------------------------------------------
# Cantor pairing function (pairing on non-negative integers)
# https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function

#' Uniquely encode two non-negative integers to a single non-negative integer.
#'
#' This function uniquely encodes two non-negative integers to a single
#' non-negative integer, using the Cantor pairing function. See the \href{https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function}{Wikipedia article}
#' for more information.
#' @param x A non-negative integer.
#' @param y A non-negative integer.
#' @return A non-negative integer.
#' @export
#' @examples
#' cantor_pairing(0, 0)
#' cantor_pairing(47, 32)
cantor_pairing <- function(x, y) {
  if (x %% 1 != 0 || y %% 1 != 0) {
    stop("x and y must be integers.")
  }
  if (any(c(x, y) < 0)) {
    stop("x and y must be non-negative.")
  }
  .cantor_pairing(x, y)
}

.cantor_pairing <- function(x, y) {
  (x + y) * (x + y + 1) / 2 + y
}


#' Invert the Cantor pairing function.
#'
#' This function is the inverse to the Cantor pairing function. See the \href{https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function}{Wikipedia article}
#' for more information.
#' @param z A non-negative integer
#' @return A vector of non-negative integers \code{(x, y)} such that
#'    \code{cantor_pairing(x, y) == z}.
#' @export
#' @examples
#' inverse_cantor_pairing(0)
#' inverse_cantor_pairing(3192)
inverse_cantor_pairing <- function(z) {
  if (z %% 1 != 0) {
    stop("z must be an integer.")
  }
  if (z < 0) {
    stop("z must be non-negative.")
  }
  .inverse_cantor_pairing(z)
}

.inverse_cantor_pairing <- function(z) {
  w <- floor((sqrt(8 * z + 1) - 1) / 2)
  t <- w * (w + 1) / 2
  c(w - z + t, z - t)
}

#-------------------------------------------------------------------------------
# Hopcroft and Ullman pairing function (pairing on positive integers)
# http://mathworld.wolfram.com/PairingFunction.html

#' Uniquely encode two positive integers to a single non-negative integer.
#'
#' This function uniquely encodes two positive integers to a single non-negative
#' integer, using the Hopcrof-Ullman pairing function.
#' See the \href{http://mathworld.wolfram.com/PairingFunction.html}{MathWorld article}
#' for more information.
#' @param x A positive integer.
#' @param y A positive integer.
#' @return A positive integer.
#' @export
#' @examples
#' hu_pairing(1, 1)
hu_pairing <- function(x, y) {
  if (x %% 1 != 0 || y %% 1 != 0) {
    stop("x and y must be integers.")
  }
  if (any(c(x, y) < 1)) {
    stop("x and y must be positive.")
  }
  .hu_pairing(x, y)
}

.hu_pairing <- function(x, y) {
  cantor_pairing(x - 1, y - 1) + 1
}


#' Invert the Hopcroft-Ullman pairing function.
#'
#' This function is the inverse to the Hopcroft-Ullman pairing function.
#' See the \href{http://mathworld.wolfram.com/PairingFunction.html}{MathWorld article}
#' for more information.
#' @param z A non-negative integer
#' @return A vector of non-negative integers \code{(x, y)} such that
#'    \code{cantor_pairing(x, y) == z}.
#' @export
#' @examples
#' inverse_hu_pairing(1)
inverse_hu_pairing <- function(z) {
  if (z %% 1 != 0) {
    stop("z must be an integer.")
  }
  if (z < 0) {
    stop("z must be positive.")
  }
  .inverse_hu_pairing(z)
}

.inverse_hu_pairing <- function(z) {
  inverse_cantor_pairing(z - 1) + c(1, 1)
}
