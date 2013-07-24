#' @rdname fit_functions
fit.sl <- function(x, q) {
  if (q$xu / q$xm <= 1) return(NA)
  
  eta <- 2 * q$z / log(q$xu / q$xm)
  gamma <- eta * log((q$xu / q$xm - 1) / sqrt(q$xu * q$xm))
  epsilon <- .5 * (q$x2 + q$x3 - q$xm * (q$xu / q$xm + 1) / 
                     (q$xu / q$xm - 1))
  
  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0) return(NA)
  
  if (all(x > epsilon)) {
    return(list(trans  = gamma + eta * log(x - epsilon), 
                params = c(eta, gamma, NA, epsilon, q$z)))
  } else return(NA)
}