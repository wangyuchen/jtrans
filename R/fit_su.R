#' @rdname fit_functions
fit.su <- function (x, q) {
  eta <- 2 * q$z / acosh(.5 * (q$xu / q$xm + q$xl / q$xm))
  gamma <- eta * asinh((q$xl / q$xm - q$xu / q$xm) / 
                         (2 * sqrt(q$xu * q$xl / q$xm^2 - 1)))
  lambda <- (2 * q$xm * sqrt(q$xu * q$xl / q$xm^2 - 1) / 
               (q$xu / q$xm + q$xl / q$xm - 2) / 
               sqrt(q$xu / q$xm + q$xl / q$xm + 2))
  epsilon <- .5 * (q$x2 + q$x3 + q$xm * (q$xl / q$xm - q$xu / q$xm) / 
                     (q$xu / q$xm + q$xl / q$xm - 2))
  
  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 0) 
    return(NA)
  
  return(list(trans  = gamma + eta * asinh((x - epsilon) / lambda),
              params = c(eta, gamma, lambda, epsilon, q$z)))
}