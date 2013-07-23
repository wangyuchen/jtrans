fit.sb <- function(x, q) {
  eta <- q$z / acosh(.5 * sqrt((1 + q$xm / q$xu) * (1 + q$xm / q$xl)))  
  gamma <- eta * asinh((q$xm / q$xl - q$xm / q$xu) * 
                         sqrt((1 + q$xm / q$xu) * 
                                (1 + q$xm / q$xl) - 4) / 
                         (2 * (q$xm^2 / q$xl / q$xu - 1))) 
  lambda <- (q$xm * sqrt(((1 + q$xm / q$xu) * 
                            (1 + q$xm / q$xl) - 2)^2 - 4) / 
               (q$xm^2 / q$xl / q$xu - 1))
  epsilon <- .5 * (q$x2 + q$x3 - lambda + 
                     q$xm * (q$xm / q$xl - q$xm / q$xu) / 
                     (q$xm^2 / q$xl / q$xu - 1))
  
  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 0) 
    return(NA)
  
  if (all(x > epsilon) & all(x < epsilon + lambda)) {
    return(list(trans  = gamma + eta *  log((x - epsilon) / 
                                              (lambda + epsilon - x)),
                params = c(eta, gamma, lambda, epsilon, q$z)))
  } else return(NA)
}