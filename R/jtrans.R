#' Johnson Transformation for Normality
#' 
#' \code{jtrans} transforms imported non-normal data to normality.
#' 
#' \code{jtrans} fits imported data to a set of 101 Johnson curves. Then a 
#' normality test is used (Shapiro-Wilk test by default) to find the best fit 
#' that have the maximum p.value under that given test. It returns the 
#' transformed data and the corresponding type of Johnson curve and parameter 
#' estimations.
#' 
#' Since the default Shapiro-Wilk test can only accept sample size with 3 and 
#' 5000, sample size larger than 5000 will meet an error. In such cases, one 
#' should specify another normality test in the test parameter, generally the
#' \code{ad.test} in the \pkg{nortest} package is recommended.
#' 
#' Sometimes, this algorithm may return poor fits. The most extreme case is 
#' that all the transformed data have smaller p.values than the p.value of the 
#' original data. In such cases, one should set \code{exclude.original}
#' to be FALSE, then \code{jtrans} will return the original data as the 
#' transformed data. 
#' 
#' @param x the non-normal numerical data.
#' @param test the normality test used to select fits.
#' @param exclude.original should the original data be excluded when comparing
#'          fits.
#'          
#' @export
#' 
#' @return A list with class "jtrans" containing the following components:
#'  \item{original}{original data.}
#'  \item{transformed}{transformed data.}
#'  \item{type}{type of transformation selected.}
#'  \item{test}{normality test used to select transformations.}
#'  \item{z}{selected z value among 101 values from 0.25 to 1.25.}
#'  \item{eta, gamma, lambda, epsilon}{transformation parameters.}
#'  \item{p.value}{the maximum p.value returned by test}
#'  
#' @references Chou, Y. M., Polansky, A. M., & Mason, R. L. (1998). 
#' Transforming non-normal data to normality in statistical process control. 
#' Journal of Quality Technology, 30(2), 133-141.
#' 
#' @examples
#' # generate 100 non-normal data and transform it.
#' x <- rexp(100, .2)
#' jt <- jtrans(x)
#' jt

jtrans <- function(x, test="shapiro.test", exclude.original=TRUE) {
  # set trans to be the original data, collect original parameters
  x <- as.numeric(x)
  trans <- x
  type <- "original"
  params <- data.frame(eta     = NA, 
                       gamma   = NA, 
                       lambda  = NA, 
                       epsilon = NA,
                       z       = NA,
                       p.value = do.call(test, list(trans))$p.value)
  
  
  # test for transformed dataset corresponding to 101 z value
  for (z in seq(from=0.25, to=1.25, by=0.01)) {
    # calculate quantiles
    q <- qtls(x, z)
    
    # fit sl distribution for every z
    res <- fit.sl(x, q)
    if (!is.na(res[1])) {
      # fit succeeds
      type <- c(type, "SL")
      params <- rbind(params, c(res$params,
                                do.call(test, list(res$trans))$p.value))
      trans <- rbind(trans, res$trans)
    }
    
    # fit either SB or SL for every z accorrding to QR
    if (q$QR <= 1) { 
      res <- fit.sb(x, q)
      if (!is.na(res[1])) {
        type <- c(type, "SB")
        params <- rbind(params, c(res$params,
                                  do.call(test, list(res$trans))$p.value))
        trans <- rbind(trans, res$trans)
      }
    } else {
      res <- fit.su(x, q)
      if (!is.na(res[1])) {
        type <- c(type, "SU")
        params <- rbind(params, c(res$params, 
                                  do.call(test, list(res$trans))$p.value))
        trans <- rbind(trans, res$trans)
      }
    }
  }
  
  if(exclude.original) {
    type <- type[-1]
    trans <- trans[-1, ]
    params <- params[-1, ]
  }
  
  max <- which.max(params$p.value)
  # collect max p.value
  RVAL <- structure(c(list(original    = x,
                           transformed = trans[max, ], 
                           type        = type[max],
                           test        = test),
                      params[max, ]), 
                    class = "jtrans")
  return(RVAL)
}