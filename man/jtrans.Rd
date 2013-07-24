\name{jtrans}
\alias{jtrans}
\title{Johnson Transformation for Normality}
\usage{
  jtrans(x, test = "shapiro.test", exclude.original = TRUE)
}
\arguments{
  \item{x}{the non-normal numerical data.}

  \item{test}{the normality test used to select fits.}

  \item{exclude.original}{should the original data be
  excluded when comparing fits.}
}
\value{
  A list with class "jtrans" containing the following
  components: \item{original}{original data.}
  \item{transformed}{transformed data.} \item{type}{type of
  transformation selected.} \item{test}{normality test used
  to select transformations.} \item{z}{selected z value
  among 101 values from 0.25 to 1.25.} \item{eta, gamma,
  lambda, epsilon}{transformation parameters.}
  \item{p.value}{the maximum p.value returned by test}
}
\description{
  \code{jtrans} transforms imported non-normal data to
  normality.
}
\details{
  \code{jtrans} fits imported data to a set of 101 Johnson
  curves. Then a normality test is used (Shapiro-Wilk test
  by default) to find the best fit that have the maximum
  p.value under that given test. It returns the transformed
  data and the corresponding type of Johnson curve and
  parameter estimations.

  Since the default Shapiro-Wilk test can only accept
  sample size with 3 and 5000, sample size larger than 5000
  will meet an error. In such cases, one should specify
  another normality test in the test parameter, generally
  the \code{ad.test} in the \pkg{nortest} package is
  recommended.

  Sometimes, this algorithm may return poor fits. The most
  extreme case is that all the transformed data have
  smaller p.values than the p.value of the original data.
  In such cases, one should set \code{exclude.original} to
  be FALSE, then \code{jtrans} will return the original
  data as the transformed data.
}
\examples{
# generate 100 non-normal data and transform it.
x <- rexp(100, .2)
jt <- jtrans(x)
jt
}
\references{
  Chou, Y. M., Polansky, A. M., & Mason, R. L. (1998).
  Transforming non-normal data to normality in statistical
  process control. Journal of Quality Technology, 30(2),
  133-141.
}

