#' Export the transformation into LaTeX formula
#' 
#' \code{trans_func} turns a \code{jtrans} object into either an R function of this transformation for future use or a LaTeX formula for display in the report.
#' 
#' @param y an jtrans object
#' @param latex whether to print the latex formula of the transformation
#' 
#' @export
#' 

trans_func <- function(y, latex = TRUE) {
  if (latex) {
    switch(y$type, 
           "SB" = {
             form <- paste("\\[ Y=\\gamma+\\eta \\ln\\frac{X-\\epsilon}{", 
                         "\\lambda+\\epsilon-x} = ",
                         sprintf("%.4f", y$gamma), "+(", 
                         sprintf("%.4f", y$eta), ")\\ln\\frac{X-(", 
                         sprintf("%.4f", y$epsilon), 
                         ")}{", sprintf("%.4f", y$lambda), "+(", 
                         sprintf("%.4f", y$epsilon), ")-X} \\]")
           },
           "SL" = {
             form <- paste("\\[ Y = \\gamma+\\eta \\ln(X-\\epsilon) = ",
                         sprintf("%.4f", y$gamma), "+(", 
                         sprintf("%.4f", y$eta), ")\\ln(X-(", 
                         sprintf("%.4f", y$epsilon), ") \\]")
           },
           "SU" = {
             form <- paste("\\[ Y=\\gamma+\\eta", 
                         "\\textrm{sinh}^{-1}\\frac{x-\\epsilon}{\\lambda}=",
                         sprintf("%.4f", y$gamma), "+(", 
                         sprintf("%.4f", y$eta), 
                         ")\\textrm{sinh}^{-1}\\frac{X-(", 
                         sprintf("%.4f", y$epsilon), 
                         ")}{", sprintf("%.4f", y$lambda), "} \\]")
           })
    cat(form)
  } else {
#     switch(y$type,
#            "SB" = {
#              func <- function(x) {
#                y = dput(y$gamma) + dput(y$eta) * 
#                  log((x-dput(y$epsilon))/(dput(y$lambda)+dput(y$epsilon)-x))
#              }
#            }, 
#            "SL" = {
#              func <- function(x) {
#                y = dput(y$gamma) + dput(y$eta) * 
#                  log((x-dput(y$epsilon))/(dput(y$lambda)+dput(y$epsilon)-x))
#              }
#            }, 
#            "SU" = {
#              func <- function(x) {
#                y = dput(y$gamma) + dput(y$eta) * 
#                  log((x-dput(y$epsilon))/(dput(y$lambda)+dput(y$epsilon)-x))
#              }
#            })
#     return(func)
  }
  
}
