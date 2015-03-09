#' @rdname fit_functions

# library(jtrans)
# x <- rexp(30, .4)
# y <- jtrans(x)
# summary(y)

print.jtrans <- function(y) {
  print(y$transformed)
}

summary.jtrans <- function(y, latex = TRUE) {
  if (latex == F) y
  else {
    switch(y$type, 
           "SB" = {
             sb <- paste("\\[Y=\\gamma+\\eta \\ln\\frac{X-\\epsilon}{", 
                         "\\lambda+\\epsilon-x} = ",
                         sprintf("%.4f", y$gamma), "+(", 
                         sprintf("%.4f", y$eta), ")\\ln\\frac{X-(", 
                         sprintf("%.4f", y$epsilon), 
                         ")}{", sprintf("%.4f", y$lambda), "+(", 
                         sprintf("%.4f", y$epsilon), ")-X}", 
                         sprintf("%.4f", y$epsilon), "< X <", 
                         sprintf("%.4f", y$epsilon + y$lambda), "\\]")
             cat(sb)
           },
           "SL" = print("SL"),
           "SU" = print("SU"))
  }
}

# \( S_B \) &\( Z=\gamma+\eta \ln(\frac{x-\xi}{\lambda+\xi-x}) \) &\( \eta>0,~\lambda>0 \) &\( \)\\
# \( S_L \) &\( Z=\gamma+\eta \ln(x-\xi) \) &\( \eta >0 \) &\( x>\xi \)\\
# \( S_U \) &\( Z=\gamma+\eta sinh^{-1}(\frac{x-\xi}{\lambda}) \) &\( \eta>0,~\lambda>0 \) &\( -\infty < x < +\infty \)\\
# 
# \( S_L \) &\( Z=\gamma+\eta \ln(x-\xi) \) &\( \eta >0 \) &\( x>\xi \)\\
# \( S_U \) &\( Z=\gamma+\eta sinh^{-1}(\frac{x-\xi}{\lambda}) \) &\( \eta>0,~\lambda>0 \) &\( -\infty < x < +\infty \)\\