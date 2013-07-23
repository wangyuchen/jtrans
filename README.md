jtrans 0.1
==========

### Why should someone use this package?

This package gives a simple solution for normality transformation based on the newest transformation algorithm by Chou, Youn Min; Polansky, A. M. M. R. L. (1998). The rich options it provides can be used for simulations on the  algorithm. It uses standard S3 class and methods, so it's an small but indispensable building block for statistical procedures which have the problem of non-normality.

### How does it compare to other existing solutions?

There are now two packages on CRAN can do Johnson normality transformations, [Johnson](http://cran.r-project.org/web/packages/Johnson/index.html) by Edgar Santos Fernandez and [JohnsonDistribution](http://cran.r-project.org/web/packages/JohnsonDistribution/index.html) by A.I. McLeod and Leanna King. However, both of them have certain limitations to performing easy and correct normality transformation.

Although Johnson package is also based on the algorithm by Chou, Youn Min; Polansky, A. M. M. R. L. (1998), it's a C style implementation and hasn't been vectorized, so it's hard to debug and it generally takes 10 times longer than __jtrans__. It implementes the sample quantile function in a non-standard way (different from the __quantile__ function from __stats__ package), which will lead to errors in the following calculations.

JohnsonDistribution package is based on I. D. Hill (1976). It aims to provide Johnson curve distribution and estimation functions, so the design concept is slightly different from Johnson normality transformation. 

### What are the main functions?

__jtrans__ is the main function. Import is a numeric vector of non-normal data. Output is the transformed data with Johnson curve and parameters. The Shapiro-Wilk test is used by default, and the p.value of the transformed data will also be returned.


