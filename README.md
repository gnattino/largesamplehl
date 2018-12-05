# largesamplehl
A modification of the Hosmer-Lemeshow test to evaluate the goodness of fit of logistic regression models in large samples.

The Hosmer-Lemeshow test is a popular statistical test to assess the goodness of fit of logistic regression models. The use of this test is problematic in large samples, because the power of the test increases with the sample size and minuscule discrepancies between estimated and true probabilities are likely to cause the rejection of the hypothesis of perfect fit.

This R package implements a modification of the Hosmer-Lemeshow test to rigorously assess whether the fit of a model is acceptable for practical purposes, albeit not perfect. The modified test is based on a standardization of the noncentrality parameter that characterizes the distribution of the Hosmer-Lemeshow statistic. This parameter, denoted with epsilon, measures the goodness of fit of a model but is not affected by the sample size.

The main function of the package is `hltest`. It can be used to estimate epsilon, to construct its confidence intervals and to perform the statistical test.

You can install the package with the function `install_github` of the package `devtools`.

```
library(devtools)
install_github("gnattino/largesamplehl")
```
