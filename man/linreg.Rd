% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solution.R
\docType{class}
\name{linreg}
\alias{linreg}
\title{Linear regrestion model}
\description{
RC type class for Liner Regression model.

This function print the coefficient of regretion.

This function plot two graph using ggplot2

Print the residuals values.

Print the  predicted values

Print the coefficients as a named vector.

Print the coefficients with their standard error, t-value and p-value as well as the estimate of ^σ and the degrees
 of freedom in the model, same as printed of lm function.
}
\details{
Print function.

Plot data

Print residuals values.

Print the  predicted values

Coefficients

Summery
}
\section{Fields}{

\describe{
\item{\code{formula}}{linear formula as symbolic model to filter data.}

\item{\code{data}}{should be data frame}
}}


\examples{
data(iris)
linreg_mod_object <- linreg$new(Petal.Length~Species, data = iris)
linreg_mod_object$print()
linreg_mod_object$plot()
linreg_mod_object$resid()
linreg_mod_object$pred()
linreg_mod_object$coef()
linreg_mod_object$summary()
}
