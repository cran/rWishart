% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wishartTest.R
\name{wishartTest}
\alias{wishartTest}
\title{Test if Matrix is a Wishart Matrix}
\usage{
wishartTest(WishMat, Sigma, vec = NULL)
}
\arguments{
\item{WishMat}{random Wishart Matrix from W_p(Sigma, df)}

\item{Sigma}{Covariance matrix for W_p(Sigma, df)}

\item{vec}{independent random vector}
}
\value{
A chi-squared random variable with df degrees of freedom.
}
\description{
Given a random Wishart matrix, B, from W_p(Sigma, df) and independent random vector \code{a}, then (a' B a) / (a' Sigma a) 
is chi-squared with df degrees of freedom.
}
\examples{
wishartTest(rWishart(1, 5, diag(1, 20), simplify = FALSE)[[1]], diag(1, 20))
}
