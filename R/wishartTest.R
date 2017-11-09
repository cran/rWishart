#' Test if Matrix is a Wishart Matrix
#' 
#' Given a random Wishart matrix, B, from W_p(Sigma, df) and independent random vector \code{a}, then (a' B a) / (a' Sigma a) 
#' is chi-squared with df degrees of freedom.
#'
#' @param WishMat random Wishart Matrix from W_p(Sigma, df)
#' @param Sigma Covariance matrix for W_p(Sigma, df)
#' @param vec independent random vector
#' 
#' @return A chi-squared random variable with df degrees of freedom.
#'
#' @export
#' @importFrom stats rnorm
#'
#' @examples wishartTest(rWishart(1, 5, diag(1, 20), simplify = FALSE)[[1]], diag(1, 20))
wishartTest <- function(WishMat, Sigma, vec = NULL){
  if(is.null(vec)){
    vec <- rnorm(ncol(WishMat))
  }
  (t(vec) %*% WishMat %*% vec) / 
           (t(vec) %*% Sigma %*% vec)
}