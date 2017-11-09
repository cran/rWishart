#' Random Psuedo Wishart Matrix
#'
#' Generate \code{n} random matrices, distributed according to the Wishart distribution with parameters \code{Sigma} and \code{df}, W_p(Sigma, df).
#'
#' @references Diaz-Garcia, Jose A, Ramon Gutierrez Jaimez, and Kanti V Mardia. 1997. “Wishart and Pseudo-Wishart Distributions and Some Applications to Shape Theory.” Journal of Multivariate Analysis 63 (1): 73–87. doi:10.1006/jmva.1997.1689.
#'
#' @inherit rWishart
#' @export
#'
#' @examples rPsuedoWishart(2, 5, diag(1, 20))
rPsuedoWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  replicate(n, rWishart::PsuedoWishart(df, Sigma, covariance),
            simplify = simplify)
}



#' Psuedo Wishart Helper Function
#'
#' @inherit rWishart
#' @export
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @importFrom lazyeval f_unwrap
#' @examples PsuedoWishart(5, diag(1, 20))
PsuedoWishart <- function(df, Sigma, covariance = FALSE){
  df <- df
  cholesky <- chol(Sigma)
  X <- mvrnorm(n = df,
               mu  = rep(0 , ncol(Sigma)),
               Sigma = Sigma)
  x <- cholesky %*% t(X) %*% X %*% t(cholesky)
  atr <- attributes(x)
  attributes(x) <- c(atr, df = f_unwrap(~ df))
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}