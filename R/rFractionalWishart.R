#' Random Fractional Wishart Matrix
#' 
#' Generate \code{n} random matrices, distributed according to the Wishart distribution with parameters \code{Sigma} and \code{df}, W_p(Sigma, df).
#'
#' @references Adhikari, S. (2008). Wishart random matrices in probabilistic structural mechanics. Journal of engineering mechanics, 134(12), doi:10.1061/(ASCE)0733-9399(2008)134:12(1029).
#'
#' @inherit rWishart
#' @export
#' 
#'
#' @examples rFractionalWishart(2, 22.5, diag(1, 20))
rFractionalWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  replicate(n, rWishart::FractionalWishart(df, Sigma, covariance),
            simplify = simplify)
}



#' Fractional Wishart Helper Function
#'
#' @inherit rWishart
#' @export
#' @keywords internal
#' @importFrom MASS mvrnorm
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom lazyeval f_unwrap
#' @examples FractionalWishart(22.5, diag(1, 20))
FractionalWishart <- function(df, Sigma, covariance = FALSE){
  if(ncol(Sigma) > df){stop("Cannot produce a Singular Fractional Wishart")}
  cholesky <- chol(Sigma)
  B <- matrix(0, ncol = ncol(Sigma), nrow = ncol(Sigma))
  for(i in 1:ncol(Sigma)){
    for(j in 1:ncol(Sigma)){
      B[i, j] <- ifelse(j < i, rnorm(1), 0)
    }
    B[i, i] <- rgamma(1, df - i + 1 / 2, scale = 1 / 2)
  }
  x <- cholesky %*% t(B) %*% B %*% t(cholesky)
  atr <- attributes(x)
  attributes(x) <- c(atr, df = f_unwrap(~ df))
  if(covariance == TRUE){
    x <- x / df
    class(x) <- c("covariance", "matrix")
    x
  }
  x
}