#' Random Wishart Distributed Matrices
#'
#' Generate \code{n} random matrices, distributed according to the Wishart distribution with parameters \code{Sigma} and \code{df}, W_p(Sigma, df).
#'
#' @inheritParams base::replicate
#' @inheritParams stats::rWishart
#' @param covariance logical on whether a covariance matrix should be generated
#'
#' @return A numeric array of dimension \code{p * p * n}, where each array is a positive semidefinite matrix, a realization of the Wishart distribution W_p(Sigma, df)
#' @export
#' 
#' @details If X_1, ..., X_m is a sample of m independent multivariate Gaussians with mean vector 0, and covariance matrix Sigma, 
#' the distribution of M = X'X is W_p(Sigma, m).
#' 
#' @importFrom Matrix rankMatrix
#'
#' @examples rWishart(2, 5, diag(1, 20))
rWishart <- function(n, df, Sigma, covariance = FALSE, simplify = "array"){
  if(rankMatrix(Sigma) < ncol(Sigma)){
    ls <- rSingularWishart(n, df, Sigma, covariance, simplify)
  }else{
    if(df >= ncol(Sigma)){
      if(round(df) - df != 0){ 
        ls <- rFractionalWishart(n, df, Sigma, covariance, simplify)
      }else{
        ls <- rNonsingularWishart(n, df, Sigma, covariance, simplify)
      }
    }else{
      ls <- rPsuedoWishart(n, df, Sigma, covariance, simplify)
    }
  }
  ls
}


