#' @title Generate the discrete data with measurement error
#'
#' @param p The probability
#' @param n The number of sample size
#' @param m The number of observation
#' @param pi1 The correctness of true
#' @param pi2 The correctness of false
#'
#' @return
#' real_data is the process proportion for IC data.
#'
#' obs_data is the process proportion for OC data.
#'
#' n is the number of the sample size.
#' @export
#'
#' @examples
#' ME_data_generate(0.7,50,50,0.95)
ME_data_generate = function(p,n,m,pi1, pi2 = pi1){
  X = rbinom(n*m,1,p)
  Y = X
  Y_0_index = sample(which(X==1),round((1-pi1)*sum(X==1)))
  Y[Y_0_index] = abs(Y[Y_0_index]-1)
  Y_1_index = sample(which(X==0),round((1-pi2)*sum(X==0)))
  Y[Y_1_index] = abs(Y[Y_1_index]-1)
  X = apply(matrix(X,ncol = n,byrow = T),1,sum)
  Y = apply(matrix(Y,ncol = n,byrow = T),1,sum)
  return(list(real_data=X ,obs_data = Y,n = n))
}
