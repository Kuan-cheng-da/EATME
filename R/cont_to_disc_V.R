#' @title Convert data to V statistic
#'
#' @description Convert continuity data to discrete data with V statistic
#'
#' @param ICdata The IC data
#' @param OCdata The OC data
#' @param var.p Population variance, when the population variance is unknown, it will be estimated by the square of sample standard deviation.
#'
#' @return
#' V0 is V statistic for IC data.
#'
#' V1 is V statistic for OC data.
#'
#' p0 is the process proportion for IC data.
#'
#' p1 is the process proportion for OC data.
#'
#' n is the number of the sample size.
#' @export
#'
#' @examples
#' IC = matrix(rnorm(100,0,1),ncol = 10,byrow = TRUE)
#' OC = matrix(rnorm(100,0,2),ncol = 10,byrow = TRUE)
#' cont_to_disc_V(IC,OC)
#' @references Yang, S. F., & Arnold, B. C. (2014). A simple approach for monitoring business service time variation. The Scientific World Journal, 2014.
cont_to_disc_V = function(ICdata,OCdata,var.p = NULL){
  n = ncol(ICdata)
  if(n%%2==1){
    ICdata = ICdata[,-n]
    OCdata = OCdata[,-n]
  }
  if(is.null(var.p)){
    s = mean(apply(ICdata,1,sd))
    var.p = s^2
  }
  n = ncol(ICdata)/2
  m = matrix(ICdata,ncol = 2,byrow = T)
  m1 = matrix(OCdata,ncol = 2,byrow = T)
  Y = (m[,1]-m[,2])^2/2
  Y1 = (m1[,1]-m1[,2])^2/2
  V = matrix(Y>var.p,ncol = n,byrow = T)
  V1 = matrix(Y1>var.p,ncol = n,byrow = T)
  D = apply(V, 1, sum)
  D1 = apply(V1, 1, sum)
  p = mean(apply(V, 1, mean))
  p1 = mean(apply(V1, 1, mean))
  return(list(V0 = D,V1 = D1,
              p0 = p,p1 = p1,n = n))
}
