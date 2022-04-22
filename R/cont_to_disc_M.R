#' @title Convert data to M statistic
#'
#' @description Convert continuity data to discrete data with M statistic
#'
#' @param ICdata The IC data
#' @param OCdata The OC data
#' @param mu.p Population mean, when the population mean is unknown, it will be estimated by the sample mean.
#'
#' @return
#' M0 is M statistic for IC data.
#'
#' M1 is M statistic for OC data.
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
#' OC = matrix(rnorm(100,2,1),ncol = 10,byrow = TRUE)
#' cont_to_disc_M(IC,OC)
#' @references Yang, S. F., & Arnold, B. C. (2014). A simple approach for monitoring business service time variation. The Scientific World Journal, 2014.
cont_to_disc_M = function(ICdata,OCdata,mu.p = mean(ICdata)){
  M = matrix(ICdata>mu.p,ncol = ncol(ICdata),byrow = T)
  M1 = matrix(OCdata>mu.p,ncol = ncol(OCdata),byrow = T)
  D = apply(M, 1, sum)
  D1 = apply(M1, 1, sum)
  p = mean(apply(M, 1, mean))
  p1 = mean(apply(M1, 1, mean))
  return(list(M0 = D,M1 = D1,
              p0 = p,p1 = p1,n = ncol(ICdata)))
}
