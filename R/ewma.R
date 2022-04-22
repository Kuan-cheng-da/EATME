#' EWMA charting statistic of input data
#'
#' @param data The data what to calculate EWMA charting statistic
#' @param lambda The EWMA smooth constant
#' @param EWMA0 The starting point of EWMA charting statistic
#'
#' @return The EWMA charting statistic of data.
#' @export
#'
#' @examples
#' x = rnorm(20,0,1)
#' ewma(x,0.05,0)
ewma = function(data,lambda,EWMA0){
  EWMA = rep(NA,length(data))
  EWMA[1] = lambda * data[1] + (1 - lambda) * EWMA0
  for (i in 2:length(data)) {
    EWMA[i] = lambda * data[i] + (1 - lambda) * EWMA[i-1]
  }
  return(EWMA)
}
