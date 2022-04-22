#' @title The two-sided control limit for EWMA-p chart
#'
#' @description This function can calculate the two-sided control limit for EWMA-p chart, and adjusted EWMA-p chart.
#' The parameters pi1 and pi2 are the correct rate of measured valued.
#' If we do not consider about measurement error, the value of pi1 and pi2 are 1.
#'
#' @param p The in-control probability
#' @param lambda The EWMA smooth constant
#' @param n The number of sample size
#' @param pi1 The correctness of true
#' @param pi2 The correctness of false
#' @param ARL0 The ARL of in-control process
#' @param M The simulation times of Monte Carlo method
#' @param error The maximum error what we can allow between hat_ARL0 and ARL0
#'
#' @return
#' L1 is a constant used to specify the width of the upper control limits.
#'
#' L2 is a constant used to specify the width of the lower control limits.
#'
#' hat_ARL0 is the simulated result for average run length with L1 and L2.
#'
#' hat_MRL0 is the simulated result for median of run length with L1 and L2.
#'
#' hat_SDRL0 is the simulated result for standard deviation of run length with L1 and L2.
#'
#' UCL is the limiting values of lower control limits with L1.
#'
#' LCL is the limiting values of lower control limits with L2.
#' @export
#' @importFrom stats rbinom
#' @importFrom stats uniroot
#' @importFrom stats sd
#' @importFrom stats median
#'
#' @examples
#' EWMA_p_two(0.2,0.05,5,1,1)
EWMA_p_two = function(p,lambda,n,pi1 = 1,pi2 = pi1,ARL0 = 200,M = 500,error = 10) {
  p0 = p
  p = (p+pi2-1)/(pi1+pi2-1)
  v = p0*(1-p0)/(pi1+pi2-1)^2
  L1 = EWMA_p_one_UCL(p,lambda,n,pi1,pi2,ARL0 = 2*ARL0,M,error)

  DRL = function(lambda,p,L1,L2,v,n){
    data = rbinom(1000,n,p)/n
    s = rep(NA,1000)
    t1 = rep(NA,1000)
    t2 = rep(NA,1000)
    s[1] = lambda * data[1] + (1 - lambda) * p
    t1[1] = p+L1*sqrt(lambda*(1-(1-lambda)^2)*v/(2-lambda)/n)
    t2[1] = p-L2*sqrt(lambda*(1-(1-lambda)^2)*v/(2-lambda)/n)
    i = 1
    while(s[i]<t1[i] && s[i]>t2[i]){
      i = i+1
      s[i] = lambda * data[i] + (1 - lambda) * s[i-1];
      t1[i] = p+L1*sqrt(lambda*(1-(1-lambda)^(2*(i)))*v/(2-lambda)/n)
      t2[i] = p-L2*sqrt(lambda*(1-(1-lambda)^(2*(i)))*v/(2-lambda)/n)
      if (i == length(data)){
        data = c(data,rbinom(1000,n,p)/n)
        s = c(s,rep(NA,1000))
        t1 = c(t1,rep(NA,1000))
        t2 = c(t2,rep(NA,1000))}
    }
    return(i)
  }

  ADRLF = function(lambda,p,L1,L2,v,n){
    a = replicate(M,DRL(lambda,p,L1,L2,v,n))
    return(list(ARL = mean(a), MRL = median(a), SDRL = sd(a)))
  }

  af = function(x){
    ADRLF(lambda,p,L1$L1,x,v,n)$ARL-ARL0
  }

  L = uniroot(af,lower = 0,upper = 3, extendInt = "yes")
  hat_ARL0 = ADRLF(lambda,p,L1$L1,L$root,v,n)
  ARL0_error = abs(hat_ARL0$ARL-ARL0)
  while (ARL0_error>error){
    L = uniroot(af,lower = 0,upper = 3, extendInt = "yes")
    hat_ARL0 = ADRLF(lambda,p,L1$L1,L$root,v,n)
    ARL0_error = abs(hat_ARL0$ARL-ARL0)
  }
  return(list(L1 = L1$L1, L2 = L$root, hat_ARL0 = hat_ARL0$ARL,
              hat_MRL = hat_ARL0$MRL, hat_SDRL = hat_ARL0$SDRL,
              UCL = p+L1$L1*sqrt(lambda*v/(2-lambda)/n),
              LCL = p-L$root*sqrt(lambda*v/(2-lambda)/n)))
}
