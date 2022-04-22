#' @title Plot one-sided upper EWMA-p control chart
#'
#' @description Plot one-sided upper EWMA-p control chart  for input IC and OC data, the input need to be number of defectives.
#'
#' @param ICdata The IC data for attributes
#' @param OCdata The OC data for attributes
#' @param lambda The EWMA smooth constant
#' @param n The number of sample size
#' @param pi1 The correctness of true
#' @param pi2 The correctness of false
#' @param ARL0 The ARL of in-control process
#' @param M The simulation times of Monte Carlo method
#' @param error The maximum error what we can allow between hat_ARL0 and ARL0
#'
#' @return The first chart is EWMA-p chart for IC data, and the second chart is EWMA-p chart for OC data
#' @export
#' @importFrom stats rbinom
#' @importFrom stats uniroot
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom graphics abline
#' @importFrom graphics text
#'
#' @examples
#' library(qcr)
#' data = orangejuice
#' IC = data[31:54,1]
#' OC = data[1:30,1]
#' EWMA_p_chart_one_UCL(IC,OC,0.05,50,1,1)
EWMA_p_chart_one_UCL = function(ICdata,OCdata,lambda,n,pi1 = 1,pi2 = pi1,ARL0 = 200,M = 500,error = 10){
  ICdata1 = ICdata/n
  p = mean(ICdata1)
  a = EWMA_p_one_UCL(p,lambda,n,pi1,pi2,ARL0,M,error)
  E = ewma(ICdata1,lambda,p)
  color = rep('black',length(ICdata1))
  color[(E-a$UCL)>0] = 'red'
  plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
       ylim = c(min(E)-0.07,max(c(E,a$UCL))+0.07),main = 'EWMA-p chart for IC data')
  abline(h = a$UCL,lty = 1)
  txt = as.character(round(a$UCL,3))
  text(x = (length(ICdata1)-3), y = a$UCL+0.007, paste('UCL=', txt))

  OCdata1 = OCdata/n
  E = ewma(OCdata1,lambda,p)
  color = rep('black',length(OCdata1))
  color[(E-a$UCL)>0] = 'red'
  plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA'
       ,ylim = c(min(E)-0.07,max(c(E,a$UCL))+0.07),main = 'EWMA-p chart for OC data')
  abline(h = a$UCL,lty = 1)
  txt = as.character(round(a$UCL,3))
  text(x = (length(OCdata1)-3), y = a$UCL+0.007, paste('UCL=', txt))
}
