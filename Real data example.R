##### Real data example #####
library(EATME)
IC = read.csv('IC_SECOM.csv',header = T)
OC = read.csv('OC_SECOM.csv',header = T)
IC_data = matrix(IC[,1],ncol = 10,byrow = T)
OC_data = matrix(OC[,1],ncol = 10,byrow = T)

(V = cont_to_disc_V(IC_data,OC_data))
lambda = 0.05

##### Analysis without measurement error correction
(CL0 = EWMA_p_two(V$p0,lambda,V$n,1,1,370.4,3000,10))

## Draw an IC control chart without measurement error correction
pi1 = 1
pi2 = 1
ICdata1 = V$V0/V$n
p = mean(ICdata1)
ICdata1 = (ICdata1+pi2-1)/(pi1+pi2-1)
E = ewma(ICdata1,lambda,mean(ICdata1))
color = rep('black',length(ICdata1))
color[E>CL0$UCL] = 'red'
color[E<CL0$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.17,0.45),main = 'EWMA variance chart for IC data with measurement error')
abline(h = CL0$UCL,lty = 1)
abline(h = CL0$LCL,lty = 1)
txt1 = as.character(round(CL0$UCL,3))
text(x = (length(ICdata1)-9), y = CL0$UCL+0.007, paste('UCL=', txt1))
txt2 = as.character(round(CL0$LCL,3))
text(x = (length(ICdata1)-9), y = CL0$LCL+0.007, paste('LCL=', txt2))

## Draw an OC control chart
OCdata1 = V$V1/V$n
OCdata1 = (OCdata1+pi2-1)/(pi1+pi2-1)
E = ewma(OCdata1,lambda,mean(ICdata1))
color = rep('black',length(OCdata1))
color[E>CL0$UCL] = 'red'
color[E<CL0$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.17,0.45),main = 'EWMA variance chart for OC data with measurement error')
abline(h = CL0$UCL,lty = 1)
abline(h = CL0$LCL,lty = 1)
txt1 = as.character(round(CL0$UCL,3))
text(x = (length(OCdata1)-5), y = CL0$UCL+0.007, paste('UCL=', txt1))
txt2 = as.character(round(CL0$LCL,3))
text(x = (length(OCdata1)-5), y = CL0$LCL+0.007, paste('LCL=', txt2))

###########################

##### Analysis with measurement error correction

(CL1 = EWMA_p_two(V$p0,lambda,V$n,0.823,0.918,370.4,3000,10))
(CL2 = EWMA_p_two(V$p0,lambda,V$n,0.720,0.870,370.4,3000,10))
(CL3 = EWMA_p_two(V$p0,lambda,V$n,0.616,0.821,370.4,3000,10))

## Draw an IC control chart with measurement error correction
pi1 = 0.823
pi2 = 0.918
ICdata1 = V$V0/V$n
p = mean(ICdata1)
ICdata1 = (ICdata1+pi2-1)/(pi1+pi2-1)
E = ewma(ICdata1,lambda,mean(ICdata1))
color = rep('black',length(ICdata1))
color[E>CL1$UCL] = 'red'
color[E<CL1$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.16,0.45),main = 'Adjusted EWMA variance chart for IC data')
abline(h = CL1$UCL,lty = 1)
abline(h = CL1$LCL,lty = 1)
txt1 = as.character(round(CL1$UCL,3))
text(x = (length(ICdata1)-1), y = CL1$UCL+0.007, paste('UCL=', txt1))
txt2 = as.character(round(CL1$LCL,3))
text(x = (length(ICdata1)-1), y = CL1$LCL+0.007, paste('LCL=', txt2))
par(new = T)
pi1 = 0.720
pi2 = 0.870
ICdata1 = V$V0/V$n
p = mean(ICdata1)
ICdata1 = (ICdata1+pi2-1)/(pi1+pi2-1)
E = ewma(ICdata1,lambda,mean(ICdata1))
color = rep('blue',length(ICdata1))
color[E>CL2$UCL] = 'red'
color[E<CL2$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.16,0.45),main = 'Adjusted EWMA variance chart for IC data')
abline(h = CL2$UCL,lty = 1,col = 'blue')
abline(h = CL2$LCL,lty = 1,col = 'blue')
txt1 = as.character(round(CL2$UCL,3))
text(x = (length(ICdata1)-5), y = CL2$UCL+0.007, paste('UCL=', txt1),col="blue")
txt2 = as.character(round(CL2$LCL,3))
text(x = (length(ICdata1)-5), y = CL2$LCL+0.007, paste('LCL=', txt2),col="blue")
par(new = T)
pi1 = 0.616
pi2 = 0.821
ICdata1 = V$V0/V$n
p = mean(ICdata1)
ICdata1 = (ICdata1+pi2-1)/(pi1+pi2-1)
E = ewma(ICdata1,lambda,mean(ICdata1))
color = rep('orange',length(ICdata1))
color[E>CL3$UCL] = 'red'
color[E<CL3$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.16,0.45),main = 'Adjusted EWMA variance chart for IC data')
abline(h = CL3$UCL,lty = 1,col = 'orange')
abline(h = CL3$LCL,lty = 1,col = 'orange')
txt1 = as.character(round(CL3$UCL,3))
text(x = (length(ICdata1)-1), y = CL3$UCL+0.007, paste('UCL=', txt1),col="orange")
txt2 = as.character(round(CL3$LCL,3))
text(x = (length(ICdata1)-1), y = CL3$LCL+0.007, paste('LCL=', txt2),col="orange")


## Draw an OC control chart
pi1 = 0.823
pi2 = 0.918
OCdata1 = V$V1/V$n
OCdata1 = (OCdata1+pi2-1)/(pi1+pi2-1)
E = ewma(OCdata1,lambda,mean(ICdata1))
color = rep('black',length(OCdata1))
color[E>CL1$UCL] = 'red'
color[E<CL1$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.16,0.5),main = 'Adjusted EWMA variance chart for OC data')
abline(h = CL1$UCL,lty = 1)
abline(h = CL1$LCL,lty = 1)
txt1 = as.character(round(CL1$UCL,3))
text(x = (length(OCdata1)-1), y = CL1$UCL+0.007, paste('UCL=', txt1))
txt2 = as.character(round(CL1$LCL,3))
text(x = (length(OCdata1)-1), y = CL1$LCL+0.007, paste('LCL=', txt2))
par(new = T)
pi1 = 0.720
pi2 = 0.870
OCdata1 = V$V1/V$n
OCdata1 = (OCdata1+pi2-1)/(pi1+pi2-1)
E = ewma(OCdata1,lambda,mean(ICdata1))
color = rep('blue',length(OCdata1))
color[E>CL2$UCL] = 'red'
color[E<CL2$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.16,0.5),main = 'Adjusted EWMA variance chart for OC data')
abline(h = CL2$UCL,lty = 1,col = 'blue')
abline(h = CL2$LCL,lty = 1,col = 'blue')
txt1 = as.character(round(CL2$UCL,3))
text(x = (length(OCdata1)-2), y = CL2$UCL+0.007, paste('UCL=', txt1),col="blue")
txt2 = as.character(round(CL2$LCL,3))
text(x = (length(OCdata1)-2), y = CL2$LCL+0.007, paste('LCL=', txt2),col="blue")
par(new = T)
pi1 = 0.616
pi2 = 0.821
OCdata1 = V$V1/V$n
OCdata1 = (OCdata1+pi2-1)/(pi1+pi2-1)
E = ewma(OCdata1,lambda,mean(ICdata1))
color = rep('orange',length(OCdata1))
color[E>CL3$UCL] = 'red'
color[E<CL3$LCL] = 'red'
plot(E,type = 'b',col = color,pch = 16,xlab = 't',ylab = 'EWMA',
     ylim = c(0.16,0.5),main = 'Adjusted EWMA variance chart for OC data')
abline(h = CL3$UCL,lty = 1,col = 'orange')
abline(h = CL3$LCL,lty = 1,col = 'orange')
txt1 = as.character(round(CL3$UCL,3))
text(x = (length(OCdata1)-3), y = CL3$UCL+0.007, paste('UCL=', txt1),col="orange")
txt2 = as.character(round(CL3$LCL,3))
text(x = (length(OCdata1)-3), y = CL3$LCL+0.007, paste('LCL=', txt2),col="orange")
