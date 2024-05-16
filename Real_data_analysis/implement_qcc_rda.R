library(EATME)
library(qcc)

IC = read.csv('IC_SECOM.csv',header = T)
OC = read.csv('OC_SECOM.csv',header = T)
IC_data = matrix(IC[,1],ncol = 10,byrow = T)
OC_data = matrix(OC[,1],ncol = 10,byrow = T)

#To monitor the dispersion of data, we need to transform the data to V statistic
(V = cont_to_disc_V(IC_data,OC_data))

st.p = stats.p(V$V0,rep(V$n,length(V$V0)))
s.p = sd.p(V$V0,rep(V$n,length(V$V0)))
CL_q = limits.p(st.p$center,s.p,V$n,1.0025)
CL_qcc = list(L1 = NA,L2 = NA,hat_ARL0 = NA,hat_MRL = NA,
              hat_SDRL = NA,UCL = CL_q[2],LUCL = CL_q[1])
qcc(V$V0,"p",rep(V$n,length(V$V0)),limits = CL_q,title = "Control chart for IC data")
qcc(V$V1,"p",rep(V$n,length(V$V1)),limits = CL_q,
    center = st.p$center,title = "Control chart for OC data")