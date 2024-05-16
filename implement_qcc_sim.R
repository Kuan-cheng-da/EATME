library(qcc)
######  Simulation for discrete data ######
set.seed(3421)
(discrete_IC = ME_data_generate(0.4,20,50,0.9,0.9))
(discrete_OC = ME_data_generate(0.6,20,20,0.9,0.9))
st.p = stats.p(discrete_IC$obs_data,rep(discrete_IC$n,length(discrete_IC$obs_data)))
s.p = sd.p(discrete_IC$obs_data,rep(discrete_IC$n,length(discrete_IC$obs_data)))
CL_q = limits.p(st.p$center,s.p,discrete_IC$n,1.005)
qcc(discrete_IC$obs_data,"p",rep(discrete_IC$n,length(discrete_IC$obs_data)),
    limits = CL_q,title = "Control chart for IC data")
qcc(discrete_OC$obs_data,"p",rep(discrete_OC$n,length(discrete_OC$obs_data)),
    limits = CL_q,center = st.p$center,title = "Control chart for OC data")

##### Simulation for continuous data #####
set.seed(1225)
conti_IC = matrix(rexp(1500,1/20),ncol = 15)
conti_OC = matrix(rexp(300,1/30),ncol = 15)
ME_conti_IC = conti_IC+matrix(rnorm(1500,0,sqrt(30)),ncol = 15)
ME_conti_OC = conti_OC+matrix(rnorm(300,0,sqrt(30)),ncol = 15)

(M = cont_to_disc_V(conti_IC,conti_OC))
(M_ME = cont_to_disc_V(ME_conti_IC,ME_conti_OC))
st.p = stats.p(M_ME$V0,rep(M_ME$n,length(M_ME$V0)))
s.p = sd.p(M_ME$V0,rep(M_ME$n,length(V$V0)))
CL_q = limits.p(st.p$center,s.p,M_ME$n,1.0025)
qcc(M_ME$V0,"p",rep(M_ME$n,length(M_ME$V0)),limits = CL_q,title = "Control chart for IC data")
qcc(M_ME$V1,"p",rep(M_ME$n,length(M_ME$V1)),limits = CL_q,
    center = st.p$center,title = "Control chart for OC data")