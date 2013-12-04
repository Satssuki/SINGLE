### R code from vignette source 'vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: vignette.Rnw:42-45
###################################################
library('SINGLE')
set.seed(1)
sim = generate_random_data(ROI=5, length_=50, seg=3, sparsity=.1, str=-.6)


###################################################
### code chunk number 2: vignette.Rnw:51-52
###################################################
sim$true_cov[,,1]


###################################################
### code chunk number 3: vignette.Rnw:57-62
###################################################
data = sim$data
h_G = choose_h(data=data, sample_size=30, kernel="gaussian", 
               h_lower=20, h_upper=100, h_step=10)
h_W = choose_h(data=data, sample_size=30, kernel="window", 
               h_lower=20, h_upper=100, h_step=10)


###################################################
### code chunk number 4: vignette.Rnw:69-70
###################################################
S = SINGLE(data=data, h=h_G, l1=.75, l2=0.5)


###################################################
### code chunk number 5: vignette.Rnw:75-77
###################################################
C_slid = get_kern_cov(data=data, h=h_W, kernel="window")
S_slid = SINGLE(data=data, C=C_slid, l1=.75, l2=0.5)


###################################################
### code chunk number 6: vignette.Rnw:85-87
###################################################
plotSINGLE(object=S, index=c(1,2,3,4,5), x.axis = seq(1,150), n.row=2, 
           col.names=seq(1,5), fix.axis=TRUE)


###################################################
### code chunk number 7: vignette.Rnw:96-99
###################################################
result = precision_recall(true_cov=sim$true_cov, estimated_cov=S$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
     main='F Score', xlab='Time') 


###################################################
### code chunk number 8: vignette.Rnw:135-137 (eval = FALSE)
###################################################
## sim_ER = generate_random_data(ROI=5, length_=50, mode='ER', seg=3, sparsity=.1)
## sim_BA = generate_random_data(ROI=5, length_=50, mode='BA', seg=3, sparsity=.1)


###################################################
### code chunk number 9: vignette.Rnw:141-144
###################################################
sim1 = generate_random_data(ROI=5, length_=25, mode='BA', seg=1, sparsity=.15)
sim2 = generate_random_data(ROI=5, length_=75, mode='BA', seg=1, sparsity=.15)
data = rbind(sim1$data, sim2$data)


###################################################
### code chunk number 10: vignette.Rnw:160-164 (eval = FALSE)
###################################################
## set.seed(1)
## sim = generate_random_data(ROI=5, length_=50, seg=3, sparsity=.1, str=-.6)
## data = sim$data
## S = SINGLE(data=data, h=50, l1=.75, l2=0.5)


###################################################
### code chunk number 11: vignette.Rnw:181-182 (eval = FALSE)
###################################################
## result = precision_recall(true_cov=sim$true_cov, estimated_cov=S$P_)


###################################################
### code chunk number 12: vignette.Rnw:191-193
###################################################
plotSINGLE(object=S, index=c(1,2,3,5), x.axis = seq(1,150), n.row=2, 
           col.names=c(1,2,3,5), fix.axis=TRUE)


