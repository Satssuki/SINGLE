### R code from vignette source 'vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: vignette.Rnw:42-45
###################################################
library('SINGLE')
set.seed(1)
sim = generate_random_data(ROI=5, length_=50, seg=3, sparsity=.15)


###################################################
### code chunk number 2: vignette.Rnw:51-52
###################################################
sim$true_cov[,,1]


###################################################
### code chunk number 3: vignette.Rnw:57-59
###################################################
data = sim$data
S = SINGLE(data=data, radius=5, l1=.75, l2=0.5, k=3)


###################################################
### code chunk number 4: vignette.Rnw:66-68
###################################################
plotSINGLE(object=S, index=c(1,2,3,4,5), x.axis = seq(1,150), n.row=2, 
           col.names=seq(1,5), fix.axis=TRUE)


###################################################
### code chunk number 5: vignette.Rnw:79-82
###################################################
result = precision_recall(true_cov=sim$true_cov, estimated_cov=S$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
     main='F Score', xlab='Time') 


###################################################
### code chunk number 6: vignette.Rnw:118-119 (eval = FALSE)
###################################################
## sim = generate_random_data(ROI=5, length_=50, seg=3, sparsity=.15)


###################################################
### code chunk number 7: vignette.Rnw:123-126
###################################################
sim1 = generate_random_data(ROI=5, length_=25, seg=1, sparsity=.15)
sim2 = generate_random_data(ROI=5, length_=75, seg=1, sparsity=.15)
data = rbind(sim1$data, sim2$data)


###################################################
### code chunk number 8: vignette.Rnw:142-144 (eval = FALSE)
###################################################
## data = sim$data
## S = SINGLE(data=data, radius=5, l1=.75, l2=0.3, k=3)


###################################################
### code chunk number 9: vignette.Rnw:161-162 (eval = FALSE)
###################################################
## result = precision_recall(true_cov=sim$true_cov, estimated_cov=S$P_)


###################################################
### code chunk number 10: vignette.Rnw:171-173
###################################################
plotSINGLE(object=S, index=c(1,3,5), x.axis = seq(1,150), n.row=1, 
           col.names=c(1,3,5), fix.axis=TRUE)


