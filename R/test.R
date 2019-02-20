# TODO: Add comment
# 
# Author: klei
###############################################################################
library(mpo)


#

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))

efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 5,display = F)
mu.efront = efront$mu.efront
library(mpo)
mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,display.wts = T,digits = 3)



