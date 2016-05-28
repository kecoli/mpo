# TODO: Add comment
# 
# Author: klei
###############################################################################


# NORMALIZED (DEFAULT) AND NON-NORMALIZED ETL

etl = function(R, alpha = 0.05, normalize = TRUE)
{
	ret = coredata(R)
	ret = sort(ret)
	n.tail = ifelse(alpha == 0, 1, ceiling(alpha*length(ret)))
	etl = -1/n.tail * sum(ret[which((1:length(ret)) <= n.tail)])
	if(normalize)
	{etl = alpha/(dnorm(qnorm(alpha))) * (etl + mean(ret))}
	etl
}


# NORMALIZED (DEFAULT) AND NON-NORMALIZED STARR

starrRatio = function(R, alpha = .05, normalize=TRUE, Rf=0)
{
	ret = coredata(R)
	mu = mean(ret) - Rf
	netl = etl(ret,alpha,normalize)
	mu/netl
}
