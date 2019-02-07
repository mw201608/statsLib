#Computes Jonckheere's non-parametric trend test for ordered alternatives
Jonckheere.test=function(y,x){
#Input: y, a vector of numeric values; x, a vector or an ordered factor specifying the grouping of each values in y
#Reference: Hollander and Wolfe. 1999. Nonparametric statistical methods (Second edition). John Wiley & Sons, Inc., New York.
#Output: a vector with three named elements, J (the test statistics), Z (normalized J value) and p.value (two-sided p value calculated from normal approximation)
	is.rm=is.na(y) | is.na(x) # NA will be removed
	y=y[!is.rm]
	x=x[!is.rm]
	dat=split(y,as.ordered(x))
	dat=dat[sapply(dat,length)>0]
	M=length(dat)
	if(M<3) stop('At least 3 groups are required\n')
	nt=sapply(dat,function(a) length(a))
	dat=unlist(dat)
	cnt=c(0,cumsum(nt))
	N=length(y)
	J=sapply(1:(M-1),function(i){
		sum(sapply(dat[(cnt[i]+1):cnt[i+1]],function(d) sum(dat[(cnt[i+1]+1):N]>d)))
	})
	J=sum(J)
	eJ=(N*N-sum(nt*nt))/4
	varJ=(N*N*(2*N+3)-sum(nt*nt*(2*nt+3)))/72
	Z=(J-eJ)/sqrt(varJ)
	p=2*pnorm(-abs(Z))
	return(c(J=J,Z=Z,p.value=p))
}
