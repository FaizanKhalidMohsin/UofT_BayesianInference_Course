#
#  SimplMCMC.r
#  January 24,2011
#  This file will generate an MCMC sample of normal data 
#   and produce a series of plots for this data
#
#  NOTE: all plot files are saved to directory: 
#     "c:/mike workstation/bayescourse9/plots."   
#  You may want to change this....
#

# prior
mu0<-66; theta<-4; alp<-1; bet<-25

# data
x<-c(64, 73, 64, 63, 69, 71)
xbar<-mean(x)
n<-length(x)

# starting values
muStart<-20;tauStart<-0.0025;

#  MCMC parameters
nbig<-50000; 
set.seed(333); # The purpose of set.seed is to get the same random values

if(T){        # set to true to rerun the MCMC
# Initialize chain
mu<-rep(0,nbig)
tau<-rep(0,nbig)

mu[1]<-muStart; tau[1]<-tauStart

mu0p<- (theta*mu0+n*xbar)/(theta+n)  # does not change in loop
alpp<-alp+(0.5)*(n+1)                # does not change in loop

tau0p<-rep(0,nbig)
betp<-rep(0,nbig)

for(i in 2:nbig){
	# sample mu
	tau0p[i]<-tau[i-1]*(n+theta)
	mu[i]<-rnorm(1,mean=mu0p,sd=1/sqrt(tau0p[i]))
	
	# sample tau
	betp[i]<- bet+ 0.5*( sum( (x-mu[i])^2) + theta*(mu[i]-mu0p)^2)
	tau[i]<-rgamma(1,alpp,betp[i])
}
}
#  the following command will give the first 20 values:
 cbind(mu,tau)[1:20,]

# The next group of plots will plot the values as the get sampled in time.  
#
# Please note, that the below command write the plots to a postscript file
# to the directory: ``\verb+c:/mike workstation/bayescourse9/plots+''. If you 
# don't have such a directory, then you need to create it or better yet, change
# the directory name in the below commands.


#
#  plots the values as they get sampled in time
#
postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Sd.eps",
onefile=FALSE,print.it=F)

iind1<-seq(1:60)

plot(mu[iind1],tau[iind1],type="n", xlab="mu", ylab="tau")
lines(mu[1:60],tau[1:60],type="b", cex=2)
title(main="First 60 values")

dev.off()
##################################

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7S2c.eps",
onefile=FALSE,print.it=F)

plot(mu[2:60],tau[2:60],type="p", xlab="mu", ylab="tau",
cex=1.5, xlim=c(55,75), ylim=c(0,.2),
main="First 60 values, dropped first value")

dev.off()

# The next set of commands compares the sampled joint distribution from the 
# MCMC method versus sampling the joint distribution using the know joint 
# distribution given in the second lecture.  Note, the below code uses the 
# function \verb+kde2d(...)+ which requires that the package "MASS" is loaded 
# in R.  


library(MASS)

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7S2e.eps",
onefile=FALSE,print.it=F)

contour(kde2d(mu[2:nbig],tau[2:nbig]),
xlab="mu", ylab="tau",
cex=1.5, xlim=c(55,75), ylim=c(0,.2),nlevels=19,
main="Joint Distribution: MCMC")

dev.off()

#####
xa1<-4;xb1<-71.797
xmu0<-66.80; xthep<-10
xn<-50000
xgam<-rgamma(xn,xa1,xb1)
xmu<-rnorm(xn,mean=xmu0, sd=1/sqrt(xthep*xgam))
#####

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7S2f.eps",
onefile=FALSE,print.it=F)

contour(kde2d(xmu,xgam),
xlab="mu", ylab="tau",
cex=1.5, xlim=c(55,75), ylim=c(0,.2),nlevels=19,
main="Joint Distribution: IID sample")

dev.off()


# The next group of plots are for the trace plots.  These plots are a simple 
# time plot of one of the parameters which is sampled with the MCMC algorithm.  
# That is, for a parameter $\theta$, let the m-th step sampled value be 
# $\theta^{(m)}$. Then, the plot is simply $m$ versus $\theta^{(m)}$.  



##########################################
#
#  trace plots
#
##########################################

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Ta.eps",
onefile=FALSE,print.it=F)

iind2<-seq(1,200)
plot(iind2,mu[iind2],type="l", xlab="Iteration number", ylab="mu",
cex=1.5, 
main="Trace plot for mu")

dev.off()

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Tb.eps",
onefile=FALSE,print.it=F)

iind2<-seq(1,200)
iind2a<-seq(6,200)

plot(iind2a,mu[iind2a],type="l", xlab="Iteration number", ylab="mu",
cex=1.5, main="Trace plot for mu, dropping first 5 obs")

dev.off()


postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Td.eps",
onefile=FALSE,print.it=F)

iind2<-seq(1,200)
iind2a<-seq(6,200)

plot(iind2a,tau[iind2a],type="l", xlab="Iteration number", ylab="tau",
cex=1.5, 
main="Trace plot for tau, dropping first 5 obs")

dev.off()

# The following sequence of plots show the estimated density of $\mu$ 
# calculated from a sample from a MCMC chain, from a sample of the 
# independent distribution, and a plot of the true posterior marginal 
# distribution.



##########################################
#
#  density plots
#
##########################################

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Dma.eps",
onefile=FALSE,print.it=F)

plot(density(mu[-(1:5)]),type="l", xlab="mu", ylab=" ",
cex=1.5, xlim=c(55,75), main="Estimate mu by MCMC")

dev.off()

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Dmb.eps",
onefile=FALSE,print.it=F)

plot(density(mu[-(1:5)]),type="l", xlab="mu", ylab=" ",
cex=1.5, xlim=c(55,75), main="Estimate mu by Independent Sampling")
lines(density(xmu),type="l")

dev.off()

postscript(file="c:/mike workstation/bayescourse9/plots/lec2Ex7Dmc.eps",
onefile=FALSE,print.it=F)

plot((dd<-density(mu[-(1:5)])),type="l", xlab="mu", ylab=" ",
cex=1.5, xlim=c(55,75),
main="True density of mu")

lines(density(xmu),type="l")
xx<-dd$x
c2<-1/sqrt( xb1/xa1/xthep)
tt<-(xx-xmu0)*c2
ddt<-dt(tt,2*xa1)*c2
lines(xx,ddt,type="l",lwd=10)

dev.off()


