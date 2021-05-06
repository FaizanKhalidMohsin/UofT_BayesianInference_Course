#Importing data and defining initial variables

set.seed(25)

nbig<-20000

data = matrix( c(    1.13, 1.75, 2.30, 3.18,
                      1.20, 1.45, 2.15, 3.10,
                      1.00, 1.55 ,2.25, 3.28,
                      0.91, 1.64, 2.40, 3.35,
                      1.05, 1.60, 2.49, 3.12), nrow = 5)
data = as.data.frame(data)
colnames(data) = c("temp40", "temp60", "temp80", "temp100")

Y1<- mean(data$temp40)
Y1
Y2<- mean(data$temp60)
Y3<- mean(data$temp80)
Y4<- mean(data$temp100)

attach(data)

#Algorithm used 

       
mu00<-0
tau00<-1
alp<-1
beta<-1
alp0<-1
beta0<-1
mu1<-rep(0,nbig)
mu2<-rep(0,nbig)
mu3<-rep(0,nbig)
mu4<-rep(0,nbig)
tau<-rep(0,nbig)
mu0post1<-rep(0,nbig) 
tau0post1<-rep(0,nbig) 
mu0post2<-rep(0,nbig)
tau0post2<-rep(0,nbig)
mu0post3<-rep(0,nbig) 
tau0post3<-rep(0,nbig)
mu0post4<-rep(0,nbig) 
tau0post4<-rep(0,nbig) 
mu0<-rep(0,nbig)
tau0<-rep(0,nbig)
mupost00<-rep(0,nbig) 
taupost00<-rep(0,nbig)  
betapost<-rep(0,nbig)
betapost0<-rep(0,nbig)
mu0[1]<-1  
mu1[1]<-0
mu2[1]<-0
mu3[1]<-0
mu4[1]<-0
tau[1]<-1
tau0[1]<-1

for(i in 2:nbig) {
  mu0post1[i]<-(tau0[i-1]*mu0[i-1]+5*tau[i-1]*Y1)/(tau0[i-1]+5*tau[i-1])
  tau0post1[i]<-tau0[i-1]+tau[i-1]*5
  mu1[i]<-rnorm(1,mu0post1[i],sd=1/sqrt(tau0post1[i]))
  mu0post2[i]<-(tau0[i-1]*mu0[i-1]+5*tau[i-1]*Y2)/(tau0[i-1]+5*tau[i-1])
  tau0post2[i]<-tau0[i-1]+tau[i-1]*5
  mu2[i]<-rnorm(1,mu0post2[i],sd=1/sqrt(tau0post2[i]))
  mu0post3[i]<-(tau0[i-1]*mu0[i-1]+5*tau[i-1]*Y3)/(tau0[i-1]+5*tau[i-1])
  tau0post3[i]<-tau0[i-1]+tau[i-1]*5
  mu3[i]<-rnorm(1,mu0post3[i],sd=1/sqrt(tau0post3[i]))
  mu0post4[i]<-(tau0[i-1]*mu0[i-1]+5*tau[i-1]*Y4)/(tau0[i-1]+5*tau[i-1])
  tau0post4[i]<-tau0[i-1]+tau[i-1]*5
  mu4[i]<-rnorm(1,mu0post4[i],sd=1/sqrt(tau0post4[i]))
  mupost00[i]<-(4*tau0[i-1]*((mu1[i-1]+mu2[i-1]+mu3[i-1]+mu4[i-1])/4)+tau00*mu00)/(4*tau0[i-1]+tau00)
  taupost00[i]<-4*tau0[i-1]+ tau00
  mu0[i]<-rnorm(1,mupost00[i],1/sqrt(taupost00[i]))
  alppost0<-alp0+(4/2) 
  betapost0[i]<-beta0+(1/2)*(sum((mu1[i]-mu0[i])^2)+sum((mu2[i]-mu0[i])^2)+sum((mu3[i]-mu0[i])^2)+sum((mu4[i]-mu0[i])^2))
  tau0[i]<-rgamma(1,alppost0,betapost0[i])
  alppost<-alp+(5+5+5+5)/2 
  betapost[i]<- beta+(0.5*(sum((temp40-mu1[i])^2)+sum((temp60-mu2[i])^2)+sum((temp80-mu3[i])^2)+sum((temp100-mu4[i])^2)))
  tau[i]<-rgamma(1,alppost,betapost[i]) 
}

cbind(mu0,mu1,mu2,mu3,mu4,tau,tau0)[1:10,]





smokeDat=read.csv("SmokeAgeDeath.csv")
library(R2OpenBUGS)
library(coda)
#Code for OpenBUGS model given below
cat("
    model{
    for(i in 1:20)
    {
    #smoke   age   death  pyears 
    
    death[i]~dpois(lam[i])
    log(lam[i]) <- log(pyears[i]) + beta0 + beta.s[smoke[i]] + beta.c[age[i]] + b[i] 
    b[i] ~dnorm(0,tau)
    b.adj[i] <- b[i] - mean(b[]) 
    }
    for(is in 1:4){
    beta.s[is]~dnorm(0,tau.s)
    beta.s.adj[is] <- beta.s[is] -mean(beta.s[]) 
    }
    for(ic in 1:5){
    beta.c[ic]~dnorm(0,tau.c)
    beta.c.adj[ic] <- beta.c[ic] - mean(beta.c[])
    }
    #  Note: total person-years per categories is less than 115,000
    #  ln(115000) ~  = 11.65....
    #  so rate has to be bigger than 1/115,000 and log(rate) > -11.65... 
    #  so log( base rate) should be between about -12 and 12.
    # 1/12/12 is about .00694
    beta0 ~ dnorm(0, .00694)  
    beta0.adj <- beta0 + mean(b[]) + mean(beta.s[])+ mean(beta.c[])
    # for the <extra poisson variation> ... 
    # assume bounded by very big number... say 1000 times... 
    # so log(1000) is about 2.3*4 which is about 9.2
    std ~ dunif(0, 9)
    tau <- 1/std/std
    # for the relative risk between groups... a very large number would be 100 times, 
    #  so, log(100) is about 2.3*2 or about 4.6
    #  also, note that 1/5/5 is 0.04
    #  
    std.s ~dunif(0, 5)
    tau.s <- 1/std.s/std.s
    std.c ~ dunif(0,5) 
    tau.c <- 1/std.c/std.c
    beta.o~dnorm(0, .04)
    }", file="smokemod.txt")

#defining parameters and data for the bugs function

params=c("beta.s.adj",  "std.s", "beta.c.adj", "std.c", "beta0.adj", "std")
attach(smokeDat)

bug.dat=list("smoke","age","death", "pyears")
init.fun=function(){list(
  beta.s=rnorm(4), std.s=runif(1,1,2),
  beta.c=rnorm(5), std.c=runif(1,1,2), 
  std=runif(1,1,2), beta0=rnorm(1),
  b=rnorm(20,0,.1))}

#using openBUGS to run our model, this code also gives the posterior which we need for 2a in OpenBUGS

smokeBug0=bugs(bug.dat, init.fun, params, model.file="smokemod.txt",
               n.chains=5, n.iter=8000, n.burnin=1000, debug=TRUE  #for production
               #	n.chains=5, n.iter=6000, n.burnin=1000, debug=TRUE   #for testing
)


#boxplots for smoking categories

boxplot(data.frame( (smokeBug0$sims.list)["beta.s.adj"]),
        xlab="beta.s.adj")

#boxplots for age categories

boxplot(data.frame( (smokeBug0$sims.list)["beta.c.adj"]),
        xlab="beta.c.adj")

# Question 2 b. 

# The following plots show that our model converges
#autocorrelation functions given below
#trace plots have been shown in the output given above that shows the convergence in chains after 8000 iterations
acf(smokeBug0$sims.array[,1,"beta.s.adj[1]"]) 

#After 10 lags the plot seems to have converged. We observe the same for age which converges twice as #fast as shown below
acf(smokeBug0$sims.array[,1,"beta.c.adj[1]"])


acf(smokeBug0$sims.array[,1,"beta0.adj"])

# Question 2 b. 

risk20<-smokeBug0$sims.list$beta.s.adj[,4]-smokeBug0$sims.list$beta.s.adj[,1]
mean(risk20)
1.382443
exp(mean(risk20))
3.984622

#The above shows those who smoke >20 cigarettes per day versus nonsmoker have approximately 4 times (=3.98) the risk. We take the exponential in the last line since the model gives us the log of the risks.

quantile(exp(risk20), c(.025, .975)) # gives the 95% Cis for above risk
#    2.5%    97.5% 
#2.763903 5.700207
