#
#  Monoclonal Dataset
#  Michael Escobar 
#  February 12, 2018
#
#-----------------------------
#
#   This code is for an in class demo of how to do a basic OpenBugs example
#   
#-----------------------------
#  
#    The goal is to write the basic code to perform the analysis  
#     1) preform the poisson regression model.
#     2) consider an overdispersion papameter
#     3) 
#     4) look at some basic convergence diagnostic
#     5) Primarily interested in looking at the occupation effect (farm)report:
#        a) get the summary statistics of the farm parameter
#        b) Look at a plot of the marginal posterior distribution
#        c) Say in words what the results is?  (That is, what is the expected 
#           increase, range of increase, and what this means in scientific terms.)
#

# WorkDir<- "c:/mike workstation/Bayescourse15/Documents/Week 6 in class excercise"
# Note: change this to the file where everything is
# setwd(WorkDir)




library(R2OpenBUGS)

######################################
######################################
Monogam.dsc=cat("data from _GLIM: An Introduction_
                by MJR Healy, pg 88
                
                Number of cases of a particular disease condition monoclonal gammopathy, for those who are interested) in the Finistrere region of France.  The subjects are classified by age and sex and by occupation into farming and non-farming groups.  The main point of interest is whether the disease prevalence is different in the two occupational groups.  There is an obvious increase in the prevalence rates with age; judging by the bottom marginal totals there appears to be a substantial occupational effect and possibly a sex effect too....")



cat(
  "case  n     age   male  farm
  1     1590  30    1     1
  1     1926  30    0     1
  2     1527  30    1     0
  0      712  30    0     0
  12    2345  40    1     1
  7     2677  40    0     1
  3      854  40    1     0
  0      401  40    0     0
  24    2787  50    1     1
  15    2902  50    0     1
  5      675  50    1     0
  4      312  50    0     0
  53    2489  60    1     1
  38    3145  60    0     1
  3      184  60    1     0
  1       80  60    0     0
  95    2381  70    1     1
  63    2918  70    0     1
  2       75  70    1     0
  0       20  70    0     0
  ", file="monogamD.txt")




monogamDat=read.table("monogamD.txt",header=TRUE,sep = "")
attach(monogamDat)
agecat <- match(age, unique(age))
### this will make age categories 1,2,3, etc.



######`#################  model 1  w/fam and overdisp  
cat("
    model{	
    for(i in 1:20){
    case[i] ~ dpois(lam[i])
    ### code needed here
     

    log(lam[i])= log(n[i]) + b.age[agecat[i]] + bf*farm[i] + bm*male[i])
    
    for (iage in 1:5){ 
    
    b.age[iage] = dnorm(agemu, tage)

    }
    
    agemu = dnorm(i, 1/10/10)
    tage = 1/sage/sage
    sage~dunif(0,7)
    bf~dnorm(0,5)
    bm~dnorm(0,5)

    }
    
    ### code needed here
    
    
    }
    ", file="MonogamM1.txt")  

bugM1.dat=list("case", "n", "male", "farm","agecat")  # what variable you need in the model
initM1.fun=function(){ list(bage=rnorm(5, 0,1), b0= rnorm(1, 0,1), b=rnorm(20, 0, .1), bf=rnorm(1, 0,1), bm=rnorm(1,0,1))}

##### need to put something here

paramsM1=c("b.age","bm", "bf", "agemu", "tage", "sage")     ### what variables you want to monitor



#### Could change the code below...
MonogamM1R=bugs(bugM1.dat, initM1.fun, paramsM1, model.file="MonogamM1.txt",
                n.chains=3, n.iter=30000, n.burnin=2000,
                n.thin=1 , debug=TRUE
)





