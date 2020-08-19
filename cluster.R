## Input report curves
reportn <- readRDS('report.rds')
reportn1 <- readRDS('report1.rds')

si <- function(ndays,alpha,theta){
  prob <- numeric(ndays)
  for (i in 1:ndays){
    prob[i] <- pgamma(i,shape = alpha,scale = theta) - pgamma(i-1,shape = alpha,scale = theta)
  }
  result <- prob/sum(prob)
  return(result)
}


## Function for simulation
## Output 1: Line-list data for Bayesian estimation, based on the known parameters of delay distributions
## Output 2: The true epidemic curve
## Output 3: The true reproductive number estimates, based on the true epidemic curve and chosen method (dynamic or specific)
simdata <- function(reportn,par,pmiss,si,maxdelay,wind,wkd,size){
  mat <- dummy(wind,wkd)
  par1 <- par[-length(par)]
  mu <- meanvec(mat,par1)
  nr <- length(reportn)
  n1 <- nr-maxdelay
  k <- length(si)
  out <- mat.or.vec(nr+maxdelay,nr)
  report1 <- reportn[1:n1]
  report <- rep(1:length(report1),times=report1)
  delay <- numeric(length(report1))
  for (i in 1:nr){
    p <- dnbinom(maxdelay:0,size = par[length(par)],mu=mu[i])/sum(dnbinom(0:maxdelay,size = par[length(par)],mu=mu[i]))
    case <- rmultinom(1,reportn[i],p)
    if (i<n1){
      input <- rep(maxdelay:0,times=case)
      input[sample(reportn[i],round(reportn[i]*pmiss,0),replace = FALSE)] <- NA
      delay[report==i] <- input
    }
    out[i:(i+maxdelay),i] <- case
  }
  week <- rep(wind[1:n1],times=report1)
  weekend <- rep(wkd[1:n1],times=report1)
  dat <- data.frame(delay,report,week,weekend)
  epicurve <- rowSums(out)
  true_curve <- epicurve[1:nr]
  true_r <- getr(true_curve,si,size)
  result <- list(data=dat,true_curve=true_curve,true_r=true_r)
  return(result)
}

simdata <- compiler::cmpfun(simdata)

## Parallel Processing
## Step 1-Make a simulation function
library(parallel)
P <- detectCores(logical = FALSE) ## P = 4
cl <- makeCluster(P)
clusterEvalQ(cl, {library(tidyverse);library(backnow)})
clusterExport(cl,c("si","simdata","reportn","reportn1"))

simfun <- function(trial,report,parameter){
  si1 <- si(14,4,1/0.6) 
  wind <- rep(1:9,times=c(rep(7,8),4))
  wkd <- c(rep(rep(c(0,1),c(5,2)),times=8),rep(0,4))
  d1 <- simdata(report,parameter,0.6,si1,20,wind,wkd,6)
  dat <- d1$data
  r <- d1$true_r
  curve <- d1$true_curve
  out <- mcmc(dat$delay,dat$report,dat$week,dat$weekend,21000,0.2,20)
  p <- out$Parameter[seq(1001,21000,by=2),]
  k <- out$Back[seq(1001,21000,by=2),]
  report1 <- reportn[1:40]
  d <- as.Date("2020-04-03")
  w <- wind[1:40]
  r1 <- c(1.7,2,2.3,2.5,2.7,2.8,2.8,2.7,2.6,2.4,2.2,2,1.8,1.7,1.6,1.5,1.5,1.4,1.4,1.4)
  r2 <- c(1.5,1.8,2,2.3,2.3,2.5,2.6,2.8,3,2.6,2.4,2.2,2,1.9,1.9,1.7,1.7,1.6,1.6,1.6)
  c1 <- report[41:60]
  c2 <- rep(600,20)
  ## dynamic <- postprocess(report1,d,w,20,out,14,4,1/0.6,6)
  ## fixed_ub <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "fixed",useR = TRUE, r0 = 2)
  ## fixed_b <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "fixed",useR = TRUE, r0 = 1.5)
  specific_ub <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,FALSE,r0=r1)
  specific_b <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,FALSE,r0=r2)
  np_ub <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,n0 = c1)
  np_b <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,n0 = c2)
  result <- list(curve = curve,r = r, sub = specific_ub, sb = specific_b, npub = np_ub, npb = np_b)
  return(result)
}

simfun1 <- function(trial,report,parameter){
  si1 <- si(14,4,1/0.6) 
  wind <- rep(1:9,times=c(rep(7,8),4))
  wkd <- c(rep(rep(c(0,1),c(5,2)),times=8),rep(0,4))
  d1 <- simdata(report,parameter,0.6,si1,20,wind,wkd,6)
  dat <- d1$data
  r <- d1$true_r
  curve <- d1$true_curve
  out <- mcmc(dat$delay,dat$report,dat$week,dat$weekend,21000,0.2,20)
  p <- out$Parameter[seq(1001,21000,by=2),]
  k <- out$Back[seq(1001,21000,by=2),]
  report1 <- reportn[1:40]
  d <- as.Date("2020-04-03")
  w <- wind[1:40]
  r1 <- c(1.9,1.9,1.8,1.7,1.7,1.6,1.5,1.5,1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.3,1.3,1.3,1.3,1.3)
  r2 <- c(1.5,1.8,1.8,1.8,1.8,1.3,1.3,1.2,1.2,1.5,1.5,1.5,1.5,1.6,1.6,1.6,1.5,1.5,1.4,1.3)
  c1 <- report[41:60]
  c2 <- rep(400,20)
  specific_ub <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,FALSE,r0=r1)
  specific_b <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,FALSE,r0=r2)
  np_ub <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,n0 = c1)
  np_b <- postprocess(report1,d,w,20,k,p,14,4,1/0.6,6,n0 = c2)
  result <- list(curve = curve,r = r, sub = specific_ub, sb = specific_b, npub = np_ub, npb = np_b)
  return(result)
}



## Scenario 1: Report curve 1 and reporting delay distribution does not change for the nowcasted dates
out1 <- clusterApply(cl,rep(1,1000),fun = simfun,report = reportn, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.5,1.5,1.5,0.2,3))
## Scenario 2: Report curve 1 and reporting delay distribution does change for the nowcasted dates
out2 <- clusterApply(cl,rep(1,1000),fun = simfun,report = reportn, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.65,1.75,1.85,0.2,3))
## Scenario 3: Report curve 2 and reporting delay distribution does not change for the nowcasted dates
out3 <- clusterApply(cl,rep(1,1000),fun = simfun1,report = reportn1, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.5,1.5,1.5,0.2,3))
## Scenario 4: Report curve 2 and reporting delay distribution does change for the nowcasted dates
out4 <- clusterApply(cl,rep(1,1000),fun = simfun1,report = reportn1, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.65,1.75,1.85,0.2,3))

saveRDS(out1,"sce1.rds")
saveRDS(out2,"sce2.rds")
saveRDS(out3,"sce3.rds")
saveRDS(out4,"sce4.rds")
stopCluster(cl)




