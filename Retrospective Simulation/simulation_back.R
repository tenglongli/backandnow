## Simulation for Bayesian calculation and nowcasting
## First, get the report data from the state of Massachusetts
## cdc <- readRDS('../Data/cdc.rds')
## library(tidyverse)
## cdc1 <- cdc%>%filter(state=='TX')
## report <- table(cdc1$report)
## report <- as.numeric(report[report>1])
## report <- c(report,round(runif(20,100,1000)))
## saveRDS(report,'report.rds')
####################### END #########################

library(backnow)

## Set up the serial interval
si <- function(ndays,alpha,theta){
  prob <- numeric(ndays)
  for (i in 1:ndays){
    prob[i] <- pgamma(i,shape = alpha,scale = theta) - pgamma(i-1,shape = alpha,scale = theta)
  }
  result <- prob/sum(prob)
  return(result)
}

si1 <- si(14,4,1/0.6) 


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

## Set parameters for delay distribution 
## There are 60 days of reporting and nine weeks (there will be no regional intercept)
## Set the dispersion parameter r as 3
## Set the weekly parameters as: [2,1.9,1.8,1.7,1.6,1.5,1.65,1.55,1.45]
## Set the weekend parameter as 0.2
reportn <- readRDS('report.rds')
## library(Rcpp)
## sourceCpp('backnow.cpp')
wind <- rep(1:9,times=c(rep(7,8),4))
wkd <- c(rep(rep(c(0,1),c(5,2)),times=8),rep(0,4))
par <- c(2,1.9,1.8,1.7,1.6,1.5,1.65,1.55,1.45,0.2,3)
## About 60% of the onset dates were missing in the CDC data
## For real data, probably needs to trim the cases with reporting delay > maxdelay 

d1 <- simdata(reportn,par,0.6,si1,20,wind,wkd,6)
dat <- d1$data

out <- mcmc(dat$delay,dat$report,dat$week,dat$weekend,11000,0.2,20) ## Probably needs 1000 burn-in and 10 thinning
## Postprocessing
report <- reportn[1:40]
d <- as.Date("2020-04-03")
w <- wind[1:40]
postprocess(report,d,w,20,k,p,14,4,1/0.6,6,n0 = reportn[41:60])


## Parallel Processing
## Step 1-Make a simulation function
library(parallel)
P <- detectCores(logical = FALSE) ## P = 4
cl <- makeCluster(P)
clusterEvalQ(cl, {library(tidyverse);library(backnow)})
clusterExport(cl,c("si","simdata","reportn"))
simfun <- function(trial,report,parameter){
  si1 <- si(14,4,1/0.6) 
  wind <- rep(1:9,times=c(rep(7,8),4))
  wkd <- c(rep(rep(c(0,1),c(5,2)),times=8),rep(0,4))
  d1 <- simdata(report,parameter,0.6,si1,20,wind,wkd,6)
  dat <- d1$data
  r <- d1$true_r
  curve <- d1$true_curve
  out <- mcmc(dat$delay,dat$week,dat$weekend,21000,0.2,20)
  out <- out[seq(1001,21000,by=10),]
  report1 <- reportn[1:40]
  d <- as.Date("2020-04-03")
  w <- wind[1:40]
  r1 <- c(1.9,1.9,1.8,1.7,1.7,1.6,1.5,1.5,1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.3,1.3,1.3,1.3,1.3)
  r2 <- c(1.5,1.8,1.8,1.8,1.8,1.3,1.3,1.2,1.2,1.5,1.5,1.5,1.5,1.6,1.6,1.6,1.5,1.5,1.4,1.3)
  c1 <- report[41:60]
  c2 <- rep(400,20)
  dynamic <- postprocess(report1,d,w,20,out,14,4,1/0.6,6)
  fixed_ub <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "fixed",useR = TRUE, r0 = 1.5)
  fixed_b <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "fixed",useR = TRUE, r0 = 1.8)
  specific_ub <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "specific",useR = TRUE, r0 = r1)
  specific_b <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "specific",useR = TRUE, r0 = r2)
  np_ub <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "np",n0 = c1)
  np_b <- postprocess(report1,d,w,20,out,14,4,1/0.6,6,method = "np",n0 = c2)
  result <- list(curve = curve,r = r, dynamic = dynamic, fub = fixed_ub, fb = fixed_b, sub = specific_ub, sb = specific_b, npub = np_ub, npb = np_b)
  return(result)
}

out <- clusterApply(cl,rep(1,4),fun = simfun,report = reportn, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.5,1.5,1.5,0.2,3))
stopCluster(cl)

## simulation scenario 1: the delay distribution does not change in the future, with report curve reportn
## out <- clusterApply(cl,rep(1,4),fun = simfun,report = reportn, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.5,1.5,1.5,0.2,3))

## simulation scenario 2: the delay distribution change in the future, with report curve reportn
## out <- clusterApply(cl,rep(1,4),fun = simfun,report = reportn, parameter = c(2,1.9,1.8,1.7,1.6,1.5,1.65,1.75,1.85,0.2,3))

reportn1 = reportn
reportn1[21:30] = round(runif(10,100,200))
reportn1[31:40] = round(runif(10,200,300))
reportn1[41:50] = round(runif(10,300,400))
reportn1[51:60] = round(runif(10,500,600))
saveRDS(reportn1,"report1.rds")


## Simulation results processing
out1 <- readRDS('sce1.rds')
out2 <- readRDS('sce2.rds')
out3 <- readRDS('sce3.rds')
out4 <- readRDS('sce4.rds')

## Gather the means and generate the graph
means <- function(output){
  rvalue <- mat.or.vec(53,1000)
  cvalue <- mat.or.vec(60,1000)
  subcl <- mat.or.vec(60,1000)
  subcm <- mat.or.vec(60,1000)
  subcu <- mat.or.vec(60,1000)
  subrl <- mat.or.vec(53,1000)
  subrm <- mat.or.vec(53,1000)
  subru <- mat.or.vec(53,1000)
  sbcl <- mat.or.vec(60,1000)
  sbcm <- mat.or.vec(60,1000)
  sbcu <- mat.or.vec(60,1000)
  sbrl <- mat.or.vec(53,1000)
  sbrm <- mat.or.vec(53,1000)
  sbru <- mat.or.vec(53,1000)
  npubcl <- mat.or.vec(60,1000)
  npubcm <- mat.or.vec(60,1000)
  npubcu <- mat.or.vec(60,1000)
  npubrl <- mat.or.vec(53,1000)
  npubrm <- mat.or.vec(53,1000)
  npubru <- mat.or.vec(53,1000)
  npbcl <- mat.or.vec(60,1000)
  npbcm <- mat.or.vec(60,1000)
  npbcu <- mat.or.vec(60,1000)
  npbrl <- mat.or.vec(53,1000)
  npbrm <- mat.or.vec(53,1000)
  npbru <- mat.or.vec(53,1000)
  for (i in 1:1000){
    rvalue[,i] <- output[[i]]$r
    cvalue[,i] <- output[[i]]$curve
    subcl[,i] <- output[[i]]$sub$Count$lower
    subcm[,i] <- output[[i]]$sub$Count$median
    subcu[,i] <- output[[i]]$sub$Count$upper
    subrl[,i] <- output[[i]]$sub$R$lower
    subrm[,i] <- output[[i]]$sub$R$median
    subru[,i] <- output[[i]]$sub$R$upper
    sbcl[,i] <- output[[i]]$sb$Count$lower
    sbcm[,i] <- output[[i]]$sb$Count$median
    sbcu[,i] <- output[[i]]$sb$Count$upper
    sbrl[,i] <- output[[i]]$sb$R$lower
    sbrm[,i] <- output[[i]]$sb$R$median
    sbru[,i] <- output[[i]]$sb$R$upper
    npubcl[,i] <- output[[i]]$npub$Count$lower
    npubcm[,i] <- output[[i]]$npub$Count$median
    npubcu[,i] <- output[[i]]$npub$Count$upper
    npubrl[,i] <- output[[i]]$npub$R$lower
    npubrm[,i] <- output[[i]]$npub$R$median
    npubru[,i] <- output[[i]]$npub$R$upper
    npbcl[,i] <- output[[i]]$npb$Count$lower
    npbcm[,i] <- output[[i]]$npb$Count$median
    npbcu[,i] <- output[[i]]$npb$Count$upper
    npbrl[,i] <- output[[i]]$npb$R$lower
    npbrm[,i] <- output[[i]]$npb$R$median
    npbru[,i] <- output[[i]]$npb$R$upper
  }
  lowerc <- c(apply(subcl,1,mean),apply(sbcl,1,mean),apply(npubcl,1,mean),apply(npbcl,1,mean))
  medc <- c(apply(subcm,1,mean),apply(sbcm,1,mean),apply(npubcm,1,mean),apply(npbcm,1,mean))
  upperc <- c(apply(subcu,1,mean),apply(sbcu,1,mean),apply(npubcu,1,mean),apply(npbcu,1,mean))
  lowerr <- c(apply(subrl,1,mean),apply(sbrl,1,mean),apply(npubrl,1,mean),apply(npbrl,1,mean))
  medr <- c(apply(subrm,1,mean),apply(sbrm,1,mean),apply(npubrm,1,mean),apply(npbrm,1,mean))
  upperr <- c(apply(subru,1,mean),apply(sbru,1,mean),apply(npubru,1,mean),apply(npbru,1,mean))
  methodc <- rep(c("SUB","SB","NPUB","NPB"),each=60)
  methodr <- rep(c("SUB","SB","NPUB","NPB"),each=53)
  c <- apply(cvalue,1,mean)
  r <- apply(rvalue,1,mean)
  truec <- rep(c,times=4)
  truer <- rep(r,times=4)
  datec <- output[[1]]$sub$Count$date
  dater <- output[[1]]$sub$R$date
  datec <- rep(datec,times=4)
  dater <- rep(dater,times=4)
  count <- data.frame(lower=lowerc,median=medc,upper=upperc,true=truec,method=methodc,date=datec)
  rest <- data.frame(lower=lowerr,median=medr,upper=upperr,true=truer,method=methodr,date=dater)
  return(list(c=count,r=rest))
}

result1 <- means(out1)
result2 <- means(out2)
result3 <- means(out3)
result4 <- means(out4)

c1 <- cbind(result1$c,scenario="Scenario 1")
c2 <- cbind(result2$c,scenario="Scenario 2")
c3 <- cbind(result3$c,scenario="Scenario 3")
c4 <- cbind(result4$c,scenario="Scenario 4")  
c <- rbind(c1,c2,c3,c4)
  
r1 <- cbind(result1$r,scenario="Scenario 1")
r2 <- cbind(result2$r,scenario="Scenario 2")
r3 <- cbind(result3$r,scenario="Scenario 3")
r4 <- cbind(result4$r,scenario="Scenario 4")  
r <- rbind(r1,r2,r3,r4)

## Make plots
library(tidyverse)
c%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, nrow = 2)
## Dynamic method is way off

## Zoom-in a little bit, create 3 additional plots
c1 <- c%>%filter(method=="FB"|method=="FUB")
c2 <- c%>%filter(method=="SB"|method=="SUB")
c3 <- c%>%filter(method=="NPB"|method=="NPUB")

c1%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, nrow = 2)

c2%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, nrow = 2)

c3%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, nrow = 2)

## For reproductive number estimates

r%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Reproductive Number")+facet_wrap(~scenario, nrow = 2)
## Dynamic method is way off

## Zoom-in a little bit, create 3 additional plots
r1 <- r%>%filter(method=="FB"|method=="FUB")
r2 <- r%>%filter(method=="SB"|method=="SUB")
r3 <- r%>%filter(method=="NPB"|method=="NPUB")

r1%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Reproductive Number")+facet_wrap(~scenario, nrow = 2)

r2%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Reproductive Number")+facet_wrap(~scenario, nrow = 2)

r3%>%ggplot(aes(x=date,y=true))+geom_line()+
  annotate("rect",xmin = as.Date("2020-03-15"),xmax=as.Date("2020-04-03"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=method),stat = "identity") +
  theme_bw()+theme(axis.text = element_text(size = 12)) + labs(x = "Date", y = "Reproductive Number")+facet_wrap(~scenario, nrow = 2)

## NP method is robust to deviations in future report case counts
reportn <- readRDS('report.rds')
reportn1 <- readRDS('report1.rds')
reportn[41:60] ## for NPB we use 600
reportn1[41:60] ## for NPB we use 400

## 
check <- function(data){
  ind <- (data[,1]-data[,3]<=0)&(data[,2]-data[,3]>=0)
  result <- return(as.numeric(ind))
}


## Compute the coverage rate
coverage <- function(output){
  sub_r <- vector("list",1000)
  sub_c <- vector("list",1000)
  sb_r <- vector("list",1000)
  sb_c <- vector("list",1000)
  npub_r <- vector("list",1000)
  npub_c <- vector("list",1000)
  npb_r <- vector("list",1000)
  npb_c <- vector("list",1000)
  for (i in 1:1000){
    subr <- as.matrix(cbind(output[[i]]$sub$R$lower,output[[i]]$sub$R$upper,output[[i]]$r))
    sub_r[[i]] <- subr
    subc <- as.matrix(cbind(output[[i]]$sub$Count$lower,output[[i]]$sub$Count$upper,output[[i]]$curve))
    sub_c[[i]] <- subc
    sbr <- as.matrix(cbind(output[[i]]$sb$R$lower,output[[i]]$sb$R$upper,output[[i]]$r))
    sb_r[[i]] <- sbr
    sbc <- as.matrix(cbind(output[[i]]$sb$Count$lower,output[[i]]$sb$Count$upper,output[[i]]$curve))
    sb_c[[i]] <- sbc
    npubr <- as.matrix(cbind(output[[i]]$npub$R$lower,output[[i]]$npub$R$upper,output[[i]]$r))
    npub_r[[i]] <- npubr
    npubc <- as.matrix(cbind(output[[i]]$npub$Count$lower,output[[i]]$npub$Count$upper,output[[i]]$curve))
    npub_c[[i]] <- npubc
    npbr <- as.matrix(cbind(output[[i]]$npb$R$lower,output[[i]]$npb$R$upper,output[[i]]$r))
    npb_r[[i]] <- npbr
    npbc <- as.matrix(cbind(output[[i]]$npb$Count$lower,output[[i]]$npb$Count$upper,output[[i]]$curve))
    npb_c[[i]] <- npbc
  }
  subrcov <- apply(matrix(unlist(lapply(sub_r,check)),byrow=T,nrow = 1000),2,sum)
  subccov <- apply(matrix(unlist(lapply(sub_c,check)),byrow=T,nrow = 1000),2,sum)
  sbrcov <- apply(matrix(unlist(lapply(sb_r,check)),byrow=T,nrow = 1000),2,sum)
  sbccov <- apply(matrix(unlist(lapply(sb_c,check)),byrow=T,nrow = 1000),2,sum)
  npubrcov <- apply(matrix(unlist(lapply(npub_r,check)),byrow=T,nrow = 1000),2,sum)
  npubccov <- apply(matrix(unlist(lapply(npub_c,check)),byrow=T,nrow = 1000),2,sum)
  npbrcov <- apply(matrix(unlist(lapply(npb_r,check)),byrow=T,nrow = 1000),2,sum)
  npbccov <- apply(matrix(unlist(lapply(npb_c,check)),byrow=T,nrow = 1000),2,sum)
  ccov <- data.frame(cbind(sub=subccov,sb=sbccov,npub=npubccov,npb=npbccov))
  rcov <- data.frame(cbind(sub=subrcov,sb=sbrcov,npub=npubrcov,npb=npbrcov))
  return(list(ccov=ccov,rcov=rcov))
}

cov1=coverage(out1)

## Increase the number of iterations or the jump size does not help the Bayesian CI
## Need to think about how to widen the CI a bit to improve the coverage rate





