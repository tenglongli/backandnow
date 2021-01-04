rpois <- Vectorize(rpois,"lambda")
rt <- readRDS('rt.rds') ## Rt input
## Set up the serial interval
si <- function(ndays,alpha,beta){
  prob <- numeric(ndays)
  for (i in 1:ndays){
    prob[i] <- pgamma(i,shape = alpha,rate = beta) - pgamma(i-1,shape = alpha,rate = beta)
  }
  result <- prob/sum(prob)
  return(result)
}

sip <- si(14,4.29,1.18) 

## Simulation function
## r is the size parameter of reporting delay distribution
## mu is the mean parameter of reporting delay distribution
## ld is the last reporting date, an integer
## Scenario 1 & 2: single reporting delay distribution 
simdata <- function(n0,r0,rt,r,m,maxdelay,ld){
  n1=rpois(1,n0*r0)
  inf1=rgamma(n1,shape = 4.29,rate = 1.18) 
  i1=ceiling(inf1) ## Date of infection
  inc1=exp(rnorm(n1,1.621,0.418)) 
  o1=i1+ceiling(inc1) ## Date of onset sympotom
  p=dnbinom(0:maxdelay,size = r, mu = m)
  d1=o1+sample(0:maxdelay,size = n1,prob = p,replace = TRUE) ## Date of report
  dat=cbind(i1,o1,d1)
  while(any(i1<=ld)){
    date=as.numeric(names(table(i1))) ## Infection dates of previous generation
    no=as.numeric(table(i1)) ## Number of cases on each date
    mu=no*rt[date]
    n1=rpois(1,mu)
    inf1=rgamma(sum(n1),shape = 4.29,rate = 1.18)
    i1=rep(date,times=n1)+ceiling(inf1)
    inc1=exp(rnorm(sum(n1),1.621,0.418)) 
    o1=i1+ceiling(inc1) ## Date of onset sympotom
    d1=o1+sample(0:maxdelay,size = sum(n1),prob = p,replace = TRUE) ## Date of report
    dat1=cbind(i1,o1,d1)
    dat=rbind(dat,dat1)
  }
  ## 3 outputs
  ## 1-Report Curve
  ## 2-Epidemic Curve
  ## 3-Data used for analysis
  dat=data.frame(dat)
  colnames(dat)=c("infect","onset","report")
  epic=table(dat[dat$onset<=ld,]$onset)
  dat <- dat[dat$report<=ld,c(2,3)]
  dat[sample(nrow(dat),round(nrow(dat)*0.6)),1] <- NA
  repc=table(dat$report) ## Report curve
  return(result=list(repc=repc,epic=epic,dat=dat))
}

simdata <- compiler::cmpfun(simdata)

## Scenario 3 & 4: two reporting delay distribution 
## cd: cut-off day, defined for onset dates, after which the reporting delay distribution will change
simdata1 <- function(n0,r0,rt,r,m,maxdelay,ld,cd){
  n1=rpois(1,n0*r0)
  inf1=rgamma(n1,shape = 4.29,rate = 1.18) 
  i1=ceiling(inf1) ## Date of infection
  inc1=exp(rnorm(n1,1.621,0.418)) 
  o1=i1+ceiling(inc1) ## Date of onset sympotom
  p1=dnbinom(0:maxdelay,size = r[1], mu = m[1])
  p2=dnbinom(0:maxdelay,size = r[2], mu = m[2])
  dat=cbind(i1,o1)
  while(any(i1<=ld)){
    date=as.numeric(names(table(i1))) ## Infection dates of previous generation
    no=as.numeric(table(i1)) ## Number of cases on each date
    mu=no*rt[date]
    n1=rpois(1,mu)
    inf1=rgamma(sum(n1),shape = 4.29,rate = 1.18)
    i1=rep(date,times=n1)+ceiling(inf1)
    inc1=exp(rnorm(sum(n1),1.621,0.418)) 
    o1=i1+ceiling(inc1) ## Date of onset sympotom
    dat1=cbind(i1,o1)
    dat=rbind(dat,dat1)
  }
  ## 3 outputs
  ## 1-Report Curve
  ## 2-Epidemic Curve
  ## 3-Data used for analysis
  dat=data.frame(dat)
  dat$d=0
  colnames(dat)=c("infect","onset","report")
  dat$report[dat$onset<cd]=dat$onset[dat$onset<cd]+sample(0:maxdelay,size = sum(dat$onset<cd),prob = p1,replace = TRUE)
  dat$report[dat$onset>=cd]=dat$onset[dat$onset>=cd]+sample(0:maxdelay,size = sum(dat$onset>=cd),prob = p2,replace = TRUE)
  epic=table(dat[dat$onset<=ld,]$onset)
  dat <- dat[dat$report<=ld,c(2,3)]
  dat[sample(nrow(dat),round(nrow(dat)*0.6)),1] <- NA
  repc=table(dat$report) ## Report curve
  return(result=list(repc=repc,epic=epic,dat=dat))
}

simdata1 <- compiler::cmpfun(simdata1)


## Scenario 5 & 6: multiple reporting delay distribution 
simdata2 <- function(n0,r0,rt,maxdelay,ld){
  n1=rpois(1,n0*r0)
  inf1=rgamma(n1,shape = 4.29,rate = 1.18) 
  i1=ceiling(inf1) ## Date of infection
  inc1=exp(rnorm(n1,1.621,0.418)) 
  o1=i1+ceiling(inc1) ## Date of onset sympotom
  p1=dnbinom(0:maxdelay,size = 3, mu = 9) ## before day 10
  p2=dnbinom(0:maxdelay,size = 3, mu = 8) ## between day 11 and day 20
  p3=dnbinom(0:maxdelay,size = 3, mu = 7) ## between day 21 and day 30
  p4=dnbinom(0:maxdelay,size = 3, mu = 6) ## between day 31 and day 40
  p5=dnbinom(0:maxdelay,size = 3, mu = 5) ## between day 41 and day 50
  p6=dnbinom(0:maxdelay,size = 3, mu = 4) ## after day 50
  dat=cbind(i1,o1)
  while(any(i1<=ld)){
    date=as.numeric(names(table(i1))) ## Infection dates of previous generation
    no=as.numeric(table(i1)) ## Number of cases on each date
    mu=no*rt[date]
    n1=rpois(1,mu)
    inf1=rgamma(sum(n1),shape = 4.29,rate = 1.18)
    i1=rep(date,times=n1)+ceiling(inf1)
    inc1=exp(rnorm(sum(n1),1.621,0.418)) 
    o1=i1+ceiling(inc1) ## Date of onset sympotom
    dat1=cbind(i1,o1)
    dat=rbind(dat,dat1)
  }
  ## 3 outputs
  ## 1-Report Curve
  ## 2-Epidemic Curve
  ## 3-Data used for analysis
  dat=data.frame(dat)
  dat$d=0
  colnames(dat)=c("infect","onset","report")
  dat$report[dat$onset<=10]=dat$onset[dat$onset<=10]+sample(0:maxdelay,size = sum(dat$onset<=10),prob = p1,replace = TRUE)
  dat$report[dat$onset>10&dat$onset<=20]=dat$onset[dat$onset>10&dat$onset<=20]+sample(0:maxdelay,size = sum(dat$onset>10&dat$onset<=20),prob = p2,replace = TRUE)
  dat$report[dat$onset>20&dat$onset<=30]=dat$onset[dat$onset>20&dat$onset<=30]+sample(0:maxdelay,size = sum(dat$onset>20&dat$onset<=30),prob = p3,replace = TRUE)
  dat$report[dat$onset>30&dat$onset<=40]=dat$onset[dat$onset>30&dat$onset<=40]+sample(0:maxdelay,size = sum(dat$onset>30&dat$onset<=40),prob = p4,replace = TRUE)
  dat$report[dat$onset>40&dat$onset<=50]=dat$onset[dat$onset>40&dat$onset<=50]+sample(0:maxdelay,size = sum(dat$onset>40&dat$onset<=50),prob = p5,replace = TRUE)
  dat$report[dat$onset>50]=dat$onset[dat$onset>50]+sample(0:maxdelay,size = sum(dat$onset>50),prob = p6,replace = TRUE)
  epic=table(dat[dat$onset<=ld,]$onset)
  dat <- dat[dat$report<=ld,c(2,3)]
  dat[sample(nrow(dat),round(nrow(dat)*0.6)),1] <- NA
  repc=table(dat$report) ## Report curve
  return(result=list(repc=repc,epic=epic,dat=dat))
}

simdata2 <- compiler::cmpfun(simdata2)


## Parallel Processing
## Step 1-Make a simulation function
library(parallel)
P <- detectCores(logical = FALSE) ## P = 4
cl <- makeCluster(P)
clusterEvalQ(cl, {library(tidyverse);library(backnow)})
clusterExport(cl,c("rt","rpois","sip","simdata","simdata1","simdata2"))

simfun <- function(trial,rt,maxdelay,si){
  out <- simdata(100,1.8,rt,3,9,maxdelay,60)
  d <- out$dat
  d <- d%>%arrange(report)
  d <- d%>%mutate(weekend=ifelse(report%%7==0|report%%7==6,1,0))
  d <- d%>%mutate(delay=report-onset,minday=min(report))
  d <- d%>%mutate(report=report-minday+1,week=ceiling(report/7))
  epic <- out$epic
  repc <- out$repc
  minday <- unique(d$minday)
  out1 <- mcmc(outcome=d$delay,days=d$report,week=d$week,weekend=d$weekend,iter=21000,sigma=0.2,maxdelay=20,si=si,size=6)
  back1 <- out1$Back[seq(1001,21000,by=2),]
  est1 <- apply(back1,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  r1 <- out1$R[seq(1001,21000,by=2),]
  estr1 <- apply(r1,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  out2 <- mcmc(outcome=d$delay,days=d$report,week=d$week,weekend=d$weekend,iter=21000,sigma=0.2,maxdelay=20,si=si,size=6,cd=31-minday)
  back2 <- out2$Back[seq(1001,21000,by=2),]
  est2 <- apply(back2,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  r2 <- out2$R[seq(1001,21000,by=2),]
  estr2 <- apply(r2,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  epicr <- getr(epic,si,6)
  repcr <- getr(repc,si,6)
  result <- list(est1=est1,est2=est2,estr1=estr1,estr2=estr2,epic=epic,epicr=epicr,repc=repc,repcr=repcr,minday=minday)
  return(result)
}

simfun1 <- function(trial,rt,maxdelay,si){
  out <- simdata1(100,1.8,rt,c(3,3),c(9,4),maxdelay,60,30)
  d <- out$dat
  d <- d%>%arrange(report)
  d <- d%>%mutate(weekend=ifelse(report%%7==0|report%%7==6,1,0))
  d <- d%>%mutate(delay=report-onset,minday=min(report))
  d <- d%>%mutate(report=report-minday+1,week=ceiling(report/7))
  epic <- out$epic
  repc <- out$repc
  minday <- unique(d$minday)
  out1 <- mcmc(outcome=d$delay,days=d$report,week=d$week,weekend=d$weekend,iter=21000,sigma=0.2,maxdelay=20,si=si,size=6)
  back1 <- out1$Back[seq(1001,21000,by=2),]
  est1 <- apply(back1,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  r1 <- out1$R[seq(1001,21000,by=2),]
  estr1 <- apply(r1,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  out2 <- mcmc(outcome=d$delay,days=d$report,week=d$week,weekend=d$weekend,iter=21000,sigma=0.2,maxdelay=20,si=si,size=6,cd=31-minday)
  back2 <- out2$Back[seq(1001,21000,by=2),]
  est2 <- apply(back2,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  r2 <- out2$R[seq(1001,21000,by=2),]
  estr2 <- apply(r2,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  epicr <- getr(epic,si,6)
  repcr <- getr(repc,si,6)
  result <- list(est1=est1,est2=est2,estr1=estr1,estr2=estr2,epic=epic,epicr=epicr,repc=repc,repcr=repcr,minday=minday)
  return(result)
}

simfun2 <- function(trial,rt,maxdelay,si){
  out <- simdata2(100,1.8,rt,maxdelay,60)
  d <- out$dat
  d <- d%>%arrange(report)
  d <- d%>%mutate(weekend=ifelse(report%%7==0|report%%7==6,1,0))
  d <- d%>%mutate(delay=report-onset,minday=min(report))
  d <- d%>%mutate(report=report-minday+1,week=ceiling(report/7))
  epic <- out$epic
  repc <- out$repc
  minday <- unique(d$minday)
  out1 <- mcmc(outcome=d$delay,days=d$report,week=d$week,weekend=d$weekend,iter=21000,sigma=0.2,maxdelay=20,si=si,size=6)
  back1 <- out1$Back[seq(1001,21000,by=2),]
  est1 <- apply(back1,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  r1 <- out1$R[seq(1001,21000,by=2),]
  estr1 <- apply(r1,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  out2 <- mcmc(outcome=d$delay,days=d$report,week=d$week,weekend=d$weekend,iter=21000,sigma=0.2,maxdelay=20,si=si,size=6,cd=31-minday)
  back2 <- out2$Back[seq(1001,21000,by=2),]
  est2 <- apply(back2,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  r2 <- out2$R[seq(1001,21000,by=2),]
  estr2 <- apply(r2,2,function(x) quantile(x,probs = c(0.025,0.5,0.975)))
  epicr <- getr(epic,si,6)
  repcr <- getr(repc,si,6)
  result <- list(est1=est1,est2=est2,estr1=estr1,estr2=estr2,epic=epic,epicr=epicr,repc=repc,repcr=repcr,minday=minday)
  return(result)
}

## Scenario 1: Single reporting delay distribution and correct maxdelay
out1 <- clusterApply(cl,rep(1,1000),fun = simfun,rt=rt,maxdelay=20,si=sip)
## Scenario 2: Single reporting delay distribution and incorrect maxdelay
out2 <- clusterApply(cl,rep(1,1000),fun = simfun,rt=rt,maxdelay=25,si=sip)
## Scenario 3: Two reporting delay distribution and correct maxdelay
out3 <- clusterApply(cl,rep(1,1000),fun = simfun1,rt=rt,maxdelay=20,si=sip)
## Scenario 4: Two reporting delay distribution and incorrect maxdelay
out4 <- clusterApply(cl,rep(1,1000),fun = simfun1,rt=rt,maxdelay=25,si=sip)
## Scenario 5: Multiple reporting delay distribution and correct maxdelay
out5 <- clusterApply(cl,rep(1,1000),fun = simfun2,rt=rt,maxdelay=20,si=sip)
## Scenario 6: Multiple reporting delay distribution and incorrect maxdelay
out6 <- clusterApply(cl,rep(1,1000),fun = simfun2,rt=rt,maxdelay=25,si=sip)


saveRDS(out1,"sce1.rds")
saveRDS(out2,"sce2.rds")
saveRDS(out3,"sce3.rds")
saveRDS(out4,"sce4.rds")
saveRDS(out5,"sce5.rds")
saveRDS(out6,"sce6.rds")

stopCluster(cl)




