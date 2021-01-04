#########################################
## Preprocessing the output: Section 1 ##
#########################################

## Read the output
sce1 <- readRDS('sce1.rds')
sce2 <- readRDS('sce2.rds')
sce3 <- readRDS('sce3.rds')
sce4 <- readRDS('sce4.rds')
sce5 <- readRDS('sce5.rds')
sce6 <- readRDS('sce6.rds')

## Summarizing curve estimates
## Collect the lower, median and upper bounds
cl11 <- mat.or.vec(1000,80)
cl12 <- mat.or.vec(1000,80)
cm11 <- mat.or.vec(1000,80)
cm12 <- mat.or.vec(1000,80)
cu11 <- mat.or.vec(1000,80)
cu12 <- mat.or.vec(1000,80)
cl21 <- mat.or.vec(1000,80)
cl22 <- mat.or.vec(1000,80)
cm21 <- mat.or.vec(1000,80)
cm22 <- mat.or.vec(1000,80)
cu21 <- mat.or.vec(1000,80)
cu22 <- mat.or.vec(1000,80)
cl31 <- mat.or.vec(1000,80)
cl32 <- mat.or.vec(1000,80)
cm31 <- mat.or.vec(1000,80)
cm32 <- mat.or.vec(1000,80)
cu31 <- mat.or.vec(1000,80)
cu32 <- mat.or.vec(1000,80)
cl41 <- mat.or.vec(1000,80)
cl42 <- mat.or.vec(1000,80)
cm41 <- mat.or.vec(1000,80)
cm42 <- mat.or.vec(1000,80)
cu41 <- mat.or.vec(1000,80)
cu42 <- mat.or.vec(1000,80)
cl51 <- mat.or.vec(1000,80)
cl52 <- mat.or.vec(1000,80)
cm51 <- mat.or.vec(1000,80)
cm52 <- mat.or.vec(1000,80)
cu51 <- mat.or.vec(1000,80)
cu52 <- mat.or.vec(1000,80)
cl61 <- mat.or.vec(1000,80)
cl62 <- mat.or.vec(1000,80)
cm61 <- mat.or.vec(1000,80)
cm62 <- mat.or.vec(1000,80)
cu61 <- mat.or.vec(1000,80)
cu62 <- mat.or.vec(1000,80)


## Extract known epidemic curve
c1 <- mat.or.vec(1000,80)
c2 <- mat.or.vec(1000,80)
c3 <- mat.or.vec(1000,80)
c4 <- mat.or.vec(1000,80)
c5 <- mat.or.vec(1000,80)
c6 <- mat.or.vec(1000,80)
## Extract the report curve
rc1 <- mat.or.vec(1000,80)
rc2 <- mat.or.vec(1000,80)
rc3 <- mat.or.vec(1000,80)
rc4 <- mat.or.vec(1000,80)
rc5 <- mat.or.vec(1000,80)
rc6 <- mat.or.vec(1000,80)


for (i in 1:1000){
  md1=sce1[[i]]$minday
  md2=sce2[[i]]$minday
  md3=sce3[[i]]$minday
  md4=sce4[[i]]$minday
  md5=sce5[[i]]$minday
  md6=sce6[[i]]$minday
  cl11[i,md1:80]=sce1[[i]]$est1[1,]
  cl12[i,md1:80]=sce1[[i]]$est2[1,]
  cm11[i,md1:80]=sce1[[i]]$est1[2,]
  cm12[i,md1:80]=sce1[[i]]$est2[2,]
  cu11[i,md1:80]=sce1[[i]]$est1[3,]
  cu12[i,md1:80]=sce1[[i]]$est2[3,]
  cl21[i,md2:80]=sce2[[i]]$est1[1,]
  cl22[i,md2:80]=sce2[[i]]$est2[1,]
  cm21[i,md2:80]=sce2[[i]]$est1[2,]
  cm22[i,md2:80]=sce2[[i]]$est2[2,]
  cu21[i,md2:80]=sce2[[i]]$est1[3,]
  cu22[i,md2:80]=sce2[[i]]$est2[3,]
  cl31[i,md3:80]=sce3[[i]]$est1[1,]
  cl32[i,md3:80]=sce3[[i]]$est2[1,]
  cm31[i,md3:80]=sce3[[i]]$est1[2,]
  cm32[i,md3:80]=sce3[[i]]$est2[2,]
  cu31[i,md3:80]=sce3[[i]]$est1[3,]
  cu32[i,md3:80]=sce3[[i]]$est2[3,]
  cl41[i,md4:80]=sce4[[i]]$est1[1,]
  cl42[i,md4:80]=sce4[[i]]$est2[1,]
  cm41[i,md4:80]=sce4[[i]]$est1[2,]
  cm42[i,md4:80]=sce4[[i]]$est2[2,]
  cu41[i,md4:80]=sce4[[i]]$est1[3,]
  cu42[i,md4:80]=sce4[[i]]$est2[3,]
  cl51[i,md5:80]=sce5[[i]]$est1[1,]
  cl52[i,md5:80]=sce5[[i]]$est2[1,]
  cm51[i,md5:80]=sce5[[i]]$est1[2,]
  cm52[i,md5:80]=sce5[[i]]$est2[2,]
  cu51[i,md5:80]=sce5[[i]]$est1[3,]
  cu52[i,md5:80]=sce5[[i]]$est2[3,]
  cl61[i,md6:80]=sce6[[i]]$est1[1,]
  cl62[i,md6:80]=sce6[[i]]$est2[1,]
  cm61[i,md6:80]=sce6[[i]]$est1[2,]
  cm62[i,md6:80]=sce6[[i]]$est2[2,]
  cu61[i,md6:80]=sce6[[i]]$est1[3,]
  cu62[i,md6:80]=sce6[[i]]$est2[3,]
  days1=as.numeric(names(sce1[[i]]$epic))+20
  c1[i,days1]=sce1[[i]]$epic 
  days2=as.numeric(names(sce2[[i]]$epic))+20
  c2[i,days2]=sce2[[i]]$epic 
  days3=as.numeric(names(sce3[[i]]$epic))+20
  c3[i,days3]=sce3[[i]]$epic 
  days4=as.numeric(names(sce4[[i]]$epic))+20
  c4[i,days4]=sce4[[i]]$epic 
  days5=as.numeric(names(sce5[[i]]$epic))+20
  c5[i,days5]=sce5[[i]]$epic 
  days6=as.numeric(names(sce6[[i]]$epic))+20
  c6[i,days6]=sce6[[i]]$epic 
  days1=as.numeric(names(sce1[[i]]$repc))+20
  rc1[i,days1]=sce1[[i]]$repc 
  days2=as.numeric(names(sce2[[i]]$repc))+20
  rc2[i,days2]=sce2[[i]]$repc 
  days3=as.numeric(names(sce3[[i]]$repc))+20
  rc3[i,days3]=sce3[[i]]$repc 
  days4=as.numeric(names(sce4[[i]]$repc))+20
  rc4[i,days4]=sce4[[i]]$repc 
  days5=as.numeric(names(sce5[[i]]$repc))+20
  rc5[i,days5]=sce5[[i]]$repc 
  days6=as.numeric(names(sce6[[i]]$repc))+20
  rc6[i,days6]=sce6[[i]]$repc 
}


######################################################
## Additional processing for coverage rate and RMSE ##
######################################################

ccov11 <- mat.or.vec(1000,80)
ccov21 <- mat.or.vec(1000,80)
ccov31 <- mat.or.vec(1000,80)
ccov41 <- mat.or.vec(1000,80)
ccov51 <- mat.or.vec(1000,80)
ccov61 <- mat.or.vec(1000,80)
ccov12 <- mat.or.vec(1000,80)
ccov22 <- mat.or.vec(1000,80)
ccov32 <- mat.or.vec(1000,80)
ccov42 <- mat.or.vec(1000,80)
ccov52 <- mat.or.vec(1000,80)
ccov62 <- mat.or.vec(1000,80)
ce11 <- mat.or.vec(1000,80)
ce12 <- mat.or.vec(1000,80)
ce21 <- mat.or.vec(1000,80)
ce22 <- mat.or.vec(1000,80)
ce31 <- mat.or.vec(1000,80)
ce32 <- mat.or.vec(1000,80)
ce41 <- mat.or.vec(1000,80)
ce42 <- mat.or.vec(1000,80)
ce51 <- mat.or.vec(1000,80)
ce52 <- mat.or.vec(1000,80)
ce61 <- mat.or.vec(1000,80)
ce62 <- mat.or.vec(1000,80)

for (i in 1:1000){
  ccov11[i,]=as.numeric((c1[i,]>=cl11[i,])&(c1[i,]<=cu11[i,]))
  ccov12[i,]=as.numeric((c1[i,]>=cl12[i,])&(c1[i,]<=cu12[i,]))
  ccov21[i,]=as.numeric((c2[i,]>=cl21[i,])&(c2[i,]<=cu21[i,]))
  ccov22[i,]=as.numeric((c2[i,]>=cl22[i,])&(c2[i,]<=cu22[i,]))
  ccov31[i,]=as.numeric((c3[i,]>=cl31[i,])&(c3[i,]<=cu31[i,]))
  ccov32[i,]=as.numeric((c3[i,]>=cl32[i,])&(c3[i,]<=cu32[i,]))
  ccov41[i,]=as.numeric((c4[i,]>=cl41[i,])&(c4[i,]<=cu41[i,]))
  ccov42[i,]=as.numeric((c4[i,]>=cl42[i,])&(c4[i,]<=cu42[i,]))
  ccov51[i,]=as.numeric((c5[i,]>=cl51[i,])&(c5[i,]<=cu51[i,]))
  ccov52[i,]=as.numeric((c5[i,]>=cl52[i,])&(c5[i,]<=cu52[i,]))
  ccov61[i,]=as.numeric((c6[i,]>=cl61[i,])&(c6[i,]<=cu61[i,]))
  ccov62[i,]=as.numeric((c6[i,]>=cl62[i,])&(c6[i,]<=cu62[i,]))
  ce11[i,]=(cm11[i,]-c1[i,])^2
  ce12[i,]=(cm12[i,]-c1[i,])^2
  ce21[i,]=(cm21[i,]-c2[i,])^2
  ce22[i,]=(cm22[i,]-c2[i,])^2
  ce31[i,]=(cm31[i,]-c3[i,])^2
  ce32[i,]=(cm32[i,]-c3[i,])^2
  ce41[i,]=(cm41[i,]-c4[i,])^2
  ce42[i,]=(cm42[i,]-c4[i,])^2
  ce51[i,]=(cm51[i,]-c5[i,])^2
  ce52[i,]=(cm52[i,]-c5[i,])^2
  ce61[i,]=(cm61[i,]-c6[i,])^2
  ce62[i,]=(cm62[i,]-c6[i,])^2
}

ccov11 <- apply(ccov11,2,sum)/1000
ccov12 <- apply(ccov12,2,sum)/1000
ccov21 <- apply(ccov21,2,sum)/1000
ccov22 <- apply(ccov22,2,sum)/1000
ccov31 <- apply(ccov31,2,sum)/1000
ccov32 <- apply(ccov32,2,sum)/1000
ccov41 <- apply(ccov41,2,sum)/1000
ccov42 <- apply(ccov42,2,sum)/1000
ccov51 <- apply(ccov51,2,sum)/1000
ccov52 <- apply(ccov52,2,sum)/1000
ccov61 <- apply(ccov61,2,sum)/1000
ccov62 <- apply(ccov62,2,sum)/1000
ce11 <- sqrt(apply(ce11,2,mean))
ce12 <- sqrt(apply(ce12,2,mean))
ce21 <- sqrt(apply(ce21,2,mean))
ce22 <- sqrt(apply(ce22,2,mean))
ce31 <- sqrt(apply(ce31,2,mean))
ce32 <- sqrt(apply(ce32,2,mean))
ce41 <- sqrt(apply(ce41,2,mean))
ce42 <- sqrt(apply(ce42,2,mean))
ce51 <- sqrt(apply(ce51,2,mean))
ce52 <- sqrt(apply(ce52,2,mean))
ce61 <- sqrt(apply(ce61,2,mean))
ce62 <- sqrt(apply(ce62,2,mean))

date <- rep(1:80,times=24)
result <- c(ccov11,ccov12,ccov21,ccov22,ccov31,ccov32,ccov41,ccov42,ccov51,ccov52,ccov61,ccov62,
            ce11,ce12,ce21,ce22,ce31,ce32,ce41,ce42,ce51,ce52,ce61,ce62)
scenario <- rep(1:6,each=160,times=2)
model <- rep(1:2,each=80,times=12)
type <- rep(c("coverage","RMSE"),each=960)
cout1 <- data.frame(cbind(date,result,scenario,model,type))

################################
## Prepare output for figures ##
################################

cl11 <- apply(cl11,2,mean)
cl12 <- apply(cl12,2,mean)
cm11 <- apply(cm11,2,mean)
cm12 <- apply(cm12,2,mean)
cu11 <- apply(cu11,2,mean)
cu12 <- apply(cu12,2,mean)
cl21 <- apply(cl21,2,mean)
cl22 <- apply(cl22,2,mean)
cm21 <- apply(cm21,2,mean)
cm22 <- apply(cm22,2,mean)
cu21 <- apply(cu21,2,mean)
cu22 <- apply(cu22,2,mean)
cl31 <- apply(cl31,2,mean)
cl32 <- apply(cl32,2,mean)
cm31 <- apply(cm31,2,mean)
cm32 <- apply(cm32,2,mean)
cu31 <- apply(cu31,2,mean)
cu32 <- apply(cu32,2,mean)
cl41 <- apply(cl41,2,mean)
cl42 <- apply(cl42,2,mean)
cm41 <- apply(cm41,2,mean)
cm42 <- apply(cm42,2,mean)
cu41 <- apply(cu41,2,mean)
cu42 <- apply(cu42,2,mean)
cl51 <- apply(cl51,2,mean)
cl52 <- apply(cl52,2,mean)
cm51 <- apply(cm51,2,mean)
cm52 <- apply(cm52,2,mean)
cu51 <- apply(cu51,2,mean)
cu52 <- apply(cu52,2,mean)
cl61 <- apply(cl61,2,mean)
cl62 <- apply(cl62,2,mean)
cm61 <- apply(cm61,2,mean)
cm62 <- apply(cm62,2,mean)
cu61 <- apply(cu61,2,mean)
cu62 <- apply(cu62,2,mean)
c1 <- apply(c1,2,mean)
c2 <- apply(c2,2,mean)
c3 <- apply(c3,2,mean)
c4 <- apply(c4,2,mean)
c5 <- apply(c5,2,mean)
c6 <- apply(c6,2,mean)
rc1 <- apply(rc1,2,mean)
rc2 <- apply(rc2,2,mean)
rc3 <- apply(rc3,2,mean)
rc4 <- apply(rc4,2,mean)
rc5 <- apply(rc5,2,mean)
rc6 <- apply(rc6,2,mean)

scenarios <- c("Single Delay Dist. & Correct Maximum Delay","Single Delay Dist. & Incorrect Maximum Delay",
               "Two Delay Dist. & Correct Maximum Delay","Two Delay Dist. & Incorrect Maximum Delay",
               "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay")
type <- c("1 Dispersion","2 Dispersions")
low <- c(cl11,cl12,cl21,cl22,cl31,cl32,cl41,cl42,cl51,cl52,cl61,cl62)
med <- c(cm11,cm12,cm21,cm22,cm31,cm32,cm41,cm42,cm51,cm52,cm61,cm62)
upp <- c(cu11,cu12,cu21,cu22,cu31,cu32,cu41,cu42,cu51,cu52,cu61,cu62)
true <- c(c1,c1,c2,c2,c3,c3,c4,c4,c5,c5,c6,c6)
report <- c(rc1,rc1,rc2,rc2,rc3,rc3,rc4,rc4,rc5,rc5,rc6,rc6)

outc <- data.frame(rep(1:80,times=12),low,med,upp,true,report,rep(type,each=80,times=6),rep(scenarios,each=160),rep(1:6,each=160))
colnames(outc) <- c("date","lower","median","upper","epic","repc","model","scenario","sn")

## Summarizing reproductive number estimates
size <- numeric(1000)
for (i in 1:1000){
  size[i] <- length(sce4[[i]]$epicr)
}
## Choose the last 47 Rt estimates so as to have consistent comparison (47 is min(size) for all scenarios)
size <- 47
## Choose the last 44 Rt estimates based on the reported curve

## Collect the lower, median and upper bounds
rl11 <- mat.or.vec(1000,size)
rl12 <- mat.or.vec(1000,size)
rm11 <- mat.or.vec(1000,size)
rm12 <- mat.or.vec(1000,size)
ru11 <- mat.or.vec(1000,size)
ru12 <- mat.or.vec(1000,size)
rl21 <- mat.or.vec(1000,size)
rl22 <- mat.or.vec(1000,size)
rm21 <- mat.or.vec(1000,size)
rm22 <- mat.or.vec(1000,size)
ru21 <- mat.or.vec(1000,size)
ru22 <- mat.or.vec(1000,size)
rl31 <- mat.or.vec(1000,size)
rl32 <- mat.or.vec(1000,size)
rm31 <- mat.or.vec(1000,size)
rm32 <- mat.or.vec(1000,size)
ru31 <- mat.or.vec(1000,size)
ru32 <- mat.or.vec(1000,size)
rl41 <- mat.or.vec(1000,size)
rl42 <- mat.or.vec(1000,size)
rm41 <- mat.or.vec(1000,size)
rm42 <- mat.or.vec(1000,size)
ru41 <- mat.or.vec(1000,size)
ru42 <- mat.or.vec(1000,size)
rl51 <- mat.or.vec(1000,size)
rl52 <- mat.or.vec(1000,size)
rm51 <- mat.or.vec(1000,size)
rm52 <- mat.or.vec(1000,size)
ru51 <- mat.or.vec(1000,size)
ru52 <- mat.or.vec(1000,size)
rl61 <- mat.or.vec(1000,size)
rl62 <- mat.or.vec(1000,size)
rm61 <- mat.or.vec(1000,size)
rm62 <- mat.or.vec(1000,size)
ru61 <- mat.or.vec(1000,size)
ru62 <- mat.or.vec(1000,size)


## Extract known epidemic curve
r1 <- mat.or.vec(1000,size)
r2 <- mat.or.vec(1000,size)
r3 <- mat.or.vec(1000,size)
r4 <- mat.or.vec(1000,size)
r5 <- mat.or.vec(1000,size)
r6 <- mat.or.vec(1000,size)
## Extract the report curve
rr1 <- mat.or.vec(1000,44)
rr2 <- mat.or.vec(1000,44)
rr3 <- mat.or.vec(1000,44)
rr4 <- mat.or.vec(1000,44)
rr5 <- mat.or.vec(1000,44)
rr6 <- mat.or.vec(1000,44)

for (i in 1:1000){
  n=46
  lr1=ncol(sce1[[i]]$estr1)
  lr2=ncol(sce2[[i]]$estr1)
  lr3=ncol(sce3[[i]]$estr1)
  lr4=ncol(sce4[[i]]$estr1)
  lr5=ncol(sce5[[i]]$estr1)
  lr6=ncol(sce6[[i]]$estr1)
  rl11[i,]=sce1[[i]]$estr1[1,(lr1-n):lr1]
  rl12[i,]=sce1[[i]]$estr2[1,(lr1-n):lr1]
  rm11[i,]=sce1[[i]]$estr1[2,(lr1-n):lr1]
  rm12[i,]=sce1[[i]]$estr2[2,(lr1-n):lr1]
  ru11[i,]=sce1[[i]]$estr1[3,(lr1-n):lr1]
  ru12[i,]=sce1[[i]]$estr2[3,(lr1-n):lr1]
  rl21[i,]=sce2[[i]]$estr1[1,(lr2-n):lr2]
  rl22[i,]=sce2[[i]]$estr2[1,(lr2-n):lr2]
  rm21[i,]=sce2[[i]]$estr1[2,(lr2-n):lr2]
  rm22[i,]=sce2[[i]]$estr2[2,(lr2-n):lr2]
  ru21[i,]=sce2[[i]]$estr1[3,(lr2-n):lr2]
  ru22[i,]=sce2[[i]]$estr2[3,(lr2-n):lr2]
  rl31[i,]=sce3[[i]]$estr1[1,(lr3-n):lr3]
  rl32[i,]=sce3[[i]]$estr2[1,(lr3-n):lr3]
  rm31[i,]=sce3[[i]]$estr1[2,(lr3-n):lr3]
  rm32[i,]=sce3[[i]]$estr2[2,(lr3-n):lr3]
  ru31[i,]=sce3[[i]]$estr1[3,(lr3-n):lr3]
  ru32[i,]=sce3[[i]]$estr2[3,(lr3-n):lr3]
  rl41[i,]=sce4[[i]]$estr1[1,(lr4-n):lr4]
  rl42[i,]=sce4[[i]]$estr2[1,(lr4-n):lr4]
  rm41[i,]=sce4[[i]]$estr1[2,(lr4-n):lr4]
  rm42[i,]=sce4[[i]]$estr2[2,(lr4-n):lr4]
  ru41[i,]=sce4[[i]]$estr1[3,(lr4-n):lr4]
  ru42[i,]=sce4[[i]]$estr2[3,(lr4-n):lr4]
  rl51[i,]=sce5[[i]]$estr1[1,(lr5-n):lr5]
  rl52[i,]=sce5[[i]]$estr2[1,(lr5-n):lr5]
  rm51[i,]=sce5[[i]]$estr1[2,(lr5-n):lr5]
  rm52[i,]=sce5[[i]]$estr2[2,(lr5-n):lr5]
  ru51[i,]=sce5[[i]]$estr1[3,(lr5-n):lr5]
  ru52[i,]=sce5[[i]]$estr2[3,(lr5-n):lr5]
  rl61[i,]=sce6[[i]]$estr1[1,(lr6-n):lr6]
  rl62[i,]=sce6[[i]]$estr2[1,(lr6-n):lr6]
  rm61[i,]=sce6[[i]]$estr1[2,(lr6-n):lr6]
  rm62[i,]=sce6[[i]]$estr2[2,(lr6-n):lr6]
  ru61[i,]=sce6[[i]]$estr1[3,(lr6-n):lr6]
  ru62[i,]=sce6[[i]]$estr2[3,(lr6-n):lr6]
  s1=length(sce1[[i]]$epicr)
  r1[i,]=sce1[[i]]$epicr[(s1-n):s1] 
  s2=length(sce2[[i]]$epicr)
  r2[i,]=sce2[[i]]$epicr[(s2-n):s2]
  s3=length(sce3[[i]]$epicr)
  r3[i,]=sce3[[i]]$epicr[(s3-n):s3]
  s4=length(sce4[[i]]$epicr)
  r4[i,]=sce4[[i]]$epicr[(s4-n):s4]
  s5=length(sce5[[i]]$epicr)
  r5[i,]=sce5[[i]]$epicr[(s5-n):s5]
  s6=length(sce6[[i]]$epicr)
  r6[i,]=sce6[[i]]$epicr[(s6-n):s6]
  s1=length(sce1[[i]]$repcr)
  rr1[i,]=sce1[[i]]$repcr[(s1-43):s1] 
  s2=length(sce2[[i]]$repcr)
  rr2[i,]=sce2[[i]]$repcr[(s2-43):s2]
  s3=length(sce3[[i]]$repcr)
  rr3[i,]=sce3[[i]]$repcr[(s3-43):s3]
  s4=length(sce4[[i]]$repcr)
  rr4[i,]=sce4[[i]]$repcr[(s4-43):s4]
  s5=length(sce5[[i]]$repcr)
  rr5[i,]=sce5[[i]]$repcr[(s5-43):s5]
  s6=length(sce6[[i]]$repcr)
  rr6[i,]=sce6[[i]]$repcr[(s6-43):s6] 
}


######################################################
## Additional processing for coverage rate and RMSE ##
######################################################
size=47
rcov11 <- mat.or.vec(1000,size)
rcov21 <- mat.or.vec(1000,size)
rcov31 <- mat.or.vec(1000,size)
rcov41 <- mat.or.vec(1000,size)
rcov51 <- mat.or.vec(1000,size)
rcov61 <- mat.or.vec(1000,size)
rcov12 <- mat.or.vec(1000,size)
rcov22 <- mat.or.vec(1000,size)
rcov32 <- mat.or.vec(1000,size)
rcov42 <- mat.or.vec(1000,size)
rcov52 <- mat.or.vec(1000,size)
rcov62 <- mat.or.vec(1000,size)
re11 <- mat.or.vec(1000,size)
re12 <- mat.or.vec(1000,size)
re21 <- mat.or.vec(1000,size)
re22 <- mat.or.vec(1000,size)
re31 <- mat.or.vec(1000,size)
re32 <- mat.or.vec(1000,size)
re41 <- mat.or.vec(1000,size)
re42 <- mat.or.vec(1000,size)
re51 <- mat.or.vec(1000,size)
re52 <- mat.or.vec(1000,size)
re61 <- mat.or.vec(1000,size)
re62 <- mat.or.vec(1000,size)

for (i in 1:1000){
  rcov11[i,]=as.numeric((r1[i,]>=rl11[i,])&(r1[i,]<=ru11[i,]))
  rcov12[i,]=as.numeric((r1[i,]>=rl12[i,])&(r1[i,]<=ru12[i,]))
  rcov21[i,]=as.numeric((r2[i,]>=rl21[i,])&(r2[i,]<=ru21[i,]))
  rcov22[i,]=as.numeric((r2[i,]>=rl22[i,])&(r2[i,]<=ru22[i,]))
  rcov31[i,]=as.numeric((r3[i,]>=rl31[i,])&(r3[i,]<=ru31[i,]))
  rcov32[i,]=as.numeric((r3[i,]>=rl32[i,])&(r3[i,]<=ru32[i,]))
  rcov41[i,]=as.numeric((r4[i,]>=rl41[i,])&(r4[i,]<=ru41[i,]))
  rcov42[i,]=as.numeric((r4[i,]>=rl42[i,])&(r4[i,]<=ru42[i,]))
  rcov51[i,]=as.numeric((r5[i,]>=rl51[i,])&(r5[i,]<=ru51[i,]))
  rcov52[i,]=as.numeric((r5[i,]>=rl52[i,])&(r5[i,]<=ru52[i,]))
  rcov61[i,]=as.numeric((r6[i,]>=rl61[i,])&(r6[i,]<=ru61[i,]))
  rcov62[i,]=as.numeric((r6[i,]>=rl62[i,])&(r6[i,]<=ru62[i,]))
  re11[i,]=(rm11[i,]-r1[i,])^2
  re12[i,]=(rm12[i,]-r1[i,])^2
  re21[i,]=(rm21[i,]-r2[i,])^2
  re22[i,]=(rm22[i,]-r2[i,])^2
  re31[i,]=(rm31[i,]-r3[i,])^2
  re32[i,]=(rm32[i,]-r3[i,])^2
  re41[i,]=(rm41[i,]-r4[i,])^2
  re42[i,]=(rm42[i,]-r4[i,])^2
  re51[i,]=(rm51[i,]-r5[i,])^2
  re52[i,]=(rm52[i,]-r5[i,])^2
  re61[i,]=(rm61[i,]-r6[i,])^2
  re62[i,]=(rm62[i,]-r6[i,])^2
}

rcov11 <- apply(rcov11,2,sum)/1000
rcov12 <- apply(rcov12,2,sum)/1000
rcov21 <- apply(rcov21,2,sum)/1000
rcov22 <- apply(rcov22,2,sum)/1000
rcov31 <- apply(rcov31,2,sum)/1000
rcov32 <- apply(rcov32,2,sum)/1000
rcov41 <- apply(rcov41,2,sum)/1000
rcov42 <- apply(rcov42,2,sum)/1000
rcov51 <- apply(rcov51,2,sum)/1000
rcov52 <- apply(rcov52,2,sum)/1000
rcov61 <- apply(rcov61,2,sum)/1000
rcov62 <- apply(rcov62,2,sum)/1000
re11 <- sqrt(apply(re11,2,mean))
re12 <- sqrt(apply(re12,2,mean))
re21 <- sqrt(apply(re21,2,mean))
re22 <- sqrt(apply(re22,2,mean))
re31 <- sqrt(apply(re31,2,mean))
re32 <- sqrt(apply(re32,2,mean))
re41 <- sqrt(apply(re41,2,mean))
re42 <- sqrt(apply(re42,2,mean))
re51 <- sqrt(apply(re51,2,mean))
re52 <- sqrt(apply(re52,2,mean))
re61 <- sqrt(apply(re61,2,mean))
re62 <- sqrt(apply(re62,2,mean))

date <- rep(34:80,times=24)
result <- c(rcov11,rcov12,rcov21,rcov22,rcov31,rcov32,rcov41,rcov42,rcov51,rcov52,rcov61,rcov62,
            re11,re12,re21,re22,re31,re32,re41,re42,re51,re52,re61,re62)
scenario <- rep(1:6,each=94,times=2)
model <- rep(1:2,each=47,times=12)
type <- rep(c("coverage","RMSE"),each=564)
rout1 <- data.frame(cbind(date,result,scenario,model,type))
eva1 <- list(c=cout1,r=rout1)
saveRDS(eva1,'eva1.rds')

################################
## Prepare output for figures ##
################################

rl11 <- apply(rl11,2,mean)
rl12 <- apply(rl12,2,mean)
rm11 <- apply(rm11,2,mean)
rm12 <- apply(rm12,2,mean)
ru11 <- apply(ru11,2,mean)
ru12 <- apply(ru12,2,mean)
rl21 <- apply(rl21,2,mean)
rl22 <- apply(rl22,2,mean)
rm21 <- apply(rm21,2,mean)
rm22 <- apply(rm22,2,mean)
ru21 <- apply(ru21,2,mean)
ru22 <- apply(ru22,2,mean)
rl31 <- apply(rl31,2,mean)
rl32 <- apply(rl32,2,mean)
rm31 <- apply(rm31,2,mean)
rm32 <- apply(rm32,2,mean)
ru31 <- apply(ru31,2,mean)
ru32 <- apply(ru32,2,mean)
rl41 <- apply(rl41,2,mean)
rl42 <- apply(rl42,2,mean)
rm41 <- apply(rm41,2,mean)
rm42 <- apply(rm42,2,mean)
ru41 <- apply(ru41,2,mean)
ru42 <- apply(ru42,2,mean)
rl51 <- apply(rl51,2,mean)
rl52 <- apply(rl52,2,mean)
rm51 <- apply(rm51,2,mean)
rm52 <- apply(rm52,2,mean)
ru51 <- apply(ru51,2,mean)
ru52 <- apply(ru52,2,mean)
rl61 <- apply(rl61,2,mean)
rl62 <- apply(rl62,2,mean)
rm61 <- apply(rm61,2,mean)
rm62 <- apply(rm62,2,mean)
ru61 <- apply(ru61,2,mean)
ru62 <- apply(ru62,2,mean)
r1 <- apply(r1,2,mean)
r2 <- apply(r2,2,mean)
r3 <- apply(r3,2,mean)
r4 <- apply(r4,2,mean)
r5 <- apply(r5,2,mean)
r6 <- apply(r6,2,mean)
rr1 <- c(rep(NA,3),apply(rr1,2,mean))
rr2 <- c(rep(NA,3),apply(rr2,2,mean))
rr3 <- c(rep(NA,3),apply(rr3,2,mean))
rr4 <- c(rep(NA,3),apply(rr4,2,mean))
rr5 <- c(rep(NA,3),apply(rr5,2,mean))
rr6 <- c(rep(NA,3),apply(rr6,2,mean))

scenarios <- c("Single Delay Dist. & Correct Maximum Delay","Single Delay Dist. & Incorrect Maximum Delay",
               "Two Delay Dist. & Correct Maximum Delay","Two Delay Dist. & Incorrect Maximum Delay",
               "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay")
type <- c("1 Dispersion","2 Dispersions")
low <- c(rl11,rl12,rl21,rl22,rl31,rl32,rl41,rl42,rl51,rl52,rl61,rl62)
med <- c(rm11,rm12,rm21,rm22,rm31,rm32,rm41,rm42,rm51,rm52,rm61,rm62)
upp <- c(ru11,ru12,ru21,ru22,ru31,ru32,ru41,ru42,ru51,ru52,ru61,ru62)
epir <- c(r1,r1,r2,r2,r3,r3,r4,r4,r5,r5,r6,r6)
repr <- c(rr1,rr1,rr2,rr2,rr3,rr3,rr4,rr4,rr5,rr5,rr6,rr6)
outr <- data.frame(rep(34:80,times=12),low,med,upp,epir,repr,rep(type,each=47,times=6),rep(scenarios,each=94),rep(1:6,each=94))
colnames(outr) <- c("date","lower","median","upper","epir","repr","model","scenario","sn")

## Finish Preprocessing: section 1


#########################################
## Preprocessing the output: Section 2 ##
#########################################

## This part is for snapshots: for chopping off the heads

## Read the output
sce1 <- readRDS('sce1_ss.rds')
sce2 <- readRDS('sce2_ss.rds')
sce3 <- readRDS('sce3_ss.rds')
sce4 <- readRDS('sce4_ss.rds')
sce5 <- readRDS('sce5_ss.rds')
sce6 <- readRDS('sce6_ss.rds')

## Summarizing curve estimates
## Collect the lower, median and upper bounds
cl11 <- mat.or.vec(1000,80)
cl12 <- mat.or.vec(1000,80)
cl13 <- mat.or.vec(1000,80)
cl14 <- mat.or.vec(1000,80)
cm11 <- mat.or.vec(1000,80)
cm12 <- mat.or.vec(1000,80)
cm13 <- mat.or.vec(1000,80)
cm14 <- mat.or.vec(1000,80)
cu11 <- mat.or.vec(1000,80)
cu12 <- mat.or.vec(1000,80)
cu13 <- mat.or.vec(1000,80)
cu14 <- mat.or.vec(1000,80)
cl21 <- mat.or.vec(1000,80)
cl22 <- mat.or.vec(1000,80)
cl23 <- mat.or.vec(1000,80)
cl24 <- mat.or.vec(1000,80)
cm21 <- mat.or.vec(1000,80)
cm22 <- mat.or.vec(1000,80)
cm23 <- mat.or.vec(1000,80)
cm24 <- mat.or.vec(1000,80)
cu21 <- mat.or.vec(1000,80)
cu22 <- mat.or.vec(1000,80)
cu23 <- mat.or.vec(1000,80)
cu24 <- mat.or.vec(1000,80)
cl31 <- mat.or.vec(1000,80)
cl32 <- mat.or.vec(1000,80)
cl33 <- mat.or.vec(1000,80)
cl34 <- mat.or.vec(1000,80)
cm31 <- mat.or.vec(1000,80)
cm32 <- mat.or.vec(1000,80)
cm33 <- mat.or.vec(1000,80)
cm34 <- mat.or.vec(1000,80)
cu31 <- mat.or.vec(1000,80)
cu32 <- mat.or.vec(1000,80)
cu33 <- mat.or.vec(1000,80)
cu34 <- mat.or.vec(1000,80)
cl41 <- mat.or.vec(1000,80)
cl42 <- mat.or.vec(1000,80)
cl43 <- mat.or.vec(1000,80)
cl44 <- mat.or.vec(1000,80)
cm41 <- mat.or.vec(1000,80)
cm42 <- mat.or.vec(1000,80)
cm43 <- mat.or.vec(1000,80)
cm44 <- mat.or.vec(1000,80)
cu41 <- mat.or.vec(1000,80)
cu42 <- mat.or.vec(1000,80)
cu43 <- mat.or.vec(1000,80)
cu44 <- mat.or.vec(1000,80)
cl51 <- mat.or.vec(1000,80)
cl52 <- mat.or.vec(1000,80)
cl53 <- mat.or.vec(1000,80)
cl54 <- mat.or.vec(1000,80)
cm51 <- mat.or.vec(1000,80)
cm52 <- mat.or.vec(1000,80)
cm53 <- mat.or.vec(1000,80)
cm54 <- mat.or.vec(1000,80)
cu51 <- mat.or.vec(1000,80)
cu52 <- mat.or.vec(1000,80)
cu53 <- mat.or.vec(1000,80)
cu54 <- mat.or.vec(1000,80)
cl61 <- mat.or.vec(1000,80)
cl62 <- mat.or.vec(1000,80)
cl63 <- mat.or.vec(1000,80)
cl64 <- mat.or.vec(1000,80)
cm61 <- mat.or.vec(1000,80)
cm62 <- mat.or.vec(1000,80)
cm63 <- mat.or.vec(1000,80)
cm64 <- mat.or.vec(1000,80)
cu61 <- mat.or.vec(1000,80)
cu62 <- mat.or.vec(1000,80)
cu63 <- mat.or.vec(1000,80)
cu64 <- mat.or.vec(1000,80)


## Extract known epidemic curve
c1 <- mat.or.vec(1000,80)
c2 <- mat.or.vec(1000,80)
c3 <- mat.or.vec(1000,80)
c4 <- mat.or.vec(1000,80)
c5 <- mat.or.vec(1000,80)
c6 <- mat.or.vec(1000,80)

## Extract the report curve
rc1 <- mat.or.vec(1000,80)
rc2 <- mat.or.vec(1000,80)
rc3 <- mat.or.vec(1000,80)
rc4 <- mat.or.vec(1000,80)
rc5 <- mat.or.vec(1000,80)
rc6 <- mat.or.vec(1000,80)


for (i in 1:1000){
  cl11[i,11:80]=sce1[[i]]$est1[1,]
  cl12[i,21:80]=sce1[[i]]$est2[1,]
  cl13[i,31:80]=sce1[[i]]$est3[1,]
  cl14[i,41:80]=sce1[[i]]$est4[1,]
  cm11[i,11:80]=sce1[[i]]$est1[2,]
  cm12[i,21:80]=sce1[[i]]$est2[2,]
  cm13[i,31:80]=sce1[[i]]$est3[2,]
  cm14[i,41:80]=sce1[[i]]$est4[2,]
  cu11[i,11:80]=sce1[[i]]$est1[3,]
  cu12[i,21:80]=sce1[[i]]$est2[3,]
  cu13[i,31:80]=sce1[[i]]$est3[3,]
  cu14[i,41:80]=sce1[[i]]$est4[3,]
  cl21[i,11:80]=sce2[[i]]$est1[1,]
  cl22[i,21:80]=sce2[[i]]$est2[1,]
  cl23[i,31:80]=sce2[[i]]$est3[1,]
  cl24[i,41:80]=sce2[[i]]$est4[1,]
  cm21[i,11:80]=sce2[[i]]$est1[2,]
  cm22[i,21:80]=sce2[[i]]$est2[2,]
  cm23[i,31:80]=sce2[[i]]$est3[2,]
  cm24[i,41:80]=sce2[[i]]$est4[2,]
  cu21[i,11:80]=sce2[[i]]$est1[3,]
  cu22[i,21:80]=sce2[[i]]$est2[3,]
  cu23[i,31:80]=sce2[[i]]$est3[3,]
  cu24[i,41:80]=sce2[[i]]$est4[3,]
  cl31[i,11:80]=sce3[[i]]$est1[1,]
  cl32[i,21:80]=sce3[[i]]$est2[1,]
  cl33[i,31:80]=sce3[[i]]$est3[1,]
  cl34[i,41:80]=sce3[[i]]$est4[1,]
  cm31[i,11:80]=sce3[[i]]$est1[2,]
  cm32[i,21:80]=sce3[[i]]$est2[2,]
  cm33[i,31:80]=sce3[[i]]$est3[2,]
  cm34[i,41:80]=sce3[[i]]$est4[2,]
  cu31[i,11:80]=sce3[[i]]$est1[3,]
  cu32[i,21:80]=sce3[[i]]$est2[3,]
  cu33[i,31:80]=sce3[[i]]$est3[3,]
  cu34[i,41:80]=sce3[[i]]$est4[3,]
  cl41[i,11:80]=sce4[[i]]$est1[1,]
  cl42[i,21:80]=sce4[[i]]$est2[1,]
  cl43[i,31:80]=sce4[[i]]$est3[1,]
  cl44[i,41:80]=sce4[[i]]$est4[1,]
  cm41[i,11:80]=sce4[[i]]$est1[2,]
  cm42[i,21:80]=sce4[[i]]$est2[2,]
  cm43[i,31:80]=sce4[[i]]$est3[2,]
  cm44[i,41:80]=sce4[[i]]$est4[2,]
  cu41[i,11:80]=sce4[[i]]$est1[3,]
  cu42[i,21:80]=sce4[[i]]$est2[3,]
  cu43[i,31:80]=sce4[[i]]$est3[3,]
  cu44[i,41:80]=sce4[[i]]$est4[3,]
  cl51[i,11:80]=sce5[[i]]$est1[1,]
  cl52[i,21:80]=sce5[[i]]$est2[1,]
  cl53[i,31:80]=sce5[[i]]$est3[1,]
  cl54[i,41:80]=sce5[[i]]$est4[1,]
  cm51[i,11:80]=sce5[[i]]$est1[2,]
  cm52[i,21:80]=sce5[[i]]$est2[2,]
  cm53[i,31:80]=sce5[[i]]$est3[2,]
  cm54[i,41:80]=sce5[[i]]$est4[2,]
  cu51[i,11:80]=sce5[[i]]$est1[3,]
  cu52[i,21:80]=sce5[[i]]$est2[3,]
  cu53[i,31:80]=sce5[[i]]$est3[3,]
  cu54[i,41:80]=sce5[[i]]$est4[3,]
  cl61[i,11:80]=sce6[[i]]$est1[1,]
  cl62[i,21:80]=sce6[[i]]$est2[1,]
  cl63[i,31:80]=sce6[[i]]$est3[1,]
  cl64[i,41:80]=sce6[[i]]$est4[1,]
  cm61[i,11:80]=sce6[[i]]$est1[2,]
  cm62[i,21:80]=sce6[[i]]$est2[2,]
  cm63[i,31:80]=sce6[[i]]$est3[2,]
  cm64[i,41:80]=sce6[[i]]$est4[2,]
  cu61[i,11:80]=sce6[[i]]$est1[3,]
  cu62[i,21:80]=sce6[[i]]$est2[3,]
  cu63[i,31:80]=sce6[[i]]$est3[3,]
  cu64[i,41:80]=sce6[[i]]$est4[3,]
  days1=as.numeric(names(sce1[[i]]$epic))+20
  c1[i,days1]=sce1[[i]]$epic 
  days2=as.numeric(names(sce2[[i]]$epic))+20
  c2[i,days2]=sce2[[i]]$epic 
  days3=as.numeric(names(sce3[[i]]$epic))+20
  c3[i,days3]=sce3[[i]]$epic 
  days4=as.numeric(names(sce4[[i]]$epic))+20
  c4[i,days4]=sce4[[i]]$epic 
  days5=as.numeric(names(sce5[[i]]$epic))+20
  c5[i,days5]=sce5[[i]]$epic 
  days6=as.numeric(names(sce6[[i]]$epic))+20
  c6[i,days6]=sce6[[i]]$epic 
  days1=as.numeric(names(sce1[[i]]$repc))+20
  rc1[i,days1]=sce1[[i]]$repc 
  days2=as.numeric(names(sce2[[i]]$repc))+20
  rc2[i,days2]=sce2[[i]]$repc 
  days3=as.numeric(names(sce3[[i]]$repc))+20
  rc3[i,days3]=sce3[[i]]$repc 
  days4=as.numeric(names(sce4[[i]]$repc))+20
  rc4[i,days4]=sce4[[i]]$repc 
  days5=as.numeric(names(sce5[[i]]$repc))+20
  rc5[i,days5]=sce5[[i]]$repc 
  days6=as.numeric(names(sce6[[i]]$repc))+20
  rc6[i,days6]=sce6[[i]]$repc 
}

######################################################
## Additional processing for coverage rate and RMSE ##
######################################################

ccov11 <- mat.or.vec(1000,80)
ccov21 <- mat.or.vec(1000,80)
ccov31 <- mat.or.vec(1000,80)
ccov41 <- mat.or.vec(1000,80)
ccov51 <- mat.or.vec(1000,80)
ccov61 <- mat.or.vec(1000,80)
ccov12 <- mat.or.vec(1000,80)
ccov22 <- mat.or.vec(1000,80)
ccov32 <- mat.or.vec(1000,80)
ccov42 <- mat.or.vec(1000,80)
ccov52 <- mat.or.vec(1000,80)
ccov62 <- mat.or.vec(1000,80)
ccov13 <- mat.or.vec(1000,80)
ccov23 <- mat.or.vec(1000,80)
ccov33 <- mat.or.vec(1000,80)
ccov43 <- mat.or.vec(1000,80)
ccov53 <- mat.or.vec(1000,80)
ccov63 <- mat.or.vec(1000,80)
ccov14 <- mat.or.vec(1000,80)
ccov24 <- mat.or.vec(1000,80)
ccov34 <- mat.or.vec(1000,80)
ccov44 <- mat.or.vec(1000,80)
ccov54 <- mat.or.vec(1000,80)
ccov64 <- mat.or.vec(1000,80)
ce11 <- mat.or.vec(1000,80)
ce12 <- mat.or.vec(1000,80)
ce21 <- mat.or.vec(1000,80)
ce22 <- mat.or.vec(1000,80)
ce31 <- mat.or.vec(1000,80)
ce32 <- mat.or.vec(1000,80)
ce41 <- mat.or.vec(1000,80)
ce42 <- mat.or.vec(1000,80)
ce51 <- mat.or.vec(1000,80)
ce52 <- mat.or.vec(1000,80)
ce61 <- mat.or.vec(1000,80)
ce62 <- mat.or.vec(1000,80)
ce13 <- mat.or.vec(1000,80)
ce14 <- mat.or.vec(1000,80)
ce23 <- mat.or.vec(1000,80)
ce24 <- mat.or.vec(1000,80)
ce33 <- mat.or.vec(1000,80)
ce34 <- mat.or.vec(1000,80)
ce43 <- mat.or.vec(1000,80)
ce44 <- mat.or.vec(1000,80)
ce53 <- mat.or.vec(1000,80)
ce54 <- mat.or.vec(1000,80)
ce63 <- mat.or.vec(1000,80)
ce64 <- mat.or.vec(1000,80)

for (i in 1:1000){
  ccov11[i,]=as.numeric((c1[i,]>=cl11[i,])&(c1[i,]<=cu11[i,]))
  ccov12[i,]=as.numeric((c1[i,]>=cl12[i,])&(c1[i,]<=cu12[i,]))
  ccov13[i,]=as.numeric((c1[i,]>=cl13[i,])&(c1[i,]<=cu13[i,]))
  ccov14[i,]=as.numeric((c1[i,]>=cl14[i,])&(c1[i,]<=cu14[i,]))
  ccov21[i,]=as.numeric((c2[i,]>=cl21[i,])&(c2[i,]<=cu21[i,]))
  ccov22[i,]=as.numeric((c2[i,]>=cl22[i,])&(c2[i,]<=cu22[i,]))
  ccov23[i,]=as.numeric((c2[i,]>=cl23[i,])&(c2[i,]<=cu23[i,]))
  ccov24[i,]=as.numeric((c2[i,]>=cl24[i,])&(c2[i,]<=cu24[i,]))
  ccov31[i,]=as.numeric((c3[i,]>=cl31[i,])&(c3[i,]<=cu31[i,]))
  ccov32[i,]=as.numeric((c3[i,]>=cl32[i,])&(c3[i,]<=cu32[i,]))
  ccov33[i,]=as.numeric((c3[i,]>=cl33[i,])&(c3[i,]<=cu33[i,]))
  ccov34[i,]=as.numeric((c3[i,]>=cl34[i,])&(c3[i,]<=cu34[i,]))
  ccov41[i,]=as.numeric((c4[i,]>=cl41[i,])&(c4[i,]<=cu41[i,]))
  ccov42[i,]=as.numeric((c4[i,]>=cl42[i,])&(c4[i,]<=cu42[i,]))
  ccov43[i,]=as.numeric((c4[i,]>=cl43[i,])&(c4[i,]<=cu43[i,]))
  ccov44[i,]=as.numeric((c4[i,]>=cl44[i,])&(c4[i,]<=cu44[i,]))
  ccov51[i,]=as.numeric((c5[i,]>=cl51[i,])&(c5[i,]<=cu51[i,]))
  ccov52[i,]=as.numeric((c5[i,]>=cl52[i,])&(c5[i,]<=cu52[i,]))
  ccov53[i,]=as.numeric((c5[i,]>=cl53[i,])&(c5[i,]<=cu53[i,]))
  ccov54[i,]=as.numeric((c5[i,]>=cl54[i,])&(c5[i,]<=cu54[i,]))
  ccov61[i,]=as.numeric((c6[i,]>=cl61[i,])&(c6[i,]<=cu61[i,]))
  ccov62[i,]=as.numeric((c6[i,]>=cl62[i,])&(c6[i,]<=cu62[i,]))
  ccov63[i,]=as.numeric((c6[i,]>=cl63[i,])&(c6[i,]<=cu63[i,]))
  ccov64[i,]=as.numeric((c6[i,]>=cl64[i,])&(c6[i,]<=cu64[i,]))
  ce11[i,]=(cm11[i,]-c1[i,])^2
  ce12[i,]=(cm12[i,]-c1[i,])^2
  ce13[i,]=(cm13[i,]-c1[i,])^2
  ce14[i,]=(cm14[i,]-c1[i,])^2
  ce21[i,]=(cm21[i,]-c2[i,])^2
  ce22[i,]=(cm22[i,]-c2[i,])^2
  ce23[i,]=(cm23[i,]-c2[i,])^2
  ce24[i,]=(cm24[i,]-c2[i,])^2
  ce31[i,]=(cm31[i,]-c3[i,])^2
  ce32[i,]=(cm32[i,]-c3[i,])^2
  ce33[i,]=(cm33[i,]-c3[i,])^2
  ce34[i,]=(cm34[i,]-c3[i,])^2
  ce41[i,]=(cm41[i,]-c4[i,])^2
  ce42[i,]=(cm42[i,]-c4[i,])^2
  ce43[i,]=(cm43[i,]-c4[i,])^2
  ce44[i,]=(cm44[i,]-c4[i,])^2
  ce51[i,]=(cm51[i,]-c5[i,])^2
  ce52[i,]=(cm52[i,]-c5[i,])^2
  ce53[i,]=(cm53[i,]-c5[i,])^2
  ce54[i,]=(cm54[i,]-c5[i,])^2
  ce61[i,]=(cm61[i,]-c6[i,])^2
  ce62[i,]=(cm62[i,]-c6[i,])^2
  ce63[i,]=(cm63[i,]-c6[i,])^2
  ce64[i,]=(cm64[i,]-c6[i,])^2
}

ccov11 <- apply(ccov11,2,sum)/1000
ccov12 <- apply(ccov12,2,sum)/1000
ccov13 <- apply(ccov13,2,sum)/1000
ccov14 <- apply(ccov14,2,sum)/1000
ccov21 <- apply(ccov21,2,sum)/1000
ccov22 <- apply(ccov22,2,sum)/1000
ccov23 <- apply(ccov23,2,sum)/1000
ccov24 <- apply(ccov24,2,sum)/1000
ccov31 <- apply(ccov31,2,sum)/1000
ccov32 <- apply(ccov32,2,sum)/1000
ccov33 <- apply(ccov33,2,sum)/1000
ccov34 <- apply(ccov34,2,sum)/1000
ccov41 <- apply(ccov41,2,sum)/1000
ccov42 <- apply(ccov42,2,sum)/1000
ccov43 <- apply(ccov43,2,sum)/1000
ccov44 <- apply(ccov44,2,sum)/1000
ccov51 <- apply(ccov51,2,sum)/1000
ccov52 <- apply(ccov52,2,sum)/1000
ccov53 <- apply(ccov53,2,sum)/1000
ccov54 <- apply(ccov54,2,sum)/1000
ccov61 <- apply(ccov61,2,sum)/1000
ccov62 <- apply(ccov62,2,sum)/1000
ccov63 <- apply(ccov63,2,sum)/1000
ccov64 <- apply(ccov64,2,sum)/1000
ce11 <- sqrt(apply(ce11,2,mean))
ce12 <- sqrt(apply(ce12,2,mean))
ce13 <- sqrt(apply(ce13,2,mean))
ce14 <- sqrt(apply(ce14,2,mean))
ce21 <- sqrt(apply(ce21,2,mean))
ce22 <- sqrt(apply(ce22,2,mean))
ce23 <- sqrt(apply(ce23,2,mean))
ce24 <- sqrt(apply(ce24,2,mean))
ce31 <- sqrt(apply(ce31,2,mean))
ce32 <- sqrt(apply(ce32,2,mean))
ce33 <- sqrt(apply(ce33,2,mean))
ce34 <- sqrt(apply(ce34,2,mean))
ce41 <- sqrt(apply(ce41,2,mean))
ce42 <- sqrt(apply(ce42,2,mean))
ce43 <- sqrt(apply(ce43,2,mean))
ce44 <- sqrt(apply(ce44,2,mean))
ce51 <- sqrt(apply(ce51,2,mean))
ce52 <- sqrt(apply(ce52,2,mean))
ce53 <- sqrt(apply(ce53,2,mean))
ce54 <- sqrt(apply(ce54,2,mean))
ce61 <- sqrt(apply(ce61,2,mean))
ce62 <- sqrt(apply(ce62,2,mean))
ce63 <- sqrt(apply(ce63,2,mean))
ce64 <- sqrt(apply(ce64,2,mean))

date <- rep(1:80,times=48)
result <- c(ccov11,ccov12,ccov13,ccov14,ccov21,ccov22,ccov23,ccov24,ccov31,ccov32,ccov33,ccov34,ccov41,ccov42,ccov43,ccov44,
            ccov51,ccov52,ccov53,ccov54,ccov61,ccov62,ccov63,ccov64,ce11,ce12,ce13,ce14,ce21,ce22,ce23,ce24,ce31,ce32,ce33,ce34,
            ce41,ce42,ce43,ce44,ce51,ce52,ce53,ce54,ce61,ce62,ce63,ce64)
scenario <- rep(1:6,each=320,times=2)
data <- rep(1:4,each=80,times=12)
type <- rep(c("coverage","RMSE"),each=1920)
cout2 <- data.frame(cbind(date,result,scenario,data,type))

################################
## Prepare output for figures ##
################################

cl11 <- apply(cl11,2,mean)
cl12 <- apply(cl12,2,mean)
cl13 <- apply(cl13,2,mean)
cl14 <- apply(cl14,2,mean)
cm11 <- apply(cm11,2,mean)
cm12 <- apply(cm12,2,mean)
cm13 <- apply(cm13,2,mean)
cm14 <- apply(cm14,2,mean)
cu11 <- apply(cu11,2,mean)
cu12 <- apply(cu12,2,mean)
cu13 <- apply(cu13,2,mean)
cu14 <- apply(cu14,2,mean)
cl21 <- apply(cl21,2,mean)
cl22 <- apply(cl22,2,mean)
cl23 <- apply(cl23,2,mean)
cl24 <- apply(cl24,2,mean)
cm21 <- apply(cm21,2,mean)
cm22 <- apply(cm22,2,mean)
cm23 <- apply(cm23,2,mean)
cm24 <- apply(cm24,2,mean)
cu21 <- apply(cu21,2,mean)
cu22 <- apply(cu22,2,mean)
cu23 <- apply(cu23,2,mean)
cu24 <- apply(cu24,2,mean)
cl31 <- apply(cl31,2,mean)
cl32 <- apply(cl32,2,mean)
cl33 <- apply(cl33,2,mean)
cl34 <- apply(cl34,2,mean)
cm31 <- apply(cm31,2,mean)
cm32 <- apply(cm32,2,mean)
cm33 <- apply(cm33,2,mean)
cm34 <- apply(cm34,2,mean)
cu31 <- apply(cu31,2,mean)
cu32 <- apply(cu32,2,mean)
cu33 <- apply(cu33,2,mean)
cu34 <- apply(cu34,2,mean)
cl41 <- apply(cl41,2,mean)
cl42 <- apply(cl42,2,mean)
cl43 <- apply(cl43,2,mean)
cl44 <- apply(cl44,2,mean)
cm41 <- apply(cm41,2,mean)
cm42 <- apply(cm42,2,mean)
cm43 <- apply(cm43,2,mean)
cm44 <- apply(cm44,2,mean)
cu41 <- apply(cu41,2,mean)
cu42 <- apply(cu42,2,mean)
cu43 <- apply(cu43,2,mean)
cu44 <- apply(cu44,2,mean)
cl51 <- apply(cl51,2,mean)
cl52 <- apply(cl52,2,mean)
cl53 <- apply(cl53,2,mean)
cl54 <- apply(cl54,2,mean)
cm51 <- apply(cm51,2,mean)
cm52 <- apply(cm52,2,mean)
cm53 <- apply(cm53,2,mean)
cm54 <- apply(cm54,2,mean)
cu51 <- apply(cu51,2,mean)
cu52 <- apply(cu52,2,mean)
cu53 <- apply(cu53,2,mean)
cu54 <- apply(cu54,2,mean)
cl61 <- apply(cl61,2,mean)
cl62 <- apply(cl62,2,mean)
cl63 <- apply(cl63,2,mean)
cl64 <- apply(cl64,2,mean)
cm61 <- apply(cm61,2,mean)
cm62 <- apply(cm62,2,mean)
cm63 <- apply(cm63,2,mean)
cm64 <- apply(cm64,2,mean)
cu61 <- apply(cu61,2,mean)
cu62 <- apply(cu62,2,mean)
cu63 <- apply(cu63,2,mean)
cu64 <- apply(cu64,2,mean)
c1 <- apply(c1,2,mean)
c2 <- apply(c2,2,mean)
c3 <- apply(c3,2,mean)
c4 <- apply(c4,2,mean)
c5 <- apply(c5,2,mean)
c6 <- apply(c6,2,mean)
rc1 <- apply(rc1,2,mean)
rc2 <- apply(rc2,2,mean)
rc3 <- apply(rc3,2,mean)
rc4 <- apply(rc4,2,mean)
rc5 <- apply(rc5,2,mean)
rc6 <- apply(rc6,2,mean)

scenarios <- c("Single Delay Dist. & Correct Maximum Delay","Single Delay Dist. & Incorrect Maximum Delay",
               "Two Delay Dist. & Correct Maximum Delay","Two Delay Dist. & Incorrect Maximum Delay",
               "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay")
type <- c("From Day 31","From Day 41","From Day 51","From Day 61")
low <- c(cl11,cl12,cl13,cl14,cl21,cl22,cl23,cl24,cl31,cl32,cl33,cl34,cl41,cl42,cl43,cl44,cl51,cl52,cl53,cl54,cl61,cl62,cl63,cl64)
med <- c(cm11,cm12,cm13,cm14,cm21,cm22,cm23,cm24,cm31,cm32,cm33,cm34,cm41,cm42,cm43,cm44,cm51,cm52,cm53,cm54,cm61,cm62,cm63,cm64)
upp <- c(cu11,cu12,cu13,cu14,cu21,cu22,cu23,cu24,cu31,cu32,cu33,cu34,cu41,cu42,cu43,cu44,cu51,cu52,cu53,cu54,cu61,cu62,cu63,cu64)
true <- c(rep(c1,times=4),rep(c2,times=4),rep(c3,times=4),rep(c4,times=4),rep(c5,times=4),rep(c6,times=4))
report <- c(rep(rc1,times=4),rep(rc2,times=4),rep(rc3,times=4),rep(rc4,times=4),rep(rc5,times=4),rep(rc6,times=4))

outc <- data.frame(rep(1:80,times=24),low,med,upp,true,report,rep(type,each=80,times=6),rep(scenarios,each=320),rep(1:6,each=320))
colnames(outc) <- c("date","lower","median","upper","epic","repc","report","scenario","sn")
saveRDS(outc,"count_ss.rds")

## Processing the Rt
size <- 48 ## Minimum epicr size; Minimum repcr size is 45 

## Collect the lower, median and upper bounds
rl11 <- mat.or.vec(1000,size)
rl12 <- mat.or.vec(1000,size)
rl13 <- mat.or.vec(1000,size)
rl14 <- mat.or.vec(1000,size)
rm11 <- mat.or.vec(1000,size)
rm12 <- mat.or.vec(1000,size)
rm13 <- mat.or.vec(1000,size)
rm14 <- mat.or.vec(1000,size)
ru11 <- mat.or.vec(1000,size)
ru12 <- mat.or.vec(1000,size)
ru13 <- mat.or.vec(1000,size)
ru14 <- mat.or.vec(1000,size)
rl21 <- mat.or.vec(1000,size)
rl22 <- mat.or.vec(1000,size)
rl23 <- mat.or.vec(1000,size)
rl24 <- mat.or.vec(1000,size)
rm21 <- mat.or.vec(1000,size)
rm22 <- mat.or.vec(1000,size)
rm23 <- mat.or.vec(1000,size)
rm24 <- mat.or.vec(1000,size)
ru21 <- mat.or.vec(1000,size)
ru22 <- mat.or.vec(1000,size)
ru23 <- mat.or.vec(1000,size)
ru24 <- mat.or.vec(1000,size)
rl31 <- mat.or.vec(1000,size)
rl32 <- mat.or.vec(1000,size)
rl33 <- mat.or.vec(1000,size)
rl34 <- mat.or.vec(1000,size)
rm31 <- mat.or.vec(1000,size)
rm32 <- mat.or.vec(1000,size)
rm33 <- mat.or.vec(1000,size)
rm34 <- mat.or.vec(1000,size)
ru31 <- mat.or.vec(1000,size)
ru32 <- mat.or.vec(1000,size)
ru33 <- mat.or.vec(1000,size)
ru34 <- mat.or.vec(1000,size)
rl41 <- mat.or.vec(1000,size)
rl42 <- mat.or.vec(1000,size)
rl43 <- mat.or.vec(1000,size)
rl44 <- mat.or.vec(1000,size)
rm41 <- mat.or.vec(1000,size)
rm42 <- mat.or.vec(1000,size)
rm43 <- mat.or.vec(1000,size)
rm44 <- mat.or.vec(1000,size)
ru41 <- mat.or.vec(1000,size)
ru42 <- mat.or.vec(1000,size)
ru43 <- mat.or.vec(1000,size)
ru44 <- mat.or.vec(1000,size)
rl51 <- mat.or.vec(1000,size)
rl52 <- mat.or.vec(1000,size)
rl53 <- mat.or.vec(1000,size)
rl54 <- mat.or.vec(1000,size)
rm51 <- mat.or.vec(1000,size)
rm52 <- mat.or.vec(1000,size)
rm53 <- mat.or.vec(1000,size)
rm54 <- mat.or.vec(1000,size)
ru51 <- mat.or.vec(1000,size)
ru52 <- mat.or.vec(1000,size)
ru53 <- mat.or.vec(1000,size)
ru54 <- mat.or.vec(1000,size)
rl61 <- mat.or.vec(1000,size)
rl62 <- mat.or.vec(1000,size)
rl63 <- mat.or.vec(1000,size)
rl64 <- mat.or.vec(1000,size)
rm61 <- mat.or.vec(1000,size)
rm62 <- mat.or.vec(1000,size)
rm63 <- mat.or.vec(1000,size)
rm64 <- mat.or.vec(1000,size)
ru61 <- mat.or.vec(1000,size)
ru62 <- mat.or.vec(1000,size)
ru63 <- mat.or.vec(1000,size)
ru64 <- mat.or.vec(1000,size)


## Extract known epidemic curve
r1 <- mat.or.vec(1000,size)
r2 <- mat.or.vec(1000,size)
r3 <- mat.or.vec(1000,size)
r4 <- mat.or.vec(1000,size)
r5 <- mat.or.vec(1000,size)
r6 <- mat.or.vec(1000,size)

## Extract the report curve
rr1 <- mat.or.vec(1000,44)
rr2 <- mat.or.vec(1000,44)
rr3 <- mat.or.vec(1000,44)
rr4 <- mat.or.vec(1000,44)
rr5 <- mat.or.vec(1000,44)
rr6 <- mat.or.vec(1000,44)

for (i in 1:1000){
  n=47
  lr1=63
  lr2=53
  lr3=43
  lr4=33
  rl11[i,]=sce1[[i]]$estr1[1,(lr1-n):lr1]
  rl12[i,]=sce1[[i]]$estr2[1,(lr2-n):lr2]
  rl13[i,]=NA
  rl13[i,(n-lr3+2):(n+1)]=sce1[[i]]$estr3[1,]
  rl14[i,]=NA
  rl14[i,(n-lr4+2):(n+1)]=sce1[[i]]$estr4[1,]
  rm11[i,]=sce1[[i]]$estr1[2,(lr1-n):lr1]
  rm12[i,]=sce1[[i]]$estr2[2,(lr2-n):lr2]
  rm13[i,]=NA
  rm13[i,(n-lr3+2):(n+1)]=sce1[[i]]$estr3[2,]
  rm14[i,]=NA
  rm14[i,(n-lr4+2):(n+1)]=sce1[[i]]$estr4[2,]
  ru11[i,]=sce1[[i]]$estr1[3,(lr1-n):lr1]
  ru12[i,]=sce1[[i]]$estr2[3,(lr2-n):lr2]
  ru13[i,]=NA
  ru13[i,(n-lr3+2):(n+1)]=sce1[[i]]$estr3[3,]
  ru14[i,]=NA
  ru14[i,(n-lr4+2):(n+1)]=sce1[[i]]$estr4[3,]
  rl21[i,]=sce2[[i]]$estr1[1,(lr1-n):lr1]
  rl22[i,]=sce2[[i]]$estr2[1,(lr2-n):lr2]
  rl23[i,]=NA
  rl23[i,(n-lr3+2):(n+1)]=sce2[[i]]$estr3[1,]
  rl24[i,]=NA
  rl24[i,(n-lr4+2):(n+1)]=sce2[[i]]$estr4[1,]
  rm21[i,]=sce2[[i]]$estr1[2,(lr1-n):lr1]
  rm22[i,]=sce2[[i]]$estr2[2,(lr2-n):lr2]
  rm23[i,]=NA
  rm23[i,(n-lr3+2):(n+1)]=sce2[[i]]$estr3[2,]
  rm24[i,]=NA
  rm24[i,(n-lr4+2):(n+1)]=sce2[[i]]$estr4[2,]
  ru21[i,]=sce2[[i]]$estr1[3,(lr1-n):lr1]
  ru22[i,]=sce2[[i]]$estr2[3,(lr2-n):lr2]
  ru23[i,]=NA
  ru23[i,(n-lr3+2):(n+1)]=sce2[[i]]$estr3[3,]
  ru24[i,]=NA
  ru24[i,(n-lr4+2):(n+1)]=sce2[[i]]$estr4[3,]
  rl31[i,]=sce3[[i]]$estr1[1,(lr1-n):lr1]
  rl32[i,]=sce3[[i]]$estr2[1,(lr2-n):lr2]
  rl33[i,]=NA
  rl33[i,(n-lr3+2):(n+1)]=sce3[[i]]$estr3[1,]
  rl34[i,]=NA
  rl34[i,(n-lr4+2):(n+1)]=sce3[[i]]$estr4[1,]
  rm31[i,]=sce3[[i]]$estr1[2,(lr1-n):lr1]
  rm32[i,]=sce3[[i]]$estr2[2,(lr2-n):lr2]
  rm33[i,]=NA
  rm33[i,(n-lr3+2):(n+1)]=sce3[[i]]$estr3[2,]
  rm34[i,]=NA
  rm34[i,(n-lr4+2):(n+1)]=sce3[[i]]$estr4[2,]
  ru31[i,]=sce3[[i]]$estr1[3,(lr1-n):lr1]
  ru32[i,]=sce3[[i]]$estr2[3,(lr2-n):lr2]
  ru33[i,]=NA
  ru33[i,(n-lr3+2):(n+1)]=sce3[[i]]$estr3[3,]
  ru34[i,]=NA
  ru34[i,(n-lr4+2):(n+1)]=sce3[[i]]$estr4[3,]
  rl41[i,]=sce4[[i]]$estr1[1,(lr1-n):lr1]
  rl42[i,]=sce4[[i]]$estr2[1,(lr2-n):lr2]
  rl43[i,]=NA
  rl43[i,(n-lr3+2):(n+1)]=sce4[[i]]$estr3[1,]
  rl44[i,]=NA
  rl44[i,(n-lr4+2):(n+1)]=sce4[[i]]$estr4[1,]
  rm41[i,]=sce4[[i]]$estr1[2,(lr1-n):lr1]
  rm42[i,]=sce4[[i]]$estr2[2,(lr2-n):lr2]
  rm43[i,]=NA
  rm43[i,(n-lr3+2):(n+1)]=sce4[[i]]$estr3[2,]
  rm44[i,]=NA
  rm44[i,(n-lr4+2):(n+1)]=sce4[[i]]$estr4[2,]
  ru41[i,]=sce4[[i]]$estr1[3,(lr1-n):lr1]
  ru42[i,]=sce4[[i]]$estr2[3,(lr2-n):lr2]
  ru43[i,]=NA
  ru43[i,(n-lr3+2):(n+1)]=sce4[[i]]$estr3[3,]
  ru44[i,]=NA
  ru44[i,(n-lr4+2):(n+1)]=sce4[[i]]$estr4[3,]
  rl51[i,]=sce5[[i]]$estr1[1,(lr1-n):lr1]
  rl52[i,]=sce5[[i]]$estr2[1,(lr2-n):lr2]
  rl53[i,]=NA
  rl53[i,(n-lr3+2):(n+1)]=sce5[[i]]$estr3[1,]
  rl54[i,]=NA
  rl54[i,(n-lr4+2):(n+1)]=sce5[[i]]$estr4[1,]
  rm51[i,]=sce5[[i]]$estr1[2,(lr1-n):lr1]
  rm52[i,]=sce5[[i]]$estr2[2,(lr2-n):lr2]
  rm53[i,]=NA
  rm53[i,(n-lr3+2):(n+1)]=sce5[[i]]$estr3[2,]
  rm54[i,]=NA
  rm54[i,(n-lr4+2):(n+1)]=sce5[[i]]$estr4[2,]
  ru51[i,]=sce5[[i]]$estr1[3,(lr1-n):lr1]
  ru52[i,]=sce5[[i]]$estr2[3,(lr2-n):lr2]
  ru53[i,]=NA
  ru53[i,(n-lr3+2):(n+1)]=sce5[[i]]$estr3[3,]
  ru54[i,]=NA
  ru54[i,(n-lr4+2):(n+1)]=sce5[[i]]$estr4[3,]
  rl61[i,]=sce6[[i]]$estr1[1,(lr1-n):lr1]
  rl62[i,]=sce6[[i]]$estr2[1,(lr2-n):lr2]
  rl63[i,]=NA
  rl63[i,(n-lr3+2):(n+1)]=sce6[[i]]$estr3[1,]
  rl64[i,]=NA
  rl64[i,(n-lr4+2):(n+1)]=sce6[[i]]$estr4[1,]
  rm61[i,]=sce6[[i]]$estr1[2,(lr1-n):lr1]
  rm62[i,]=sce6[[i]]$estr2[2,(lr2-n):lr2]
  rm63[i,]=NA
  rm63[i,(n-lr3+2):(n+1)]=sce6[[i]]$estr3[2,]
  rm64[i,]=NA
  rm64[i,(n-lr4+2):(n+1)]=sce6[[i]]$estr4[2,]
  ru61[i,]=sce6[[i]]$estr1[3,(lr1-n):lr1]
  ru62[i,]=sce6[[i]]$estr2[3,(lr2-n):lr2]
  ru63[i,]=NA
  ru63[i,(n-lr3+2):(n+1)]=sce6[[i]]$estr3[3,]
  ru64[i,]=NA
  ru64[i,(n-lr4+2):(n+1)]=sce6[[i]]$estr4[3,]
  s1=length(sce1[[i]]$epicr)
  r1[i,]=sce1[[i]]$epicr[(s1-n):s1] 
  s2=length(sce2[[i]]$epicr)
  r2[i,]=sce2[[i]]$epicr[(s2-n):s2]
  s3=length(sce3[[i]]$epicr)
  r3[i,]=sce3[[i]]$epicr[(s3-n):s3] 
  s4=length(sce4[[i]]$epicr)
  r4[i,]=sce4[[i]]$epicr[(s4-n):s4]
  s5=length(sce5[[i]]$epicr)
  r5[i,]=sce5[[i]]$epicr[(s5-n):s5] 
  s6=length(sce6[[i]]$epicr)
  r6[i,]=sce6[[i]]$epicr[(s6-n):s6]
  s1=length(sce1[[i]]$repcr)
  rr1[i,]=sce1[[i]]$repcr[(s1-43):s1] 
  s2=length(sce2[[i]]$repcr)
  rr2[i,]=sce2[[i]]$repcr[(s2-43):s2]
  s3=length(sce3[[i]]$repcr)
  rr3[i,]=sce3[[i]]$repcr[(s3-43):s3] 
  s4=length(sce4[[i]]$repcr)
  rr4[i,]=sce4[[i]]$repcr[(s4-43):s4]
  s5=length(sce5[[i]]$repcr)
  rr5[i,]=sce5[[i]]$repcr[(s5-43):s5] 
  s6=length(sce6[[i]]$repcr)
  rr6[i,]=sce6[[i]]$repcr[(s6-43):s6]
}

######################################################
## Additional processing for coverage rate and RMSE ##
######################################################

size=48
rcov11 <- mat.or.vec(1000,size)
rcov21 <- mat.or.vec(1000,size)
rcov31 <- mat.or.vec(1000,size)
rcov41 <- mat.or.vec(1000,size)
rcov51 <- mat.or.vec(1000,size)
rcov61 <- mat.or.vec(1000,size)
rcov12 <- mat.or.vec(1000,size)
rcov22 <- mat.or.vec(1000,size)
rcov32 <- mat.or.vec(1000,size)
rcov42 <- mat.or.vec(1000,size)
rcov52 <- mat.or.vec(1000,size)
rcov62 <- mat.or.vec(1000,size)
rcov13 <- mat.or.vec(1000,size)
rcov23 <- mat.or.vec(1000,size)
rcov33 <- mat.or.vec(1000,size)
rcov43 <- mat.or.vec(1000,size)
rcov53 <- mat.or.vec(1000,size)
rcov63 <- mat.or.vec(1000,size)
rcov14 <- mat.or.vec(1000,size)
rcov24 <- mat.or.vec(1000,size)
rcov34 <- mat.or.vec(1000,size)
rcov44 <- mat.or.vec(1000,size)
rcov54 <- mat.or.vec(1000,size)
rcov64 <- mat.or.vec(1000,size)
re11 <- mat.or.vec(1000,size)
re12 <- mat.or.vec(1000,size)
re21 <- mat.or.vec(1000,size)
re22 <- mat.or.vec(1000,size)
re31 <- mat.or.vec(1000,size)
re32 <- mat.or.vec(1000,size)
re41 <- mat.or.vec(1000,size)
re42 <- mat.or.vec(1000,size)
re51 <- mat.or.vec(1000,size)
re52 <- mat.or.vec(1000,size)
re61 <- mat.or.vec(1000,size)
re62 <- mat.or.vec(1000,size)
re13 <- mat.or.vec(1000,size)
re14 <- mat.or.vec(1000,size)
re23 <- mat.or.vec(1000,size)
re24 <- mat.or.vec(1000,size)
re33 <- mat.or.vec(1000,size)
re34 <- mat.or.vec(1000,size)
re43 <- mat.or.vec(1000,size)
re44 <- mat.or.vec(1000,size)
re53 <- mat.or.vec(1000,size)
re54 <- mat.or.vec(1000,size)
re63 <- mat.or.vec(1000,size)
re64 <- mat.or.vec(1000,size)

for (i in 1:1000){
  rcov11[i,]=as.numeric((r1[i,]>=rl11[i,])&(r1[i,]<=ru11[i,]))
  rcov12[i,]=as.numeric((r1[i,]>=rl12[i,])&(r1[i,]<=ru12[i,]))
  rcov13[i,]=as.numeric((r1[i,]>=rl13[i,])&(r1[i,]<=ru13[i,]))
  rcov14[i,]=as.numeric((r1[i,]>=rl14[i,])&(r1[i,]<=ru14[i,]))
  rcov21[i,]=as.numeric((r2[i,]>=rl21[i,])&(r2[i,]<=ru21[i,]))
  rcov22[i,]=as.numeric((r2[i,]>=rl22[i,])&(r2[i,]<=ru22[i,]))
  rcov23[i,]=as.numeric((r2[i,]>=rl23[i,])&(r2[i,]<=ru23[i,]))
  rcov24[i,]=as.numeric((r2[i,]>=rl24[i,])&(r2[i,]<=ru24[i,]))
  rcov31[i,]=as.numeric((r3[i,]>=rl31[i,])&(r3[i,]<=ru31[i,]))
  rcov32[i,]=as.numeric((r3[i,]>=rl32[i,])&(r3[i,]<=ru32[i,]))
  rcov33[i,]=as.numeric((r3[i,]>=rl33[i,])&(r3[i,]<=ru33[i,]))
  rcov34[i,]=as.numeric((r3[i,]>=rl34[i,])&(r3[i,]<=ru34[i,]))
  rcov41[i,]=as.numeric((r4[i,]>=rl41[i,])&(r4[i,]<=ru41[i,]))
  rcov42[i,]=as.numeric((r4[i,]>=rl42[i,])&(r4[i,]<=ru42[i,]))
  rcov43[i,]=as.numeric((r4[i,]>=rl43[i,])&(r4[i,]<=ru43[i,]))
  rcov44[i,]=as.numeric((r4[i,]>=rl44[i,])&(r4[i,]<=ru44[i,]))
  rcov51[i,]=as.numeric((r5[i,]>=rl51[i,])&(r5[i,]<=ru51[i,]))
  rcov52[i,]=as.numeric((r5[i,]>=rl52[i,])&(r5[i,]<=ru52[i,]))
  rcov53[i,]=as.numeric((r5[i,]>=rl53[i,])&(r5[i,]<=ru53[i,]))
  rcov54[i,]=as.numeric((r5[i,]>=rl54[i,])&(r5[i,]<=ru54[i,]))
  rcov61[i,]=as.numeric((r6[i,]>=rl61[i,])&(r6[i,]<=ru61[i,]))
  rcov62[i,]=as.numeric((r6[i,]>=rl62[i,])&(r6[i,]<=ru62[i,]))
  rcov63[i,]=as.numeric((r6[i,]>=rl63[i,])&(r6[i,]<=ru63[i,]))
  rcov64[i,]=as.numeric((r6[i,]>=rl64[i,])&(r6[i,]<=ru64[i,]))
  re11[i,]=(rm11[i,]-r1[i,])^2
  re12[i,]=(rm12[i,]-r1[i,])^2
  re13[i,]=(rm13[i,]-r1[i,])^2
  re14[i,]=(rm14[i,]-r1[i,])^2
  re21[i,]=(rm21[i,]-r2[i,])^2
  re22[i,]=(rm22[i,]-r2[i,])^2
  re23[i,]=(rm23[i,]-r2[i,])^2
  re24[i,]=(rm24[i,]-r2[i,])^2
  re31[i,]=(rm31[i,]-r3[i,])^2
  re32[i,]=(rm32[i,]-r3[i,])^2
  re33[i,]=(rm33[i,]-r3[i,])^2
  re34[i,]=(rm34[i,]-r3[i,])^2
  re41[i,]=(rm41[i,]-r4[i,])^2
  re42[i,]=(rm42[i,]-r4[i,])^2
  re43[i,]=(rm43[i,]-r4[i,])^2
  re44[i,]=(rm44[i,]-r4[i,])^2
  re51[i,]=(rm51[i,]-r5[i,])^2
  re52[i,]=(rm52[i,]-r5[i,])^2
  re53[i,]=(rm53[i,]-r5[i,])^2
  re54[i,]=(rm54[i,]-r5[i,])^2
  re61[i,]=(rm61[i,]-r6[i,])^2
  re62[i,]=(rm62[i,]-r6[i,])^2
  re63[i,]=(rm63[i,]-r6[i,])^2
  re64[i,]=(rm64[i,]-r6[i,])^2
}

rcov11 <- apply(rcov11,2,sum)/1000
rcov12 <- apply(rcov12,2,sum)/1000
rcov13 <- apply(rcov13,2,sum)/1000
rcov14 <- apply(rcov14,2,sum)/1000
rcov21 <- apply(rcov21,2,sum)/1000
rcov22 <- apply(rcov22,2,sum)/1000
rcov23 <- apply(rcov23,2,sum)/1000
rcov24 <- apply(rcov24,2,sum)/1000
rcov31 <- apply(rcov31,2,sum)/1000
rcov32 <- apply(rcov32,2,sum)/1000
rcov33 <- apply(rcov33,2,sum)/1000
rcov34 <- apply(rcov34,2,sum)/1000
rcov41 <- apply(rcov41,2,sum)/1000
rcov42 <- apply(rcov42,2,sum)/1000
rcov43 <- apply(rcov43,2,sum)/1000
rcov44 <- apply(rcov44,2,sum)/1000
rcov51 <- apply(rcov51,2,sum)/1000
rcov52 <- apply(rcov52,2,sum)/1000
rcov53 <- apply(rcov53,2,sum)/1000
rcov54 <- apply(rcov54,2,sum)/1000
rcov61 <- apply(rcov61,2,sum)/1000
rcov62 <- apply(rcov62,2,sum)/1000
rcov63 <- apply(rcov63,2,sum)/1000
rcov64 <- apply(rcov64,2,sum)/1000
re11 <- sqrt(apply(re11,2,mean))
re12 <- sqrt(apply(re12,2,mean))
re13 <- sqrt(apply(re13,2,mean))
re14 <- sqrt(apply(re14,2,mean))
re21 <- sqrt(apply(re21,2,mean))
re22 <- sqrt(apply(re22,2,mean))
re23 <- sqrt(apply(re23,2,mean))
re24 <- sqrt(apply(re24,2,mean))
re31 <- sqrt(apply(re31,2,mean))
re32 <- sqrt(apply(re32,2,mean))
re33 <- sqrt(apply(re33,2,mean))
re34 <- sqrt(apply(re34,2,mean))
re41 <- sqrt(apply(re41,2,mean))
re42 <- sqrt(apply(re42,2,mean))
re43 <- sqrt(apply(re43,2,mean))
re44 <- sqrt(apply(re44,2,mean))
re51 <- sqrt(apply(re51,2,mean))
re52 <- sqrt(apply(re52,2,mean))
re53 <- sqrt(apply(re53,2,mean))
re54 <- sqrt(apply(re54,2,mean))
re61 <- sqrt(apply(re61,2,mean))
re62 <- sqrt(apply(re62,2,mean))
re63 <- sqrt(apply(re63,2,mean))
re64 <- sqrt(apply(re64,2,mean))

date <- rep(33:80,times=48)
result <- c(rcov11,rcov12,rcov13,rcov14,rcov21,rcov22,rcov23,rcov24,rcov31,rcov32,rcov33,rcov34,rcov41,rcov42,rcov43,rcov44,
            rcov51,rcov52,rcov53,rcov54,rcov61,rcov62,rcov63,rcov64,re11,re12,re13,re14,re21,re22,re23,re24,re31,re32,re33,re34,
            re41,re42,re43,re44,re51,re52,re53,re54,re61,re62,re63,re64)
scenario <- rep(1:6,each=192,times=2)
data <- rep(1:4,each=48,times=12)
type <- rep(c("coverage","RMSE"),each=1152)
rout2 <- data.frame(cbind(date,result,scenario,data,type))
eva2 <- list(c=cout2,r=rout2)
saveRDS(eva2,'eva2.rds')

################################
## Prepare output for figures ##
################################

rl11 <- apply(rl11,2,mean)
rl12 <- apply(rl12,2,mean)
rl13 <- apply(rl13,2,mean)
rl14 <- apply(rl14,2,mean)
rm11 <- apply(rm11,2,mean)
rm12 <- apply(rm12,2,mean)
rm13 <- apply(rm13,2,mean)
rm14 <- apply(rm14,2,mean)
ru11 <- apply(ru11,2,mean)
ru12 <- apply(ru12,2,mean)
ru13 <- apply(ru13,2,mean)
ru14 <- apply(ru14,2,mean)
rl21 <- apply(rl21,2,mean)
rl22 <- apply(rl22,2,mean)
rl23 <- apply(rl23,2,mean)
rl24 <- apply(rl24,2,mean)
rm21 <- apply(rm21,2,mean)
rm22 <- apply(rm22,2,mean)
rm23 <- apply(rm23,2,mean)
rm24 <- apply(rm24,2,mean)
ru21 <- apply(ru21,2,mean)
ru22 <- apply(ru22,2,mean)
ru23 <- apply(ru23,2,mean)
ru24 <- apply(ru24,2,mean)
rl31 <- apply(rl31,2,mean)
rl32 <- apply(rl32,2,mean)
rl33 <- apply(rl33,2,mean)
rl34 <- apply(rl34,2,mean)
rm31 <- apply(rm31,2,mean)
rm32 <- apply(rm32,2,mean)
rm33 <- apply(rm33,2,mean)
rm34 <- apply(rm34,2,mean)
ru31 <- apply(ru31,2,mean)
ru32 <- apply(ru32,2,mean)
ru33 <- apply(ru33,2,mean)
ru34 <- apply(ru34,2,mean)
rl41 <- apply(rl41,2,mean)
rl42 <- apply(rl42,2,mean)
rl43 <- apply(rl43,2,mean)
rl44 <- apply(rl44,2,mean)
rm41 <- apply(rm41,2,mean)
rm42 <- apply(rm42,2,mean)
rm43 <- apply(rm43,2,mean)
rm44 <- apply(rm44,2,mean)
ru41 <- apply(ru41,2,mean)
ru42 <- apply(ru42,2,mean)
ru43 <- apply(ru43,2,mean)
ru44 <- apply(ru44,2,mean)
rl51 <- apply(rl51,2,mean)
rl52 <- apply(rl52,2,mean)
rl53 <- apply(rl53,2,mean)
rl54 <- apply(rl54,2,mean)
rm51 <- apply(rm51,2,mean)
rm52 <- apply(rm52,2,mean)
rm53 <- apply(rm53,2,mean)
rm54 <- apply(rm54,2,mean)
ru51 <- apply(ru51,2,mean)
ru52 <- apply(ru52,2,mean)
ru53 <- apply(ru53,2,mean)
ru54 <- apply(ru54,2,mean)
rl61 <- apply(rl61,2,mean)
rl62 <- apply(rl62,2,mean)
rl63 <- apply(rl63,2,mean)
rl64 <- apply(rl64,2,mean)
rm61 <- apply(rm61,2,mean)
rm62 <- apply(rm62,2,mean)
rm63 <- apply(rm63,2,mean)
rm64 <- apply(rm64,2,mean)
ru61 <- apply(ru61,2,mean)
ru62 <- apply(ru62,2,mean)
ru63 <- apply(ru63,2,mean)
ru64 <- apply(ru64,2,mean)
r1 <- apply(r1,2,mean)
r2 <- apply(r2,2,mean)
r3 <- apply(r3,2,mean)
r4 <- apply(r4,2,mean)
r5 <- apply(r5,2,mean)
r6 <- apply(r6,2,mean)

rr1 <- c(rep(NA,4),apply(rr1,2,mean))
rr2 <- c(rep(NA,4),apply(rr2,2,mean))
rr3 <- c(rep(NA,4),apply(rr3,2,mean))
rr4 <- c(rep(NA,4),apply(rr4,2,mean))
rr5 <- c(rep(NA,4),apply(rr5,2,mean))
rr6 <- c(rep(NA,4),apply(rr6,2,mean))

scenarios <- c("Single Delay Dist. & Correct Maximum Delay","Single Delay Dist. & Incorrect Maximum Delay",
               "Two Delay Dist. & Correct Maximum Delay","Two Delay Dist. & Incorrect Maximum Delay",
               "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay")
type <- c("From Day 31","From Day 41","From Day 51","From Day 61")
low <- c(rl11,rl12,rl13,rl14,rl21,rl22,rl23,rl24,rl31,rl32,rl33,rl34,rl41,rl42,rl43,rl44,rl51,rl52,rl53,rl54,rl61,rl62,rl63,rl64)
med <- c(rm11,rm12,rm13,rm14,rm21,rm22,rm23,rm24,rm31,rm32,rm33,rm34,rm41,rm42,rm43,rm44,rm51,rm52,rm53,rm54,rm61,rm62,rm63,rm64)
upp <- c(ru11,ru12,ru13,ru14,ru21,ru22,ru23,ru24,ru31,ru32,ru33,ru34,ru41,ru42,ru43,ru44,ru51,ru52,ru53,ru54,ru61,ru62,ru63,ru64)
true <- c(rep(r1,times=4),rep(r2,times=4),rep(r3,times=4),rep(r4,times=4),rep(r5,times=4),rep(r6,times=4))
report <- c(rep(rr1,times=4),rep(rr2,times=4),rep(rr3,times=4),rep(rr4,times=4),rep(rr5,times=4),rep(rr6,times=4))


outr <- data.frame(rep(33:80,times=24),low,med,upp,true,report,rep(type,each=48,times=6),rep(scenarios,each=192),rep(1:6,each=192))
colnames(outr) <- c("date","lower","median","upper","epir","repr","report","scenario","sn")
saveRDS(outr,"rest_ss.rds")



###################################
## Prepare the output: Section 3 ##
###################################


## This part is for snapshots: for chopping off the tails

## Read the output
sce1 <- readRDS('sce1_ss1.rds')
sce2 <- readRDS('sce2_ss1.rds')
sce3 <- readRDS('sce3_ss1.rds')
sce4 <- readRDS('sce4_ss1.rds')
sce5 <- readRDS('sce5_ss1.rds')
sce6 <- readRDS('sce6_ss1.rds')

## Summarizing curve estimates
## Collect the lower, median and upper bounds
cl11 <- matrix(NA,nrow=1000,ncol=80)
cl12 <- matrix(NA,nrow=1000,ncol=80)
cm11 <- matrix(NA,nrow=1000,ncol=80)
cm12 <- matrix(NA,nrow=1000,ncol=80)
cu11 <- matrix(NA,nrow=1000,ncol=80)
cu12 <- matrix(NA,nrow=1000,ncol=80)
cl21 <- matrix(NA,nrow=1000,ncol=80)
cl22 <- matrix(NA,nrow=1000,ncol=80)
cm21 <- matrix(NA,nrow=1000,ncol=80)
cm22 <- matrix(NA,nrow=1000,ncol=80)
cu21 <- matrix(NA,nrow=1000,ncol=80)
cu22 <- matrix(NA,nrow=1000,ncol=80)
cl31 <- matrix(NA,nrow=1000,ncol=80)
cl32 <- matrix(NA,nrow=1000,ncol=80)
cm31 <- matrix(NA,nrow=1000,ncol=80)
cm32 <- matrix(NA,nrow=1000,ncol=80)
cu31 <- matrix(NA,nrow=1000,ncol=80)
cu32 <- matrix(NA,nrow=1000,ncol=80)
cl41 <- matrix(NA,nrow=1000,ncol=80)
cl42 <- matrix(NA,nrow=1000,ncol=80)
cm41 <- matrix(NA,nrow=1000,ncol=80)
cm42 <- matrix(NA,nrow=1000,ncol=80)
cu41 <- matrix(NA,nrow=1000,ncol=80)
cu42 <- matrix(NA,nrow=1000,ncol=80)
cl51 <- matrix(NA,nrow=1000,ncol=80)
cl52 <- matrix(NA,nrow=1000,ncol=80)
cm51 <- matrix(NA,nrow=1000,ncol=80)
cm52 <- matrix(NA,nrow=1000,ncol=80)
cu51 <- matrix(NA,nrow=1000,ncol=80)
cu52 <- matrix(NA,nrow=1000,ncol=80)
cl61 <- matrix(NA,nrow=1000,ncol=80)
cl62 <- matrix(NA,nrow=1000,ncol=80)
cm61 <- matrix(NA,nrow=1000,ncol=80)
cm62 <- matrix(NA,nrow=1000,ncol=80)
cu61 <- matrix(NA,nrow=1000,ncol=80)
cu62 <- matrix(NA,nrow=1000,ncol=80)


## Extract known epidemic curve
c1 <- mat.or.vec(1000,80)
c2 <- mat.or.vec(1000,80)
c3 <- mat.or.vec(1000,80)
c4 <- mat.or.vec(1000,80)
c5 <- mat.or.vec(1000,80)
c6 <- mat.or.vec(1000,80)

## Extract the report curve
rc1 <- mat.or.vec(1000,80)
rc2 <- mat.or.vec(1000,80)
rc3 <- mat.or.vec(1000,80)
rc4 <- mat.or.vec(1000,80)
rc5 <- mat.or.vec(1000,80)
rc6 <- mat.or.vec(1000,80)


for (i in 1:1000){
  md1=sce1[[i]]$minday
  md2=sce2[[i]]$minday
  md3=sce3[[i]]$minday
  md4=sce4[[i]]$minday
  md5=sce5[[i]]$minday
  md6=sce6[[i]]$minday
  cl11[i,md1:48]=sce1[[i]]$est1[1,]
  cl12[i,md1:58]=sce1[[i]]$est2[1,]
  cm11[i,md1:48]=sce1[[i]]$est1[2,]
  cm12[i,md1:58]=sce1[[i]]$est2[2,]
  cu11[i,md1:48]=sce1[[i]]$est1[3,]
  cu12[i,md1:58]=sce1[[i]]$est2[3,]
  cl21[i,md2:48]=sce2[[i]]$est1[1,]
  cl22[i,md2:58]=sce2[[i]]$est2[1,]
  cm21[i,md2:48]=sce2[[i]]$est1[2,]
  cm22[i,md2:58]=sce2[[i]]$est2[2,]
  cu21[i,md2:48]=sce2[[i]]$est1[3,]
  cu22[i,md2:58]=sce2[[i]]$est2[3,]
  cl31[i,md3:48]=sce3[[i]]$est1[1,]
  cl32[i,md3:58]=sce3[[i]]$est2[1,]
  cm31[i,md3:48]=sce3[[i]]$est1[2,]
  cm32[i,md3:58]=sce3[[i]]$est2[2,]
  cu31[i,md3:48]=sce3[[i]]$est1[3,]
  cu32[i,md3:58]=sce3[[i]]$est2[3,]
  cl41[i,md4:48]=sce4[[i]]$est1[1,]
  cl42[i,md4:58]=sce4[[i]]$est2[1,]
  cm41[i,md4:48]=sce4[[i]]$est1[2,]
  cm42[i,md4:58]=sce4[[i]]$est2[2,]
  cu41[i,md4:48]=sce4[[i]]$est1[3,]
  cu42[i,md4:58]=sce4[[i]]$est2[3,]
  cl51[i,md5:48]=sce5[[i]]$est1[1,]
  cl52[i,md5:58]=sce5[[i]]$est2[1,]
  cm51[i,md5:48]=sce5[[i]]$est1[2,]
  cm52[i,md5:58]=sce5[[i]]$est2[2,]
  cu51[i,md5:48]=sce5[[i]]$est1[3,]
  cu52[i,md5:58]=sce5[[i]]$est2[3,]
  cl61[i,md6:48]=sce6[[i]]$est1[1,]
  cl62[i,md6:58]=sce6[[i]]$est2[1,]
  cm61[i,md6:48]=sce6[[i]]$est1[2,]
  cm62[i,md6:58]=sce6[[i]]$est2[2,]
  cu61[i,md6:48]=sce6[[i]]$est1[3,]
  cu62[i,md6:58]=sce6[[i]]$est2[3,]
  days1=as.numeric(names(sce1[[i]]$epic))+20
  c1[i,days1]=sce1[[i]]$epic 
  days2=as.numeric(names(sce2[[i]]$epic))+20
  c2[i,days2]=sce2[[i]]$epic 
  days3=as.numeric(names(sce3[[i]]$epic))+20
  c3[i,days3]=sce3[[i]]$epic 
  days4=as.numeric(names(sce4[[i]]$epic))+20
  c4[i,days4]=sce4[[i]]$epic 
  days5=as.numeric(names(sce5[[i]]$epic))+20
  c5[i,days5]=sce5[[i]]$epic 
  days6=as.numeric(names(sce6[[i]]$epic))+20
  c6[i,days6]=sce6[[i]]$epic 
  days1=as.numeric(names(sce1[[i]]$repc))+20
  rc1[i,days1]=sce1[[i]]$repc 
  days2=as.numeric(names(sce2[[i]]$repc))+20
  rc2[i,days2]=sce2[[i]]$repc 
  days3=as.numeric(names(sce3[[i]]$repc))+20
  rc3[i,days3]=sce3[[i]]$repc 
  days4=as.numeric(names(sce4[[i]]$repc))+20
  rc4[i,days4]=sce4[[i]]$repc 
  days5=as.numeric(names(sce5[[i]]$repc))+20
  rc5[i,days5]=sce5[[i]]$repc 
  days6=as.numeric(names(sce6[[i]]$repc))+20
  rc6[i,days6]=sce6[[i]]$repc 
}


######################################################
## Additional processing for coverage rate and RMSE ##
######################################################

ccov11 <- mat.or.vec(1000,80)
ccov21 <- mat.or.vec(1000,80)
ccov31 <- mat.or.vec(1000,80)
ccov41 <- mat.or.vec(1000,80)
ccov51 <- mat.or.vec(1000,80)
ccov61 <- mat.or.vec(1000,80)
ccov12 <- mat.or.vec(1000,80)
ccov22 <- mat.or.vec(1000,80)
ccov32 <- mat.or.vec(1000,80)
ccov42 <- mat.or.vec(1000,80)
ccov52 <- mat.or.vec(1000,80)
ccov62 <- mat.or.vec(1000,80)
ce11 <- mat.or.vec(1000,80)
ce12 <- mat.or.vec(1000,80)
ce21 <- mat.or.vec(1000,80)
ce22 <- mat.or.vec(1000,80)
ce31 <- mat.or.vec(1000,80)
ce32 <- mat.or.vec(1000,80)
ce41 <- mat.or.vec(1000,80)
ce42 <- mat.or.vec(1000,80)
ce51 <- mat.or.vec(1000,80)
ce52 <- mat.or.vec(1000,80)
ce61 <- mat.or.vec(1000,80)
ce62 <- mat.or.vec(1000,80)

for (i in 1:1000){
  ccov11[i,]=as.numeric((c1[i,]>=cl11[i,])&(c1[i,]<=cu11[i,]))
  ccov12[i,]=as.numeric((c1[i,]>=cl12[i,])&(c1[i,]<=cu12[i,]))
  ccov21[i,]=as.numeric((c2[i,]>=cl21[i,])&(c2[i,]<=cu21[i,]))
  ccov22[i,]=as.numeric((c2[i,]>=cl22[i,])&(c2[i,]<=cu22[i,]))
  ccov31[i,]=as.numeric((c3[i,]>=cl31[i,])&(c3[i,]<=cu31[i,]))
  ccov32[i,]=as.numeric((c3[i,]>=cl32[i,])&(c3[i,]<=cu32[i,]))
  ccov41[i,]=as.numeric((c4[i,]>=cl41[i,])&(c4[i,]<=cu41[i,]))
  ccov42[i,]=as.numeric((c4[i,]>=cl42[i,])&(c4[i,]<=cu42[i,]))
  ccov51[i,]=as.numeric((c5[i,]>=cl51[i,])&(c5[i,]<=cu51[i,]))
  ccov52[i,]=as.numeric((c5[i,]>=cl52[i,])&(c5[i,]<=cu52[i,]))
  ccov61[i,]=as.numeric((c6[i,]>=cl61[i,])&(c6[i,]<=cu61[i,]))
  ccov62[i,]=as.numeric((c6[i,]>=cl62[i,])&(c6[i,]<=cu62[i,]))
  ce11[i,]=(cm11[i,]-c1[i,])^2
  ce12[i,]=(cm12[i,]-c1[i,])^2
  ce21[i,]=(cm21[i,]-c2[i,])^2
  ce22[i,]=(cm22[i,]-c2[i,])^2
  ce31[i,]=(cm31[i,]-c3[i,])^2
  ce32[i,]=(cm32[i,]-c3[i,])^2
  ce41[i,]=(cm41[i,]-c4[i,])^2
  ce42[i,]=(cm42[i,]-c4[i,])^2
  ce51[i,]=(cm51[i,]-c5[i,])^2
  ce52[i,]=(cm52[i,]-c5[i,])^2
  ce61[i,]=(cm61[i,]-c6[i,])^2
  ce62[i,]=(cm62[i,]-c6[i,])^2
}

ccov11 <- apply(ccov11,2,sum)/1000
ccov12 <- apply(ccov12,2,sum)/1000
ccov21 <- apply(ccov21,2,sum)/1000
ccov22 <- apply(ccov22,2,sum)/1000
ccov31 <- apply(ccov31,2,sum)/1000
ccov32 <- apply(ccov32,2,sum)/1000
ccov41 <- apply(ccov41,2,sum)/1000
ccov42 <- apply(ccov42,2,sum)/1000
ccov51 <- apply(ccov51,2,sum)/1000
ccov52 <- apply(ccov52,2,sum)/1000
ccov61 <- apply(ccov61,2,sum)/1000
ccov62 <- apply(ccov62,2,sum)/1000
ce11 <- sqrt(apply(ce11,2,mean))
ce12 <- sqrt(apply(ce12,2,mean))
ce21 <- sqrt(apply(ce21,2,mean))
ce22 <- sqrt(apply(ce22,2,mean))
ce31 <- sqrt(apply(ce31,2,mean))
ce32 <- sqrt(apply(ce32,2,mean))
ce41 <- sqrt(apply(ce41,2,mean))
ce42 <- sqrt(apply(ce42,2,mean))
ce51 <- sqrt(apply(ce51,2,mean))
ce52 <- sqrt(apply(ce52,2,mean))
ce61 <- sqrt(apply(ce61,2,mean))
ce62 <- sqrt(apply(ce62,2,mean))

date <- rep(1:80,times=24)
result <- c(ccov11,ccov12,ccov21,ccov22,ccov31,ccov32,ccov41,ccov42,ccov51,ccov52,ccov61,ccov62,
            ce11,ce12,ce21,ce22,ce31,ce32,ce41,ce42,ce51,ce52,ce61,ce62)
scenario <- rep(1:6,each=160,times=2)
data <- rep(1:2,each=80,times=12)
type <- rep(c("coverage","RMSE"),each=960)
cout3 <- data.frame(cbind(date,result,scenario,data,type))

################################
## Prepare output for figures ##
################################

cl11 <- apply(cl11,2,mean)
cl12 <- apply(cl12,2,mean)
cm11 <- apply(cm11,2,mean)
cm12 <- apply(cm12,2,mean)
cu11 <- apply(cu11,2,mean)
cu12 <- apply(cu12,2,mean)
cl21 <- apply(cl21,2,mean)
cl22 <- apply(cl22,2,mean)
cm21 <- apply(cm21,2,mean)
cm22 <- apply(cm22,2,mean)
cu21 <- apply(cu21,2,mean)
cu22 <- apply(cu22,2,mean)
cl31 <- apply(cl31,2,mean)
cl32 <- apply(cl32,2,mean)
cm31 <- apply(cm31,2,mean)
cm32 <- apply(cm32,2,mean)
cu31 <- apply(cu31,2,mean)
cu32 <- apply(cu32,2,mean)
cl41 <- apply(cl41,2,mean)
cl42 <- apply(cl42,2,mean)
cm41 <- apply(cm41,2,mean)
cm42 <- apply(cm42,2,mean)
cu41 <- apply(cu41,2,mean)
cu42 <- apply(cu42,2,mean)
cl51 <- apply(cl51,2,mean)
cl52 <- apply(cl52,2,mean)
cm51 <- apply(cm51,2,mean)
cm52 <- apply(cm52,2,mean)
cu51 <- apply(cu51,2,mean)
cu52 <- apply(cu52,2,mean)
cl61 <- apply(cl61,2,mean)
cl62 <- apply(cl62,2,mean)
cm61 <- apply(cm61,2,mean)
cm62 <- apply(cm62,2,mean)
cu61 <- apply(cu61,2,mean)
cu62 <- apply(cu62,2,mean)
c1 <- apply(c1,2,mean)
c2 <- apply(c2,2,mean)
c3 <- apply(c3,2,mean)
c4 <- apply(c4,2,mean)
c5 <- apply(c5,2,mean)
c6 <- apply(c6,2,mean)
rc1 <- apply(rc1,2,mean)
rc2 <- apply(rc2,2,mean)
rc3 <- apply(rc3,2,mean)
rc4 <- apply(rc4,2,mean)
rc5 <- apply(rc5,2,mean)
rc6 <- apply(rc6,2,mean)

scenarios <- c("Single Delay Dist. & Correct Maximum Delay","Single Delay Dist. & Incorrect Maximum Delay",
               "Two Delay Dist. & Correct Maximum Delay","Two Delay Dist. & Incorrect Maximum Delay",
               "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay")
type <- c("Until Day 48","Until Day 58")
low <- c(cl11,cl12,cl21,cl22,cl31,cl32,cl41,cl42,cl51,cl52,cl61,cl62)
med <- c(cm11,cm12,cm21,cm22,cm31,cm32,cm41,cm42,cm51,cm52,cm61,cm62)
upp <- c(cu11,cu12,cu21,cu22,cu31,cu32,cu41,cu42,cu51,cu52,cu61,cu62)
true <- c(c1,c1,c2,c2,c3,c3,c4,c4,c5,c5,c6,c6)
report <- c(rc1,rc1,rc2,rc2,rc3,rc3,rc4,rc4,rc5,rc5,rc6,rc6)

outc <- data.frame(rep(1:80,times=12),low,med,upp,true,report,rep(type,each=80,times=6),rep(scenarios,each=160),rep(1:6,each=160))
colnames(outc) <- c("date","lower","median","upper","epic","repc","report","scenario","sn")
saveRDS(outc,'count_ss1.rds')

## Summarizing reproductive number estimates
size <- numeric(1000)
for (i in 1:1000){
  size[i] <- length(sce4[[i]]$epicr)
}
## Choose the last 48 Rt estimates so as to have consistent comparison (48 is min(size) for all scenarios)
size <- 48
## Choose the last 44 Rt estimates based on the reported curve

## Collect the lower, median and upper bounds
rl11 <- matrix(NA,nrow=1000,ncol=size)
rl12 <- matrix(NA,nrow=1000,ncol=size)
rm11 <- matrix(NA,nrow=1000,ncol=size)
rm12 <- matrix(NA,nrow=1000,ncol=size)
ru11 <- matrix(NA,nrow=1000,ncol=size)
ru12 <- matrix(NA,nrow=1000,ncol=size)
rl21 <- matrix(NA,nrow=1000,ncol=size)
rl22 <- matrix(NA,nrow=1000,ncol=size)
rm21 <- matrix(NA,nrow=1000,ncol=size)
rm22 <- matrix(NA,nrow=1000,ncol=size)
ru21 <- matrix(NA,nrow=1000,ncol=size)
ru22 <- matrix(NA,nrow=1000,ncol=size)
rl31 <- matrix(NA,nrow=1000,ncol=size)
rl32 <- matrix(NA,nrow=1000,ncol=size)
rm31 <- matrix(NA,nrow=1000,ncol=size)
rm32 <- matrix(NA,nrow=1000,ncol=size)
ru31 <- matrix(NA,nrow=1000,ncol=size)
ru32 <- matrix(NA,nrow=1000,ncol=size)
rl41 <- matrix(NA,nrow=1000,ncol=size)
rl42 <- matrix(NA,nrow=1000,ncol=size)
rm41 <- matrix(NA,nrow=1000,ncol=size)
rm42 <- matrix(NA,nrow=1000,ncol=size)
ru41 <- matrix(NA,nrow=1000,ncol=size)
ru42 <- matrix(NA,nrow=1000,ncol=size)
rl51 <- matrix(NA,nrow=1000,ncol=size)
rl52 <- matrix(NA,nrow=1000,ncol=size)
rm51 <- matrix(NA,nrow=1000,ncol=size)
rm52 <- matrix(NA,nrow=1000,ncol=size)
ru51 <- matrix(NA,nrow=1000,ncol=size)
ru52 <- matrix(NA,nrow=1000,ncol=size)
rl61 <- matrix(NA,nrow=1000,ncol=size)
rl62 <- matrix(NA,nrow=1000,ncol=size)
rm61 <- matrix(NA,nrow=1000,ncol=size)
rm62 <- matrix(NA,nrow=1000,ncol=size)
ru61 <- matrix(NA,nrow=1000,ncol=size)
ru62 <- matrix(NA,nrow=1000,ncol=size)


## Extract known epidemic curve
r1 <- mat.or.vec(1000,size)
r2 <- mat.or.vec(1000,size)
r3 <- mat.or.vec(1000,size)
r4 <- mat.or.vec(1000,size)
r5 <- mat.or.vec(1000,size)
r6 <- mat.or.vec(1000,size)
## Extract the report curve
rr1 <- mat.or.vec(1000,44)
rr2 <- mat.or.vec(1000,44)
rr3 <- mat.or.vec(1000,44)
rr4 <- mat.or.vec(1000,44)
rr5 <- mat.or.vec(1000,44)
rr6 <- mat.or.vec(1000,44)

for (i in 1:1000){
  lr1=ncol(sce1[[i]]$estr1)
  lr2=ncol(sce2[[i]]$estr1)
  lr3=ncol(sce3[[i]]$estr1)
  lr4=ncol(sce4[[i]]$estr1)
  lr5=ncol(sce5[[i]]$estr1)
  lr6=ncol(sce6[[i]]$estr1)
  rl11[i,1:16]=sce1[[i]]$estr1[1,(lr1-15):lr1]
  rl12[i,1:26]=sce1[[i]]$estr2[1,(lr1-15):(lr1+10)]
  rm11[i,1:16]=sce1[[i]]$estr1[2,(lr1-15):lr1]
  rm12[i,1:26]=sce1[[i]]$estr2[2,(lr1-15):(lr1+10)]
  ru11[i,1:16]=sce1[[i]]$estr1[3,(lr1-15):lr1]
  ru12[i,1:26]=sce1[[i]]$estr2[3,(lr1-15):(lr1+10)]
  rl21[i,1:16]=sce2[[i]]$estr1[1,(lr2-15):lr2]
  rl22[i,1:26]=sce2[[i]]$estr2[1,(lr2-15):(lr2+10)]
  rm21[i,1:16]=sce2[[i]]$estr1[2,(lr2-15):lr2]
  rm22[i,1:26]=sce2[[i]]$estr2[2,(lr2-15):(lr2+10)]
  ru21[i,1:16]=sce2[[i]]$estr1[3,(lr2-15):lr2]
  ru22[i,1:26]=sce2[[i]]$estr2[3,(lr2-15):(lr2+10)]
  rl31[i,1:16]=sce3[[i]]$estr1[1,(lr3-15):lr3]
  rl32[i,1:26]=sce3[[i]]$estr2[1,(lr3-15):(lr3+10)]
  rm31[i,1:16]=sce3[[i]]$estr1[2,(lr3-15):lr3]
  rm32[i,1:26]=sce3[[i]]$estr2[2,(lr3-15):(lr3+10)]
  ru31[i,1:16]=sce3[[i]]$estr1[3,(lr3-15):lr3]
  ru32[i,1:26]=sce3[[i]]$estr2[3,(lr3-15):(lr3+10)]
  rl41[i,1:16]=sce4[[i]]$estr1[1,(lr4-15):lr4]
  rl42[i,1:26]=sce4[[i]]$estr2[1,(lr4-15):(lr4+10)]
  rm41[i,1:16]=sce4[[i]]$estr1[2,(lr4-15):lr4]
  rm42[i,1:26]=sce4[[i]]$estr2[2,(lr4-15):(lr4+10)]
  ru41[i,1:16]=sce4[[i]]$estr1[3,(lr4-15):lr4]
  ru42[i,1:26]=sce4[[i]]$estr2[3,(lr4-15):(lr4+10)]
  rl51[i,1:16]=sce5[[i]]$estr1[1,(lr5-15):lr5]
  rl52[i,1:26]=sce5[[i]]$estr2[1,(lr5-15):(lr5+10)]
  rm51[i,1:16]=sce5[[i]]$estr1[2,(lr5-15):lr5]
  rm52[i,1:26]=sce5[[i]]$estr2[2,(lr5-15):(lr5+10)]
  ru51[i,1:16]=sce5[[i]]$estr1[3,(lr5-15):lr5]
  ru52[i,1:26]=sce5[[i]]$estr2[3,(lr5-15):(lr5+10)]
  rl61[i,1:16]=sce6[[i]]$estr1[1,(lr6-15):lr6]
  rl62[i,1:26]=sce6[[i]]$estr2[1,(lr6-15):(lr6+10)]
  rm61[i,1:16]=sce6[[i]]$estr1[2,(lr6-15):lr6]
  rm62[i,1:26]=sce6[[i]]$estr2[2,(lr6-15):(lr6+10)]
  ru61[i,1:16]=sce6[[i]]$estr1[3,(lr6-15):lr6]
  ru62[i,1:26]=sce6[[i]]$estr2[3,(lr6-15):(lr6+10)]
  n=47
  s1=length(sce1[[i]]$epicr)
  r1[i,]=sce1[[i]]$epicr[(s1-n):s1] 
  s2=length(sce2[[i]]$epicr)
  r2[i,]=sce2[[i]]$epicr[(s2-n):s2]
  s3=length(sce3[[i]]$epicr)
  r3[i,]=sce3[[i]]$epicr[(s3-n):s3]
  s4=length(sce4[[i]]$epicr)
  r4[i,]=sce4[[i]]$epicr[(s4-n):s4]
  s5=length(sce5[[i]]$epicr)
  r5[i,]=sce5[[i]]$epicr[(s5-n):s5]
  s6=length(sce6[[i]]$epicr)
  r6[i,]=sce6[[i]]$epicr[(s6-n):s6]
  s1=length(sce1[[i]]$repcr)
  rr1[i,]=sce1[[i]]$repcr[(s1-43):s1] 
  s2=length(sce2[[i]]$repcr)
  rr2[i,]=sce2[[i]]$repcr[(s2-43):s2]
  s3=length(sce3[[i]]$repcr)
  rr3[i,]=sce3[[i]]$repcr[(s3-43):s3]
  s4=length(sce4[[i]]$repcr)
  rr4[i,]=sce4[[i]]$repcr[(s4-43):s4]
  s5=length(sce5[[i]]$repcr)
  rr5[i,]=sce5[[i]]$repcr[(s5-43):s5]
  s6=length(sce6[[i]]$repcr)
  rr6[i,]=sce6[[i]]$repcr[(s6-43):s6] 
}

######################################################
## Additional processing for coverage rate and RMSE ##
######################################################
size=48
rcov11 <- mat.or.vec(1000,size)
rcov21 <- mat.or.vec(1000,size)
rcov31 <- mat.or.vec(1000,size)
rcov41 <- mat.or.vec(1000,size)
rcov51 <- mat.or.vec(1000,size)
rcov61 <- mat.or.vec(1000,size)
rcov12 <- mat.or.vec(1000,size)
rcov22 <- mat.or.vec(1000,size)
rcov32 <- mat.or.vec(1000,size)
rcov42 <- mat.or.vec(1000,size)
rcov52 <- mat.or.vec(1000,size)
rcov62 <- mat.or.vec(1000,size)
re11 <- mat.or.vec(1000,size)
re12 <- mat.or.vec(1000,size)
re21 <- mat.or.vec(1000,size)
re22 <- mat.or.vec(1000,size)
re31 <- mat.or.vec(1000,size)
re32 <- mat.or.vec(1000,size)
re41 <- mat.or.vec(1000,size)
re42 <- mat.or.vec(1000,size)
re51 <- mat.or.vec(1000,size)
re52 <- mat.or.vec(1000,size)
re61 <- mat.or.vec(1000,size)
re62 <- mat.or.vec(1000,size)

for (i in 1:1000){
  rcov11[i,]=as.numeric((r1[i,]>=rl11[i,])&(r1[i,]<=ru11[i,]))
  rcov12[i,]=as.numeric((r1[i,]>=rl12[i,])&(r1[i,]<=ru12[i,]))
  rcov21[i,]=as.numeric((r2[i,]>=rl21[i,])&(r2[i,]<=ru21[i,]))
  rcov22[i,]=as.numeric((r2[i,]>=rl22[i,])&(r2[i,]<=ru22[i,]))
  rcov31[i,]=as.numeric((r3[i,]>=rl31[i,])&(r3[i,]<=ru31[i,]))
  rcov32[i,]=as.numeric((r3[i,]>=rl32[i,])&(r3[i,]<=ru32[i,]))
  rcov41[i,]=as.numeric((r4[i,]>=rl41[i,])&(r4[i,]<=ru41[i,]))
  rcov42[i,]=as.numeric((r4[i,]>=rl42[i,])&(r4[i,]<=ru42[i,]))
  rcov51[i,]=as.numeric((r5[i,]>=rl51[i,])&(r5[i,]<=ru51[i,]))
  rcov52[i,]=as.numeric((r5[i,]>=rl52[i,])&(r5[i,]<=ru52[i,]))
  rcov61[i,]=as.numeric((r6[i,]>=rl61[i,])&(r6[i,]<=ru61[i,]))
  rcov62[i,]=as.numeric((r6[i,]>=rl62[i,])&(r6[i,]<=ru62[i,]))
  re11[i,]=(rm11[i,]-r1[i,])^2
  re12[i,]=(rm12[i,]-r1[i,])^2
  re21[i,]=(rm21[i,]-r2[i,])^2
  re22[i,]=(rm22[i,]-r2[i,])^2
  re31[i,]=(rm31[i,]-r3[i,])^2
  re32[i,]=(rm32[i,]-r3[i,])^2
  re41[i,]=(rm41[i,]-r4[i,])^2
  re42[i,]=(rm42[i,]-r4[i,])^2
  re51[i,]=(rm51[i,]-r5[i,])^2
  re52[i,]=(rm52[i,]-r5[i,])^2
  re61[i,]=(rm61[i,]-r6[i,])^2
  re62[i,]=(rm62[i,]-r6[i,])^2
}

rcov11 <- apply(rcov11,2,sum)/1000
rcov12 <- apply(rcov12,2,sum)/1000
rcov21 <- apply(rcov21,2,sum)/1000
rcov22 <- apply(rcov22,2,sum)/1000
rcov31 <- apply(rcov31,2,sum)/1000
rcov32 <- apply(rcov32,2,sum)/1000
rcov41 <- apply(rcov41,2,sum)/1000
rcov42 <- apply(rcov42,2,sum)/1000
rcov51 <- apply(rcov51,2,sum)/1000
rcov52 <- apply(rcov52,2,sum)/1000
rcov61 <- apply(rcov61,2,sum)/1000
rcov62 <- apply(rcov62,2,sum)/1000
re11 <- sqrt(apply(re11,2,mean))
re12 <- sqrt(apply(re12,2,mean))
re21 <- sqrt(apply(re21,2,mean))
re22 <- sqrt(apply(re22,2,mean))
re31 <- sqrt(apply(re31,2,mean))
re32 <- sqrt(apply(re32,2,mean))
re41 <- sqrt(apply(re41,2,mean))
re42 <- sqrt(apply(re42,2,mean))
re51 <- sqrt(apply(re51,2,mean))
re52 <- sqrt(apply(re52,2,mean))
re61 <- sqrt(apply(re61,2,mean))
re62 <- sqrt(apply(re62,2,mean))

date <- rep(33:80,times=24)
result <- c(rcov11,rcov12,rcov21,rcov22,rcov31,rcov32,rcov41,rcov42,rcov51,rcov52,rcov61,rcov62,
            re11,re12,re21,re22,re31,re32,re41,re42,re51,re52,re61,re62)
scenario <- rep(1:6,each=96,times=2)
data <- rep(1:2,each=48,times=12)
type <- rep(c("coverage","RMSE"),each=576)
rout3 <- data.frame(cbind(date,result,scenario,data,type))
eva3 <- list(c=cout3,r=rout3)
saveRDS(eva3,'eva3.rds')

################################
## Prepare output for figures ##
################################

rl11 <- apply(rl11,2,mean)
rl12 <- apply(rl12,2,mean)
rm11 <- apply(rm11,2,mean)
rm12 <- apply(rm12,2,mean)
ru11 <- apply(ru11,2,mean)
ru12 <- apply(ru12,2,mean)
rl21 <- apply(rl21,2,mean)
rl22 <- apply(rl22,2,mean)
rm21 <- apply(rm21,2,mean)
rm22 <- apply(rm22,2,mean)
ru21 <- apply(ru21,2,mean)
ru22 <- apply(ru22,2,mean)
rl31 <- apply(rl31,2,mean)
rl32 <- apply(rl32,2,mean)
rm31 <- apply(rm31,2,mean)
rm32 <- apply(rm32,2,mean)
ru31 <- apply(ru31,2,mean)
ru32 <- apply(ru32,2,mean)
rl41 <- apply(rl41,2,mean)
rl42 <- apply(rl42,2,mean)
rm41 <- apply(rm41,2,mean)
rm42 <- apply(rm42,2,mean)
ru41 <- apply(ru41,2,mean)
ru42 <- apply(ru42,2,mean)
rl51 <- apply(rl51,2,mean)
rl52 <- apply(rl52,2,mean)
rm51 <- apply(rm51,2,mean)
rm52 <- apply(rm52,2,mean)
ru51 <- apply(ru51,2,mean)
ru52 <- apply(ru52,2,mean)
rl61 <- apply(rl61,2,mean)
rl62 <- apply(rl62,2,mean)
rm61 <- apply(rm61,2,mean)
rm62 <- apply(rm62,2,mean)
ru61 <- apply(ru61,2,mean)
ru62 <- apply(ru62,2,mean)
r1 <- apply(r1,2,mean)
r2 <- apply(r2,2,mean)
r3 <- apply(r3,2,mean)
r4 <- apply(r4,2,mean)
r5 <- apply(r5,2,mean)
r6 <- apply(r6,2,mean)
rr1 <- c(rep(NA,4),apply(rr1,2,mean))
rr2 <- c(rep(NA,4),apply(rr2,2,mean))
rr3 <- c(rep(NA,4),apply(rr3,2,mean))
rr4 <- c(rep(NA,4),apply(rr4,2,mean))
rr5 <- c(rep(NA,4),apply(rr5,2,mean))
rr6 <- c(rep(NA,4),apply(rr6,2,mean))

scenarios <- c("Single Delay Dist. & Correct Maximum Delay","Single Delay Dist. & Incorrect Maximum Delay",
               "Two Delay Dist. & Correct Maximum Delay","Two Delay Dist. & Incorrect Maximum Delay",
               "Mutiple Delay Dist. & Correct Maximum Delay", "Mutiple Delay Dist. & Incorrect Maximum Delay")
type <- c("Until Day 48","Untile Day 58")
low <- c(rl11,rl12,rl21,rl22,rl31,rl32,rl41,rl42,rl51,rl52,rl61,rl62)
med <- c(rm11,rm12,rm21,rm22,rm31,rm32,rm41,rm42,rm51,rm52,rm61,rm62)
upp <- c(ru11,ru12,ru21,ru22,ru31,ru32,ru41,ru42,ru51,ru52,ru61,ru62)
epir <- c(r1,r1,r2,r2,r3,r3,r4,r4,r5,r5,r6,r6)
repr <- c(rr1,rr1,rr2,rr2,rr3,rr3,rr4,rr4,rr5,rr5,rr6,rr6)
outr <- data.frame(rep(33:80,times=12),low,med,upp,epir,repr,rep(type,each=48,times=6),rep(scenarios,each=96),rep(1:6,each=96))
colnames(outr) <- c("date","lower","median","upper","epir","repr","report","scenario","sn")
saveRDS(outr,'rest_ss1.rds')



##############################################END############################################





