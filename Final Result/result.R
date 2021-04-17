
## Make the figure 1 in the introduction section

par(mgp=c(2,0.5,0.5))
plot.new()
plot.window(xlim = c(0,10),ylim=c(0,0.5))
axis(1,at=c(0,2,8.1,10),labels = c("t-l","t","T-l+1","T"),padj=0,tcl=0.5,lwd = 2,cex.axis=1.5)
abline(v=8.1,lty=2,lwd=2)
arrows(10,0.1,0,0.1,length = 0.2,angle = 20,lwd = 2)
text(4,0.12,"1-Back-calculation",cex = 1.4)
arrows(8.1,0.15,10,0.15,length = 0.2,angle = 20,lwd = 2)
text(9,0.17,"2-Nowcasting",cex=1.4)

## Save tiff file
tiff("fig1.tiff", units="in", width=7.5, height=4, res=300)
par(mgp=c(2,0.5,0.5))
plot.new()
plot.window(xlim = c(0,10),ylim=c(0,0.5))
axis(1,at=c(0,2,8.1,10),labels = c("t-l","t","T-l+1","T"),padj=0,tcl=0.5,lwd = 1,cex.axis=1)
abline(v=8.1,lty=2,lwd=1)
arrows(10,0.1,0,0.1,length = 0.1,angle = 20,lwd = 1)
text(4,0.12,"1-Back-calculation",cex = 0.85)
arrows(8.1,0.15,10,0.15,length = 0.1,angle = 20,lwd = 1)
text(9,0.17,"2-Nowcasting",cex=0.85)
dev.off()

## Extract the geweke's diagnostics
g <- readRDS('geweke.rds')
g <- rbind(g$g1,g$g2,g$g3,g$g4,g$g5,g$g6)
apply(g,2,mean) ## On average, 92% of the daily count and reproduction number estimates passed the geweke's test for convergence


##############################################
## Generate supplementary figure 1, 2 and 3 ##
##############################################

## Read the processed output
outc <- readRDS('count.rds')
outr <- readRDS('rest.rds')
library(tidyverse)
library(ggpubr)

outc <- outc%>%filter(date>20)%>%mutate(date=date-20)
## Assuming the first day is 2020-02-01 and the last day is 2020-03-31
outc <- outc%>%mutate(date=as.Date('2020-01-31')+date)
outr <- outr%>%filter(date>20)%>%mutate(date=date-20)
outr <- outr%>%mutate(date=as.Date('2020-01-31')+date)

outc[outc$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Two Delay Dist. & Correct Maximum Delay",]$scenario="Sharp Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Mutiple Delay Dist. & Correct Maximum Delay",]$scenario="Gradual Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Single Delay Dist. & Incorrect Maximum Delay",]$scenario="No Delay Improvement & Incorrect Maximum Delay"
outc[outc$scenario=="Two Delay Dist. & Incorrect Maximum Delay",]$scenario="Sharp Delay Improvement & Incorrect Maximum Delay"
outc[outc$scenario=="Mutiple Delay Dist. & Incorrect Maximum Delay",]$scenario="Gradual Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Two Delay Dist. & Correct Maximum Delay",]$scenario="Sharp Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Mutiple Delay Dist. & Correct Maximum Delay",]$scenario="Gradual Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Single Delay Dist. & Incorrect Maximum Delay",]$scenario="No Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Two Delay Dist. & Incorrect Maximum Delay",]$scenario="Sharp Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Mutiple Delay Dist. & Incorrect Maximum Delay",]$scenario="Gradual Delay Improvement & Incorrect Maximum Delay"


out1 <- outc%>%filter(sn==1|sn==2)

pc1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") + geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
  legend.text = element_text(size=11)) +labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, ncol = 2)


out2 <- outc%>%filter(sn==3|sn==4)

pc2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity")+geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, ncol = 2)


out3 <- outc%>%filter(sn==5|sn==6)

pc3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity")+geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Daily Counts")+facet_wrap(~scenario, ncol = 2)


out1 <- outr%>%filter(sn==1|sn==2)

pr1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") + geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Reproduction Number")+facet_wrap(~scenario, ncol = 2)


out2 <- outr%>%filter(sn==3|sn==4)

pr2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Reproduction Number")+facet_wrap(~scenario, ncol = 2)


out3 <- outr%>%filter(sn==5|sn==6)

pr3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),strip.text.x = element_text(size=12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Reprodution Number")+facet_wrap(~scenario, ncol = 2)

ggarrange(pc1,pr1,nrow = 2,legend = "right",align = "hv",common.legend = TRUE)
ggarrange(pc2,pr2,nrow = 2,legend = "right",align = "hv",common.legend = TRUE)
ggarrange(pc3,pr3,nrow = 2,legend = "right",align = "hv",common.legend = TRUE)

## Main text figure 2

out1 <- outc%>%filter(sn==1)

pc1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity")+geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out2 <- outc%>%filter(sn==3)

pc2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") + geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")


out3 <- outc%>%filter(sn==5)

pc3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity")+geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")


out1 <- outr%>%filter(sn==1)

pr1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = expression('R'['t']))


out2 <- outr%>%filter(sn==3)

pr2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = expression('R'['t']))


out3 <- outr%>%filter(sn==5)

pr3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=model),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = expression('R'['t']))

ggarrange(pc1,pr1,pc2,pr2,pc3,pr3,labels = c("A","B","C","D","E","F"),font.label = list(size=10),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)

## Save tiff file
tiff("fig2.tiff", units="in", width=7.3, height=5.3, res=300)
ggarrange(pc1,pr1,pc2,pr2,pc3,pr3,labels = c("A","B","C","D","E","F"),font.label = list(size=10),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)
dev.off()


################################# 
## Generate main text figure 3 ##
#################################

library(tidyverse)

## Read the processed output
outc <- readRDS('count_ss.rds')
outr <- readRDS('rest_ss.rds')
outc <- outc%>%filter(date>20)%>%mutate(date=date-20)
## Assuming the first day is 2020-02-01 and the last day is 2020-03-31
outc <- outc%>%mutate(date=as.Date('2020-01-31')+date)
outr <- outr%>%filter(date>20)%>%mutate(date=date-20)
outr <- outr%>%mutate(date=as.Date('2020-01-31')+date)

outc[outc$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Two Delay Dist. & Correct Maximum Delay",]$scenario="Sharp Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Mutiple Delay Dist. & Correct Maximum Delay",]$scenario="Gradual Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Single Delay Dist. & Incorrect Maximum Delay",]$scenario="No Delay Improvement & Incorrect Maximum Delay"
outc[outc$scenario=="Two Delay Dist. & Incorrect Maximum Delay",]$scenario="Sharp Delay Improvement & Incorrect Maximum Delay"
outc[outc$scenario=="Mutiple Delay Dist. & Incorrect Maximum Delay",]$scenario="Gradual Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Two Delay Dist. & Correct Maximum Delay",]$scenario="Sharp Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Mutiple Delay Dist. & Correct Maximum Delay",]$scenario="Gradual Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Single Delay Dist. & Incorrect Maximum Delay",]$scenario="No Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Two Delay Dist. & Incorrect Maximum Delay",]$scenario="Sharp Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Mutiple Delay Dist. & Incorrect Maximum Delay",]$scenario="Gradual Delay Improvement & Incorrect Maximum Delay"

outc[outc$report=="From Day 31",]$report="From Feb 11"
outc[outc$report=="From Day 41",]$report="From Feb 21"
outc[outc$report=="From Day 51",]$report="From Mar 02"
outc[outc$report=="From Day 61",]$report="From Mar 12"
outr[outr$report=="From Day 31",]$report="From Feb 11"
outr[outr$report=="From Day 41",]$report="From Feb 21"
outr[outr$report=="From Day 51",]$report="From Mar 02"
outr[outr$report=="From Day 61",]$report="From Mar 12"


## Main text figure 3

out1 <- outc%>%filter(sn==1)

pc1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out1 <- outr%>%filter(sn==1)

pr1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = expression('R'['t']))

out2 <- outc%>%filter(sn==3)

pc2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out2 <- outr%>%filter(sn==3)

pr2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date",y = expression('R'['t']))

out3 <- outc%>%filter(sn==5)

pc3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out3 <- outr%>%filter(sn==5)

pr3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date",y = expression('R'['t']))

ggarrange(pc1,pr1,pc2,pr2,pc3,pr3,labels = c("A","B","C","D","E","F"),font.label = list(size=10),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)

## Save tiff file
tiff("fig3.tiff", units="in", width=7.3, height=5.3, res=300)
ggarrange(pc1,pr1,pc2,pr2,pc3,pr3,labels = c("A","B","C","D","E","F"),font.label = list(size=10),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)
dev.off()


################################# 
## Generate main text figure 4 ##
#################################

library(tidyverse)

## Read the processed output
outc <- readRDS('count_ss1.rds')
outr <- readRDS('rest_ss1.rds')
outc <- outc%>%filter(date>20)%>%mutate(date=date-20)
## Assuming the first day is 2020-02-01 and the last day is 2020-03-31
outc <- outc%>%mutate(date=as.Date('2020-01-31')+date)
outr <- outr%>%filter(date>20)%>%mutate(date=date-20)
outr <- outr%>%mutate(date=as.Date('2020-01-31')+date)

outc[outc$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Two Delay Dist. & Correct Maximum Delay",]$scenario="Sharp Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Mutiple Delay Dist. & Correct Maximum Delay",]$scenario="Gradual Delay Improvement & Correct Maximum Delay"
outc[outc$scenario=="Single Delay Dist. & Incorrect Maximum Delay",]$scenario="No Delay Improvement & Incorrect Maximum Delay"
outc[outc$scenario=="Two Delay Dist. & Incorrect Maximum Delay",]$scenario="Sharp Delay Improvement & Incorrect Maximum Delay"
outc[outc$scenario=="Mutiple Delay Dist. & Incorrect Maximum Delay",]$scenario="Gradual Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Single Delay Dist. & Correct Maximum Delay",]$scenario="No Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Two Delay Dist. & Correct Maximum Delay",]$scenario="Sharp Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Mutiple Delay Dist. & Correct Maximum Delay",]$scenario="Gradual Delay Improvement & Correct Maximum Delay"
outr[outr$scenario=="Single Delay Dist. & Incorrect Maximum Delay",]$scenario="No Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Two Delay Dist. & Incorrect Maximum Delay",]$scenario="Sharp Delay Improvement & Incorrect Maximum Delay"
outr[outr$scenario=="Mutiple Delay Dist. & Incorrect Maximum Delay",]$scenario="Gradual Delay Improvement & Incorrect Maximum Delay"

outc[outc$report=="Until Day 48",]$report="Until Feb 28"
outc[outc$report=="Until Day 58",]$report="Until Mar 09"
outr[outr$report=="Until Day 48",]$report="Until Feb 28"
outr[outr$report=="Untile Day 58",]$report="Until Mar 09"

## Main text figure 4

out1 <- outc%>%filter(sn==1)

pc1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+  
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out1 <- outr%>%filter(sn==1)

pr1 <- out1%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date",y = expression('R'['t']) )

out2 <- outc%>%filter(sn==3)

pc2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+  
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out2 <- outr%>%filter(sn==3)

pr2 <- out2%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date",y = expression('R'['t']))

out3 <- outc%>%filter(sn==5)

pc3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repc),size=1.2,linetype="dashed")+  
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epic),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = "Daily Counts")

out3 <- outr%>%filter(sn==5)

pr3 <- out3%>%ggplot()+geom_line(aes(x=date,y=repr),size=1.2,linetype="dashed")+
  geom_smooth(aes(x=date,y=median,ymax=upper,ymin=lower,color=report),size=1.2,stat = "identity") +geom_line(aes(x=date,y=epir),size=1.2)+
  theme_bw()+theme(axis.text = element_text(size = 10),legend.title = element_text(size=10),
                   legend.text = element_text(size=10)) + scale_x_date(date_labels = "%m/%d") + labs(x = "Date", y = expression('R'['t']))

ggarrange(pc1,pr1,pc2,pr2,pc3,pr3,labels = c("A","B","C","D","E","F"),font.label = list(size=10),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)

## Save tiff file
tiff("fig4.tiff", units="in", width=7.3, height=5.3, res=300)
ggarrange(pc1,pr1,pc2,pr2,pc3,pr3,labels = c("A","B","C","D","E","F"),font.label = list(size=10),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)
dev.off()

## Extract coverage rates and RMSE for the counts and reproductive number estimates
## Table 1, 2 and 3 in the main text
eva1 <- readRDS('eva1.rds')
eva2 <- readRDS('eva2.rds')
eva3 <- readRDS('eva3.rds')
library(tidyverse)
## Complete data
c1 <- eva1$c
c1 <- c1%>%group_by(model,scenario,type)%>%summarise(m=round(mean(result),digits = 2))
r1 <- eva1$r
r1 <- r1%>%group_by(model,scenario,type)%>%summarise(m=round(mean(result),digits = 2))
## Data without heads
c2 <- eva2$c
c2 <- c2%>%group_by(data,scenario,type)%>%summarise(m=round(mean(result,na.rm = TRUE),digits = 2))
r2 <- eva2$r
r2 <- r2%>%group_by(data,scenario,type)%>%summarise(m=round(mean(result,na.rm = TRUE),digits = 2))
## Different for chopping off the tails
c3 <- eva3$c
c31 <- c3%>%filter(date<=as.Date("2020-02-28"),data=="Until Feb 28") ## Only take available days, not going further 
c31 <- c31%>%group_by(scenario,type)%>%summarise(m=round(mean(result,na.rm = TRUE),digits = 2))
c32 <- c3%>%filter(date<=as.Date("2020-03-09"),data=="Until Mar 09")
c32 <- c32%>%group_by(scenario,type)%>%summarise(m=round(mean(result,na.rm = TRUE),digits = 2))
r3 <- eva3$r
r31 <- r3%>%filter(date<=as.Date("2020-02-28"),data==1) ## Only take available days, not going further 
r31 <- r31%>%group_by(scenario,type)%>%summarise(m=round(mean(result,na.rm = TRUE),digits = 2))
r32 <- r3%>%filter(date<=as.Date("2020-03-09"),data==2)
r32 <- r32%>%group_by(scenario,type)%>%summarise(m=round(mean(result,na.rm = TRUE),digits = 2))


##############################################################
## Create supplementary figures for coverage rates and RMSE ##
##############################################################

eva1 <- readRDS('eva1.rds')
eva2 <- readRDS('eva2.rds')
eva3 <- readRDS('eva3.rds')

c1 <- eva1$c
c2 <- eva2$c
c3 <- eva3$c

c1[c1$model==1,]$model="1 Dispersion"
c1[c1$model==2,]$model="2 Dispersions"

## Supplementary figure 4: Coverage rate for complete data

out1 <- c1%>%filter(scenario==1&type=="coverage")

pc1 <- out1%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out2 <- c1%>%filter(scenario==2&type=="coverage")

pc2 <- out2%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out3 <- c1%>%filter(scenario==3&type=="coverage")

pc3 <- out3%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + ylim(0.2,1)+labs(x = "Date", y = "Coverage Rate")

out4 <- c1%>%filter(scenario==4&type=="coverage")

pc4 <- out4%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +ylim(0.2,1)+labs(x = "Date", y = "Coverage Rate")

out5 <- c1%>%filter(scenario==5&type=="coverage")

pc5 <- out5%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out6 <- c1%>%filter(scenario==6&type=="coverage")

pc6 <- out6%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

ggarrange(pc1,pc2,pc3,pc4,pc5,pc6,labels = c("A","B","C","D","E","F"),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)


## Supplementary figure 5: RMSE for complete data

out1 <- c1%>%filter(scenario==1&type=="RMSE")

pc1 <- out1%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + ylim(0,20)+labs(x = "Date", y = "RMSE")

out2 <- c1%>%filter(scenario==2&type=="RMSE")

pc2 <- out2%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + ylim(0,20)+labs(x = "Date", y = "RMSE")

out3 <- c1%>%filter(scenario==3&type=="RMSE")

pc3 <- out3%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +ylim(0,25)+labs(x = "Date", y = "RMSE")

out4 <- c1%>%filter(scenario==4&type=="RMSE")

pc4 <- out4%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11))+ylim(0,25) +labs(x = "Date", y = "RMSE")

out5 <- c1%>%filter(scenario==5&type=="RMSE")

pc5 <- out5%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11))+ylim(0,20)+ labs(x = "Date", y = "RMSE")

out6 <- c1%>%filter(scenario==6&type=="RMSE")

pc6 <- out6%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=model),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11))+ylim(0,20)+ labs(x = "Date", y = "RMSE")

ggarrange(pc1,pc2,pc3,pc4,pc5,pc6,labels = c("A","B","C","D","E","F"),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)


## Figures for data without heads

c2[c2$data==1,]$data="From Feb 11"
c2[c2$data==2,]$data="From Feb 21"
c2[c2$data==3,]$data="From Mar 02"
c2[c2$data==4,]$data="From Mar 12"

## Supplementary figure 6: Coverage rate for data without heads

out1 <- c2%>%filter(scenario==1&type=="coverage")

pc1 <- out1%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out2 <- c2%>%filter(scenario==2&type=="coverage")

pc2 <- out2%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out3 <- c2%>%filter(scenario==3&type=="coverage")

pc3 <- out3%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "Coverage Rate")

out4 <- c2%>%filter(scenario==4&type=="coverage")

pc4 <- out4%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "Coverage Rate")

out5 <- c2%>%filter(scenario==5&type=="coverage")

pc5 <- out5%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out6 <- c2%>%filter(scenario==6&type=="coverage")

pc6 <- out6%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

ggarrange(pc1,pc2,pc3,pc4,pc5,pc6,labels = c("A","B","C","D","E","F"),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)


## Supplementary figure 7: RMSE for data without heads

out1 <- c2%>%filter(scenario==1&type=="RMSE")

pc1 <- out1%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "RMSE")

out2 <- c2%>%filter(scenario==2&type=="RMSE")

pc2 <- out2%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "RMSE")

out3 <- c2%>%filter(scenario==3&type=="RMSE")

pc3 <- out3%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "RMSE")

out4 <- c2%>%filter(scenario==4&type=="RMSE")

pc4 <- out4%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "RMSE")

out5 <- c2%>%filter(scenario==5&type=="RMSE")

pc5 <- out5%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "RMSE")

out6 <- c2%>%filter(scenario==6&type=="RMSE")

pc6 <- out6%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-03-11"),xmax=as.Date("2020-03-31"),ymin = -Inf, ymax=Inf, alpha = 0.2)+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "RMSE")

ggarrange(pc1,pc2,pc3,pc4,pc5,pc6,labels = c("A","B","C","D","E","F"),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)


## Figures for data without tails

## Supplementary figure 8: Coverage rate for data without tails

out1 <- c3%>%filter(scenario==1&type=="coverage")

pc1 <- out1%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out2 <- c3%>%filter(scenario==2&type=="coverage")

pc2 <- out2%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out3 <- c3%>%filter(scenario==3&type=="coverage")

pc3 <- out3%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "Coverage Rate")

out4 <- c3%>%filter(scenario==4&type=="coverage")

pc4 <- out4%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +labs(x = "Date", y = "Coverage Rate")

out5 <- c3%>%filter(scenario==5&type=="coverage")

pc5 <- out5%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

out6 <- c3%>%filter(scenario==6&type=="coverage")

pc6 <- out6%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + labs(x = "Date", y = "Coverage Rate")

ggarrange(pc1,pc2,pc3,pc4,pc5,pc6,labels = c("A","B","C","D","E","F"),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)


## Supplementary figure 9: RMSE for data without tails

out1 <- c3%>%filter(scenario==1&type=="RMSE")

pc1 <- out1%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +ylim(0,135)+labs(x = "Date", y = "RMSE")

out2 <- c3%>%filter(scenario==2&type=="RMSE")

pc2 <- out2%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +ylim(0,135)+labs(x = "Date", y = "RMSE")

out3 <- c3%>%filter(scenario==3&type=="RMSE")

pc3 <- out3%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +ylim(0,135)+labs(x = "Date", y = "RMSE")

out4 <- c3%>%filter(scenario==4&type=="RMSE")

pc4 <- out4%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) +ylim(0,135)+labs(x = "Date", y = "RMSE")

out5 <- c3%>%filter(scenario==5&type=="RMSE")

pc5 <- out5%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + ylim(0,135)+labs(x = "Date", y = "RMSE")

out6 <- c3%>%filter(scenario==6&type=="RMSE")

pc6 <- out6%>%ggplot(aes(x=date,y=result))+geom_line(aes(color=data),size=1.2)+
  annotate("rect",xmin = as.Date("2020-02-09"),xmax=as.Date("2020-02-28"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#F8766D")+  
  annotate("rect",xmin = as.Date("2020-02-19"),xmax=as.Date("2020-03-09"),ymin = -Inf, ymax=Inf, alpha = 0.2,colour="#00BFC4")+
  theme_bw()+theme(axis.text = element_text(size = 12),legend.title = element_text(size=12),
                   legend.text = element_text(size=11)) + ylim(0,135)+labs(x = "Date", y = "RMSE")

ggarrange(pc1,pc2,pc3,pc4,pc5,pc6,labels = c("A","B","C","D","E","F"),nrow = 3,ncol = 2,legend = "right",align = "h",common.legend = TRUE)

