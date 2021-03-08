Cache=read.csv("artificial cache data.csv",header=TRUE)
TotalEaten=colSums(Cache[,2:5])
NotEaten=60-TotalEaten
mytable=rbind(TotalEaten,NotEaten)
mytable=as.data.frame(mytable)
names(mytable)=names(Cache)[2:5]
library(ggplot2)
library(tidyverse)
mytablelong=pivot_longer(mytable,cols=1:4,names_to="treatment",values_to="nuts")
mytablelong$eaten=c(rep("yes",4),rep("no",4))

ggplot(mytablelong,aes(x=treatment,y=nuts,fill=eaten))+geom_bar(stat="identity")
chisq.test(mytable)


Days=read.csv("daystaken.csv",header=TRUE)
library(survival)
dayslong=pivot_longer(Days,cols=3:6,names_to="treatment",values_to="time")
dayslong$status=as.numeric(dayslong$time!=60)
survfit(Surv(time,status)~treatment,data=dayslong)
plot(survfit(Surv(time,status)~treatment,data=dayslong),col=1:4)
legend(0,0.4,c("farclump","farspace","nearclump","nearspace"),col=1:4,bty="n",lwd=1)

survdiff(Surv(time,status)~treatment,data=dayslong)
