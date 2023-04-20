library(nnet)
library(car)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # If using RStudio, sets script directory to WD
data <- read.csv('Pshe_Scent_Choice_Data.csv',header=T)

data$mated <- as.factor(data$mated)
data$id <- as.factor(data$id)

mn0 <- multinom(cbind(water,grub,male)~1,data=data,Hess=T)
mn1 <- multinom(cbind(water,grub,male)~treat,data=data,Hess=T)
mn1a <- multinom(cbind(water,grub,male)~receptivity,data=data,Hess=T)
mn2 <- multinom(cbind(water,grub,male)~treat+receptivity,data=data,Hess=T)
mn3 <- multinom(cbind(water,grub,male)~treat*receptivity,data=data,Hess=T)

anova(mn0,mn1)
anova(mn1,mn2)
anova(mn1a,mn2)
anova(mn2,mn3)

summary(mn3)

exp(coef(mn1))
exp(coef(mn2))
exp(coef(mn3))

est <- fitted(mn3)[c(31,21,11,1),]
row.names(est) <- c('Nonmated/Saline','Nonmated/WE','Mated/Saline','Mated/WE')
est


graphics.off()

x.bar <- c(0.25,0.33,0.5,1)
saline.bar <- data.frame(x=x.bar,grubodds=-1.793655+2.785397*x.bar,maleodds=-2.938397+4.047887*x.bar)
saline.bar <- data.frame(saline.bar,water=1/(1+exp(saline.bar$grubodds)+exp(saline.bar$maleodds)),
                         grub=exp(saline.bar$grubodds)/(1+exp(saline.bar$grubodds)+exp(saline.bar$maleodds)),
                         male=exp(saline.bar$maleodds)/(1+exp(saline.bar$grubodds)+exp(saline.bar$maleodds)))
saline.bar <- data.frame(saline.bar,notgrub=1-saline.bar$grub)

we.bar <- data.frame(x=x.bar,grubodds=-1.793655+0.6625901+(2.785397-1.190685)*x.bar,maleodds=-2.938397+2.7583301+(4.047887-2.647976)*x.bar)
we.bar <- data.frame(we.bar,water=1/(1+exp(we.bar$grubodds)+exp(we.bar$maleodds)),
                     grub=exp(we.bar$grubodds)/(1+exp(we.bar$grubodds)+exp(we.bar$maleodds)),
                     male=exp(we.bar$maleodds)/(1+exp(we.bar$grubodds)+exp(we.bar$maleodds)))
we.bar <- data.frame(we.bar,notgrub=1-we.bar$grub)

saline.tbar <- t(saline.bar)[c(6,4,5),]
we.tbar <- t(we.bar)[c(6,4,5),]
combined.tbar <- data.frame(x=1:3)
matrix.tbar <- c(1,2,3)
for(x in 1:4)	{
  matrix.tbar <- cbind(matrix.tbar,saline.tbar[,x],we.tbar[,x])
}
matrix.tbar <- matrix.tbar[,2:9]



graphics.off()
pdf('Time_by_Receptivity-Treatment_Barplot.pdf',width=6.5,height=6,pointsize=14)

par(mgp=c(1.7,0.6,0),mar=c(2.5,3,1,0.5))
bp <- barplot(matrix.tbar,col=gray(c(0.3,0.85,0.55)),space=c(0.05,0.05,0.4,0.05,0.4,0.05,0.4,0.05),ylim=c(0.0,1),ylab='Relative time on substrate (%)')
box()
par(mgp=c(3,0.6,0))
axis(1,at=bp,labels=c('S','P','S','P','S','P','S','P'),tick=T,lwd=1)
par(mgp=c(3,1.6,0))
axis(1,at=c(mean(bp[1:2]),mean(bp[3:4]),mean(bp[5:6]),mean(bp[7:8])),labels=c(0.25,0.33,0.5,1.0),tick=F,lwd=0)

graphics.off()




