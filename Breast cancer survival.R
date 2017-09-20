install.packages("HSAUR3")
library("HSAUR3")

library("survival")

install.packages("coin")
library("coin")

install.packages("partykit")
library("partykit")

data("GBSG2",package="TH.data")

plot(survfit(Surv(time,cens)~horTh,data=GBSG2),lty=1:2,mark.time=FALSE,ylab="Probability", xlab="Survival Time in Days")
legend(250,0.2,legend=c("yes","no"),lty=c(2,1),title="Hormonal Therapy",bty="n")
legend(250,0.2,legend=c("yes","no"),lty=c(2,1),title="Hormonal Therapy")

GBSG2_coxph<-coxph(Surv(time,cens)~.,data=GBSG2)
summary(GBSG2_coxph)
ci<-confint(GBSG2_coxph)
exp(cbind(coef(GBSG2_coxph),ci))["horThyes",]

GBSG2_zph<-cox.zph(GBSG2_coxph)
plot(GBSG2_zph,var="age")

layout(matrix(1:3,ncol=3))
res<-residuals(GBSG2_coxph)
plot(res~age,data=GBSG2,ylim=c(-2.5,1.5),pch=".",ylab="Martingale Residuals")
abline(h=0,lty=3)

plot(res~pnodes,data=GBSG2,ylim=c(-2.5,1.5),pch=".",ylab="")
abline(h=0,lty=3)

plot(res~log(progrec),data=GBSG2,ylim=c(-2.5,1.5),pch=".",ylab="")
abline(h=0,lty=3)

GBSG2_ctree<-ctree(Surv(time,cens)~.,data=GBSG2)
plot(GBSG2_ctree)


