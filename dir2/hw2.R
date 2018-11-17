rm(list=ls())

library(ggplot2)
a = 1
b = 1
n = 10
y = 10
?qbeta
quantiles = data.frame(q = qbeta(c(0.025, 0.975), a + y, b + n - y));quantiles
quantiles = data.frame(q = qbeta(c(0.05), a + y, b + n - y));quantiles

#q1<-data.frame(c(0.025, 0.975));q1
 
df = data.frame(
  theta = seq(0, 1, by = 0.001),
  p = dbeta(seq(0, 1, by = 0.001), a + y, b + n - y)) 
ggplot(df, aes(x = theta, y = p)) +
  geom_line() +  geom_vline(data = quantiles, mapping = aes(xintercept = q))

####HPD

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
theta.support<-seq(0,1,length=5000)
plot(theta.support, dbeta(theta.support, a+y, b+n-y), type="l",
     xlab=expression(theta),ylab=expression(paste(italic("p("),theta,"|y)"))) 
pth<-dbeta(theta.support, a+y, b+n-y)
ord<- order(-pth)
xpx<-cbind(theta.support[ord], pth[ord])
xpx<-cbind(xpx,cumsum(xpx[,2])/sum(xpx[,2]))

hpd<-function(x,dx,p){
  md<-x[dx==max(dx)]
  px<-dx/sum(dx)
  pxs<--sort(-px)
  ct<-min(pxs[cumsum(pxs)< p])
  list(hpdr=range(x[px>=ct]),mode=md) }

tmp<-hpd(xpx[,1],xpx[,2],.95)$hpdr;tmp


######
x<-seq(-30,30,0.01)
y<-exp(x)/(1+exp(x))^2
plot(x,y,type="l")
 
###########
###HPD check

theta.support<-seq(0,1,length=15)
plot(theta.support, dbeta(theta.support, a+y, b+n-y), type="l",
     xlab=expression(theta),ylab=expression(paste(italic("p("),theta,"|y)"))) 
pth<-dbeta(theta.support, a+y, b+n-y)
plot(theta.support,pth)
ord<- order(-pth)
#order(-theta.support)
##this is to upside the x and y coord of the curve
xpx<-cbind(theta.support[ord], pth[ord])
xpx<-cbind(xpx,cumsum(xpx[,2])/sum(xpx[,2])) ###F(X) compute gareko
#####
x<-xpx[,1]
dx<-xpx[,2]
p<-0.95 


hpd<-function(x,dx,p){
  md<-x[dx==max(dx)]
  #which.max(x)
  px<-dx/sum(dx);sum(px);sum(dx);px
  pxs<--sort(-px);head(pxs)  
  ct<-min(pxs[cumsum(pxs)< p]);ct
  list(hpdr=range(x[px>=ct]),mode=md) }
 




###########plot
tmp<-hpd(xpx[,1],xpx[,2],.5)$hpdr
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(.75),lwd=2   )
tmp<-hpd(xpx[,1],xpx[,2],.75)$hpdr
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(.5),lwd=2   )
tmp<-hpd(xpx[,1],xpx[,2],.95)$hpdr;tmp
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(0),lwd=2   )

tmp<-qbeta( c(.025,.975), a+y,b+n-y)
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(0),lwd=2 ,lty=2  )


legend(.5, 2.75, c("50% HPD","75% HPD","95% HPD","95% quantile-based"), 
       col=c(gray(.75),gray(.5),
             gray(0),gray(0)),lty=c(1,1,1,2),lwd=c(2,2,2,2),
       bty="n")








######
?ggplot
#pdf("fig3_2.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

n<-10
theta<-.2
plot(0:n,dbinom(0:n,n,theta), type="h",lwd=2,xlab=expression(italic(y)),
    ylab=expression(paste("Pr(",italic("Y=y"),"|",theta==.2,italic(", n="),"10)",sep="")))
#MTEXT(EXpression(
#   italic(paste("n=",10,", ",theta==0.2))),side=3,cex=.8)
n<-10
theta<-.8
plot(0:n,dbinom(0:n,n,theta), type="h",lwd=2,xlab=expression(italic(y)),
    ylab=expression(paste("Pr(",italic("Y=y"),"|",theta==.8,italic(", n="),"10)",sep="")))
#mtext(expression(
#   italic(paste("n=",10,", ",theta==0.8))),side=3,cex=.8)
#dev.off()
######

######
#pdf("fig3_3.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

n<-100
theta<-.2
plot(0:n,dbinom(0:n,n,theta), type="h",lwd=2,xlab=expression(italic(y)),
    ylab=expression(paste("Pr(",italic("Y=y"),"|",theta==.2,italic(", n="),"100)",sep="")))

n<-100
theta<-.8
plot(0:n,dbinom(0:n,n,theta), type="h",lwd=2,xlab=expression(italic(y)),
    ylab=expression(paste("Pr(",italic("Y=y"),"|",theta==.8,italic(", n="),"100)",sep="")))
#dev.off()
######

######
#pdf("fig3_4.pdf",family="Times",height=4.4,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0),oma=c(0,0,.5,0))
par(mfrow=c(2,2))
theta<-seq(0,1,length=100)
a<-1; b<-1
n<-10 ; y<-10
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=
 expression(paste(italic("p("),theta,"|y)",sep="")), xlab=expression(theta), 
 lwd=2)
mtext(expression(paste("beta(1,1) prior,  ", italic("n"),"=5  ",italic(sum(y[i])),"=1",sep="")), side=3,line=.1)
#abline(v=c((a+y-1)/(a+b+n-2),(a+y)/(a+b+n)),col=c("black","gray"),lty=c(2,2))
lines(theta,dbeta(theta,a,b),type="l",col="gray",lwd=2)
legend(.45,2.4,legend=c("prior","posterior"),lwd=c(2,2),col=c("gray","black"), bty="n")

a<-3; b<-2
n<-5 ; y<-1
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=
 expression(paste(italic("p("),theta,"|y)",sep="")), xlab=expression(theta), 
 lwd=2)
#   expression(italic(paste("p(",theta,"|y)",sep=""))), xlab=expression(theta),lwd=2)
mtext(expression(paste("beta(3,2) prior,  ", italic("n"),"=5  ",italic(sum(y[i])),"=1",sep="")), side=3,line=.1)
#abline(v=c((a+y-1)/(a+b+n-2), (a+y)/(a+b+n)) , col=c("green","red") )
lines(theta,dbeta(theta,a,b),type="l",col="gray",lwd=2)
legend(.45,2.4,legend=c("prior","posterior"),lwd=c(2,2),col=c("gray","black"), bty="n")


a<-1 ; b<-1
n<-100; y<-20
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=
 expression(paste(italic("p("),theta,"|y)",sep="")), xlab=expression(theta), 
 lwd=2)
#    expression(italic(paste("p(",theta,"|y)",sep=""))),    xlab=expression(theta),lwd=2)
mtext(expression(paste("beta(1,1) prior,  ", italic("n"),"=100  ",italic(sum(y[i])),"=20",sep="")), side=3,line=.1)
#abline(v=c((a+y-1)/(a+b+n-2), (a+y)/(a+b+n)) , col=c("green","red") )
lines(theta,dbeta(theta,a,b),type="l",col="gray",lwd=2)

a<-3 ; b<-2
n<-100; y<-20
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=
 expression(paste(italic("p("),theta,"|y)",sep="")), xlab=expression(theta), 
 lwd=2)
#    expression(italic(paste("p(",theta,"|y)",sep=""))),xlab=expression(theta),
#    lwd=2)
mtext(expression(paste("beta(3,2) prior,  ", italic("n"),"=100  ",italic(sum(y[i])),"=20",sep="")), side=3,line=.1)
#abline(v=c((a+y-1)/(a+b+n-2), (a+y)/(a+b+n)) , col=c("green","red") )
lines(theta,dbeta(theta,a,b),type="l",col="gray",lwd=2)

#dev.off()
######








######
#pdf("fig3_5.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

a<-1  ; b<-1   #prior
n<-10 ; y<-10   #data
theta.support<-seq(0,1,length=100)
plot(theta.support, dbeta(theta.support, a+y, b+n-y), type="l",
   xlab=expression(theta),ylab=expression(paste(italic("p("),theta,"|y)"))) 
abline(v=qbeta( c(.025,.975), a+y,b+n-y))
#dev.off()
######

######
#pdf("fig3_6.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

theta.support<-seq(0,1,length=5000)
plot(theta.support, dbeta(theta.support, a+y, b+n-y), type="l",
     xlab=expression(theta),ylab=expression(paste(italic("p("),theta,"|y)"))) 
xpth<-dbeta(theta.support, a+y, b+n-y)
ord<- order(-pth)
xpx<-cbind(theta.support[ord], pth[ord])
xpx<-cbind(xpx,cumsum(xpx[,2])/sum(xpx[,2]))

hpd<-function(x,dx,p){
md<-x[dx==max(dx)]
px<-dx/sum(dx)
pxs<--sort(-px)
ct<-min(pxs[cumsum(pxs)< p])
list(hpdr=range(x[px>=ct]),mode=md) }

tmp<-hpd(xpx[,1],xpx[,2],.75)$hpdr


tmp<-hpd(xpx[,1],xpx[,2],.5)$hpdr
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(.75),lwd=2   )
tmp<-hpd(xpx[,1],xpx[,2],.75)$hpdr
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(.5),lwd=2   )
tmp<-hpd(xpx[,1],xpx[,2],.95)$hpdr
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(0),lwd=2   )

tmp<-qbeta( c(.025,.975), a+y,b+n-y)
lines( x=c(tmp[1],tmp[1],tmp[2],tmp[2]),
       y=dbeta(c(0,tmp[1],tmp[2],0),a+y,b+n-y)  ,col=gray(0),lwd=2 ,lty=2  )


legend(.5, 2.75, c("50% HPD","75% HPD","95% HPD","95% quantile-based"), 
     col=c(gray(.75),gray(.5),
    gray(0),gray(0)),lty=c(1,1,1,2),lwd=c(2,2,2,2),
        bty="n")

#dev.off()
######

######
#pdf("fig3_7.pdf",family="Times",height=3.5,width=7)
#source("/Users/hoff/Work/Datasets/GSS/alldata.r")

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

#CHILDS<-Y$CHILDS[Y$FEMALE==1 &  Y$YEAR==1998 & Y$AGE>=40]
#CHILDS<-Y$CHILDS[Y$FEMALE==1 &  Y$YEAR==1996 & Y$AGE==40 & !is.na(Y$DEG)]
CHILDS<-Y$CHILDS[Y$FEMALE==1&Y$YEAR==1998 & Y$AGE>=40 & Y$AGE<50 & !is.na(Y$DEG)]
CHILDS<-Y$CHILDS[Y$FEMALE==1 & Y$YEAR>=1990 & Y$AGE==40 & !is.na(Y$DEG)]
CHILDS<-CHILDS[!is.na(CHILDS)]
ecdf<-(table(c(CHILDS,0:8))-1 )/sum(table(CHILDS))
plot(0:8+.1,ecdf,type="h",lwd=5,xlab="number of children", 
     ylab=expression(paste("Pr(",italic(Y[i]==y[i]),")",sep="")),col="gray")
points(0:8-.1, dpois(0:8,mean(CHILDS,na.rm=T)),lwd=5,col="black",type="h")

legend(2.25,.31,
     legend=c("Poisson model","empirical distribution"),lwd=c(2,2),col=
       c("black","gray"),bty="n",cex=.75)

#ys<-NULL
#for(ns in 1:10000) { ys<-c(ys,sum(sample(CHILDS,10,replace=T),na.rm=T)) }
#plot(0:52-.1, (table(c(ys,0:52))-1)/10000,type="h",lwd=3 )
plot(0:52,dpois(0:52,10*mean(CHILDS)),lwd=3,col="black",type="h",
     xlab="number of children", 
     ylab=expression(paste("Pr(",italic(sum(Y[i])==y),"|",theta==1.83,")",sep="")))

#dev.off()




###### 
y2<-Y$CHILDS[Y$FEMALE==1 &  Y$YEAR>=1990  & Y$AGE==40 & Y$DEG>=3 ]
y1<-Y$CHILDS[Y$FEMALE==1 &  Y$YEAR>=1990  & Y$AGE==40 & Y$DEG<3 ]



y2<-y2[!is.na(y2)]
y1<-y1[!is.na(y1)]

#pdf("fig3_9.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))

set.seed(1) 
n1<-length(y1) ; n2<-length(y2)
s1<-sum(y1)
s2<-sum(y2)

par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(table(y1), type="h",xlab=expression(italic(y)),ylab=expression(italic(n[1](y))),col=gray(.5) ,lwd=3)
mtext("Less than bachelor's",side=3)
plot(table(y2), type="h",xlab=expression(italic(y)),ylab=expression(italic(n[2](y))),col=gray(0),lwd=3)
mtext("Bachelor's or higher",side=3,lwd=3)
#dev.off()
######

######
#pdf("fig3_10.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
a<-2
b<-1
xtheta<-seq(0,5,length=1000)
plot(xtheta,dgamma(xtheta,a+s1,b+n1),type="l",col=gray(.5),xlab=expression(theta),
  ylab=expression(paste(italic("p("),theta,"|",y[1],"...",y[n],")",sep="")))
lines(xtheta,dgamma(xtheta,a+s2,b+n2),col=gray(0),lwd=2)
lines(xtheta,dgamma(xtheta,a,b),type="l",lty=2,lwd=2)
abline(h=0,col="black")
#dev.off()
#pdf("fig4_6.pdf",family="Times",height=3,width=6)
#par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
#par(mfrow=c(1,2))
y<-(0:12)
plot(y-.1, dnbinom(y, size=(a+s1), mu=(a+s1)/(b+n1)) , col=gray(.5) ,type="h",
ylab=expression(paste(italic("p("),y[n+1],"|",y[1],"...",y[n],")",sep="")), 
   xlab=expression(italic(y[n+1])),ylim=c(0,.35),lwd=3)
points(y+.1, dnbinom(y, size=(a+s2), mu=(a+s2)/(b+n2)) , col=gray(0) ,type="h",lwd=3)
legend(1,.375,legend=c("Less than bachelor's","Bachelor's or higher"),bty="n",
  lwd=c(3,3),col=c(gray(.5),gray(0)))
#dev.off()
######





a<-2 ; b<-1
n1<-length(y1) ; s1<-sum(y1)
n2<-length(y2) ; s2<-sum(y2)


a<-2 ; b<-1          # prior parameters
n1<-111 ; s1<-217    # data in group 1
n2<-44  ; s2<-66     # data in group 2


(a+s1)/(b+n1)        # posterior mean 
(a+s1-1)/(b+n1)      # posterior mode
qgamma( c(.025,.975),a+s1,b+n1)   # posterior 95% CI

(a+s2)/(b+n2)
(a+s2-1)/(b+n2)
qgamma( c(.025,.975),a+s2,b+n2)


th1_mc<-rgamma(100000,a+s1,b+n1)

th2_mc<-rgamma(100000,a+s2,b+n2)

mean(th1_mc>th2_mc)

y1_mc<-rpois(1000000,th1_mc)
y2_mc<-rpois(1000000,th2_mc)
mean(y1_mc>y2_mc)
mean(y1_mc>=y2_mc)
mean(y1_mc==y2_mc)


options(width=60)

y<- 0:10

dnbinom(y, size=(a+s1), mu=(a+s1)/(b+n1))

dnbinom(y, size=(a+s2), mu=(a+s2)/(b+n2))


#################3
a<-1 ; b<-1
n<-10; y<-10
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=
       expression(paste(italic("p("),theta,"|y)",sep="")), xlab=expression(theta), 
     lwd=2)
#    expression(italic(paste("p(",theta,"|y)",sep=""))),    xlab=expression(theta),lwd=2)
mtext(expression(paste("beta(1,1) prior,  ", italic("n"),"=100  ",italic(sum(y[i])),"=20",sep="")), side=3,line=.1)
#abline(v=c((a+y-1)/(a+b+n-2), (a+y)/(a+b+n)) , col=c("green","red") )
lines(theta,dbeta(theta,a,b),type="l",col="gray",lwd=2)

