#####
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
#####

###################################################
install.packages("Flury")
library(Flury)
data(midge)  
y<-midge[midge[,1]=="Af",3]



par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))



############################################################
#1. One parameter normal (sigma known)


mu0<-1.9
tau0<-(.5*1.9)
t02<-tau0^2
ybar<- mean(y)
s2<- var(y)
n<- length(y)
sigma<-sqrt(s2)

mun<-( mu0/(t02) + n*ybar/s2)/( 1/t02+n/s2)
t2n<-1/(1/t02 +n/s2)

qnorm(c(.025,.975),mun,sqrt(t2n))

ys<-seq(0,mu0*2,length=500)
plot(ys,dnorm(ys,mun,sqrt(t2n)),type="l",col="black",xlab=expression(theta),
    ylab=expression(paste(italic("p("),theta,"|",italic(y[1]),"...",italic(y[n]),",",
          sigma^2==0.017,")",sep="")),lwd=2)  
lines(ys,dnorm(ys,mu0,sqrt(t02)),type="l",col="gray",lwd=2)
dev.off()
#####




########################################


library(rjags)
library(runjags)

model_string <-"model
{
  for( i in 1 : N ) {
  y[i] ~ dnorm(theta,1/0.01687778)
  }
  theta ~ dnorm(1.9, 0.95)

}
"

data <- list(y=y, N =length(y))
inits<- list(theta = 1.8)

#run JAGS
norm1p.fit=run.jags(model=model_string,
                   monitor= c("theta"),
                   burnin=1000, sample=200000,
                   data=data, n.chains=1, method="rjags", inits=inits)
norm1p.fit
############################################################
#2. Two parameters normal (sigma unknown)

# prior
mu0<-1.9  ; k0<-1
s20<-0.01 ; nu0<-1

# data
y<-c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
n<-length(y) ; ybar<-mean(y) ; s2<-var(y)

# posterior inference
# theta
mu0 = 1.9
kappa0 = 1
s20 = 0.01
nu0 = 1

kappan = kappa0 + n
nun = nu0 + n
mun = (kappa0 * mu0 + n * ybar) / kappan
s2n = (1 / nun) * (nu0 * s20 + (n - 1) * s2 + (kappa0 * n / kappan) * (ybar - mu0)^2)

#######################
Theta = seq(1.6, 2.0, by = 0.005)
Sigma2 = seq(0, 0.04, by = 0.0001)

library(invgamma)
post.func = function(theta, sigma2) {
  dnorm(theta, mun, sqrt(sigma2 / kappan)) *
    dinvgamma(sigma2, nun / 2, s2n * nun / 2)
}

d = outer(Theta, Sigma2, post.func)
rownames(d) = Theta
colnames(d) = Sigma2

library(reshape)
head(d)
?melt
df = melt(d)
colnames(df) = c('theta', 'sigma2', 'density')

 ## joint posterior distribution plot
library(ggplot2)
ggplot(df, aes(x = theta, y = sigma2, z = density)) +
  geom_contour(aes(color = ..level..)) + guides(color = FALSE)                    

#######################
# monte carlo sampling
s2.mc = rinvgamma(10000, nun / 2, s2n * nun / 2)
theta.mc = rnorm(10000, mun, sqrt(s2.mc / kappan)) # Accepts a vector of parameters

mean(theta.mc)
quantile(theta.mc, c(.025, .975))

############################################################



model_string <-"model
{
  for( i in 1 : N ) {
  y[i] ~ dnorm(theta,tau)
  }
  theta ~ dnorm(1.9, tau)
  tau ~ dgamma(0.5, 0.005)
  sigma2 <- 1/tau
  }
  "

data <- list(y=y, N =length(y))
inits<- list(theta = 1.8, tau=1)

#run JAGS
norm2p.fit=run.jags(model=model_string,
                   monitor= c("theta", "tau", "sigma2"),
                   burnin=1000, sample=20000,
                   data=data, n.chains=1, method="rjags", inits=inits)
norm2p.fit


############################################################


#####
pdf("fig5_6.pdf",family="Times",height=3.5,width=7)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

b<-(100-112)^2
s2<-13^2
n<-1:50 

k<-1 ; brk1<- (n/(k+n))^2 + n*(k/(k+n))^2*b/s2
k<-2 ; brk2<- (n/(k+n))^2 + n*(k/(k+n))^2*b/s2
k<-3 ; brk3<- (n/(k+n))^2 + n*(k/(k+n))^2*b/s2

plot(range(n),c(0.4,1.1),type="n",xlab="sample size", ylab="relative MSE")
abline(h=1,lty=2,lwd=2)
lines(n, brk1,col=gray(.25),lwd=2)
lines(n, brk2,col=gray(.5),lwd=2)
lines(n, brk3,col=gray(.75),lwd=2)
legend(20,.8,legend=c(expression(kappa[0]==0),expression(kappa[0]==1), expression(kappa[0]==2), 
  expression(kappa[0]==3) ),lwd=c(2,2,2),lty=c(2,1,1,1),col=c(gray(c(0,.25,.5,.75))),bty="n")

####
theta0<-112
mu0<-100
n<-10 
s2m<-s2/n
x<-seq(theta0-4*sqrt(s2m),theta0+4*sqrt(s2m), length=100)
plot(x,dnorm(x,theta0,sqrt(s2m)),type="l",lwd=2,ylim=c(0,.13),lty=2, xlab="IQ",
    ylab="")
abline(v=theta0)
for(k in 1:3) {
w<- n/(n+k) 
lines(x,dnorm(x,w*theta0+(1-w)*mu0,sqrt(w^2*s2m)),type="l",col=gray(k/4),lwd=2) 
              } 

#---
y<-0
y <- c(1.64, 1.70, 1.72, 1.74, 1.82, 1.82, 1.82, 1.90, 2.08)
mean(y)
