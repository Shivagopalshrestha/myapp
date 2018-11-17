############################################
############inverse sampling################
############################################
set.seed(999)
#ex1.
#exponential(2)= Gamma(1, 2)

lambda=2
U=runif(5000)
X=-log(1-U)/lambda

par(mfrow=c(1, 2), mar=c(4, 4, 1, 1))
hist(X, freq=F, breaks=100)
hist(rexp(5000, rate=2), freq=F, breaks=100)

#ex2.
#normal(0, 9)

U=runif(5000)
X=qnorm(U, mean=0, sd=3)

hist(X, freq=F, breaks=100)
hist(rnorm(5000, 0, 3), freq=F, breaks=100)

hist(X, freq=F, breaks=100)

#small x
curve(dnorm(x,0,3),add = TRUE)

p.vec <- c(0.2, 0.3, 0.1, 0.1, 0.3)
sum(p.vec)
cump.vec <- cumsum(p.vec)

U  <- runif(5000)
p.vec <- c(0.2, 0.3, 0.1, 0.12, 0.31)

zx<-cut(U,p.vec,labels = FALSE)
plot(zx)
X <- U
X[X < cump.vec[1]] <- 1
for(i in 2:length(p.vec)) {
  X[X >= cump.vec[i-1] & X < cump.vec[i]] <- i
}
plot(X)
table(X)/length(X)

?table

############################################
############rejection sampling##############
############################################

#ex1.
##Beta(1.3, 2.6)
x<-seq(0,1,0.0001)
plot(dbeta(x, 1.3, 2.6))
mean(pbeta  (x, 1.3, 2.6))
h = function(x) -dbeta(x, 1.3, 2.6)
M = -optimize(f=h, c(0,1))$objective



U = runif(50000, 0, 1) # accept or reject
theta = runif(50000) # uniform proposal g
summary(theta)
gtheta=1
ftheta = dbeta(theta, 1.3, 2.6)
accept = U <= ftheta/(M*gtheta)
Y.theta = theta[accept] #accepted subsample
## accepted ratio
mean(accept)
plot(theta)
plot(theta[accept], M*U[accept], col="blue")
points(theta[!accept], M*U[!accept], col="red")
curve(dbeta(x, 1.3, 2.6), add=T, col="black", lwd=22)




############################################
################Gibbs sampling##############
############################################


#ex1.
##normal semi-conjugate prior

# priors
mu0<-1.9  ; t20<-0.95^2
s20<-.01 ; nu0<-1

#data
y<-c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)
n<-length(y) ; mean.y<-mean(y) ; var.y<-var(y)


S<-5000
PHI<-matrix(nrow=S,ncol=2)
PHI[1,]<-phi<-c( mean.y, 1/var.y)

### Gibbs sampling
for(s in 2:S) {
  
  # generate a new theta value from its full conditional
  mun<-  ( mu0/t20 + n*mean.y*phi[2] ) / ( 1/t20 + n*phi[2] )
  t2n<- 1/( 1/t20 + n*phi[2] )
  phi[1]<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new sigma^2 value from its full conditional
  nun<- nu0+n
  s2n<- (nu0*s20 + (n-1)*var.y + n*(mean.y-phi[1])^2 ) /nun
  phi[2]<- rgamma(1, nun/2, nun*s2n/2)
  
  PHI[s,]<-phi         }



par(mfrow=c(1,3),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))
m1<-5
plot( PHI[1:m1,],type="l",xlim=range(PHI[1:100,1]), ylim=range(PHI[1:100,2]),
      lty=1,col="gray",xlab=expression(theta),ylab=expression(tilde(sigma)^2))
text(  PHI[1:m1,1], PHI[1:m1,2], c(1:m1) )

m1<-15
plot( PHI[1:m1,],type="l",xlim=range(PHI[1:100,1]), ylim=range(PHI[1:100,2]),
      lty=1,col="gray",xlab=expression(theta),ylab=expression(tilde(sigma)^2))
text(  PHI[1:m1,1], PHI[1:m1,2], c(1:m1) )

m1<-100
plot( PHI[1:m1,],type="l",xlim=range(PHI[1:100,1]), ylim=range(PHI[1:100,2]),
      lty=1,col="gray",xlab=expression(theta),ylab=expression(tilde(sigma)^2))
text(  PHI[1:m1,1], PHI[1:m1,2], c(1:m1) )

df <- data.frame(theta=PHI[, 1],
                 sigma2=PHI[, 2])

library(LaplacesDemon)
joint.density.plot(df$theta, df$sigma2, Title="Joint Density Plot",
                   contour=TRUE, color=FALSE)

cor(df$theta, df$sigma2)


#ex2.
##bivariate normal 
## bias 


gibbs<-function (n, mu1, s1, mu2, s2, rho) 
{
  mat <- matrix(ncol = 2, nrow = n)
  #poor starting values
  theta1 <- 10
  theta2 <- 10
  mat[1, ] <- c(theta1, theta2)
  for (i in 2:n) {
    theta1 <- rnorm(1, mu1 + 
                 (s1/s2) * rho * (theta2 - mu2), sqrt((1 - rho^2)*s1^2))
    theta2 <- rnorm(1, mu2 + 
                 (s2/s1) * rho * (theta1 - mu1), sqrt((1 - rho^2)*s2^2))
    mat[i, ] <- c(theta1, theta2)
  }
  mat
}

mu1 <- 0
mu2 <- 0
s1 <- 1
s2 <- 1
n <- 1000

summarymatrix <- matrix(0, 4, 4)

rhoseq <- c(0.5, 0.9, 0.99, 0.999)
n0seq <- c(50, 100, 500, 1000)

## run 100 simulations
for (k in 1:100){
  print(k)
  for(j in 1:4){
    rho <- rhoseq[j]
    for(i in 1:4){
      n0 <- n0seq[i]
      bivsample <- gibbs(n+no, mu1, s1, mu2, s2, rho)
      summarymatrix[i, j] <- summarymatrix[i, j]+ mean(bivsample[(n0+1):(n+n0), 1]) - mu1
    }
  }
  
}


summarymatrix/100


############################################
######adaptive rejection sampling###########
############################################
#ex1.
#Normal(0,9)


#install.packages("ars")
library(ars)
f<-function(x,mu,sigma){-1/(2*sigma^2)*(x-mu)^2}
fprima<-function(x,mu,sigma){-1/sigma^2*(x-mu)}
mysample<-ars(5000,f,fprima,mu=0,sigma=3)
hist(mysample, freq=F, breaks=100)
curve(dnorm(x, 0, 3), add=T, col="red", breaks=100)


#ex2.
##Beta(1.3, 2.6)
f2<-function(x,a,b){(a-1)*log(x)+(b-1)*log(1-x)}
f2prima<-function(x,a,b){(a-1)/x-(b-1)/(1-x)}
mysample2<-ars(5000,f2,f2prima,x=c(0.3,0.6),m=2,lb=TRUE,xlb=0,ub=TRUE,xub=1,a=1.3,b=2.6)
hist(mysample2, freq=F,breaks=100)
curve(dbeta(x, 1.3, 2.6), add=T, col="red", lwd=2)

















############################################
#############metropolis hasting#############
############################################

#ex1
## N(0, 9)

logposterior <- function(x){
  -1/(2*9)*(x-0)^2
}


theta0 <- 0
# theta0 <- 5

theta <- theta0
mysample <- matrix(0, 10000, 1)


accept <- 0
for(i in 1:10000){
    #sample from proposal N(2, 4)
    thetanew <- rnorm(1, mean=theta, sd=2)
    u <- runif(1)
    logu <- log(u)
    # symmtric proposal, the last two terms get cancelled
    aratio <- logposterior(thetanew)-logposterior(theta)
                # -1/(2*4)*(theta-thetanew)^2+-1/(2*4)*(theta-thetanew)^2
    aratio <- min(aratio, 0)
    if(logu <= aratio){
      theta <- thetanew
      accept <- accept + 1
    }
    mysample[i] <-theta 
   
}

accept/10000

hist(mysample[1:5000], freq=F, breaks=100)
curve(dnorm(x, 0, 3), add=T, col="red")

hist(mysample[5001:10000], freq=F, breaks=100)
curve(dnorm(x, 0, 3), add=T, col="red")


