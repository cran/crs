### R code from vignette source 'crs.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: crs.Rnw:64-66
###################################################
library(crs)
options(prompt = "R> ", crs.messages = FALSE, digits = 3)


###################################################
### code chunk number 2: crs.Rnw:289-293
###################################################
degree <- 5
x <- seq(0,1,length=1000)
B <- gsl.bs(x,degree=degree,intercept=TRUE)
matplot(x,B,type="l")


###################################################
### code chunk number 3: crs.Rnw:295-298
###################################################
deriv <- 1
B <- gsl.bs(x,degree=degree,deriv=deriv,intercept=TRUE)
matplot(x,B,type="l")


###################################################
### code chunk number 4: crs.Rnw:502-510
###################################################
set.seed(42)
n <- 1000
x <- runif(n)
z <- rbinom(n,1,.5)
y <- cos(2*pi*x) + z + rnorm(n,sd=0.25)
z <- factor(z)
model <- crs(y~x+z)
summary(model)


###################################################
### code chunk number 5: crs.Rnw:527-529
###################################################
getOption("SweaveHooks")[["multifig"]]()
options(SweaveHooks = list(multifig = function() par(mfrow=c(2,2))))
plot(model,mean=TRUE)


###################################################
### code chunk number 6: crs.Rnw:540-550
###################################################
set.seed(1234)
n <- 1000
x1 <- runif(n)
x2 <- runif(n)
z <- ifelse(x1>.5,1,0)
dgp <- cos(2*pi*x1)+sin(2*pi*x2)+2*z
z <- factor(z)
y <- dgp + rnorm(n,sd=1)
model <- crs(y~x1+x2+z)
summary(model)


###################################################
### code chunk number 7: crs.Rnw:563-573
###################################################
## When kernel=FALSE, we could use the anova() function
model.res <- crs(y~x1+x2+z,cv="none",degree=model$degree,basis=model$basis,include=0)
## anova(model.res$model.lm,model$model.lm)
## We could also do this manually...
F <- model$df.residual*(sum(residuals(model.res)^2)
                        -sum(residuals(model)^2))/sum(residuals(model)^2)
F
## Compute the P-value for the F-statistic
P <- 1-pf(F,1,model$df.residual)
P


###################################################
### code chunk number 8: crs.Rnw:578-611
###################################################
num.eval <- 50
x1.seq.0 <- seq(min(x1[z==0]),max(x1[z==0]),length=num.eval)
x2.seq.0 <- seq(min(x2[z==0]),max(x2[z==0]),length=num.eval)
x.grid <- expand.grid(x1.seq.0,x2.seq.0)
newdata <- data.frame(x1=x.grid[,1],x2=x.grid[,2],z=factor(rep(0,num.eval**2),levels=c(0,1)))
z0 <- matrix(predict(model,newdata=newdata),num.eval,num.eval)

x1.seq.1 <- seq(min(x1[z==1]),max(x1[z==1]),length=num.eval)
x2.seq.1 <- seq(min(x2[z==1]),max(x2[z==1]),length=num.eval)
x.grid <- expand.grid(x1.seq.1,x2.seq.1)
newdata <- data.frame(x1=x.grid[,1],x2=x.grid[,2],z=factor(rep(1,num.eval),levels=c(0,1)))
z1 <- matrix(predict(model,newdata=newdata),num.eval,num.eval)
xlim <- c(0,1)
zlim=c(min(z0,z1),max(z0,z1))
theta <- 15
phi <- 10
persp(x=x1.seq.0,y=x2.seq.0,z=z0,
      xlab="x1",ylab="x2",zlab="y",
      xlim=xlim,
      ylim=xlim,      
      zlim=zlim,
      ticktype="detailed",      
      border="red",
      theta=theta,phi=phi)
par(new=TRUE)
persp(x=x1.seq.1,y=x2.seq.1,z=z1,
      xlab="x1",ylab="x2",zlab="y",
      xlim=xlim,
      ylim=xlim,      
      zlim=zlim,
      theta=theta,phi=phi,
      ticktype="detailed",
      border="blue")


