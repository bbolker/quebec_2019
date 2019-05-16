library(emdbook)
data(ReedfrogFuncresp)
?ReedfrogFuncresp
head(ReedfrogFuncresp)
plot(Killed~Initial,data=ReedfrogFuncresp)
plot(Killed/Initial~Initial,data=ReedfrogFuncresp,
     ylab="fraction killed")
NLLfun <- function(p) {
    a <- p[1]
    h <- p[2]
    prob <- a/(1+a*h*ReedfrogFuncresp$Initial)
    prob <- pmin(prob, 0.999)
    lik <- dbinom(ReedfrogFuncresp$Killed, prob=prob, size = ReedfrogFuncresp$Initial)
    nll <- -sum(log(lik))
    ## cat(a,h,nll,max(prob),"\n")
    return(nll)
}

options(warn=1)
p0 <- c(a=0.5,h=0.03)
NLLfun(p0)
opt1 <- optim(fn=NLLfun, par=p0, method="L-BFGS-B", lower=c(0.01,0.01),
              hessian=TRUE)
v <- solve(opt1$hessian)  ## Wald var-cov matrix
sqrt(diag(v))

library(bbmle)
parnames(NLLfun) <- c("a","h")
opt2 <- mle2(NLLfun,start=p0,vecpar=TRUE)

coef(opt2)
vcov(opt2)
prof <- profile(opt2)
plot(prof,show.points=TRUE)
confint(prof)
confint(opt2,method="quad")

NLLfun2 <- function(p) {
    a <- p[1]
    h <- p[2]
    prob <- a/(1+a*h*Initial)
    prob <- pmin(prob, 0.999)
    nll <- -sum(dbinom(Killed, prob=prob, size = Initial, log=TRUE))
    ## cat(a,h,nll,max(prob),"\n")
    return(nll)
}
parnames(NLLfun2) <- c("a","h")
opt2 <- mle2(NLLfun2,start=p0,vecpar=TRUE, data=ReedfrogFuncresp)


opt3 <- mle2(Killed ~ dbinom(size=Initial,
                             prob=a/(1+a*h*Initial)),
             ## MUST USE initial parameters as list, not vector, here!
             start=as.list(p0),
             data=ReedfrogFuncresp)
predict(opt3)
simulate(opt3)
simulate(opt3)
nboot <- 200
set.seed(101)
m <- matrix(NA,nrow=nboot,ncol=2)
for (i in 1:nboot) {
    bootdata <- ReedfrogFuncresp[sample(nrow(ReedfrogFuncresp),replace=TRUE), ]
    bootfit <- update(opt2, data=bootdata)
    m[i,] <- coef(bootfit)
}

## parametric bootstrap
set.seed(101)
m_par <- matrix(NA,nrow=nboot,ncol=2)
for (i in 1:nboot) {
    ss <- simulate(opt3)
    bootdata <- transform(ReedfrogFuncresp,Killed=ss)
    suppressWarnings(bootfit <- update(opt3, data=bootdata))
    m_par[i,] <- coef(bootfit)
}

plot(m[,1],m[,2])
points(m_par[,1],m_par[,2], col="blue")

t(apply(m, MARGIN=2, FUN=quantile, c(0.025,0.975)))
