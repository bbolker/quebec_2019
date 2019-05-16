frogs <- emdbook::ReedfrogFuncresp
## predicted number killed with two different 
rogers_pred <- function(N0, a, h, P, time) {
  N0 - emdbook::lambertW(a * h * N0 * exp(-a * (P * time - h * N0)))/(a * h)
}
holling_pred <- function(N0, a, h, P, time) {
    a*N0*P*time/(1+a*h*N0)
}
time <- 14 ## time period
P <- 3  ## number of predators

library(bbmle)
plot(Killed ~ Initial, data=frogs)
m1 <- mle2(Killed ~ dbinom(size=Initial,
                     prob=holling_pred(Initial, a, h, P, time)/Initial),
     data=frogs,
     start=list(a=0.25,h=10))

plot(Killed ~ Initial, data=frogs)
with(as.list(coef(m1)),
     curve(holling_pred(x,a=a,h=h,P=P,time=time), add=TRUE))
