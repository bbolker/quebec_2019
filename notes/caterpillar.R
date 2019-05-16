dd <- read.table(header=TRUE,
           text="
density larvae surviving
1   90  60
5   90  60
10  89  56
15  87  41
20  93  31
")

library(bbmle)

library(ggplot2); theme_set(theme_bw())
ggplot(dd,aes(density,surviving/larvae))+
    geom_point(aes(size=larvae))+
    geom_smooth(method="glm",
                formula=y~poly(x,2),
                method.args=list(family=binomial),
                aes(weight=larvae))


m0 <- glm(surviving/larvae~density,
          weight=larvae,
          data=dd,
          family=binomial)

m0Q <- glm(surviving/larvae~poly(density,2),
          weight=larvae,
          data=dd,
          family=binomial)



m1 <- mle2(surviving~dbinom(prob=plogis(b0+b1*density),size=larvae),
           data=dd,
           start=list(b0=1,b1=1))

m2 <- mle2(surviving~dbinom(prob=plogis(a+density^b),size=larvae),
           data=dd,
           start=list(a=0,b=-1))

bbmle::AICtab(m1,m2,m0Q)
