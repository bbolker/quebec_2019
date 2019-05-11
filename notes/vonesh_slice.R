library(emdbook)
library(bbmle)
data(ReedfrogSizepred)
## sizep1: rate parameter (1/logit change scale)
## sizep2: power
## sizep3: half-maximum

logist2 <- function(x,sizep1,sizep2,sizep3) {
    exp(sizep1*(sizep3-x))/(1+exp(sizep2*sizep1*(sizep3-x)))
}
mle.logist2 <- mle2(Kill ~ dbinom(logist2(TBL,sizep1,sizep2,sizep3),
                                  size=10),
                    start=list(sizep1=0,sizep2=1,sizep3=12),
                    data=ReedfrogSizepred,
                    method="Nelder-Mead")
mle.logist2B <- update(mle.logist2,
                       method="BFGS",control=list(parscale=c(0.3,30,10)))
mle.logist2C <- update(mle.logist2,
                    method="BFGS",control=list(maxit=1000))
mle.logist2D <- update(mle.logist2,
                       start=as.list(coef(mle.logist2C)),
                       control=list(maxit=1000))
slice2 <- calcslice(mle.logist2,mle.logist2C,n=1000)



## PLOT

png("vonesh_slice.png")
op <- par(cex=1.5,mgp=c(2.5,1,0),las=1,lwd=2,bty="l")
plot(slice2,type="l",ylab="Negative log-likelihood",xlab="")
## plot(slice2,type="l",ylab="Negative log-likelihood",xlab="",ylim=c(0,20))
points(0,-logLik(mle.logist2))
points(1,-logLik(mle.logist2C))
##abline(h=-logLik(mle.logist2C))
abline(h=-logLik(mle.logist2C))
abline(h=-logLik(mle.logist2C)+qchisq(0.95,1)/2,lty=2)
par(mar=c(2,2,0,0),bty="l",cex=0.7,lwd=1, mgp=c(1,0,0))
par(new=TRUE,fig=c(0.22,0.50,0.55,0.85))
plotrix::sizeplot(ReedfrogSizepred$TBL,ReedfrogSizepred$Kill, xlim=c(5,30),ylim=c(-0.4,10),axes=FALSE,
         xlab="size",ylab="killed",cex.lab=1.5)
with(as.list(coef(mle.logist2)),curve(logist2(x,sizep1,sizep2,sizep3)*10,add=TRUE,lwd=2))
box()
par(new=TRUE,fig=c(0.7,0.98,0.55,0.85))
plotrix::sizeplot(ReedfrogSizepred$TBL,ReedfrogSizepred$Kill, xlim=c(5,30),ylim=c(-0.4,10),axes=FALSE,
         xlab="size",ylab="killed",cex.lab=1.5)
with(as.list(coef(mle.logist2C)),curve(logist2(x,sizep1,sizep2,sizep3)*10,add=TRUE,lwd=2))
box()
par(op)
dev.off()

cc1 <- coef(mle.logist2)
cc2 <- coef(mle.logist2C)
dd <- expand.grid(sizep1=seq(cc1["sizep1"]*1.1,cc2["sizep1"]*1.1,length=41),
            sizep2=round(emdbook::lseq(1,65,9)),
            sizep3=seq(cc1["sizep3"]*1.1,cc2["sizep3"]*0.9,length=41))
dd$z <- numeric(nrow(dd))
for (i in 1:nrow(dd)) {
    dd$z[i] <- with(dd[i,],
                    mle.logist2@minuslogl(sizep1,sizep2,sizep3))
}
library(ggplot2); theme_set(theme_bw())
library(viridisLite)

gg_vonesh <- ggplot(dd,aes(sizep1,sizep3,fill=log10(z-min(z)+0.001),z=z))+
    geom_tile()+
    scale_fill_viridis_c()+
    geom_contour(colour="red",alpha=0.2)+
    facet_wrap(~sizep2, labeller=label_both)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    theme(panel.spacing=grid::unit(0,"lines"))
ggsave("vonesh_surface.png")

