library(emdbook)
library(bbmle)
data(ReedfrogSizepred)


##' @param x 
##' @param sizep1 rate parameter (1/logit change scale)
##' @param sizep2 power
##' @param sizep3 half-maximum
logist2 <- function(x,sizep1,sizep2,sizep3) {
    exp(sizep1*(sizep3-x))/(1+exp(sizep2*sizep1*(sizep3-x)))
}
##' @param a scale
##' @param b y-scale
##' @param alpha power
powricker <- function(x,a,b,alpha) {
    b*((x/a)*exp(1-(x/a)))^alpha
}

## fit models
mle_logist2 <- mle2(Kill ~ dbinom(logist2(TBL,sizep1,sizep2,sizep3),
                                  size=10),
                    start=list(sizep1=0,sizep2=1,sizep3=12),
                    data=ReedfrogSizepred,
                    method="Nelder-Mead")
mle_powricker <- mle2(Kill ~ dbinom(powricker(TBL,a,b,alpha),
                                  size=10),
                    start=list(a=10,b=1,alpha=1),
                    data=ReedfrogSizepred,
                    method="Nelder-Mead")

## alternate fit (focal)
mle_logist2C <- update(mle_logist2,
                       method="BFGS",
                       control=list(maxit=1000))

## alternate fits (not used)
mle_logist2B <- update(mle_logist2,
                       method="BFGS",
                       control=list(parscale=c(0.3,30,10)))
mle_logist2D <- update(mle_logist2,
                       start=as.list(coef(mle_logist2C)),
                       control=list(maxit=1000))
slice2 <- calcslice(mle_logist2,mle_logist2C,n=1000)
save.image(file="vonesh_slice_dat.rda")

cc1 <- coef(mle_logist2)
cc2 <- coef(mle_logist2C)
dd <- expand.grid(sizep1=seq(cc1["sizep1"]*1.1,cc2["sizep1"]*1.1,length=41),
                  sizep2=round(emdbook::lseq(1,65,9)),
                  sizep3=seq(cc1["sizep3"]*1.1,cc2["sizep3"]*0.9,length=41),
                  z=NA)
for (i in 1:nrow(dd)) {
    dd$z[i] <- with(dd[i,],
                    mle_logist2@minuslogl(sizep1,sizep2,sizep3))
}

cc_pr <- coef(mle_powricker)
dd_powricker <-
    expand.grid(a=seq(cc_pr["a"]*0.9,cc_pr["a"]*1.1,length=41),
                b=seq(cc_pr["b"]*0.9,cc_pr["b"]*1.1,length=41),
                alpha=seq(12,29,by=2),
                z=NA)
for (i in 1:nrow(dd_powricker)) {
    dd_powricker$z[i] <- with(dd_powricker[i,],
                    mle_powricker@minuslogl(a,b,alpha))
}

library(ggplot2); theme_set(theme_bw())
library(viridisLite)

get_zmin <- function(x,eps=0.001) log10(x-min(x)+eps)
dd$zmin <- get_zmin(dd$z)
dd_powricker$zmin <- get_zmin(dd_powricker$z)
gg_vonesh <- ggplot(dd,aes(sizep1,sizep3,fill=zmin,z=z))+
    geom_tile()+
    scale_fill_viridis_c()+
    geom_contour(colour="red",alpha=0.2)+
    facet_wrap(~sizep2, labeller=label_both)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    theme(panel.spacing=grid::unit(0,"lines"))
ggsave("vonesh_surface.png")

##
png("vonesh_surface2.png",height=400,width=400)
lattice::levelplot(zmin~sizep1*sizep3|sizep2, data=dd, as.table=TRUE,
                   col.regions=viridis(100))
dev.off()

png("vonesh_surface3.png",height=400,width=400)
lattice::levelplot(zmin~a*b|alpha, data=dd_powricker, as.table=TRUE,
                   col.regions=viridis(100))
dev.off()

