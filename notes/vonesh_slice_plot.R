load("vonesh_slice_dat.rda")
png("vonesh_slice.png")
op <- par(cex=1.5,mgp=c(2.5,1,0),las=1,lwd=2,bty="l")
plot(slice2,type="l",ylab="Negative log-likelihood",xlab="")
## plot(slice2,type="l",ylab="Negative log-likelihood",xlab="",ylim=c(0,20))
points(0,-logLik(mle_logist2))
points(1,-logLik(mle_logist2C))
##abline(h=-logLik(mle_logist2C))
abline(h=-logLik(mle_logist2C))
abline(h=-logLik(mle_logist2C)+qchisq(0.95,1)/2,lty=2)
par(mar=c(2,2,0,0),bty="l",cex=0.7,lwd=1, mgp=c(1,0,0))
par(new=TRUE,fig=c(0.22,0.50,0.55,0.85))
plotrix::sizeplot(ReedfrogSizepred$TBL,ReedfrogSizepred$Kill, xlim=c(5,30),ylim=c(-0.4,10),axes=FALSE,
         xlab="size",ylab="killed",cex.lab=1.5)
with(as.list(coef(mle_logist2)),curve(logist2(x,sizep1,sizep2,sizep3)*10,add=TRUE,lwd=2))
box()
par(new=TRUE,fig=c(0.7,0.98,0.55,0.85))
plotrix::sizeplot(ReedfrogSizepred$TBL,ReedfrogSizepred$Kill, xlim=c(5,30),ylim=c(-0.4,10),axes=FALSE,
         xlab="size",ylab="killed",cex.lab=1.5)
with(as.list(coef(mle_logist2C)),curve(logist2(x,sizep1,sizep2,sizep3)*10,add=TRUE,lwd=2))
box()
par(op)
dev.off()
