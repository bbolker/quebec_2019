dd <-read.csv("../height_diameter.csv",header=TRUE,sep=";")
library(ggplot2); theme_set(theme_bw())
ggplot(dd,aes(dbh_cm,height_h,group=interaction(plot_id,tree_id)))+
    stat_binhex(aes(group=1))+
    geom_line(alpha=0.1,col="red")
## Chapman-Richards: y*(1-exp(-kt)^p
nls(height_h ~ hmax*(1-exp(-dbh_cm/exp(logr))),
    start=list(hmax=20, logr=1),
    data=dd)
nls(height_h ~ hmax*(1-exp(-dbh_cm/exp(logr)))^p,
    start=list(hmax=20, logr=2.7,p=1),
    data=dd)
library(nlme)
## https://stackoverflow.com/questions/36205261/error-in-pars-nm-incorrect-number-of-dimensions
m0 <- nlme(height_h ~ hmax*(1-exp(-dbh_cm/exp(logr))),
     fixed=list(hmax+logr~1),
     random=hmax~1|plot_id/tree_id,
     start=c(hmax=22, logr=2.7),
     data=dd)

m1 <- nlme(height_h ~ hmax*(1-exp(-dbh_cm/exp(logr)))^p,
     fixed=list(hmax+logr+p~1),
     random=hmax~1|plot_id/tree_id,
     start=c(hmax=22, logr=2.7,p=1),
     data=dd)

m2 <- nlme(height_h ~ hmax*(1-exp(-dbh_cm/exp(logr)))^p,
     fixed=list(hmax+logr+p~1),
     random=hmax+logr+p~1|plot_id/tree_id,
     start=c(hmax=22, logr=2.7,p=1),
     data=dd)

library(lme4)
ff <- deriv(~ hmax*(1-exp(-dbh_cm/exp(logr)))^p,
            c("hmax","logr","p"),
            function.arg=c("dbh_cm","hmax","logr","p"))
## m3 <- nlmer(height_h ~ ff(dbh_cm,hmax,logr,p) ~ logr | plot_id/tree_id,
##             start=c(hmax=22, logr=2.7,p=1),
##             data=dd,
##             control=nlmerControl(optimizer=nloptwrap),
##             verbose=1)
