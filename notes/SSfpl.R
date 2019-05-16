initial = function(mCall, data, LHS)
{
    xy <- sortedXyData(mCall[["input"]], LHS, data)
    if (nrow(xy) < 5) {
        stop("too few distinct input values to fit a four-parameter logistic")
    }
    ## convert the response to a proportion (i.e. contained in (0,1))
    rng <- range(xy$y); drng <- diff(rng)
    xy$prop <- (xy$y - rng[1L] + 0.05 * drng)/(1.1 * drng)
    ## inverse regression of the x values on the proportion
    ir <- as.vector(coef(lm(x ~ I(log(prop/(1-prop))), data = xy)))
    pars <- as.vector(coef(nls(y ~ cbind(1, 1/(1 + exp((xmid - x)/exp(lscal)))),
                               data = xy,
                               start = list(xmid = ir[1L],
                                            lscal = log(abs(ir[2L]))),
                               algorithm = "plinear")))
    setNames(c(pars[3L], pars[3L] + pars[4L], pars[1L], exp(pars[2L])),
             nm = mCall[c("A", "B", "xmid", "scal")])
}, parameters = c("A", "B", "xmid", "scal"))
