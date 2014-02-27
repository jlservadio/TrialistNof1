data.ord <- function (Y, Covs, ncat, prior, Treat) 
{
    inData = list(Treat = Treat, Y = Y, ncat = ncat, nobs = length(Y), 
                  mean.beta = prior$mean.beta, prec.beta = prior$prec.beta, 
                  lower.dc = prior$lower.dc, upper.dc = prior$upper.dc,
                  lower.c1 = prior$lower.c1, upper.c1 = prior$upper.c1)
    if (!is.null(Covs)) {
        inData[[1 + length(inData)]] = Covs
        names(inData)[[length(inData)]] = "x"
        inData[[1 + length(inData)]] = prior$mean.slope
        names(inData)[[length(inData)]] = "mean.slope"
        inData[[1 + length(inData)]] = prior$prec.slope
        names(inData)[[length(inData)]] = "prec.slope"
    }
    return(inData)
}
