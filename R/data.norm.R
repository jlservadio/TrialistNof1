data.norm <- function (Y, Covs, prior, Treat) 
{
    inData = list(Treat = Treat, Y = Y, mean.alpha = prior$mean.alpha, prec.alpha = prior$prec.alpha, mean.beta = 
		prior$mean.beta, prec.beta = prior$prec.beta, prec.1 = prior$prec.1, prec.2 = prior$prec.2)
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
