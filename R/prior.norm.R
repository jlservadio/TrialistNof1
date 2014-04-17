prior.norm <- function (Covs, alphaprior, betaprior, slopeprior, varprior, varprior.params) 
{
    mean.alpha = alphaprior[[2]]
    prec.alpha = alphaprior[[3]]
    mean.beta = betaprior[[2]]
    prec.beta = betaprior[[3]]
    Prior.alpha <- paste("d", alphaprior[[1]], "(mean.alpha,prec.alpha)", sep = "")
    Prior.beta <- paste("d", betaprior[[1]], "(mean.beta,prec.beta)", sep = "")
    if (!is.null(Covs)) {
        mean.slope = slopeprior[[2]]
        prec.slope = slopeprior[[3]]
        Prior.slope = paste("d", slopeprior[[1]], "(mean.slope,prec.slope)", sep = "")
    }
    prec.1 = varprior.params[1]
    prec.2 = varprior.params[2]
    if (varprior[[1]] == "prec") {
        if (varprior[[2]] == "gamma") 
            Prior.prec = paste("dgamma(prec.1, prec.2)", sep = "")
        else return("Only Gamma currently supported for precision distribution")
    } else if (varprior[[1]] == "Sd") {
        if (varprior[[2]] == "unif") 
            Prior.prec = paste("dunif(prec.1, prec.2)", sep = "")
        else if (varprior[[2]] == "hn") 
            Prior.prec = paste("dnorm(0,1)I(", varprior[[3]][1], ",", varprior[[3]][2], ")", sep = "")
        else return("Unsupported Sd distribution")
    } else if (varprior[[1]] == "var") {
        if (varprior[[2]] == "unif") 
            Prior.prec = paste("dunif(prec.1,prec.2)", sep = "")
        else if (varprior[[2]] == "hn") 
            Prior.prec = paste("dnorm(0,1),I(", varprior[[3]][1], ",", varprior[[3]][2], ")", sep = "")
        else return("Unsupported var distribution")
    }
    out <- list(mean.alpha = mean.alpha, prec.alpha = prec.alpha, mean.beta = mean.beta, prec.beta = prec.beta, 
		Prior.alpha = Prior.alpha, Prior.beta = Prior.beta, prec.1 = prec.1, prec.2 = prec.2, Prior.prec = Prior.prec)
    if (!is.null(Covs)) {
        out[[1 + length(out)]] = mean.slope
        names(out)[[length(out)]] = "mean.slope"
        out[[1 + length(out)]] = prec.slope
        names(out)[[length(out)]] = "prec.slope"
        out[[1 + length(out)]] = Prior.slope
        names(out)[[length(out)]] = "Prior.slope"
    } 
    return(out)
}

