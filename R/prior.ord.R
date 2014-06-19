prior.ord <- function (Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 0, 0.1)) 
{
    mean.beta = betaprior[[2]]
    prec.beta = betaprior[[3]]
    lower.dc = dcprior[[2]]
    upper.dc = dcprior[[3]] 
    lower.c1 = c1prior[[2]]
    upper.c1 = c1prior[[3]]
    Prior.beta = paste("d", betaprior[[1]], "(mean.beta,prec.beta)", sep = "")
    Prior.dc = paste("d", dcprior[[1]], "(lower.dc, upper.dc)", sep = "")
    Prior.c1 = paste("d", c1prior[[1]], "(lower.c1, upper.c1)", sep = "")
    
    if (!is.null(Covs)) {
        mean.slope = slopeprior[[2]]
        prec.slope = slopeprior[[3]]
        Prior.slope = paste("d", slopeprior[[1]], "(mean.slope,prec.slope)", sep = "")
    }
    out <- list(mean.beta = mean.beta, prec.beta = prec.beta, Prior.beta = Prior.beta, 
                lower.dc = lower.dc, upper.dc = upper.dc, Prior.dc = Prior.dc,
                lower.c1 = lower.c1, upper.c1 = upper.c1, Prior.c1 = Prior.c1)
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
