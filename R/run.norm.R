run.norm <-
function (Y, Treat, Covs, model, var.model, nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000,
    setsize = 4000, slopeprior, alphaprior, betaprior, varprior, varprior.params, path, insuf)
{
    nobs = length(Y)
	
	stopifnot(insuf == FALSE)
	
    if (!is.null(Covs))
        Covs = as.matrix(Covs)
    prior = prior.norm(Covs, alphaprior, betaprior, slopeprior, varprior, varprior.params)
    inInits = inits.norm(Y, Covs, Treat, varprior, ntreat = 2)
    inData <- data.norm(Y, Covs, prior, Treat)
    model.norm(nobs, Covs, prior, varprior, path)
    pars.to.save <- c("alpha", "beta")
    if (!is.null(Covs))
        pars.to.save = c(pars.to.save, "slope")
    pars.to.save = c(pars.to.save, "Sd")
    jags.out <- jags.fit(inData, inInits, pars.to.save, model, "model.txt", nChains, niters, conv.limit, setsize, nruns, Covs)
    burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
    varnames <- dimnames(samples)[[3]]
    nvars <- dim(samples)[3]
    alpha.vars <- grep("alpha", varnames)
    alpha <- as.vector(samples[, , alpha.vars])
    beta.vars <- grep("beta", varnames)
    beta <- as.vector(samples[, , beta.vars])
    Sd.vars <- grep("Sd", varnames)
    Sd <- as.vector(samples[, , Sd.vars])
    if (!is.null(Covs)) {
        slope.vars <- grep("slope", varnames)
        slope <- samples[, , slope.vars]
    }
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, alpha, beta, Sd)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "alpha", "beta", "Sd")
    }
    else {
        out <- list(burn.in, no.runs, Y, alpha, beta, Sd, slope)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "alpha", "beta", "Sd", "Slopes")
    }
    return(out)
}
