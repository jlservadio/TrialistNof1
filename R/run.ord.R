run.ord <-
function (Y, Treat, Covs, ncat, model = "cumlogit", nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000,
    setsize = 4000, betaprior, dcprior, c1prior, path)
{
    nobs = length(Y)
	
	stopifnot(insuf == FALSE)
	
    if (!is.null(Covs))
        Covs = as.matrix(Covs)
    prior = prior.ord(Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 0, 0.1))
    inInits = inits.ord(Y, Covs, Treat, ncat)
    inData <- data.ord(Y, Covs, ncat, prior, Treat)
    model.ord(Covs, prior, path)
    pars.to.save <- c("beta","or","c","p")
    if (!is.null(Covs))
        pars.to.save = c(pars.to.save, "slope")
    jags.out <- jags.fit(inData, inInits, pars.to.save, model, "model.txt", nChains, niters, conv.limit, setsize, nruns, Covs)
    burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
    varnames <- dimnames(samples)[[3]]
    nvars <- dim(samples)[3]
    
    beta.vars <- grep("beta", varnames)
    beta <- as.vector(samples[, , beta.vars])
    
    or.vars <- grep("or", varnames)
    or <- as.vector(samples[, , or.vars])

	c.vars <- grep("c", varnames)
    c <- matrix(samples[, , c.vars], c(no.runs * nChains, length(c.vars)))
    
    p.vars <- grep("p", varnames)
    p <- array(matrix(samples[, , p.vars], c(no.runs * nChains, length(p.vars))), c(no.runs * nChains, nobs, ncat))
    
    if (!is.null(Covs)) {
        slope.vars <- grep("slope", varnames)
        slope <- samples[, , slope.vars]
    }
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, beta, or, c, p)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "beta", "or", "c", "p")
    }
    else {
        out <- list(burn.in, no.runs, Y, beta, or, p, slope)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "beta", "or", "p", "Slopes")
    }
    return(out)
}

treat.diff.change <-
function (treat.diff, score.range)
{
    treat.diff.change = treat.diff/score.range
    treat.diff.change.1 = as.vector(c(quantile(treat.diff.change, 0.025), median(treat.diff.change), quantile(treat.diff.change, 0.975)))
    treat.diff.change.2 = as.numeric(table(cut(treat.diff.change, breaks = c(-1, -0.2, 0, 0.2, 1))))/length(treat.diff.change)
    return(list(treat.diff.change.1, treat.diff.change.2))
}
