run.ord <- function (Y, Treat, Covs, ncat, model = "cumlogit", nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, 
    setsize = 4000, betaprior, dcprior, c1prior, path, i) 
{

	stopifnot(max(Y[!is.na(Y)]) > min(Y[!is.na(Y)]))

    nobs = length(Y)
    if (!is.null(Covs)) 
        Covs = as.matrix(Covs)
    prior = prior.ord(Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 0, 0.1))
    inInitials = inits.ord(Y, Covs, Treat, ncat)
	inInits = list(inInitials[[1]], inInitials[[2]], inInitials[[3]])
	Diagnostics = inInitials[[4]]
		
    inData <- data.ord(Y, Covs, ncat, prior, Treat, i)
    model.ord(Covs, prior, path, i)
    pars.to.save <- c("beta","or","c","p")
    if (!is.null(Covs)) 
        pars.to.save = c(pars.to.save, "slope")
    jags.out <- jags.fit(inData, inInits, pars.to.save, model, "model.txt", nChains, niters, 
		conv.limit, setsize, nruns=5000, Covs)
    burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
	DIC = jags.out[[4]]
    varnames <- dimnames(samples)[[3]]
    nvars <- dim(samples)[3]
	
	Diagnostics[[length(Diagnostics) + 1]] = DIC
	names(Diagnostics)[length(Diagnostics)] = "DIC"
    
    beta.vars <- grep("beta", varnames)
    beta <- as.vector(samples[, , beta.vars])
    
    or.vars <- grep("or", varnames)
    or <- as.vector(samples[, , or.vars])

	c.vars <- grep("c", varnames)
    c <- matrix(samples[, , c.vars], c(no.runs * nChains, length(c.vars)))
    
    p.vars <- grep("p", varnames)
    p <- array(matrix(samples[, , p.vars], c(no.runs * nChains, length(p.vars))), c(no.runs * nChains, nobs, ncat))
    
	Posterior = list("beta" = mean(beta), "or" = mean(or), "c" = mean(c), "p" = mean(p))
	Diagnostics[[length(Diagnostics) + 1]] = Posterior
	names(Diagnostics)[length(Diagnostics)] = "Posterior"
	
    if (!is.null(Covs)) {
        slope.vars <- grep("slope", varnames)
        slope <- samples[, , slope.vars]
    }
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, beta, or, c, p, Diagnostics)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "beta", "or", "c", "p", "Diagnostics")
    }
    else {
        out <- list(burn.in, no.runs, Y, beta, or, p, slope, Diagnostics)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "beta", "or", "p", "Slopes", "Diagnostics")
    }
    return(out)
}
