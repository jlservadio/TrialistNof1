run.ord <- function (Y, Treat, Covs, ncat, model = "cumlogit", nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, 
    setsize = 4000, betaprior, dcprior, c1prior, path, mod.id) 
{

	stopifnot(max(Y[!is.na(Y)]) > min(Y[!is.na(Y)]))

    nobs = length(Y)
    if (!is.null(Covs)) 
        Covs = as.matrix(Covs)
    prior = prior.ord(Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 0, 0.1))
    inInits = inits.ord(Y, Covs, Treat, ncat)
	
	if (length(inInits[[1]]$slope) >= 2) {
		for (i in 1:nChains) {
			inInits[[i]]$slope1 = inInits[[i]]$slope[1]
			inInits[[i]]$slope2 = inInits[[i]]$slope[2]
		}
	}
	if (length(inInits[[1]]$slope) >= 3) {
		for (i in 1:nChains) { inInits[[i]]$slope3 = inInits[[i]]$slope[3] }
	}
	if (length(inInits[[1]]$slope) >= 4) {
		for (i in 1:nChains) { inInits[[i]]$slope4 = inInits[[i]]$slope[4] }
	}
	if (length(inInits[[1]]$slope) >= 5) {
		for (i in 1:nChains) { inInits[[i]]$slope5 = inInits[[i]]$slope[5] }
	}
	if (length(inInits[[1]]$slope) >= 6) {
		for (i in 1:nChains) { inInits[[i]]$slope6 = inInits[[i]]$slope[6] }
	}
	
	if (length(inInits[[1]]$slope) >= 2) {
		for (i in 1:nChains) { inInits[[i]]$slope = NULL }
	}
		
    inData <- data.ord(Y, Covs, ncat, prior, Treat, mod.id)
	if (mod.id == 2) { inData$x = as.numeric(inData$x) 
	} else if (mod.id == 3) {
		if (ncol(inData$x) == 1) { inData$x1 = as.numeric(inData$x[ , 1])
		} else if (ncol(inData$x) == 2) {
			inData$x1 = as.numeric(inData$x[ , 1])
			inData$x2 = as.numeric(inData$x[ , 2])
		} else if (ncol(inData$x) == 3) {
			inData$x1 = as.numeric(inData$x[ , 1])
			inData$x2 = as.numeric(inData$x[ , 2])
			inData$x3 = as.numeric(inData$x[ , 3])
		}
		inData$x = NULL
	} else if (mod.id == 4.1) { inData$x = inData$x[ , 2] 
	} else if (mod.id == 4.2) {
		if (ncol(Covs) == 2) {
			inData$x1 = as.numeric(inData$x[ , 2])
		} else if (ncol(Covs) == 3) {
			inData$x1 = as.numeric(inData$x[ , 2])
			inData$x2 = as.numeric(inData$x[ , 3])
		} else if (ncol(Covs) == 4) {
			inData$x1 = as.numeric(inData$x[ , 2])
			inData$x2 = as.numeric(inData$x[ , 3])
			inData$x3 = as.numeric(inData$x[ , 4])
		}
		inData$x = NULL
	} else if (mod.id == 5.1) {
		inData$z1 = as.numeric(inData$x[ , 1])
		inData$z2 = as.numeric(inData$x[ , 2])
	} else if (mod.id == 5.2) {
		inData$z1 = as.numeric(inData$x[ , 2])
		inData$z2 = as.numeric(inData$x[ , 3])
		inData$x = inData$x[ , 1]
	} else if (mod.id == 5.3) {
		if (ncol(Covs) == 3) {
			inData$x1 = as.numeric(inData$x[ , 1])
			inData$z1 = as.numeric(inData$x[ , 2])
			inData$z2 = as.numeric(inData$x[ , 3])
		} else if (ncol(Covs) == 4) {
			inData$x1 = as.numeric(inData$x[ , 1])
			inData$x2 = as.numeric(inData$x[ , 2])
			inData$z1 = as.numeric(inData$x[ , 3])
			inData$z2 = as.numeric(inData$x[ , 4])
		} else if (ncol(Covs) == 5) {
			inData$x1 = as.numeric(inData$x[ , 1])
			inData$x2 = as.numeric(inData$x[ , 2])
			inData$x3 = as.numeric(inData$x[ , 3])
			inData$z1 = as.numeric(inData$x[ , 4])
			inData$z2 = as.numeric(inData$x[ , 5])
		}
	} else if (mod.id == 5.4) {
		inData$z1 = as.numeric(inData$x[ , 2])
		inData$z2 = as.numeric(inData$x[ , 3])
	} else if (mod.id == 5.41) {
		inData$z1 = as.numeric(inData$x[ , 3])
		inData$z2 = as.numeric(inData$x[ , 4])
		inData$x = inData$x[ , 2]
	} else if (mod.id == 5.42) {
		if (ncol(Covs) == 4) {
			inData$z1 = as.numeric(inData$x[ , 3])
			inData$z2 = as.numeric(inData$x[ , 4])
			inData$x1 = as.numeric(inData$x[ , 2])
		} else if (ncol(Covs) == 5) {
			inData$z1 = as.numeric(inData$x[ , 4])
			inData$z2 = as.numeric(inData$x[ , 5])
			inData$x1 = as.numeric(inData$x[ , 2])
			inData$x2 = as.numeric(inData$x[ , 3])
		} else if (ncol(Covs) == 6) {
			inData$z1 = as.numeric(inData$x[ , 5])
			inData$z2 = as.numeric(inData$x[ , 6])
			inData$x1 = as.numeric(inData$x[ , 2])
			inData$x2 = as.numeric(inData$x[ , 3])
			inData$x3 = as.numeric(inData$x[ , 4])
		}
	}
	
#	if (ncat == 6) { inData$v = c(0.1, 0.15, 0.25, 0.25, 0.15, 0.1) }
		
    model.ord(Covs, prior, path, mod.id)
    pars.to.save <- c("Y", "beta","or","c","p")
	
	if (is.null(Covs)) { pars.to.save = pars.to.save
	} else if (ncol(Covs) == 1) {
		pars.to.save = c(pars.to.save, "slope")
	} else if (ncol(Covs) == 2) {
		pars.to.save = c(pars.to.save, "slope1", "slope2")
	} else if (ncol(Covs) == 3) {
		pars.to.save = c(pars.to.save, "slope1", "slope2", "slope3")
	} else if (ncol(Covs) == 4) {
		pars.to.save = c(pars.to.save, "slope1", "slope2", "slope3", "slope4")
	} else if (ncol(Covs) == 5) {
		pars.to.save = c(pars.to.save, "slope1", "slope2", "slope3", "slope4", "slope5")
	} else if (ncol(Covs) == 6) {
		pars.to.save = c(pars.to.save, "slope1", "slope2", "slope3", "slope4", "slope5", "slope6")
	}
	
	jags.out <- jags.fit(inData, inInits, pars.to.save, model, "model.txt", nChains, niters, 
		conv.limit, setsize, nruns=5000, Covs, mod.id)

	burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
	DIC = jags.out[[4]]
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
	
	Y.vars <- grep("Y", varnames)
	Y.samp <- samples[ , , Y.vars]
	
    if (!is.null(Covs)) {
        slope.vars <- grep("slope", varnames)
        slope <- samples[, , slope.vars]
    }
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, beta, or, c, p, DIC, Y.samp)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "beta", "or", "c", "p", "DIC", "Y.samp")
    }
    else {
        out <- list(burn.in, no.runs, Y, beta, or, p, slope, DIC, Y.samp)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "beta", "or", "p", "Slopes", "DIC", "Y.samp")
    }
    return(out)
}
