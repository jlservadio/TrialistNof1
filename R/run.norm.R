run.norm <- function (Y, Treat, Covs, model, var.model, nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, 
    setsize = 4000, slopeprior, alphaprior, betaprior, varprior, varprior.params, path, mod.id) 
{

	stopifnot(max(Y[!is.na(Y)]) > min(Y[!is.na(Y)]))
	
    nobs = length(Y)
    if (!is.null(Covs)) { Covs = as.matrix(Covs) }
    prior = prior.norm(Covs, alphaprior, betaprior, slopeprior, varprior, varprior.params)
    inInits = inits.norm(Y, Covs, Treat, varprior, ntreat = 2)
		
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
		
	inData <- data.norm(Y, Covs, prior, Treat, mod.id)
	
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
	
    model.norm(nobs, Covs, prior, varprior, path, mod.id)
    pars.to.save <- c("Y", "D", "alpha", "beta")
	
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
	
	if (mod.id %in% c(2, 4.1, 5.2, 5.41)) {
		pars.to.save = c(pars.to.save, "x")
	}
	
    pars.to.save = c(pars.to.save, "Sd")
    jags.out <- jags.fit(inData, inInits, pars.to.save, model, "model.txt", nChains, niters, conv.limit, 
		setsize, nruns=5000, Covs, mod.id)
    burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
	DIC = jags.out[[4]]
	
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
	
	D.vars <- grep("D", varnames)
	Dev1 <- samples[ , , D.vars]
	Dev = rbind(Dev1[ , 1, ], Dev1[ , 2, ], Dev1[ , 3, ])
	Y.vars <- grep("Y", varnames)
	Y.samp <- samples[ , , Y.vars]
	Time.vars <- grep("x", varnames)
	Time.samp <- samples[ , , Time.vars]
	
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, alpha, beta, Sd, DIC, Dev, Y.samp)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "alpha", "beta", "Sd", "DIC", "Dev", "Y.samp")
    } else {
        out <- list(burn.in, no.runs, Y, alpha, beta, Sd, slope, DIC, Dev, Y.samp)
        names(out) <- c("Burn In", "Number runs per chain", "Y", "alpha", "beta", "Sd", "Slopes", "DIC", "Dev", "Y.samp")
    }
	
	if (mod.id %in% c(2, 4.1, 5.2, 5.41)) {
		out[[length(out) + 1]] = Time.samp
		names(out)[length(out)] = "Time.samp"
	}
    return(out)
}
