jags.fit <- function (inData, inInits, pars.to.save, model, model.file, n.chains, niters, conv.limit, 
	setsize, nruns=5000, Covs) 
{

	

    mod = jags.model(model.file, data = inData, inits = inInits, n.chains, n.adapt = 0)
	DIC1 = dic.samples(mod, niters)
	DIC2 = as.list(DIC1)
	DIC = sum(DIC2[[1]])
    done.adapt = adapt(mod, n.iter = 2 * setsize, end.adaptation = FALSE)
    while (!done.adapt) done.adapt = adapt(mod, n.iter = 2 * setsize, end.adaptation = FALSE)
    samples <- coda.samples(model = mod, variable.names = pars.to.save, n.iter = setsize, thin = 1)
    nsamples = setsize
    varnames <- dimnames(samples[[3]])[[2]]
    alpha.vars <- grep("alpha", varnames)
    beta.vars <- grep("beta", varnames)
    Sd.vars <- grep("Sd", varnames)
    if (!is.null(Covs)) 
        slope.vars <- grep("slope", varnames)
    Mean.vars <- grep("theta", varnames)
    if (is.null(Covs)) {
        temp.samples.array = array(NA, c(niters, n.chains, length(alpha.vars) + length(beta.vars) + length(Sd.vars)))
        temp.samples.array[seq(nsamples), 1, ] <- samples[[1]][, c(alpha.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 2, ] <- samples[[2]][, c(alpha.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 3, ] <- samples[[3]][, c(alpha.vars, beta.vars, Sd.vars)]
        dimnames(temp.samples.array) <- list(NULL, NULL, varnames[c(alpha.vars, beta.vars, Sd.vars)])
    } else {
        temp.samples.array = array(NA, c(niters, n.chains, length(alpha.vars) + length(beta.vars) + length(Sd.vars) + 
			length(slope.vars)))
        temp.samples.array[seq(nsamples), 1, ] <- samples[[1]][, c(alpha.vars, slope.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 2, ] <- samples[[2]][, c(alpha.vars, slope.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 3, ] <- samples[[3]][, c(alpha.vars, slope.vars, beta.vars, Sd.vars)]
        dimnames(temp.samples.array) <- list(NULL, NULL, varnames[c(Sd.vars, slope.vars, beta.vars, Sd.vars)])
    }	
    max.bgrRatio = max(apply(temp.samples.array[seq(nsamples), , ,drop=F ], 3, maxbgrRatio))
    check <- max.bgrRatio > conv.limit
    print(max.bgrRatio)
    flush.console()
    if (check) {
        count <- 1
        while (check & (count < niters/setsize)) {
            samples <- coda.samples(mod, variable.names = pars.to.save, n.iter = setsize, thin = 1)
            count <- count + 1
            if (is.null(Covs)) {
                temp.samples.array[nsamples + seq(setsize), 1, ] = samples[[1]][, c(alpha.vars, beta.vars, Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 2, ] = samples[[2]][, c(alpha.vars, beta.vars, Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 3, ] = samples[[3]][, c(alpha.vars, beta.vars, Sd.vars)]
            } else {
                temp.samples.array[nsamples + seq(setsize), 1, ] = samples[[1]][, c(alpha.vars, slope.vars, beta.vars, 
					Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 2, ] = samples[[2]][, c(alpha.vars, slope.vars, beta.vars, 
					Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 3, ] = samples[[3]][, c(alpha.vars, slope.vars, beta.vars, 
					Sd.vars)]
            }
            nsamples = nsamples + setsize
            max.bgrRatio = max(apply(temp.samples.array[seq(nsamples), , ,drop=F], 3, maxbgrRatio))
            check <- max.bgrRatio > conv.limit
            print(max.bgrRatio)
            flush.console()
        }
    }
    no.to.converge = nsamples
    print(max.bgrRatio)
    flush.console()
    no.to.keep <- no.to.converge/2
    if (nruns > no.to.keep) { 
        samples = coda.samples(mod, variable.names = pars.to.save, n.iter = nruns, thin = 1)
    } else {
        thin <- floor(no.to.keep/nruns)
        samples <- coda.samples(mod, variable.names = pars.to.save, n.iter = no.to.keep, n.thin = thin)
    }
    samples.array = array(NA, c(dim(samples[[1]])[1], n.chains, length(varnames)))
    samples.array[, 1, ] = samples[[1]]
    samples.array[, 2, ] = samples[[2]]
    samples.array[, 3, ] = samples[[3]]
    dimnames(samples.array) = list(NULL,NULL,varnames)
    out <- list(no.to.converge, dim(samples.array)[1], samples.array, DIC)
    names(out) <- c("BurnIn", "No. Runs Per Chain", "Samples", "DIC")
    return(out)
}
