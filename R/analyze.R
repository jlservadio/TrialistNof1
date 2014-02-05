analyze <-
function(Pain, Fatigue, Drowsy, Sleep, Thinking, Constipation, Treat, Neuropain, score.range = c(30, 4, 5, 4, 4, 4,30),
           Covs,  slopeprior, nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, setsize = 4000, alphaprior, 
		   beta.norm.prior, beta.ord.prior, c1prior, dcprior, varprior, varprior.params, path = "")
  {

    # Pain, Fatigue, Drowsy, Sleep, Thinking, Constipation, Neuropain are vectors of outcomes
    # Treat is vector of treatment labels
    # score.range is vector of range of scores in outcome order
    # Covs is matrix of covariates in meta-regression model; if none given, it is NULL
    # model is normal (for now)
    # var.model is homogeneous by default for now
    # nChains is number of MCMC chains to run
    # conv.limit is limit for BGR convergence
    # niters is number of MCMC iterations to run
    # nruns is number of MCMC iterations to save
    # setsize is number of MCMC iterations after which convergence diagnostics should be checked
    # alphaprior is prior for intercept as 3 element list (distribution, mean, precision)
    # beta.norm.prior is prior for treatment effect with continuous outcome as 3 element list (distribution, mean, precision)
    # beta.ord.prior is prior for treatment effect with ordinal outcome as 3 element list (distribution, mean, precision)
    # c1prior is prior for first cutpoint with ordinal outcome as 3 element list (distribution, lower bound, upper bound)
    # dcprior is prior for differences between cutpoints starting with second minus first for ordinal outcome as 3 element 
	#		list (distribution, lower bound, upper bound)
    # varprior is 2 element list of parameter and distribution upon which prior for variance is put 
    #    choices for parameter: precision(prec), variance(var), Sd(Sd)
    #    choices for distribution: gamma(gamma), Wishart(wish), uniform(unif), halfnormal(hn)
    # varprior.params is vector of parameters of variance distribution
    # path is directory where data are stored

    nobs = length(Pain)
    stopifnot(length(Pain)==nobs, length(Fatigue)==nobs, length(Drowsy)==nobs, length(Sleep)==nobs, length(Thinking)==nobs, 
		length(Constipation)==nobs, length(Neuropain)==nobs, length(Treat)==nobs)
    stopifnot(length(varprior)==2)
    stopifnot(length(alphaprior)==3, length(beta.norm.prior)==3, length(beta.ord.prior)==3)
    stopifnot(length(dcprior) == 3, length(c1prior) == 3, length(varprior.params)==2)
    
    varprior = as.list(varprior)
    alphaprior = as.list(alphaprior)
    beta.norm.prior = as.list(beta.norm.prior)
    beta.ord.prior = as.list(beta.ord.prior)
    dcprior = as.list(dcprior)
    
    outnames = c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropathic Pain")
    names(score.range) = outnames
    
    Pain.out = run.norm(Pain, Treat, Covs, "normal", "hom", nChains, conv.limit, niters, nruns, setsize, slopeprior, alphaprior, 
		beta.norm.prior, varprior,varprior.params, path = "")  
    Fatigue.out = run.ord(Fatigue, Treat, Covs, score.range[2]+1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
		beta.ord.prior, dcprior, c1prior, path)
    Drowsy.out = run.ord(Drowsy, Treat, Covs, score.range[3]+1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
		beta.ord.prior, dcprior, c1prior, path)    
    Sleep.out = run.ord(Sleep, Treat, Covs, score.range[4]+1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
		beta.ord.prior, dcprior, c1prior, path)
    Thinking.out = run.ord(Thinking, Treat, Covs, score.range[5]+1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
		beta.ord.prior, dcprior, c1prior, path)
    Constipation.out = run.ord(Constipation, Treat, Covs, score.range[6]+1, "cumlogit", nChains, conv.limit, niters, nruns, 
		setsize, beta.ord.prior, dcprior, c1prior, path)
    Neuropain.out = run.norm(Neuropain, Treat, Covs, "normal", "hom", nChains, conv.limit, niters, nruns, setsize, slopeprior, 
		alphaprior, beta.norm.prior, varprior,varprior.params, path = "")  

    Pain.treat.diff = Pain.out[["beta"]]
    Fatigue.treat.diff = treat.diffs(Fatigue.out[["p"]], Treat)
    Drowsy.treat.diff = treat.diffs(Drowsy.out[["p"]], Treat)
    Sleep.treat.diff = treat.diffs(Sleep.out[["p"]], Treat)
    Thinking.treat.diff = treat.diffs(Thinking.out[["p"]], Treat)
    Constipation.treat.diff = treat.diffs(Constipation.out[["p"]], Treat)
    Neuropain.treat.diff = Neuropain.out[["beta"]]

    Pain.treat.diff.change = treat.diff.change(Pain.treat.diff, score.range[1])
    Fatigue.treat.diff.change = treat.diff.change(Fatigue.treat.diff,score.range[2])
    Drowsy.treat.diff.change = treat.diff.change(Drowsy.treat.diff,score.range[3])
    Sleep.treat.diff.change = treat.diff.change(Sleep.treat.diff,score.range[4])
    Thinking.treat.diff.change = treat.diff.change(Thinking.treat.diff,score.range[5])
    Constipation.treat.diff.change = treat.diff.change(Constipation.treat.diff,score.range[6])
    Neuropain.treat.diff.change = treat.diff.change(Neuropain.treat.diff, score.range[1])

    change.interval = round(rbind(Pain.treat.diff.change[[1]], Fatigue.treat.diff.change[[1]], Drowsy.treat.diff.change[[1]], 
		Sleep.treat.diff.change[[1]], Thinking.treat.diff.change[[1]], Constipation.treat.diff.change[[1]], 
		Neuropain.treat.diff.change[[1]]),3)
    change.probs = round(rbind(Pain.treat.diff.change[[2]], Fatigue.treat.diff.change[[2]], Drowsy.treat.diff.change[[2]], 
		Sleep.treat.diff.change[[2]], Thinking.treat.diff.change[[2]], Constipation.treat.diff.change[[2]], 
		Neuropain.treat.diff.change[[2]]),3)
    
    #make into list for easier json encoding
    output <- list();
    for(i in 1:nrow(change.interval)){
      output[[i]] = list(
        "interval" = structure(as.list(change.interval[i,]), names=c("P025", "Median", "P975")),
        "probs" = structure(as.list(change.probs[i,]), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
      ) 
    }
    names(output) = outnames
		
    return(output)    
  }
