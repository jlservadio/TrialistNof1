analyze <-
  function(Pain, Fatigue, Drowsy, Sleep, Thinking, Constipation, Treat, Neuropain, score.range = c(30, 4, 5, 4, 4, 4,30),
           Covs,  slopeprior, nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, setsize = 4000, alphaprior, 
		   beta.norm.prior, beta.ord.prior, c1prior, dcprior, varprior, varprior.params, path = "", mod.id)
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
		
	if (mod.id %in% c(1, 2, 3, 5.1, 5.2, 5.3)) {
	
		cat("\n \n \n Pain", mod.id, ".1 \n \n \n")
		Pain.out = tryCatch({run.norm(Pain, Treat, Covs, "normal", "hom", nChains, conv.limit, niters, nruns, setsize, 
			slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)})  
				
		cat("\n \n \n Fatigue", mod.id, ".2 \n \n \n")
		Fatigue.out = tryCatch({run.ord(Fatigue, Treat, Covs, score.range[2]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		
		cat("\n \n \n Drowsy", mod.id, ".3 \n \n \n")
		Drowsy.out = tryCatch({run.ord(Drowsy, Treat, Covs, score.range[3]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		
		cat("\n \n \n Sleep", mod.id, ".4 \n \n \n")
		Sleep.out = tryCatch({run.ord(Sleep, Treat, Covs, score.range[4]+1, "cumlogit", nChains, conv.limit, niters, nruns, 
			setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
			
		cat("\n \n \n Thinking", mod.id, ".5 \n \n \n")
		Thinking.out = tryCatch({run.ord(Thinking, Treat, Covs, score.range[5]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		
		cat("\n \n \n Constipation", mod.id, ".6 \n \n \n")
		Constipation.out = tryCatch({run.ord(Constipation, Treat, Covs, score.range[6]+1, "cumlogit", nChains, conv.limit, niters,
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		
		cat("\n \n \n Neuropain", mod.id, ".7 \n \n \n")
		Neuropain.out = tryCatch({run.norm(Neuropain, Treat, Covs, "normal", "hom", nChains, conv.limit, niters, nruns, 
			setsize, slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)})
			
	} else if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42)) {
		cat("\n \n \n Pain", mod.id, ".1 \n \n \n")
		Pain.out = tryCatch({run.norm(Pain, Treat, Covs[ , -c(2:7)], "normal", "hom", nChains, conv.limit, niters, nruns, setsize, 
			slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)})  
			
		cat("\n \n \n Fatigue", mod.id, ".2 \n \n \n")
		Fatigue.out = tryCatch({run.ord(Fatigue, Treat, Covs[ , -c(1, 3:7)], score.range[2]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
			
		cat("\n \n \n Drowsy", mod.id, ".3 \n \n \n")
		Drowsy.out = tryCatch({run.ord(Drowsy, Treat, Covs[ , -c(1:2, 4:7)], score.range[3]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
			
		cat("\n \n \n Sleep", mod.id, ".4 \n \n \n")
		Sleep.out = tryCatch({run.ord(Sleep, Treat, Covs[ , -c(1:3, 5:7)], score.range[4]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
			
		cat("\n \n \n Thinking", mod.id, ".5 \n \n \n")
		Thinking.out = tryCatch({run.ord(Thinking, Treat, Covs[ , -c(1:4, 6:7)], score.range[5]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		
		cat("\n \n \n Constipation", mod.id, ".6 \n \n \n")
		Constipation.out = tryCatch({run.ord(Constipation, Treat, Covs[ , -c(1:5, 7)], score.range[6]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		
		cat("\n \n \n Neuropain", mod.id, ".7 \n \n \n")
		Neuropain.out = tryCatch({run.norm(Neuropain, Treat, Covs[ , -c(1:6)], "normal", "hom", nChains, conv.limit, niters, nruns, 
			setsize, slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)})
		
	} 

    if (length(Pain.out) > 1) {Pain.treat.diff = Pain.out[["beta"]]}
    if (length(Fatigue.out) > 1) {Fatigue.treat.diff = treat.diffs(Fatigue.out[["p"]], Treat)}
    if (length(Drowsy.out) > 1) {Drowsy.treat.diff = treat.diffs(Drowsy.out[["p"]], Treat)}
    if (length(Sleep.out) > 1) {Sleep.treat.diff = treat.diffs(Sleep.out[["p"]], Treat)}
    if (length(Thinking.out) > 1) {Thinking.treat.diff = treat.diffs(Thinking.out[["p"]], Treat)}
    if (length(Constipation.out) > 1) {Constipation.treat.diff = treat.diffs(Constipation.out[["p"]], Treat)}
    if (length(Neuropain.out) > 1) {Neuropain.treat.diff = Neuropain.out[["beta"]]}

	if (length(Pain.out) > 1) {
		Pain.treat.diff.change = treat.diff.change(Pain.treat.diff, score.range[1])
		Pain.urun = FALSE
	} else {
		Pain.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Pain.urun = TRUE
	}
    
	if (length(Fatigue.out) > 1) {
		Fatigue.treat.diff.change = treat.diff.change(Fatigue.treat.diff,score.range[2])
		Fatigue.urun = FALSE
    } else{
		Fatigue.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Fatigue.urun = TRUE
	}
    
	if (length(Drowsy.out) > 1) {
		Drowsy.treat.diff.change = treat.diff.change(Drowsy.treat.diff,score.range[3])
		Drowsy.urun = FALSE
    } else{
		Drowsy.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Drowsy.urun = TRUE
	}
    
	if (length(Sleep.out) > 1) {
		Sleep.treat.diff.change = treat.diff.change(Sleep.treat.diff,score.range[4])
		Sleep.urun = FALSE
    } else{
		Sleep.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Sleep.urun = TRUE
	}
    
	if (length(Thinking.out) > 1) {
		Thinking.treat.diff.change = treat.diff.change(Thinking.treat.diff,score.range[5])
		Thinking.urun = FALSE
    } else{
		Thinking.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Thinking.urun = TRUE
	}
    
	if (length(Constipation.out) > 1) {
		Constipation.treat.diff.change = treat.diff.change(Constipation.treat.diff,score.range[6])
		Constipation.urun = FALSE
    } else{
		Constipation.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Constipation.urun = TRUE
	}
    
	if (length(Neuropain.out) > 1) {
		Neuropain.treat.diff.change = treat.diff.change(Neuropain.treat.diff, score.range[1])
		Neuropain.urun = FALSE
	} else{
		Neuropain.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
		Neuropain.urun = TRUE
	}
    
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
	
	output[[1 + length(output)]] = list("Pain.urun" = Pain.urun, "Fatigue.urun" = Fatigue.urun, "Drowsy.urun" = 
		Drowsy.urun, "Sleep.urun" = Sleep.urun, "Thinking.urun" = Thinking.urun, "Constipation.urun" = Constipation.urun, 
		"Neuropain.urun" = Neuropain.urun)
		
	names(output)[length(output)] = "uruns"
	
	output[[1 + length(output)]] = c(Pain.out$DIC, Fatigue.out$DIC, Drowsy.out$DIC, Sleep.out$DIC, Thinking.out$DIC, 
		Constipation.out$DIC, Neuropain.out$DIC)
	
	names(output)[length(output)] = "DICs"
	
	output[[length(output) + 1]] = list("PainPosterior" = list("Painalpha" = Pain.out$alpha, "Painbeta" = 
		Pain.out$beta, "PainSd" = Pain.out$Sd, "PainSlopes" = Pain.out$Slopes), 
		"FatiguePosterior" = list("Fatiguebeta" = Fatigue.out$beta, "FatigueSlopes" = Fatigue.out$Slopes, 
		"Fatiguec" = Fatigue.out$c, "Fatiguep" = Fatigue.out$p), 
		"DrowsyPosterior" = list("Drowsybeta" = Drowsy.out$beta, "DrowsySlopes" = Drowsy.out$Slopes, 
		"Drowsyc" = Drowsy.out$c, "Drowsyp" = Drowsy.out$p),
		"SleepPosterior" = list("Sleepbeta" = Sleep.out$beta, "SleepSlopes" = Sleep.out$Slopes, 
		"Sleepc" = Sleep.out$c, "Sleepp" = Sleep.out$p),
		"ThinkingPosterior" = list("Thinkingbeta" = Thinking.out$beta, "ThinkingSlopes" = Thinking.out$Slopes, 
		"Thinkingc" = Thinking.out$c, "Thinkingp" = Thinking.out$p),
		"ConstipationPosterior" = list("Constipationbeta" = Constipation.out$beta, "ConstipationSlopes" = 
		Constipation.out$Slopes, 
		"Constipationc" = Constipation.out$c, "Constipationp" = Constipation.out$p), "NeuropainPosterior" = 
		list("Neuropainalpha" = Neuropain.out$alpha, "Neuropainbeta" = 
		Neuropain.out$beta, "NeuropainSd" = Neuropain.out$Sd, "NeuropainSlopes" = Neuropain.out$Slopes)
		)
		
	names(output)[length(output)] = "ForPPC"
		
    return(output)    
}
