analyze <- function(Y, Outcome, Treat, score.range = c(30, 4, 5, 4, 4, 4,30), Covs,  slopeprior, nChains = 3, conv.limit = 1.05, 
	niters = 50000, nruns = 5000, setsize = 4000, alphaprior, beta.norm.prior, beta.ord.prior, c1prior, dcprior, varprior, 
	varprior.params, path = "", mod.id)
{

	nobs = length(Y)
  
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
	
	if (Outcome == "Pain") { index = 1
	} else if (Outcome == "Fatigue") { index = 2
	} else if (Outcome == "Drowsy") { index = 3
	} else if (Outcome == "Sleep") { index = 4
	} else if (Outcome == "Thinking") { index = 5
	} else if (Outcome == "Constipation") { index = 6
	} else if (Outcome == "Neuropain") { index = 7
	}
	
	if (mod.id %in% c(1, 2, 3, 5.1, 5.2, 5.3)) {
	
		cat("\n \n \n", Outcome, mod.id, "\n \n \n")
		if (index == 1) {
			Pain.out = tryCatch({run.norm(Y, Treat, Covs, "normal", "hom", nChains, conv.limit, niters, nruns, setsize, 
			slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)}) 
		} else if (index == 2) {
			Fatigue.out = tryCatch({run.ord(Y, Treat, Covs, score.range[2]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 3) {
			Drowsy.out = tryCatch({run.ord(Y, Treat, Covs, score.range[3]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})	
		} else if (index == 4) {
			Sleep.out = tryCatch({run.ord(Y, Treat, Covs, score.range[4]+1, "cumlogit", nChains, conv.limit, niters, nruns, 
			setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 5) {
			Thinking.out = tryCatch({run.ord(Y, Treat, Covs, score.range[5]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 6) {
			Constipation.out = tryCatch({run.ord(Y, Treat, Covs, score.range[6]+1, "cumlogit", nChains, conv.limit, niters,
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 7) {
			Neuropain.out = tryCatch({run.norm(Y, Treat, Covs, "normal", "hom", nChains, conv.limit, niters, nruns, 
			setsize, slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)})
		}
	} else if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42)) {
		
		cat("\n \n \n", Outcome, mod.id, "\n \n \n")
		if (index == 1) {
			Pain.out = tryCatch({run.norm(Y, Treat, Covs[ , -c(2:7)], "normal", "hom", nChains, conv.limit, niters, nruns, setsize, 
			slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)}) 
		} else if (index == 2) {
			Fatigue.out = tryCatch({run.ord(Y, Treat, Covs[ , -c(1, 3:7)], score.range[2]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 3) {
			Drowsy.out = tryCatch({run.ord(Y, Treat, Covs[ , -c(1:2, 4:7)], score.range[3]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 4) {
			Sleep.out = tryCatch({run.ord(Y, Treat, Covs[ , -c(1:3, 5:7)], score.range[4]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 5) {
			Thinking.out = tryCatch({run.ord(Y, Treat, Covs[ , -c(1:4, 6:7)], score.range[5]+1, "cumlogit", nChains, conv.limit, niters, 
			nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 6) {
			Constipation.out = tryCatch({run.ord(Y, Treat, Covs[ , -c(1:5, 7)], score.range[6]+1, "cumlogit", nChains, conv.limit, 
			niters, nruns, setsize, beta.ord.prior, dcprior, c1prior, path, mod.id)}, error=function(cond){caught <- list("DIC" = 1000)})
		} else if (index == 7) {
			Neuropain.out = tryCatch({run.norm(Y, Treat, Covs[ , -c(1:6)], "normal", "hom", nChains, conv.limit, niters, nruns, 
			setsize, slopeprior, alphaprior, beta.norm.prior, varprior,varprior.params, path = "", mod.id)}, error=function(cond){
			caught <- list("DIC" = 1000)})
		}
	}
	
	out = list()
	
	if (index == 1) {
		if (length(Pain.out) > 1) {Pain.treat.diff = Pain.out[["beta"]]}
		if (length(Pain.out) > 1) {
			Pain.treat.diff.change = treat.diff.change(Pain.treat.diff, score.range[1])
			Pain.urun = FALSE
		} else {
			Pain.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Pain.urun = TRUE
		}
		change.interval = round(Pain.treat.diff.change[[1]], 3)
		change.probs = round(Pain.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Pain.urun, Pain.treat.diff.change, Pain.out$alpha, Pain.out$beta, Pain.out$Sd, Pain.out$Slope, Pain.out$DIC, Pain.out$Dev)
		names(out) = c("pain", "Outcome", "mod.id", "Pain.urun", "Pain.treat.diff.change", "alpha", "beta", "Sd", "Slope", "DIC", "Dev")
	} else if (index == 2) {
		if (length(Fatigue.out) > 1) {Fatigue.treat.diff = treat.diffs(Fatigue.out[["p"]], Treat)}
		if (length(Fatigue.out) > 1) {
			Fatigue.treat.diff.change = treat.diff.change(Fatigue.treat.diff,score.range[2])
			Fatigue.urun = FALSE
		} else{
			Fatigue.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Fatigue.urun = TRUE
		}
		change.interval = round(Fatigue.treat.diff.change[[1]], 3)
		change.probs = round(Fatigue.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Fatigue.urun, Fatigue.treat.diff.change, Fatigue.out$beta, Fatigue.out$c, Fatigue.out$p, Fatigue.out$Slopes, 
			Fatigue.out$DIC)
		names(out) = c("fatigue", "Outcome", "mod.id", "Fatigue.urun", "Fatigue.treat.diff.change", "beta", "c", "p", "Slope", "DIC")
	} else if (index == 3) {
		if (length(Drowsy.out) > 1) {Drowsy.treat.diff = treat.diffs(Drowsy.out[["p"]], Treat)}
		if (length(Drowsy.out) > 1) {
			Drowsy.treat.diff.change = treat.diff.change(Drowsy.treat.diff,score.range[3])
			Drowsy.urun = FALSE
		} else{
			Drowsy.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Drowsy.urun = TRUE
		}
		change.interval = round(Drowsy.treat.diff.change[[1]], 3)
		change.probs = round(Drowsy.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Drowsy.urun, Drowsy.treat.diff.change, Drowsy.out$beta, Drowsy.out$c, Drowsy.out$p, Drowsy.out$Slopes, 
			Drowsy.out$DIC)
		names(out) = c("drowsy", "Outcome", "mod.id", "Drowsy.urun", "Drowsy.treat.diff.change", "beta", "c", "p", "Slope", "DIC")
	} else if (index == 4) {
		if (length(Sleep.out) > 1) {Sleep.treat.diff = treat.diffs(Sleep.out[["p"]], Treat)}
		
		if (length(Sleep.out) > 1) {
			Sleep.treat.diff.change = treat.diff.change(Sleep.treat.diff,score.range[4])
			Sleep.urun = FALSE
		} else{
			Sleep.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Sleep.urun = TRUE
		}
		change.interval = round(Sleep.treat.diff.change[[1]], 3)
		change.probs = round(Sleep.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Sleep.urun, Sleep.treat.diff.change, Sleep.out$beta, Sleep.out$c, Sleep.out$p, Sleep.out$Slopes, Sleep.out$DIC)
		names(out) = c("sleep", "Outcome", "mod.id", "Sleep.urun", "Sleep.treat.diff.change", "beta", "c", "p", "Slope", "DIC")
	} else if (index == 5) {
		if (length(Thinking.out) > 1) {Thinking.treat.diff = treat.diffs(Thinking.out[["p"]], Treat)}
		if (length(Thinking.out) > 1) {
			Thinking.treat.diff.change = treat.diff.change(Thinking.treat.diff,score.range[5])
			Thinking.urun = FALSE
		} else{
			Thinking.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Thinking.urun = TRUE
		}
		change.interval = round(Thinking.treat.diff.change[[1]], 3)
		change.probs = round(Thinking.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Thinking.urun, Thinking.treat.diff.change, Thinking.out$beta, Thinking.out$c, Thinking.out$p, 
			Thinking.out$Slopes, Thinking.out$DIC)
		names(out) = c("thinking", "Outcome", "mod.id", "Thinking.urun", "Thinking.treat.diff.change", "beta", "c", "p", "Slope", "DIC")
	} else if (index == 6) {
		if (length(Constipation.out) > 1) {Constipation.treat.diff = treat.diffs(Constipation.out[["p"]], Treat)}
		if (length(Constipation.out) > 1) {
			Constipation.treat.diff.change = treat.diff.change(Constipation.treat.diff,score.range[6])
			Constipation.urun = FALSE
		} else{
			Constipation.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Constipation.urun = TRUE
		}
		change.interval = round(Constipation.treat.diff.change[[1]], 3)
		change.probs = round(Constipation.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Constipation.urun, Constipation.treat.diff.change, Constipation.out$beta, Constipation.out$c, 
			Constipation.out$p, Constipation.out$Slopes, Constipation.out$DIC)
		names(out) = c("constipation", "Outcome", "mod.id", "Constipation.urun", "Constipation.treat.diff.change", "beta", "c", "p", "Slope", 
		"DIC")
	} else if (index == 7) {
		if (length(Neuropain.out) > 1) {Neuropain.treat.diff = Neuropain.out[["beta"]]}
		if (length(Neuropain.out) > 1) {
			Neuropain.treat.diff.change = treat.diff.change(Neuropain.treat.diff, score.range[1])
			Neuropain.urun = FALSE
		} else{
			Neuropain.treat.diff.change = list(c(-1, 0, 1), c(0, 0.5, 0.5, 0))
			Neuropain.urun = TRUE
		}
		change.interval = round(Neuropain.treat.diff.change[[1]], 3)
		change.probs = round(Neuropain.treat.diff.change[[2]], 3)
		out = list(list(
			"interval" = structure(as.list(change.interval), names=c("P025", "Median", "P975")),
			"probs" = structure(as.list(change.probs), names=paste("Proportion",c("< -0.2", "-0.2 - 0", "0 - 0.2", "> 0.2")))
			), Outcome, mod.id, Neuropain.urun, Neuropain.treat.diff.change, Neuropain.out$alpha, Neuropain.out$beta, Neuropain.out$Sd, 
			Neuropain.out$Slopes, Neuropain.out$DIC, Neuropain.out$Dev)
		names(out) = c("neuropain", "Outcome", "mod.id", "Neuropain.urun", "Neuropain.treat.diff.change", "alpha", "beta", "Sd", "Slope", "DIC", "Dev")
	}
	
	if (index == 1) {
		out[[length(out) + 1]] = Pain.out$Y.samp
		names(out)[length(out)] = "Y.samp"
		if (mod.id %in% c(2, 4.1, 5.2, 5.41)) {
			out[[length(out) + 1]] = Pain.out$Time.samp
			names(out)[length(out)] = "Time.samp"
		}
	} else if (index == 2) {
		out[[length(out) + 1]] = Fatigue.out$Y.samp
		names(out)[length(out)] = "Y.samp"
	} else if (index == 3) {
		out[[length(out) + 1]] = Drowsy.out$Y.samp
		names(out)[length(out)] = "Y.samp"
	} else if (index == 4) {
		out[[length(out) + 1]] = Sleep.out$Y.samp
		names(out)[length(out)] = "Y.samp"
	} else if (index == 5) {
		out[[length(out) + 1]] = Thinking.out$Y.samp
		names(out)[length(out)] = "Y.samp"
	} else if (index == 6) {
		out[[length(out) + 1]] = Constipation.out$Y.samp
		names(out)[length(out)] = "Y.samp"
	} else if (index == 7) {
		out[[length(out) + 1]] = Neuropain.out$Y.samp
		names(out)[length(out)] = "Y.samp"
		if (mod.id %in% c(2, 4.1, 5.2, 5.41)) {
			out[[length(out) + 1]] = Neuropain.out$Time.samp
			names(out)[length(out)] = "Time.samp"
		}
	}
	
	return(out)
	
} 

