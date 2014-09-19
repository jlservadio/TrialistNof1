evaluate <- function(observations, Outcome, Covs, mod.id, No_Neuropain) {

	if (Outcome == "Pain") { index = 1
	} else if (Outcome == "Fatigue") { index = 2
	} else if (Outcome == "Drowsy") { index = 3
	} else if (Outcome == "Sleep") { index = 4
	} else if (Outcome == "Thinking") { index = 5
	} else if (Outcome == "Constipation") { index = 6
	} else if (Outcome == "Neuropain") { index = 7
	}

	ct = 1
	nof1 = NULL
	nof1[[4]] = TRUE
	while(ct <= 20 && nof1[[4]] == TRUE) {
		nof1 = analyze(
		Y = observations[ , (index + 2)],
		Outcome = Outcome,
		Treat = observations$Treat2,
		score.range = c(30, 4, 5, 4, 4, 4,30),
		Covs = Covs,
		slopeprior = list("norm", 0, 0.1),
		nChains = 3,
		conv.limit=1.05,
		niters=10000,
		setsize=1000,
		alphaprior = list("norm",0,1e-6),
		beta.norm.prior = list("norm",0,1e-6),
		beta.ord.prior = list("norm",0,1e-6),
		dcprior = list("unif",0,20),
		c1prior = list("unif",-20,20),
		varprior=list("Sd","unif"),
		varprior.params=c(0.1, 5),
		path="",
		mod.id = mod.id)
		
		ct = ct + 1
		flush.console()
	}

	names(nof1)[c(1, 4, 5)] = c("o", "o.urun", "o.treat.diff.change")
	
	Results = c(nof1$o$interval$P025, nof1$o$interval$Median, nof1$o$interval$P975, nof1$o$probs$'Proportion < -0.2', 
		nof1$o$probs$'Proportion -0.2 - 0', nof1$o$probs$'Proportion 0 - 0.2', nof1$o$probs$'Proportion > 0.2')
		
	failed.slope.array = array(0, dim = c(100, 3, 10))
	failed.slope.array[ , 1, ] = -50:49 + rnorm(100, 0, 1)
	failed.slope.array[ , 2, ] = -50:49 + rnorm(100, 0, 1)
	failed.slope.array[ , 3, ] = -50:49 + rnorm(100, 0, 1)
	
	if (nof1$o.urun == TRUE) {
		nof1$beta = -50:49
		nof1$Slope = failed.slope.array
	}
	
	if (!is.null(Covs)) { Covs = as.matrix(Covs) }
	
	if (length(dim(nof1$Slope)) == 2) {
		b = array(dim = c(dim(nof1$Slope)[1], 
			dim(nof1$Slope)[2], 1))
		b[ , , 1] = nof1$Slope
		nof1$Slope = b
	}
	
	if (mod.id == 1) {
		Slopes.Sig = FALSE
	} else if (mod.id %in% c(2, 3, 5.1, 5.2, 5.3)) {
		Slopes.Sig = matrix(FALSE, nrow = 1, ncol = ncol(Covs))
	} else if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42)) {
		Slopes.Sig = matrix(FALSE, nrow = 1, ncol = ncol(Covs)-6)
	}
	
	if (mod.id != 1) {
		for (i in 1:ncol(Slopes.Sig)) {
			int.slope = quantile(nof1$Slope[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[1, i] = TRUE } 
		}
	}
	
	Beta.Sig = FALSE
	int.beta = quantile(nof1$beta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig = TRUE }
	
	Sigs = cbind(Beta.Sig, Slopes.Sig)
	colnames(Sigs)[1] = "Beta"
	for (i in 2:ncol(Sigs)) { colnames(Sigs)[i] = "Slope" }
	
	
	if (index %in% c(1, 7) && !nof1[[4]]) {
		mean.Deviances = mean(rowSums(nof1$Dev))
		if (mod.id == 1) {
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2
		} else if (mod.id == 2) {
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + 
			mean(nof1$Slope)*colMeans(rbind(nof1$Time.samp[ , 1, ], nof1$Time.samp[ , 2, ], nof1$Time.samp[ , 3, ]))
		} else if (mod.id == 3) {
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope)*Covs
		} else if (mod.id == 4) {
			Y.lag = rep(NA, length(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))))
			for (i in 2:length(Y.lag)) { Y.lag[i] = colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))[i-1] }
			Y.lag[1] = rnorm(1, mean(nof1$Y.samp, 2))
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope)*Y.lag
		} else if (mod.id == 4.1) {
			Y.lag = rep(NA, length(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))))
			for (i in 2:length(Y.lag)) { Y.lag[i] = colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))[i-1] }
			Y.lag[1] = rnorm(1, mean(nof1$Y.samp, 2))
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*Y.lag + 
			mean(nof1$Slope[ , , 2])*colMeans(rbind(nof1$Time.samp[ , 1, ], nof1$Time.samp[ , 2, ], nof1$Time.samp[ , 3, ]))
		} else if (mod.id == 4.2) {
			Y.lag = rep(NA, length(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))))
			for (i in 2:length(Y.lag)) { Y.lag[i] = colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))[i-1] }
			Y.lag[1] = rnorm(1, mean(nof1$Y.samp, 2))
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*Y.lag + mean(nof1$Slope[ , , 2])*Covs[ , ncol(Covs)]
		} else if (mod.id == 5.1) {
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*observations$car.A + 
			mean(nof1$Slope[ , , 2])*observations$car.B
		} else if (mod.id == 5.2) {
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*colMeans(rbind(nof1$Time.samp[ , 1, ], 
			nof1$Time.samp[ , 2, ], nof1$Time.samp[ , 3, ])) + mean(nof1$Slope[ , , 2])*observations$car.A + mean(nof1$Slope[ , , 3])*observations$car.B
		} else if (mod.id == 5.3) {
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*Covs[ , 1] + 
			mean(nof1$Slope[ , , 2])*observations$car.A + mean(nof1$Slope[ , , 3])*observations$car.B
		} else if (mod.id == 5.4) {
			Y.lag = rep(NA, length(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))))
			for (i in 2:length(Y.lag)) { Y.lag[i] = colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))[i-1] }
			Y.lag[1] = rnorm(1, mean(nof1$Y.samp, 2))
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*Y.lag + 
			mean(nof1$Slope[ , , 2])*observations$car.A + mean(nof1$Slope[ , , 3])*observations$car.B
		} else if (mod.id == 5.41) {
			Y.lag = rep(NA, length(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))))
			for (i in 2:length(Y.lag)) { Y.lag[i] = colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))[i-1] }
			Y.lag[1] = rnorm(1, mean(nof1$Y.samp, 2))
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*Y.lag + mean(nof1$Slope[ , , 2])*colMeans(
			rbind(nof1$Time.samp[ , 1, ], nof1$Time.samp[ , 2, ], nof1$Time.samp[ , 3, ])) + 
			mean(nof1$Slope[ , , 3])*observations$car.A + mean(nof1$Slope[ , , 4])*observations$car.B
		} else if (mod.id == 5.42) {
			Y.lag = rep(NA, length(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))))
			for (i in 2:length(Y.lag)) { Y.lag[i] = colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ]))[i-1] }
			Y.lag[1] = rnorm(1, mean(nof1$Y.samp, 2))
			mu.mean = mean(nof1$alpha) + mean(nof1$beta)*observations$Treat2 + mean(nof1$Slope[ , , 1])*Y.lag + mean(nof1$Slope[ , , 2])*Covs[ , ncol(Covs)] + 
			mean(nof1$Slope[ , , 3])*observations$car.A + mean(nof1$Slope[ , , 4])*observations$car.B
		}
		prec = (1 / mean(nof1$Sd))^2
		D.m.byDay = -log(prec) + log(2*pi) + prec*(colMeans(rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ])) - mu.mean)^2
		Deviance.mean = sum(D.m.byDay)
		DIC.manual = 2*mean.Deviances - Deviance.mean
	} else if (index %in% c(2, 4:6) && !nof1[[4]]) {
		Y.sample = rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ])
		D = matrix(NA, dim(Y.sample)[1], dim(Y.sample)[2])
		
		p2 = nof1$p
		for (i in 1:5) {
			p2[ , , i] = nof1$p[ , , i]^(1*(Y.sample == i))
		}
		D = -2 * (log(p2[ , , 1]) + log(p2[ , , 2]) + log(p2[ , , 3]) + log(p2[ , , 4]) + log(p2[ , , 5]))	
		
		mean.Deviance = mean(rowSums(D))
		
		avg.p = matrix(NA, dim(nof1$p)[2], dim(nof1$p)[3])
		for (i in 1:nrow(avg.p)) {
			for (j in 1:ncol(avg.p)) {
				avg.p[i, j] = mean(nof1$p[ , i, j])
			}
		}
		D.m = rep(NA, nrow(avg.p))
		for (i in 1:length(D.m)) {
			D.m[i] = log(avg.p[i, 1]^((round(colMeans(Y.sample))[i] == 1) * 1)) + log(avg.p[i, 2]^((round(colMeans(Y.sample))[i] == 2) * 1)) + 
			log(avg.p[i, 3]^((round(colMeans(Y.sample))[i] == 3) * 1)) + log(avg.p[i, 4]^((round(colMeans(Y.sample))[i] == 4) * 1)) + 
			log(avg.p[i, 5]^((round(colMeans(Y.sample))[i] == 5) * 1))
		}
		D.m = -2 * D.m
		Deviance.mean = sum(D.m)
		DIC.manual = (2*mean.Deviance) - Deviance.mean
	} else if (index == 3 && !nof1[[4]]) {
		Y.sample = rbind(nof1$Y.samp[ , 1, ], nof1$Y.samp[ , 2, ], nof1$Y.samp[ , 3, ])
		D = matrix(NA, dim(Y.sample)[1], dim(Y.sample)[2])

		p2 = nof1$p
		for (i in 1:6) {
			p2[ , , i] = nof1$p[ , , i]^(1*(Y.sample == i))
		}
		D = -2 * (log(p2[ , , 1]) + log(p2[ , , 2]) + log(p2[ , , 3]) + log(p2[ , , 4]) + log(p2[ , , 5]) + log(p2[ , , 6]))

		mean.Deviance = mean(rowSums(D))
		
		avg.p = matrix(NA, dim(nof1$p)[2], dim(nof1$p)[3])
		for (i in 1:nrow(avg.p)) {
			for (j in 1:ncol(avg.p)) {
				avg.p[i, j] = mean(nof1$p[ , i, j])
			}
		}
		D.m = rep(NA, nrow(avg.p))
		for (i in 1:length(D.m)) {
			D.m[i] = log(avg.p[i, 1]^((round(colMeans(Y.sample))[i] == 1) * 1)) + log(avg.p[i, 2]^((round(colMeans(Y.sample))[i] == 2) * 1)) + 
			log(avg.p[i, 3]^((round(colMeans(Y.sample))[i] == 3) * 1)) + log(avg.p[i, 4]^((round(colMeans(Y.sample))[i] == 4) * 1)) + 
			log(avg.p[i, 5]^((round(colMeans(Y.sample))[i] == 5) * 1)) + log(avg.p[i, 6]^((round(colMeans(Y.sample))[i] == 6) * 1))
		}
		D.m = -2 * D.m
		Deviance.mean = sum(D.m)
		DIC.manual = (2*mean.Deviance) - Deviance.mean
	}
	
	if (nof1[[4]]) {
		nof1$Dev = NULL
		DIC.manual = 1000
	}
	
	
	if (index %in% c(1, 7)) {		
		ForPPC = list(nof1$Outcome, nof1$mod.id, nof1$alpha, nof1$beta, nof1$Sd, nof1$Slope, nof1$Dev)
		names(ForPPC) = c("Outcome", "mod.id", "alpha", "beta", "Sd", "Slope", "Dev")
	} else if (index %in% c(2:6)) {
		ForPPC = list(nof1$Outcome, nof1$mod.id, nof1$beta, nof1$c, nof1$p, nof1$Slope)
		names(ForPPC) = c("Outcome", "mod.id", "beta", "c", "p", "Slope")
	}
	
	out = list(Results, Sigs, nof1$o.urun, ForPPC, 1000, 1000)
	names(out) = c("Results", "Sigs", "urun", "ForPPC", "DIC", "DIC.manual")
	
	return(out)
}

