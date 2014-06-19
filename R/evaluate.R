evaluate <- function(observations, Covs, mod.id, No_Neuropain) {

	nof1 = analyze(
	Pain = observations$Pain2,
	Fatigue = observations$Fatigue2,
	Drowsy = observations$Drowsy2,
	Sleep = observations$Sleep2,
	Thinking = observations$Thinking2,
	Constipation = observations$Constipation2,
	Neuropain = observations$Neuropain2,
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

	P025.1 = t(cbind(nof1$Pain$interval$P025, nof1$Fatigue$interval$P025, nof1$Drowsy$interval$P025,
	nof1$Sleep$interval$P025, nof1$Thinking$interval$P025, nof1$Constipation$interval$P025,
	nof1$'Neuropathic Pain'$interval$P025))

	Median.1 = t(cbind(nof1$Pain$interval$Median, nof1$Fatigue$interval$Median, nof1$Drowsy$interval$Median,
	nof1$Sleep$interval$Median, nof1$Thinking$interval$Median, nof1$Constipation$interval$Median,
	nof1$'Neuropathic Pain'$interval$Median))

	P975.1 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
	nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975,
	nof1$'Neuropathic Pain'$interval$P975))

	P975.1 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
	nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975,
	nof1$'Neuropathic Pain'$interval$P975))

	Prob1.1 = t(cbind(nof1$Pain$probs$'Proportion < -0.2', nof1$Fatigue$probs$'Proportion < -0.2',
	nof1$Drowsy$probs$'Proportion < -0.2', nof1$Sleep$probs$'Proportion < -0.2',
	nof1$Thinking$probs$'Proportion < -0.2', nof1$Constipation$probs$'Proportion < -0.2',
	nof1$'Neuropathic Pain'$probs$'Proportion < -0.2'))

	Prob2.1 = t(cbind(nof1$Pain$probs$'Proportion -0.2 - 0', nof1$Fatigue$probs$'Proportion -0.2 - 0',
	nof1$Drowsy$probs$'Proportion -0.2 - 0', nof1$Sleep$probs$'Proportion -0.2 - 0',
	nof1$Thinking$probs$'Proportion -0.2 - 0', nof1$Constipation$probs$'Proportion -0.2 - 0',
	nof1$'Neuropathic Pain'$probs$'Proportion -0.2 - 0'))

	Prob3.1 = t(cbind(nof1$Pain$probs$'Proportion 0 - 0.2', nof1$Fatigue$probs$'Proportion 0 - 0.2',
	nof1$Drowsy$probs$'Proportion 0 - 0.2', nof1$Sleep$probs$'Proportion 0 - 0.2',
	nof1$Thinking$probs$'Proportion 0 - 0.2', nof1$Constipation$probs$'Proportion 0 - 0.2',
	nof1$'Neuropathic Pain'$probs$'Proportion 0 - 0.2'))

	Prob4.1 = t(cbind(nof1$Pain$probs$'Proportion > 0.2', nof1$Fatigue$probs$'Proportion > 0.2',
	nof1$Drowsy$probs$'Proportion > 0.2', nof1$Sleep$probs$'Proportion > 0.2',
	nof1$Thinking$probs$'Proportion > 0.2', nof1$Constipation$probs$'Proportion > 0.2',
	nof1$'Neuropathic Pain'$probs$'Proportion > 0.2'))

	Results.1 = cbind(P025.1, Median.1, P975.1, Prob1.1, Prob2.1, Prob3.1, Prob4.1)
	colnames(Results.1) <- c("P025", "Median", "P975", "P(< - 0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
	rownames(Results.1) <- c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")
	
	failed.slope.array = array(0, dim = c(100, 3, 10))
	failed.slope.array[ , 1, ] = -50:49 + rnorm(100, 0, 1)
	failed.slope.array[ , 2, ] = -50:49 + rnorm(100, 0, 1)
	failed.slope.array[ , 3, ] = -50:49 + rnorm(100, 0, 1)
	
	if (nof1$uruns$Pain.urun == TRUE) {
		nof1$ForPPC$PainPosterior$Painbeta = -50:49
		nof1$ForPPC$PainPosterior$PainSlopes = failed.slope.array
	}
	if (nof1$uruns$Fatigue.urun == TRUE) {
		nof1$ForPPC$FatiguePosterior$Fatiguebeta = -50:49
		nof1$ForPPC$FatiguePosterior$FatigueSlopes = failed.slope.array
	}
	if (nof1$uruns$Sleep.urun == TRUE) {
		nof1$ForPPC$SleepPosterior$Sleepbeta = -50:49
		nof1$ForPPC$SleepPosterior$SleepSlopes = failed.slope.array
	}
	if (nof1$uruns$Drowsy.urun == TRUE) {
		nof1$ForPPC$DrowsyPosterior$Drowsybeta = -50:49
		nof1$ForPPC$DrowsyPosterior$DrowsySlopes = failed.slope.array
	}
	if (nof1$uruns$Thinking.urun == TRUE) {
		nof1$ForPPC$ThinkingPosterior$Thinkingbeta = -50:49
		nof1$ForPPC$ThinkingPosterior$ThinkingSlopes = failed.slope.array
	}
	if (nof1$uruns$Constipation.urun == TRUE) {
		nof1$ForPPC$ConstipationPosterior$Constipationbeta = -50:49
		nof1$ForPPC$ConstipationPosterior$ConstipationSlopes = failed.slope.array
	}
	if (nof1$uruns$Neuropain.urun == TRUE) {
		nof1$ForPPC$NeuropainPosterior$Neuropainbeta = -50:49
		nof1$ForPPC$NeuropainPosterior$NeuropainSlopes = failed.slope.array
	}
	
	if (!is.null(Covs)) { Covs = as.matrix(Covs) }
	
	if (length(dim(nof1$ForPPC$PainPosterior$PainSlopes)) == 2) {
		b = array(dim = c(dim(nof1$ForPPC$PainPosterior$PainSlopes)[1], 
			dim(nof1$ForPPC$PainPosterior$PainSlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$PainPosterior$PainSlopes
		nof1$ForPPC$PainPosterior$PainSlopes = b
	}
	if (length(dim(nof1$ForPPC$FatiguePosterior$FatigueSlopes)) == 2) {
		b = array(dim = c(dim(nof1$ForPPC$FatiguePosterior$FatigueSlopes)[1], 
			dim(nof1$ForPPC$FatiguePosterior$FatigueSlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$FatiguePosterior$FatigueSlopes
		nof1$ForPPC$FatiguePosterior$FatigueSlopes = b
	}
	if (length(dim(nof1$ForPPC$DrowsyPosterior$DrowsySlopes)) == 2) {
		b = array(dim = c(dim(nof1$ForPPC$DrowsyPosterior$DrowsySlopes)[1], 	
			dim(nof1$ForPPC$DrowsyPosterior$DrowsySlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$DrowsyPosterior$DrowsySlopes
		nof1$ForPPC$DrowsyPosterior$DrowsySlopes = b
	}
	if (length(dim(nof1$ForPPC$SleepPosterior$SleepSlopes)) == 2) {	
		b = array(dim = c(dim(nof1$ForPPC$SleepPosterior$SleepSlopes)[1], 
			dim(nof1$ForPPC$SleepPosterior$SleepSlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$SleepPosterior$SleepSlopes
		nof1$ForPPC$SleepPosterior$SleepSlopes = b
	}
	if (length(dim(nof1$ForPPC$ThinkingPosterior$ThinkingSlopes)) == 2) {
		b = array(dim = c(dim(nof1$ForPPC$ThinkingPosterior$ThinkingSlopes)[1], 
			dim(nof1$ForPPC$ThinkingPosterior$ThinkingSlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$ThinkingPosterior$ThinkingSlopes
		nof1$ForPPC$ThinkingPosterior$ThinkingSlopes = b
	}
	if (length(dim(nof1$ForPPC$ConstipationPosterior$ConstipationSlopes)) == 2) {	
		b = array(dim = c(dim(nof1$ForPPC$ConstipationPosterior$ConstipationSlopes)[1], 
			dim(nof1$ForPPC$ConstipationPosterior$ConstipationSlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$ConstipationPosterior$ConstipationSlopes
		nof1$ForPPC$ConstipationPosterior$ConstipationSlopes = b
	}
	if (length(dim(nof1$ForPPC$NeuropainPosterior$NeuropainSlopes)) == 2) {
		b = array(dim = c(dim(nof1$ForPPC$NeuropainPosterior$NeuropainSlopes)[1], 
			dim(nof1$ForPPC$NeuropainPosterior$NeuropainSlopes)[2], 1))
		b[ , , 1] = nof1$ForPPC$NeuropainPosterior$NeuropainSlopes
		nof1$ForPPC$NeuropainPosterior$NeuropainSlopes = b
	}
	
	if (mod.id == 1) { 
		Slopes.Sig = matrix(FALSE, nrow = 6, ncol = 1)
		if (!No_Neuropain) { Slopes.Sig = matrix(FALSE, nrow = 7, ncol = 1) }

	} else {
		if (mod.id %in% c(2, 3, 5.1, 5.2, 5.3)) {
			if (No_Neuropain) { Slopes.Sig = matrix(FALSE, nrow = 6, ncol = ncol(Covs))
			} else { Slopes.Sig = matrix(FALSE, nrow = 7, ncol = ncol(Covs)) }
		} else if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42)) {
			if (No_Neuropain) { Slopes.Sig = matrix(FALSE, nrow = 6, ncol = ncol(Covs)-6)
			} else { Slopes.Sig = matrix(FALSE, nrow = 7, ncol = ncol(Covs)-6) }
		}
		
		for (i in 1:ncol(Slopes.Sig)) {
			int.slope = quantile(nof1$ForPPC$PainPosterior$PainSlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[1, i] = TRUE } 
			
			int.slope = quantile(nof1$ForPPC$FatiguePosterior$FatigueSlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[2, i] = TRUE }
			
			int.slope = quantile(nof1$ForPPC$DrowsyPosterior$DrowsySlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[3, i] = TRUE }
			
			int.slope = quantile(nof1$ForPPC$SleepPosterior$SleepSlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[4, i] = TRUE }
			
			int.slope = quantile(nof1$ForPPC$ThinkingPosterior$ThinkingSlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[5, i] = TRUE }
			
			int.slope = quantile(nof1$ForPPC$ConstipationPosterior$ConstipationSlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[6, i] = TRUE }
			
			int.slope = quantile(nof1$ForPPC$NeuropainPosterior$NeuropainSlopes[ , , i], c(0.025, 0.975))
			if (int.slope[1] > 0 || int.slope[2] < 0) { Slopes.Sig[7, i] = TRUE }
		}
	} 
	
	Beta.Sig = rep(FALSE, 7)
	int.beta = quantile(nof1$ForPPC$PainPosterior$Painbeta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[1] = TRUE }
	
	int.beta = quantile(nof1$ForPPC$FatiguePosterior$Fatiguebeta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[2] = TRUE }
	
	int.beta = quantile(nof1$ForPPC$SleepPosterior$Sleepbeta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[3] = TRUE }
	
	int.beta = quantile(nof1$ForPPC$DrowsyPosterior$Drowsybeta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[4] = TRUE }
	
	int.beta = quantile(nof1$ForPPC$ThinkingPosterior$Thinkingbeta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[5] = TRUE }
	
	int.beta = quantile(nof1$ForPPC$ConstipationPosterior$Constipationbeta, c(0.025, 0.975))
	if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[6] = TRUE }
	
	if (!No_Neuropain) {
		int.beta = quantile(nof1$ForPPC$NeuropainPosterior$Neuropainbeta, c(0.025, 0.975))
		if (int.beta[1] > 0 || int.beta[2] < 0) { Beta.Sig[7] = TRUE }
	} else { Beta.Sig = Beta.Sig[-7] }

	Sigs.1 = cbind(Beta.Sig, Slopes.Sig)
		
	uruns.1 = c(nof1$uruns$Pain.urun, nof1$uruns$Fatigue.urun, nof1$uruns$Drowsy.urun, nof1$uruns$Sleep.urun,
	nof1$uruns$Thinking.urun, nof1$uruns$Constipation.urun, nof1$uruns$Neuropain.urun)
	
	DICs.1 = nof1$DICs
	
	if (No_Neuropain) {
		Results.1 = Results.1[-nrow(Results.1), ]
		DICs.1 = DICs.1[-length(DICs.1)]
		uruns.1 = uruns.1[-length(uruns.1)]
	}
	
	out = list("Results" = Results.1, "Sigs" = Sigs.1, "uruns" = uruns.1, "ForPPC" = nof1$ForPPC, "DIC" = DICs.1)
	
	return(out)
}
