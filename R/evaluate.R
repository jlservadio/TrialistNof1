evaluate = function(observations, Covs, i, No_Neuropain) {

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
	varprior.params=c(1, 5),
	path="",
	i = i)

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
	
	Diagnostics.1 = nof1$Diagnostics	
	
	uruns.1 = c(nof1$uruns$Pain.urun, nof1$uruns$Fatigue.urun, nof1$uruns$Drowsy.urun, nof1$uruns$Sleep.urun,
	nof1$uruns$Thinking.urun, nof1$uruns$Constipation.urun, nof1$uruns$Neuropain.urun)
	
	if (No_Neuropain) {
		Results.1 = Results.1[-nrow(Results.1), ]
		DICs.1 = DICs.1[-length(DICs.1)]
		uruns.1 = uruns.1[-length(uruns.1)]
	}
	
	out = list("Results" = Results.1, "Diagnostics" = Diagnostics.1, "uruns" = uruns.1, "Inputs" = nof1$Inputs, 
		"ForPPC" = nof1$ForPPC)
	
	return(out)
}

