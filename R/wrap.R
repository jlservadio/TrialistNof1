wrap <-
function(data, metadata) {

#####################################
# Converting Output to Desired Format
#####################################

	unsuccessful_run = FALSE

	Intensity2 = Enjoyment2 = Activity2 = Fatigue2 = Drowsy2 = Sleep2 =
	Thinking2 = Constipation2 = Sharpness2 = Hotness2 = Sensitivity2 =
	Block2 = Day2 = rep(NA, nrow(data))

	for (i in 1:nrow(data)) {
		Day2[i] = as.numeric(as.Date(data[["timestamp"]][i]) - as.Date(metadata[["trial_start_date"]]))
		Block2[i] = data[["cycle"]][i]
		Intensity2[i] = data[["averagePainIntensity"]][i]
		Enjoyment2[i] = data[["enjoymentOfLife"]][i]
		Activity2[i] = data[["generalActivity"]][i]
		Fatigue2[i] = data[["fatiguePrompt"]][i]
		Drowsy2[i] = data[["drowsinessPrompt"]][i]
		Sleep2[i] = data[["sleepDisturbancePrompt"]][i]
		Constipation2[i] = data[["constipationPrompt"]][i]
		Sharpness2[i] = data[["painSharpness"]][i]
		Hotness2[i] = data[["painHotness"]][i]
		Sensitivity2[i] = data[["painSensitivity"]][i]
		
		if (metadata[["cognitiveFunctionPromptKey"]] == "cognitiveFunctionFoggyThinkingPrompt") {
			Thinking2[i] = data[["cognitiveFunctionFoggyThinkingPrompt"]][i]
		} else if (metadata[["cognitiveFunctionPromptKey"]] == "cognitiveFunctionWorkingHarderPrompt") {
			Thinking2[i] = data[["cognitiveFunctionWorkingHarderPrompt"]][i]
		}
	}
	
	Block.2 = Block.3 = Block.4 = rep(0, length(Block2))
	for (i in 1:length(Block2)) {
		if (Block2[i] == 2) {Block.2[i] = 1
		} else if (Block2[i] == 3) {Block.3[i] = 1
		} else if (Block2[i] == 4) {Block.4[i] = 1}
	}
	
	Days_in_Trial = as.numeric(as.Date(metadata[["trial_end_date"]]) - as.Date(metadata[["trial_start_date"]]))

	Day2 = Day2 + 1

	Pain2 = Intensity2 + Enjoyment2 + Activity2
	Neuropain2 = Sharpness2 + Hotness2 + Sensitivity2

	Treat2 = rep(NA, nrow(data))
	for (i in 1:nrow(data)) {
		if (!is.na(data[["regimen"]][i]) & data[["regimen"]][i] == "A") {Treat2[i] = 0}
		if (!is.na(data[["regimen"]][i]) & data[["regimen"]][i] == "B") {Treat2[i] = 1}
	}
	
	Pain2.lag = Fatigue2.lag = Drowsy2.lag = Sleep2.lag = Thinking2.lag = Constipation2.lag = Neuropain2.lag = 
		rep(NA, length(Pain2))
	
	for (i in 2:length(Pain2)) {
		Pain2.lag[i] = Pain2[i - 1]
		Fatigue2.lag[i] = Fatigue2[i - 1]
		Drowsy2.lag[i] = Drowsy2[i - 1]
		Sleep2.lag[i] = Sleep2[i - 1]
		Thinking2.lag[i] = Thinking2[i - 1]
		Constipation2.lag[i] = Constipation2[i - 1]
		Neuropain2.lag[i] = Neuropain2[i - 1]
	}

	observations = cbind(Day2, Pain2, Fatigue2, Drowsy2, Sleep2, Thinking2,
		Constipation2, Neuropain2, Treat2, Block2)
		
	if (max(Pain2) == min(Pain2)) {unsuccessful_run = TRUE}
	if (max(Fatigue2) == min(Fatigue2)) {unsuccessful_run = TRUE}
	if (max(Drowsy2) == min(Drowsy2)) {unsuccessful_run = TRUE}
	if (max(Sleep2) == min(Sleep2)) {unsuccessful_run = TRUE}
	if (max(Thinking2) == min(Thinking2)) {unsuccessful_run = TRUE}
	if (max(Constipation2) == min(Constipation2)) {unsuccessful_run = TRUE}
	if (max(Neuropain2) == min(Neuropain2)) {unsuccessful_run = TRUE}
	
	alphaprior = list("norm",0,1e-6)
	beta.norm.prior = list("norm",0,1e-6)
	beta.ord.prior = list("norm",0,1e-6)

	####################################
	# Analyzing Data for Null Covariates
	####################################
	
	nof1 = NULL
	
	if (unsuccessful_run == FALSE) {	
		nof1 = analyze(
		Pain=Pain2,
		Fatigue=Fatigue2, 
		Drowsy=Drowsy2, 
		Sleep=Sleep2,
		Thinking=Thinking2, 
		Constipation=Constipation2,
		Neuropain=Neuropain2,
		Treat= Treat2, 
		score.range = c(30, 5, 6, 5, 5, 5,30),
		Covs = NULL,
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
		varprior.params=c(2,4),
		path="", 
		i = 1)
	}
	
	if (!is.null(nof1)) {
		unsuccessful_run = FALSE

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
		
		DICs.1 = c(nof1$DICs$Pain.DIC, nof1$DICs$Fatigue.DIC, nof1$DICs$Drowsy.DIC, nof1$DICs$Sleep.DIC, 
			nof1$DICs$Thinking.DIC, nof1$DICs$Constipation.DIC, nof1$DICs$Neuropain.DIC)
	} else {
		unsuccessful_run = TRUE
	}
		
	#####################################
	# Analyzing Data for Block Covariates
	#####################################
	
	if (unsuccessful_run == FALSE) {
	
		nof1 = NULL
	
		nof1 = analyze(
		Pain=Pain2,
		Fatigue=Fatigue2, 
		Drowsy=Drowsy2, 
		Sleep=Sleep2,
		Thinking=Thinking2, 
		Constipation=Constipation2,
		Neuropain=Neuropain2,
		Treat= Treat2, 
		score.range = c(30, 5, 6, 5, 5, 5,30),
		Covs = cbind(Block.2, Block.3, Block.4),
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
		varprior.params=c(2,4),
		path="", 
		i = 2)
		
		if (!is.null(nof1)) {
		
			P025.2 = t(cbind(nof1$Pain$interval$P025, nof1$Fatigue$interval$P025, nof1$Drowsy$interval$P025,
				nof1$Sleep$interval$P025, nof1$Thinking$interval$P025, nof1$Constipation$interval$P025, 
				nof1$'Neuropathic Pain'$interval$P025))
				
			Median.2 = t(cbind(nof1$Pain$interval$Median, nof1$Fatigue$interval$Median, nof1$Drowsy$interval$Median,
				nof1$Sleep$interval$Median, nof1$Thinking$interval$Median, nof1$Constipation$interval$Median, 
				nof1$'Neuropathic Pain'$interval$Median))
				
			P975.2 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
				nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
				nof1$'Neuropathic Pain'$interval$P975))
				
			P975.2 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
				nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
				nof1$'Neuropathic Pain'$interval$P975))
				
			Prob1.2 = t(cbind(nof1$Pain$probs$'Proportion < -0.2', nof1$Fatigue$probs$'Proportion < -0.2', 
				nof1$Drowsy$probs$'Proportion < -0.2', nof1$Sleep$probs$'Proportion < -0.2', 
				nof1$Thinking$probs$'Proportion < -0.2', nof1$Constipation$probs$'Proportion < -0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion < -0.2'))
				
			Prob2.2 = t(cbind(nof1$Pain$probs$'Proportion -0.2 - 0', nof1$Fatigue$probs$'Proportion -0.2 - 0', 
				nof1$Drowsy$probs$'Proportion -0.2 - 0', nof1$Sleep$probs$'Proportion -0.2 - 0', 
				nof1$Thinking$probs$'Proportion -0.2 - 0', nof1$Constipation$probs$'Proportion -0.2 - 0', 
				nof1$'Neuropathic Pain'$probs$'Proportion -0.2 - 0'))
				
			Prob3.2 = t(cbind(nof1$Pain$probs$'Proportion 0 - 0.2', nof1$Fatigue$probs$'Proportion 0 - 0.2', 
				nof1$Drowsy$probs$'Proportion 0 - 0.2', nof1$Sleep$probs$'Proportion 0 - 0.2', 
				nof1$Thinking$probs$'Proportion 0 - 0.2', nof1$Constipation$probs$'Proportion 0 - 0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion 0 - 0.2'))
				
			Prob4.2 = t(cbind(nof1$Pain$probs$'Proportion > 0.2', nof1$Fatigue$probs$'Proportion > 0.2', 
				nof1$Drowsy$probs$'Proportion > 0.2', nof1$Sleep$probs$'Proportion > 0.2', 
				nof1$Thinking$probs$'Proportion > 0.2', nof1$Constipation$probs$'Proportion > 0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion > 0.2'))
				
			Results.2 = cbind(P025.2, Median.2, P975.2, Prob1.2, Prob2.2, Prob3.2, Prob4.2)
			colnames(Results.2) <- c("P025", "Median", "P975", "P(< - 0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
			rownames(Results.2) <- c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")
			
			DICs.2 = c(nof1$DICs$Pain.DIC, nof1$DICs$Fatigue.DIC, nof1$DICs$Drowsy.DIC, nof1$DICs$Sleep.DIC, 
				nof1$DICs$Thinking.DIC, nof1$DICs$Constipation.DIC, nof1$DICs$Neuropain.DIC)
		} else {
			unsuccessful_run = TRUE
		}
	}
	
	###################################
	# Analyzing Data for Day Covariates
	###################################
	
	if (unsuccessful_run == FALSE) {
	
		nof1 = NULL
	
		nof1 = analyze(
		Pain=Pain2,
		Fatigue=Fatigue2, 
		Drowsy=Drowsy2, 
		Sleep=Sleep2,
		Thinking=Thinking2, 
		Constipation=Constipation2,
		Neuropain=Neuropain2,
		Treat= Treat2, 
		score.range = c(30, 5, 6, 5, 5, 5,30),
		Covs = Day2,
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
		varprior.params=c(2,4),
		path="", 
		i = 3)
		
		if (!is.null(nof1)) {
		
			P025.3 = t(cbind(nof1$Pain$interval$P025, nof1$Fatigue$interval$P025, nof1$Drowsy$interval$P025,
				nof1$Sleep$interval$P025, nof1$Thinking$interval$P025, nof1$Constipation$interval$P025, 
				nof1$'Neuropathic Pain'$interval$P025))
				
			Median.3 = t(cbind(nof1$Pain$interval$Median, nof1$Fatigue$interval$Median, nof1$Drowsy$interval$Median,
				nof1$Sleep$interval$Median, nof1$Thinking$interval$Median, nof1$Constipation$interval$Median, 
				nof1$'Neuropathic Pain'$interval$Median))
				
			P975.3 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
				nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
				nof1$'Neuropathic Pain'$interval$P975))
				
			P975.3 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
				nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
				nof1$'Neuropathic Pain'$interval$P975))
				
			Prob1.3 = t(cbind(nof1$Pain$probs$'Proportion < -0.2', nof1$Fatigue$probs$'Proportion < -0.2', 
				nof1$Drowsy$probs$'Proportion < -0.2', nof1$Sleep$probs$'Proportion < -0.2', 
				nof1$Thinking$probs$'Proportion < -0.2', nof1$Constipation$probs$'Proportion < -0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion < -0.2'))
				
			Prob2.3 = t(cbind(nof1$Pain$probs$'Proportion -0.2 - 0', nof1$Fatigue$probs$'Proportion -0.2 - 0', 
				nof1$Drowsy$probs$'Proportion -0.2 - 0', nof1$Sleep$probs$'Proportion -0.2 - 0', 
				nof1$Thinking$probs$'Proportion -0.2 - 0', nof1$Constipation$probs$'Proportion -0.2 - 0', 
				nof1$'Neuropathic Pain'$probs$'Proportion -0.2 - 0'))
				
			Prob3.3 = t(cbind(nof1$Pain$probs$'Proportion 0 - 0.2', nof1$Fatigue$probs$'Proportion 0 - 0.2', 
				nof1$Drowsy$probs$'Proportion 0 - 0.2', nof1$Sleep$probs$'Proportion 0 - 0.2', 
				nof1$Thinking$probs$'Proportion 0 - 0.2', nof1$Constipation$probs$'Proportion 0 - 0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion 0 - 0.2'))
				
			Prob4.3 = t(cbind(nof1$Pain$probs$'Proportion > 0.2', nof1$Fatigue$probs$'Proportion > 0.2', 
				nof1$Drowsy$probs$'Proportion > 0.2', nof1$Sleep$probs$'Proportion > 0.2', 
				nof1$Thinking$probs$'Proportion > 0.2', nof1$Constipation$probs$'Proportion > 0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion > 0.2'))
				
			Results.3 = cbind(P025.3, Median.3, P975.3, Prob1.3, Prob2.3, Prob3.3, Prob4.3)
			colnames(Results.3) <- c("P025", "Median", "P975", "P(< - 0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
			rownames(Results.3) <- c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")
			
			DICs.3 = c(nof1$DICs$Pain.DIC, nof1$DICs$Fatigue.DIC, nof1$DICs$Drowsy.DIC, nof1$DICs$Sleep.DIC, 
				nof1$DICs$Thinking.DIC, nof1$DICs$Constipation.DIC, nof1$DICs$Neuropain.DIC)
		} else {
			unsuccessful_run = TRUE
		}
	}
	
	######################################
	# Analyzing Data for Day and Day*Treat
	######################################
	
	if (unsuccessful_run == FALSE) {
	
		nof1 = NULL
	
		nof1 = analyze(
		Pain=Pain2,
		Fatigue=Fatigue2, 
		Drowsy=Drowsy2, 
		Sleep=Sleep2,
		Thinking=Thinking2, 
		Constipation=Constipation2,
		Neuropain=Neuropain2,
		Treat= Treat2, 
		score.range = c(30, 5, 6, 5, 5, 5,30),
		Covs = cbind(Day2, Treat2*Day2),
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
		varprior.params=c(2,4),
		path="", 
		i = 4)
		
		if (!is.null(nof1)) {
		
			P025.4 = t(cbind(nof1$Pain$interval$P025, nof1$Fatigue$interval$P025, nof1$Drowsy$interval$P025,
				nof1$Sleep$interval$P025, nof1$Thinking$interval$P025, nof1$Constipation$interval$P025, 
				nof1$'Neuropathic Pain'$interval$P025))
				
			Median.4 = t(cbind(nof1$Pain$interval$Median, nof1$Fatigue$interval$Median, nof1$Drowsy$interval$Median,
				nof1$Sleep$interval$Median, nof1$Thinking$interval$Median, nof1$Constipation$interval$Median, 
				nof1$'Neuropathic Pain'$interval$Median))
				
			P975.4 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
				nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
				nof1$'Neuropathic Pain'$interval$P975))
				
			P975.4 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
				nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
				nof1$'Neuropathic Pain'$interval$P975))
				
			Prob1.4 = t(cbind(nof1$Pain$probs$'Proportion < -0.2', nof1$Fatigue$probs$'Proportion < -0.2', 
				nof1$Drowsy$probs$'Proportion < -0.2', nof1$Sleep$probs$'Proportion < -0.2', 
				nof1$Thinking$probs$'Proportion < -0.2', nof1$Constipation$probs$'Proportion < -0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion < -0.2'))
				
			Prob2.4 = t(cbind(nof1$Pain$probs$'Proportion -0.2 - 0', nof1$Fatigue$probs$'Proportion -0.2 - 0', 
				nof1$Drowsy$probs$'Proportion -0.2 - 0', nof1$Sleep$probs$'Proportion -0.2 - 0', 
				nof1$Thinking$probs$'Proportion -0.2 - 0', nof1$Constipation$probs$'Proportion -0.2 - 0', 
				nof1$'Neuropathic Pain'$probs$'Proportion -0.2 - 0'))
				
			Prob3.4 = t(cbind(nof1$Pain$probs$'Proportion 0 - 0.2', nof1$Fatigue$probs$'Proportion 0 - 0.2', 
				nof1$Drowsy$probs$'Proportion 0 - 0.2', nof1$Sleep$probs$'Proportion 0 - 0.2', 
				nof1$Thinking$probs$'Proportion 0 - 0.2', nof1$Constipation$probs$'Proportion 0 - 0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion 0 - 0.2'))
				
			Prob4.4 = t(cbind(nof1$Pain$probs$'Proportion > 0.2', nof1$Fatigue$probs$'Proportion > 0.2', 
				nof1$Drowsy$probs$'Proportion > 0.2', nof1$Sleep$probs$'Proportion > 0.2', 
				nof1$Thinking$probs$'Proportion > 0.2', nof1$Constipation$probs$'Proportion > 0.2', 
				nof1$'Neuropathic Pain'$probs$'Proportion > 0.2'))
				
			Results.4 = cbind(P025.4, Median.4, P975.4, Prob1.4, Prob2.4, Prob3.4, Prob4.4)
			colnames(Results.4) <- c("P025", "Median", "P975", "P(< - 0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
			rownames(Results.4) <- c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")
			
			DICs.4 = c(nof1$DICs$Pain.DIC, nof1$DICs$Fatigue.DIC, nof1$DICs$Drowsy.DIC, nof1$DICs$Sleep.DIC, 
				nof1$DICs$Thinking.DIC, nof1$DICs$Constipation.DIC, nof1$DICs$Neuropain.DIC)
		} else {
			unsuccessful_run = TRUE
		}
	}
	
	if (unsuccessful_run == FALSE) {
	
		##########################
		# Selecting the Best Model
		##########################
		
		Better = Results.1	
		s.b = sum(DICs.1)
		Better.Model = 1
		
		s.1 = sum(DICs.1)
		s.2 = sum(DICs.2)
		s.3 = sum(DICs.3)
		s.4 = sum(DICs.4) 
		
		if (s.2 < s.b - 3) {
			Better = Results.2
			Better.Model = 2
			s.b = s.2
		}
		
		if (s.3 < s.b - 3) {
			Better = Results.3
			Better.Model = 3
			s.b = s.3
		}
		
		if (s.4 < s.b - 3) {
			Better = Results.4
			Better.Model = 4
			s.b = s.4
		}
		
		Results = Better

		###########################
		# Rearranging nof1 Output
		###########################

		Results_mod = matrix(NA, ncol = 10, nrow = 7)
		for (i in 1:nrow(Results_mod)) {
			if (Results[i, 2] < 0) {Results_mod[i, 1] = "B"}
			if (Results[i, 2] > 0) {Results_mod[i, 1] = "A"}
			if (Results[i, 2] == 0) {Results_mod[i, 1] = "Neither"}

			if (Results[i, 3] < 0) {Results_mod[i, 4] = "B"}
			if (Results[i, 3] > 0) {Results_mod[i, 4] = "A"}

			if (Results[i, 1] < 0) {Results_mod[i, 6] = "B"}
			if (Results[i, 1] > 0) {Results_mod[i, 6] = "A"}
			
			Results_mod[i, 2] = abs(Results[i, 2])
			Results_mod[i, 3] = abs(Results[i, 3])
			Results_mod[i, 5] = abs(Results[i, 1])
			Results_mod[i, 7] = Results[i, 4]
			Results_mod[i, 8] = Results[i, 5]
			Results_mod[i, 9] = Results[i, 7]
			Results_mod[i, 10] = Results[i, 6]
		}

		colnames(Results_mod) = c("more_effective_regimen", "median_effect", "upper_bound", "upper_bound_regimen", 
			"lower_bound", "lower_bound_regimen", "b_clinically_better", "b_marginally_better", "a_clinically_better", 
			"a_marginally_better")
		rownames(Results_mod) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems", 
			"constipation", "neuropathic_pain")

		####################
		# The list
		####################
			
		graph_5 = list("more_effective_regimen" = Results_mod[1, 1], "median_effect" = as.numeric(Results_mod[1, 2]), 
			"upper_bound" = as.numeric(Results_mod[1, 3]), "upper_bound_regimen" = Results_mod[1, 4], 
			"lower_bound" = as.numeric(Results_mod[1, 5]), "lower_bound_regimen" = Results_mod[1, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[1, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[1, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[1, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[1, 10]))
			
		pain = list(graph_5, graph_6)
		names(pain) = c("graph_5", "graph_6")
			
		graph_5 = list("more_effective_regimen" = Results_mod[4, 1], "median_effect" = as.numeric(Results_mod[4, 2]), 
			"upper_bound" = as.numeric(Results_mod[4, 3]), "upper_bound_regimen" = Results_mod[4, 4], 
			"lower_bound" = as.numeric(Results_mod[4, 5]), "lower_bound_regimen" = Results_mod[4, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[4, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[4, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[4, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[4, 10]))
			
		sleep_problems = list(graph_5, graph_6)
		names(sleep_problems) = c("graph_5", "graph_6")

		graph_5 = list("more_effective_regimen" = Results_mod[6, 1], "median_effect" = as.numeric(Results_mod[6, 2]), 
			"upper_bound" = as.numeric(Results_mod[6, 3]), "upper_bound_regimen" = Results_mod[6, 4], 
			"lower_bound" = as.numeric(Results_mod[6, 5]), "lower_bound_regimen" = Results_mod[6, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[6, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[6, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[6, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[6, 10]))
			
		constipation = list(graph_5, graph_6)
		names(constipation) = c("graph_5", "graph_6")

		graph_5 = list("more_effective_regimen" = Results_mod[3, 1], "median_effect" = as.numeric(Results_mod[3, 2]), 
			"upper_bound" = as.numeric(Results_mod[3, 3]), "upper_bound_regimen" = Results_mod[3, 4], 
			"lower_bound" = as.numeric(Results_mod[3, 5]), "lower_bound_regimen" = Results_mod[3, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[3, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[3, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[3, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[3, 10]))
			
		drowsiness = list(graph_5, graph_6)
		names(drowsiness) = c("graph_5", "graph_6")

		graph_5 = list("more_effective_regimen" = Results_mod[5, 1], "median_effect" = as.numeric(Results_mod[5, 2]), 
			"upper_bound" = as.numeric(Results_mod[5, 3]), "upper_bound_regimen" = Results_mod[5, 4], 
			"lower_bound" = as.numeric(Results_mod[5, 5]), "lower_bound_regimen" = Results_mod[5, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[5, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[5, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[5, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[5, 10]))
			
		thinking_problems = list(graph_5, graph_6)
		names(thinking_problems) = c("graph_5", "graph_6")
			
		graph_5 = list("more_effective_regimen" = Results_mod[2, 1], "median_effect" = as.numeric(Results_mod[2, 2]), 
			"upper_bound" = as.numeric(Results_mod[2, 3]), "upper_bound_regimen" = Results_mod[2, 4], 
			"lower_bound" = as.numeric(Results_mod[2, 5]), "lower_bound_regimen" = Results_mod[2, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[2, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[2, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[2, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[2, 10]))
			
		fatigue = list(graph_5, graph_6)
		names(fatigue) = c("graph_5", "graph_6")	

		graph_5 = list("more_effective_regimen" = Results_mod[7, 1], "median_effect" = as.numeric(Results_mod[7, 2]), 
			"upper_bound" = as.numeric(Results_mod[7, 3]), "upper_bound_regimen" = Results_mod[7, 4], 
			"lower_bound" = as.numeric(Results_mod[7, 5]), "lower_bound_regimen" = Results_mod[7, 6])
			
		graph_6 = list("b_clinically_better" = as.numeric(Results_mod[7, 7]), 
			"b_marginally_better" = as.numeric(Results_mod[7, 8]), 
			"a_clinically_better" = as.numeric(Results_mod[7, 9]), 
			"a_marginally_better" = as.numeric(Results_mod[7, 10]))
			
		neuropathic_pain = list(graph_5, graph_6)
		names(neuropathic_pain) = c("graph_5", "graph_6")
			
		srun.out = list(pain, sleep_problems, constipation, drowsiness, thinking_problems, fatigue, neuropathic_pain, 
			Results, Better.Model, c(sum(DICs.1), sum(DICs.2)))
		names(srun.out) = c("pain", "sleep_problems", "constipation", "drowsiness", "thinking_problems", "fatigue", 
			"neuropathic_pain", "Results", "Better_Model", "DIC_Values")
	} else {
	
		###############################
		# metadata for unsuccessful run
		###############################
		
		graph_5 = list("more_effective_regimen" = "Neither", "median_effect" = 0, 
			"upper_bound" = 1, "upper_bound_regimen" = "A", 
			"lower_bound" = -1, "lower_bound_regimen" = "B")
			
		graph_6 = list("b_clinically_better" = 0, 
			"b_marginally_better" = 0.5, 
			"a_clinically_better" = 0, 
			"a_marginally_better" = 0.5)
			
		pain = list(graph_5, graph_6)
		names(pain) = c("graph_5", "graph_6")
			
		sleep_problems = list(graph_5, graph_6)
		names(sleep_problems) = c("graph_5", "graph_6")

		constipation = list(graph_5, graph_6)
		names(constipation) = c("graph_5", "graph_6")

		drowsiness = list(graph_5, graph_6)
		names(drowsiness) = c("graph_5", "graph_6")

		thinking_problems = list(graph_5, graph_6)
		names(thinking_problems) = c("graph_5", "graph_6")
			
		fatigue = list(graph_5, graph_6)
		names(fatigue) = c("graph_5", "graph_6")	

		neuropathic_pain = list(graph_5, graph_6)
		names(neuropathic_pain) = c("graph_5", "graph_6")
		
		urun.out = list(observations, alphaprior, beta.norm.prior, beta.ord.prior, pain, sleep_problems, 
			constipation, drowsiness, thinking_problems, fatigue, neuropathic_pain, unsuccessful_run)
		names(urun.out) = c("observations", "alpha prior", "beta prior (norm)", "beta prior (ord)", "pain", "sleep problems", 
			"constipation", "drowsiness", "thinking problems", "fatigue", "neuropathic pain", "urun")
	}
		
	##################################
	# Returning the appropriate output
	##################################

	if (unsuccessful_run == FALSE) {
		return(srun.out)	
	} else {
		return(urun.out)
	}
}

