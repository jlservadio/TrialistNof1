wrap <-
function(data, metadata) {

#####################################
# Converting Output to Desired Format
#####################################

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
		Thinking2[i] = data[["cognitiveFunctionSlowThinkingPrompt"]][i]
		Constipation2[i] = data[["constipationPrompt"]][i]
		Sharpness2[i] = data[["painSharpness"]][i]
		Hotness2[i] = data[["painHotness"]][i]
		Sensitivity2[i] = data[["painSensitivity"]][i]
	}
	
	Day2 = Day2 + 1
	
	Pain2 = Intensity2 + Enjoyment2 + Activity2
	Neuropain2 = Sharpness2 + Hotness2 + Sensitivity2
	
	Treat2 = rep(NA, nrow(data))
	for (i in 1:nrow(data)) {
		if (!is.na(data[["regimen"]][i]) & data[["regimen"]][i] == "A") {Treat2[i] = 0}
		if (!is.na(data[["regimen"]][i]) & data[["regimen"]][i] == "B") {Treat2[i] = 1}
	}
	
	Covs2 = cbind(Day2, Block2)
	
	observations = cbind(Day2, Pain2, Fatigue2, Drowsy2, Sleep2, Thinking2, 
		Constipation2, Neuropain2, Treat2, Block2)
	
	################
	# Analyzing Data
	################
	
	nof1 = analyze(
	Pain=Pain2,
	Fatigue=Fatigue2, 
	Drowsy=Drowsy2, 
	Sleep=Sleep2,
	Thinking=Thinking2, 
	Constipation=Constipation2,
	Neuropain=Neuropain2,
	Treat= Treat2, 
	score.range = c(30, 5, 6, 5, 5, 5, 30),
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
	varprior.params=c(0,5),
	path="")

	P025 = t(cbind(nof1$Pain$interval$P025, nof1$Fatigue$interval$P025, nof1$Drowsy$interval$P025,
		nof1$Sleep$interval$P025, nof1$Thinking$interval$P025, nof1$Constipation$interval$P025, 
		nof1$'Neuropathic Pain'$interval$P025))
		
	Median = t(cbind(nof1$Pain$interval$Median, nof1$Fatigue$interval$Median, nof1$Drowsy$interval$Median,
		nof1$Sleep$interval$Median, nof1$Thinking$interval$Median, nof1$Constipation$interval$Median, 
		nof1$'Neuropathic Pain'$interval$Median))
		
	P975 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
		nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
		nof1$'Neuropathic Pain'$interval$P975))
		
	P975 = t(cbind(nof1$Pain$interval$P975, nof1$Fatigue$interval$P975, nof1$Drowsy$interval$P975,
		nof1$Sleep$interval$P975, nof1$Thinking$interval$P975, nof1$Constipation$interval$P975, 
		nof1$'Neuropathic Pain'$interval$P975))
		
	Prob1 = t(cbind(nof1$Pain$probs$'Proportion < -0.2', nof1$Fatigue$probs$'Proportion < -0.2', 
		nof1$Drowsy$probs$'Proportion < -0.2', nof1$Sleep$probs$'Proportion < -0.2', 
		nof1$Thinking$probs$'Proportion < -0.2', nof1$Constipation$probs$'Proportion < -0.2', 
		nof1$'Neuropathic Pain'$probs$'Proportion < -0.2'))
		
	Prob2 = t(cbind(nof1$Pain$probs$'Proportion -0.2 - 0', nof1$Fatigue$probs$'Proportion -0.2 - 0', 
		nof1$Drowsy$probs$'Proportion -0.2 - 0', nof1$Sleep$probs$'Proportion -0.2 - 0', 
		nof1$Thinking$probs$'Proportion -0.2 - 0', nof1$Constipation$probs$'Proportion -0.2 - 0', 
		nof1$'Neuropathic Pain'$probs$'Proportion -0.2 - 0'))
		
	Prob3 = t(cbind(nof1$Pain$probs$'Proportion 0 - 0.2', nof1$Fatigue$probs$'Proportion 0 - 0.2', 
		nof1$Drowsy$probs$'Proportion 0 - 0.2', nof1$Sleep$probs$'Proportion 0 - 0.2', 
		nof1$Thinking$probs$'Proportion 0 - 0.2', nof1$Constipation$probs$'Proportion 0 - 0.2', 
		nof1$'Neuropathic Pain'$probs$'Proportion 0 - 0.2'))
		
	Prob4 = t(cbind(nof1$Pain$probs$'Proportion > 0.2', nof1$Fatigue$probs$'Proportion > 0.2', 
		nof1$Drowsy$probs$'Proportion > 0.2', nof1$Sleep$probs$'Proportion > 0.2', 
		nof1$Thinking$probs$'Proportion > 0.2', nof1$Constipation$probs$'Proportion > 0.2', 
		nof1$'Neuropathic Pain'$probs$'Proportion > 0.2'))
		
	Results = cbind(P025, Median, P975, Prob1, Prob2, Prob3, Prob4)
	colnames(Results) <- c("P025", "Median", "P975", "P(< - 0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
	rownames(Results) <- c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")

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
		
	y = list(pain, sleep_problems, constipation, drowsiness, thinking_problems, fatigue, neuropathic_pain)
	names(y) = c("pain", "sleep_problems", "constipation", "drowsiness", "thinking_problems", 
		"fatigue", "neuropathic_pain")

	return(y)
	
}
