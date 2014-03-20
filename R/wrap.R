wrap <- function(data, metadata) {

	#####################################
	# Converting Output to Desired Format
	#####################################

	Days_in_Trial = as.numeric(as.Date(metadata[["trial_end_date"]]) - as.Date(metadata[["trial_start_date"]])) + 1

	Intensity2 = Enjoyment2 = Activity2 = Fatigue2 = Drowsy2 = Sleep2 =
	Thinking2 = Constipation2 = Sharpness2 = Hotness2 = Sensitivity2 =
	Block2 = Day2 = rep(NA, dim(data)[1])

	for (i in 1:dim(data)[1]) {
		Day2[i] = as.numeric(as.Date(data[["timestamp"]][i]) - as.Date(metadata[["trial_start_date"]]))
		Block2[i] = data[["cycle"]][i]
		Intensity2[i] = data[["averagePainIntensity"]][i]
		Enjoyment2[i] = data[["enjoymentOfLife"]][i]
		Activity2[i] = data[["generalActivity"]][i]
		Fatigue2[i] = data[["fatiguePrompt"]][i]
		Drowsy2[i] = data[["drowsinessPrompt"]][i]
		Sleep2[i] = data[["sleepDisturbancePrompt"]][i]
		Constipation2[i] = data[["constipationPrompt"]][i]
		if (!is.null(data[["painSharpness"]])) {Sharpness2[i] = data[["painSharpness"]][i]}
		if (!is.null(data[["painHotness"]])) {Hotness2[i] = data[["painHotness"]][i]}
		if (!is.null(data[["painSensitivity"]])) {Sensitivity2[i] = data[["painSensitivity"]][i]}

		if (metadata[["cognitiveFunctionPromptKey"]] == "cognitiveFunctionFoggyThinkingPrompt") {
			Thinking2[i] = data[["cognitiveFunctionFoggyThinkingPrompt"]][i]
		} else if (metadata[["cognitiveFunctionPromptKey"]] == "cognitiveFunctionWorkingHarderPrompt") {
			Thinking2[i] = data[["cognitiveFunctionWorkingHarderPrompt"]][i]
		}
	}

	Day2 = Day2[!is.na(Day2)]
	Block2 = Block2[!is.na(Block2)]
	Intensity2 = Intensity2[!is.na(Intensity2)]
	Enjoyment2 = Enjoyment2[!is.na(Enjoyment2)]
	Activity2 = Activity2[!is.na(Activity2)]
	Fatigue2 = Fatigue2[!is.na(Fatigue2)]
	Drowsy2 = Drowsy2[!is.na(Drowsy2)]
	Sleep2 = Sleep2[!is.na(Sleep2)]
	Constipation2 = Constipation2[!is.na(Constipation2)]
	if (!is.null(data[["painSharpness"]])) {Sharpness2 = Sharpness2[!is.na(Sharpness2)]}
	if (!is.null(data[["painHotness"]])) {Hotness2 = Hotness2[!is.na(Hotness2)]}
	if (!is.null(data[["painSensitivity"]])) {Sensitivity2 = Sensitivity2[!is.na(Sensitivity2)]}
	Thinking2 = Thinking2[!is.na(Thinking2)]

	num.obs = length(Day2)
	stopifnot(length(Day2) == num.obs, length(Block2) == num.obs, length(Intensity2) == num.obs, 
		length(Enjoyment2) == num.obs, length(Activity2) == num.obs, length(Fatigue2) == num.obs, 
		length(Drowsy2) == num.obs, length(Sleep2) == num.obs, length(Constipation2) == num.obs, 
		length(Sharpness2) == num.obs, length(Hotness2) == num.obs, length(Sensitivity2) == num.obs, 
		length(Thinking2) == num.obs)

	Block.2 = Block.3 = Block.4 = rep(0, length(Block2))
	for (i in 1:length(Block2)) {
		if (Block2[i] == 2) {Block.2[i] = 1
		} else if (Block2[i] == 3) {Block.3[i] = 1
		} else if (Block2[i] == 4) {Block.4[i] = 1}
	}

	Day2 = Day2 + 1

	Pain2 = Intensity2 + Enjoyment2 + Activity2
	if (!is.null(data[["painSharpness"]])) {
		Neuropain2 = Sharpness2 + Hotness2 + Sensitivity2
	} else {
		Neuropain2 = rep(15, length(Day2))
	}

	Treat2 = rep(NA, dim(data)[1])
	for (i in 1:Days_in_Trial) {
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

	observations = cbind(Day2, Pain2, Fatigue2, Drowsy2, Sleep2, Thinking2, Constipation2, Neuropain2, Treat2, Block2)

	####################################
	# Analyzing Data for Null Covariates
	####################################

	nof1 = analyze(
	Pain=Pain2,
	Fatigue=Fatigue2,
	Drowsy=Drowsy2,
	Sleep=Sleep2,
	Thinking=Thinking2,
	Constipation=Constipation2,
	Neuropain=Neuropain2,
	Treat= Treat2,
	score.range = c(30, 4, 5, 4, 4, 4,30),
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
	colnames(Results.1) <- c("P025", "Median", "P975", "P(< - 0.2)(B)", "P(-0.2 - 0)(B)", "P(0 - 0.2)(A)", "P(> 0.2)(A)")
	rownames(Results.1) <- c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")

	DICs.1 = c(nof1$DICs$Pain.DIC, nof1$DICs$Fatigue.DIC, nof1$DICs$Drowsy.DIC, nof1$DICs$Sleep.DIC,
	nof1$DICs$Thinking.DIC, nof1$DICs$Constipation.DIC, nof1$DICs$Neuropain.DIC)

	uruns.1 = c(nof1$uruns$Pain.urun, nof1$uruns$Fatigue.urun, nof1$uruns$Drowsy.urun, nof1$uruns$Sleep.urun,
	nof1$uruns$Thinking.urun, nof1$uruns$Constipation.urun, nof1$uruns$Neuropain.urun)

	meta.data = list()
	meta.data[[1]] = nof1[["metadata"]]

	#####################################
	# Analyzing Data for Block Covariates
	#####################################



	###################################
	# Analyzing Data for Day Covariates
	###################################



	#################################################
	# Analyzing Data for Day and Day*Treat Covariates
	#################################################



	##########################
	# Selecting the Best Model
	##########################

	Better = Results.1
	s.b = sum(DICs.1)
	Better.Model = 1
	uruns.b = uruns.1

	s.1 = sum(DICs.1)

	Results = Better
	uruns = uruns.b

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
		Results_mod[i, 7] = Results[i, 7]
		Results_mod[i, 8] = Results[i, 6]
		Results_mod[i, 9] = Results[i, 4]
		Results_mod[i, 10] = Results[i, 5]
	}

	colnames(Results_mod) = c("more_effective_regimen", "median_effect", "upper_bound", "upper_bound_regimen",
		"lower_bound", "lower_bound_regimen", "a_clinically_better", "a_marginally_better", "b_clinically_better",
		"b_marginally_better")
	rownames(Results_mod) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems",
		"constipation", "neuropathic_pain")

	####################
	# The list
	####################

	graph_5 = list("more_effective_regimen" = Results_mod[1, 1], "median_effect" = as.numeric(Results_mod[1, 2]),
		"upper_bound" = as.numeric(Results_mod[1, 3]), "upper_bound_regimen" = Results_mod[1, 4],
		"lower_bound" = as.numeric(Results_mod[1, 5]), "lower_bound_regimen" = Results_mod[1, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[1, 7]),
		"a_marginally_better" = as.numeric(Results_mod[1, 8]),
		"b_clinically_better" = as.numeric(Results_mod[1, 9]),
		"b_marginally_better" = as.numeric(Results_mod[1, 10]))

	pain = list(as.logical(1 - uruns[1]), graph_5, graph_6)
	names(pain) = c("successful_run", "graph_5", "graph_6")

	graph_5 = list("more_effective_regimen" = Results_mod[4, 1], "median_effect" = as.numeric(Results_mod[4, 2]),
		"upper_bound" = as.numeric(Results_mod[4, 3]), "upper_bound_regimen" = Results_mod[4, 4],
		"lower_bound" = as.numeric(Results_mod[4, 5]), "lower_bound_regimen" = Results_mod[4, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[4, 7]),
		"a_marginally_better" = as.numeric(Results_mod[4, 8]),
		"b_clinically_better" = as.numeric(Results_mod[4, 9]),
		"b_marginally_better" = as.numeric(Results_mod[4, 10]))

	sleep_problems = list(as.logical(1 - uruns[4]), graph_5, graph_6)
	names(sleep_problems) = c("successful_run", "graph_5", "graph_6")

	graph_5 = list("more_effective_regimen" = Results_mod[6, 1], "median_effect" = as.numeric(Results_mod[6, 2]),
		"upper_bound" = as.numeric(Results_mod[6, 3]), "upper_bound_regimen" = Results_mod[6, 4],
		"lower_bound" = as.numeric(Results_mod[6, 5]), "lower_bound_regimen" = Results_mod[6, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[6, 7]),
		"a_marginally_better" = as.numeric(Results_mod[6, 8]),
		"b_clinically_better" = as.numeric(Results_mod[6, 9]),
		"b_marginally_better" = as.numeric(Results_mod[6, 10]))

	constipation = list(as.logical(1 - uruns[6]), graph_5, graph_6)
	names(constipation) = c("successful_run", "graph_5", "graph_6")

	graph_5 = list("more_effective_regimen" = Results_mod[3, 1], "median_effect" = as.numeric(Results_mod[3, 2]),
		"upper_bound" = as.numeric(Results_mod[3, 3]), "upper_bound_regimen" = Results_mod[3, 4],
		"lower_bound" = as.numeric(Results_mod[3, 5]), "lower_bound_regimen" = Results_mod[3, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[3, 7]),
		"a_marginally_better" = as.numeric(Results_mod[3, 8]),
		"b_clinically_better" = as.numeric(Results_mod[3, 9]),
		"b_marginally_better" = as.numeric(Results_mod[3, 10]))

	drowsiness = list(as.logical(1 - uruns[3]), graph_5, graph_6)
	names(drowsiness) = c("successful_run", "graph_5", "graph_6")

	graph_5 = list("more_effective_regimen" = Results_mod[5, 1], "median_effect" = as.numeric(Results_mod[5, 2]),
		"upper_bound" = as.numeric(Results_mod[5, 3]), "upper_bound_regimen" = Results_mod[5, 4],
		"lower_bound" = as.numeric(Results_mod[5, 5]), "lower_bound_regimen" = Results_mod[5, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[5, 7]),
		"a_marginally_better" = as.numeric(Results_mod[5, 8]),
		"b_clinically_better" = as.numeric(Results_mod[5, 9]),
		"b_marginally_better" = as.numeric(Results_mod[5, 10]))

	thinking_problems = list(as.logical(1 - uruns[5]), graph_5, graph_6)
	names(thinking_problems) = c("successful_run", "graph_5", "graph_6")

	graph_5 = list("more_effective_regimen" = Results_mod[2, 1], "median_effect" = as.numeric(Results_mod[2, 2]),
		"upper_bound" = as.numeric(Results_mod[2, 3]), "upper_bound_regimen" = Results_mod[2, 4],
		"lower_bound" = as.numeric(Results_mod[2, 5]), "lower_bound_regimen" = Results_mod[2, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[2, 7]),
		"a_marginally_better" = as.numeric(Results_mod[2, 8]),
		"b_clinically_better" = as.numeric(Results_mod[2, 9]),
		"b_marginally_better" = as.numeric(Results_mod[2, 10]))

	fatigue = list(as.logical(1 - uruns[2]), graph_5, graph_6)
	names(fatigue) = c("successful_run", "graph_5", "graph_6")	

	graph_5 = list("more_effective_regimen" = Results_mod[7, 1], "median_effect" = as.numeric(Results_mod[7, 2]),
		"upper_bound" = as.numeric(Results_mod[7, 3]), "upper_bound_regimen" = Results_mod[7, 4],
		"lower_bound" = as.numeric(Results_mod[7, 5]), "lower_bound_regimen" = Results_mod[7, 6])

	graph_6 = list("a_clinically_better" = as.numeric(Results_mod[7, 7]),
		"a_marginally_better" = as.numeric(Results_mod[7, 8]),
		"b_clinically_better" = as.numeric(Results_mod[7, 9]),
		"b_marginally_better" = as.numeric(Results_mod[7, 10]))

	neuropathic_pain = list(as.logical(1 - uruns[7]), graph_5, graph_6)
	names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")

	meta.data[[2]] = list("Models" = c("1. Null Covs"),
		"Better Model" = Better.Model, "DICs" = c(sum(DICs.1)), "Successful Runs" =
		list("Model1" = as.logical(1 - uruns.1)))
	names(meta.data) = c("Input for Model 1", "Final Results")

	out = list(pain, sleep_problems, constipation, drowsiness, thinking_problems, fatigue, neuropathic_pain,
		Results, meta.data)
	names(out) = c("pain", "sleep_problems", "constipation", "drowsiness", "thinking_problems", "fatigue",
		"neuropathic_pain", "Results", "Metadata")

	return(out)
}

