wrap <- function(data, metadata) {

	#####################################
	# Converting Output to Desired Format
	#####################################

	insufficient_data = FALSE

	days_in_trial = as.numeric(as.Date(metadata[["trial_end_date"]]) - as.Date(metadata[["trial_start_date"]])) + 1

	No_Neuropain = is.null(data[["painSharpness"]])

	meta.data = list()
	meta.data[[1]] = No_Neuropain
	names(meta.data)[length(meta.data)] = "No_Neuropain"

	Day2 = as.numeric(as.Date(data[["timestamp"]]) - as.Date(metadata[["trial_start_date"]]))
	Intensity2 = data[["averagePainIntensity"]]
	Enjoyment2 = data[["enjoymentOfLife"]]
	Activity2 = data[["generalActivity"]]
	Fatigue2 = data[["fatiguePrompt"]]
	Drowsy2 = data[["drowsinessPrompt"]]
	Sleep2 = data[["sleepDisturbancePrompt"]]
	Constipation2 = data[["constipationPrompt"]]
	if (!is.null(data[["painSharpness"]])) {
		Sharpness2 = data[["painSharpness"]]
		Hotness2 = data[["painHotness"]]
		Sensitivity2 = data[["painSensitivity"]]
	}
	if (metadata[["cognitiveFunctionPromptKey"]] == "cognitiveFunctionFoggyThinkingPrompt") {
		Thinking2 = data[["cognitiveFunctionFoggyThinkingPrompt"]]
	} else if (metadata[["cognitiveFunctionPromptKey"]] == "cognitiveFunctionWorkingHarderPrompt") {
		Thinking2 = data[["cognitiveFunctionWorkingHarderPrompt"]]
	}

	Day2 = Day2[!is.na(Day2)]
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
	stopifnot(length(Day2) == num.obs, length(Intensity2) == num.obs, length(Enjoyment2) == num.obs, 
		length(Activity2) == num.obs, length(Fatigue2) == num.obs, length(Drowsy2) == num.obs, 
		length(Sleep2) == num.obs, length(Constipation2) == num.obs, length(Thinking2) == num.obs)

	if (num.obs <= 1) {
		insufficient_data = TRUE
	}

	Day2 = Day2 + 1

	Pain2 = Intensity2 + Enjoyment2 + Activity2
	if (!is.null(data[["painSharpness"]])) {
		Neuropain2 = Sharpness2 + Hotness2 + Sensitivity2
	} else {
		Neuropain2 = rep(15, length(Day2))
	}

	Time2 = data[["timestamp"]]

	for (i in 1:length(Time2)) {
		Time2[i] = substr(Time2[i], 12, 13)
	}
	Time2 = as.numeric(Time2)
	Time2 = Time2 / 24
	
	Sleep2 = 6 - Sleep2

	observations = cbind(Day2, Time2, Pain2, Fatigue2, Drowsy2, Sleep2, Thinking2, Constipation2, Neuropain2)

	#############################
	# Completing the Day Variable
	#############################

	if (observations[1, 1] != 1) {
		observations = rbind(c(1, rep(NA, ncol(observations) - 1)), observations)
	}

	if (observations[nrow(observations), 1] != days_in_trial) {
		observations = rbind(observations, c(days_in_trial, rep(NA, ncol(observations) - 1)))
	}

	gaps = rep(0, nrow(observations) - 1)
	for (i in 2:(nrow(observations))) {
		gaps[i - 1] = observations[i, 1] - observations[i - 1]
	}

	observations = rbind(observations, rep(NA, ncol(observations)))

	j = 1
	while (j <= max(gaps)) {

		for (i in (nrow(observations) - 2):1) { 
			if (observations[i, 1] < observations[i + 1, 1] - 1) {
				observations[nrow(observations), 1] = observations[i + 1, 1] - 1
				observations = as.data.frame(observations)
				observations = observations[order(observations$Day2), ]
				observations = as.matrix(observations)
				observations = rbind(observations, rep(NA, ncol(observations)))
			}
		}

		j = j + 1
	}

	observations = observations[-nrow(observations), ]

	#####################################
	# Adding Complete Treatment and Block
	#####################################

	treatment.schedule = rep(NA, nchar(metadata[["cycle_ab_pairs"]]))
	Treat.full = NULL
	for (i in 1:length(treatment.schedule)) {
		treatment.schedule[i] = substr(metadata[["cycle_ab_pairs"]], i, i)
		if (treatment.schedule[i] == ",") { treatment.schedule[i] = NA }	
	}

	treatment.schedule = treatment.schedule[!is.na(treatment.schedule)]

	for (i in 1:length(treatment.schedule)) {
		if (treatment.schedule[i] == "A") { Treat.full = c(Treat.full, rep(0, metadata[["regimen_duration"]])) 
		} else if (treatment.schedule[i] == "B") {Treat.full = c(Treat.full, rep(1, metadata[["regimen_duration"]]))
		}
	}

	rm(i)
	i = 1
	Block.full = NULL
	while (i <= metadata[["number_of_cycles"]]) {
		Block.full = c(Block.full, rep(i, (2 * metadata[["regimen_duration"]])))
		i = i + 1
	}

	Treat2 = Block2 = rep(NA, nrow(observations))
	observations = cbind(observations, Treat2, Block2)

	for (i in 1:nrow(observations)) {
		observations[i, 10] = Treat.full[observations[i, 1]]
		observations[i, 11] = Block.full[observations[i, 1]]
	}

	observations = as.data.frame(observations)

	###########################
	# Creating Block Categories
	###########################
	
	for (i in 2:nrow(observations)) {
		if (is.na(observations$Treat2[i])) {
			observations$Treat2[i] = observations$Treat2[i-1]
			observations$Block2[i] = observations$Block2[i-1]
		}
	}

	Block.2 = Block.3 = Block.4 = rep(0, nrow(observations))
	for (i in 1:nrow(observations)) {
		if (observations$Block2[i] == 2) {
			Block.2[i] = 1
		} else if (observations$Block2[i] == 3) {
			Block.3[i] = 1
		} else if (observations$Block2[i] == 4) {
			Block.4[i] = 1
		}
	}

	Block.Covs = Block.2

	if (sum(Block.3) != 0) { 
		Block.Covs = cbind(Block.Covs, Block.3) 
		colnames(Block.Covs) = c("Block.2", "Block.3")
	}
	if (sum(Block.4) != 0) { 
		Block.Covs = cbind(Block.Covs, Block.4) 
		colnames(Block.Covs) = c("Block.2", "Block.3", "Block.4")
	}

	#########################
	# Creating Lag Covariates
	#########################

	Lag.Covs = make.lag(observations)

	#################################
	# Adjusting for insufficient Data
	#################################

	Treatments = data[["regimen"]]

	Blocks = data[["cycle"]]

	if (dim(table(Treatments)) != 2) { insufficient_data = TRUE }
	if (min(Blocks) == max(Blocks)) { insufficient_data = TRUE }

	if (insufficient_data == TRUE) {
		observations$Pain2 = observations$Fatigue2 = observations$Drowsy2 = observations$Sleep2 = 
		observations$Thinking2 = observations$Constipation2 = observations$Neuropain2 = 
		rep(3, nrow(observations))
	}

	########################
	# Making Time Continuous
	########################

	observations$Time2 = observations$Day2 + observations$Time2

	meta.data[[length(meta.data) + 1]] = insufficient_data
	names(meta.data)[length(meta.data)] = "Insufficient_Data"

	meta.data[[length(meta.data) + 1]] = observations
	names(meta.data)[length(meta.data)] = "Observations"

	####################################
	# Analyzing Data for Null Covariates
	####################################

	nof1 = evaluate(observations, Covs = NULL, i = 1, No_Neuropain)
	Results.1 = nof1$Results
	Diagnostics.1 = nof1$Diagnostics
	uruns.1 = nof1$uruns
	ForPPC.1 = nof1$ForPPC

	####################################
	# Analyzing Data for Time Covariates
	####################################

	nof1 = evaluate(observations, Covs = observations$Time2, i = 2, No_Neuropain)
	Results.2 = nof1$Results
	Diagnostics.2 = nof1$Diagnostics
	uruns.2 = nof1$uruns
	ForPPC.2 = nof1$ForPPC

	#####################################
	# Analyzing Data for Block Covariates
	#####################################

	nof1 = evaluate(observations, Covs = observations$Block2, i = 3, No_Neuropain)
	Results.3 = nof1$Results
	Diagnostics.3 = nof1$Diagnostics
	uruns.3 = nof1$uruns
	ForPPC.3 = nof1$ForPPC

	######################################
	# Analyzing Data for Lagged Covariates
	######################################

	nof1 = evaluate(observations, Covs = Lag.Covs, i = 4, No_Neuropain)
	Results.4 = nof1$Results
	Diagnostics.4 = nof1$Diagnostics
	uruns.4 = nof1$uruns
	ForPPC.4 = nof1$ForPPC

	meta.data[[length(meta.data) + 1]] = nof1$Inputs
	names(meta.data)[length(meta.data)] = "Inputs"

	#################
	# PPC for model 1
	#################



	#################
	# PPC for model 2
	#################



	#################
	# PPC for model 3
	#################



	#################
	# PPC for model 4
	#################



	##########################
	# Selecting the Best Model
	##########################

	Better = Results.1
	Better.Model = 1
	uruns.b = uruns.1




	Results = Better
	uruns = uruns.b

	###########################
	# Rearranging nof1 Output
	###########################

	Results_mod = matrix(NA, ncol = 10, nrow = nrow(Results))
	for (i in 1:nrow(Results_mod)) {
		if (Results[i, 2] < 0) {Results_mod[i, 1] = "B"}
		if (Results[i, 2] > 0) {Results_mod[i, 1] = "A"}
		if (Results[i, 2] == 0) {Results_mod[i, 1] = "Neither"}

		if (Results[i, 3] < 0) {Results_mod[i, 4] = "B"}
		if (Results[i, 3] > 0) {Results_mod[i, 4] = "A"}

		if (Results[i, 1] < 0) {Results_mod[i, 6] = "B"}
		if (Results[i, 1] > 0) {Results_mod[i, 6] = "A"}
	}

	Results_mod[ , 2] = abs(Results[ , 2])
	Results_mod[ , 3] = abs(Results[ , 3])
	Results_mod[ , 5] = abs(Results[ , 1])
	Results_mod[ , 7] = Results[ , 7]
	Results_mod[ , 8] = Results[ , 6]
	Results_mod[ , 9] = Results[ , 4]
	Results_mod[ , 10] = Results[ , 5]

	if (No_Neuropain) {
		colnames(Results_mod) = c("more_effective_regimen", "median_effect", "upper_bound", "upper_bound_regimen",
			"lower_bound", "lower_bound_regimen", "a_clinically_better", "a_marginally_better", "b_clinically_better",
			"b_marginally_better")
		rownames(Results_mod) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems",
			"constipation")
	} else{
		colnames(Results_mod) = c("more_effective_regimen", "median_effect", "upper_bound", "upper_bound_regimen",
			"lower_bound", "lower_bound_regimen", "a_clinically_better", "a_marginally_better", "b_clinically_better",
			"b_marginally_better")
		rownames(Results_mod) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems",
			"constipation", "neuropathic_pain")
	}

	#####################
	# Constructing Output
	#####################

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

	if (!No_Neuropain) {

		graph_5 = list("more_effective_regimen" = Results_mod[7, 1], "median_effect" = as.numeric(Results_mod[7, 2]),
			"upper_bound" = as.numeric(Results_mod[7, 3]), "upper_bound_regimen" = Results_mod[7, 4],
			"lower_bound" = as.numeric(Results_mod[7, 5]), "lower_bound_regimen" = Results_mod[7, 6])

		graph_6 = list("a_clinically_better" = as.numeric(Results_mod[7, 7]),
			"a_marginally_better" = as.numeric(Results_mod[7, 8]),
			"b_clinically_better" = as.numeric(Results_mod[7, 9]),
			"b_marginally_better" = as.numeric(Results_mod[7, 10]))

		neuropathic_pain = list(as.logical(1 - uruns[7]), graph_5, graph_6)
		names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")

	}

	meta.data[[length(meta.data) + 1]] = list(
		"Models" = c("1. Null Covs", "2. Time Covs", "3. Block Covs", "4. Lagged Effects"),
		"Best Model" = Better.Model, 
		"Successful Runs" = list("Model1" = as.logical(1 - uruns.1), "Model2" = as.logical(1 - uruns.2), 
			"Model3" = as.logical(1 - uruns.3), "Model4" = as.logical(1 - uruns.4))
	)
	names(meta.data)[length(meta.data)] = "Final Results"

	Diagnostics = list("Diagnostics.1" = Diagnostics.1, "Diagnostics.2" = Diagnostics.2, 
			"Diagnostics.3" = Diagnostics.3, "Diagnostics.4" = Diagnostics.4)

	if (No_Neuropain) {
		out = list(pain, sleep_problems, constipation, drowsiness, thinking_problems, fatigue,
			Results, Diagnostics, meta.data)
		names(out) = c("pain", "sleep_problems", "constipation", "drowsiness", "thinking_problems", "fatigue",
			"Results", "Diagnostics", "Metadata")
	} else{	
		out = list(pain, sleep_problems, constipation, drowsiness, thinking_problems, fatigue, neuropathic_pain,
			Results, Diagnostics, meta.data)
		names(out) = c("pain", "sleep_problems", "constipation", "drowsiness", "thinking_problems", "fatigue",
			"neuropathic_pain", "Results", "Diagnostics", "Metadata")
	}

	return(out)
}

