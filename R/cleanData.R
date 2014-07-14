cleanData = function(data, metadata) {

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
	
	if (observations[nrow(observations), 1] < days_in_trial) {
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
	
	#########################
	# Creating Lag Covariates
	#########################
	
	Lag.Covs = make.lag(observations)

	#################################
	# Adjusting for Insufficient Data
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
	
	################################################
	# Correcting for observations outside trial time
	################################################
	
	a = 1
	while (a <= 3) {
		if (observations$Day2[length(observations$Day2)] > days_in_trial + 1) {
			observations = observations[-nrow(observations), ]
		}
		a = a + 1
	}
	
	for (i in 2:nrow(observations)) {
		if (!is.na(observations$Time2[i]) && observations$Time2[i] > days_in_trial) {
			observations$Treat2[i] = observations$Treat2[i-1]
			observations$Block2[i] = observations$Block2[i-1]
		}
	}
	
	###########################
	# Creating Block Categories
	###########################
	
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
	
	observations = cbind(observations, Block.2)
	if (sum(Block.3) != 0) { observations = cbind(observations, Block.3) }
	if (sum(Block.4) != 0) { observations = cbind(observations, Block.4) }
	
	#########################
	# Creating Carryover Data
	#########################
	
	car.A = car.B = rep(0, nrow(observations))
	for (i in 2:nrow(observations)) {
		if (observations$Treat2[i] != observations$Treat2[i-1]) {
			if (observations$Treat2[i] == 0) { car.A[i] = 1
			} else { car.B[i] = 1
			}
		}
	}
	if (observations$Treat2[1] == 0) { car.A[1] = 1
	} else { car.B[1] = 1
	}
	
	if (metadata[["regimen_duration"]] >= 14) {
		for (i in 1:(length(car.B)-1)) {
			if (car.A[i] == 1) { car.A[i + 1] = 1 }
			if (car.B[i] == 1) { car.B[i + 1] = 1 }
		}
	}
	
	observations = cbind(observations, car.A, car.B)
	
	out = list(observations, Block.Covs, Lag.Covs, No_Neuropain, meta.data, insufficient_data)
	return(out)
}

