wrap <- function(data, metadata) {
	cleaned = cleanData(data, metadata)
	observations = cleaned[[1]]
	Block.Covs = cleaned[[2]]
	Lag.Covs = cleaned[[3]]
	No_Neuropain = cleaned[[4]]
	meta.data = cleaned[[5]]
	insufficient_data = cleaned[[6]]
	
	#################
	# Running Model 1
	#################
	
	nof1 = evaluate(observations, Covs = NULL, mod.id = 1, No_Neuropain)
	Results.1 = nof1$Results
	DIC.1 = nof1$DIC
	Sigs.1 = nof1$Sigs
	uruns.1 = nof1$uruns
	ForPPC.1 = nof1$ForPPC
	
	#################
	# PPC for Model 1
	#################	
	
	cat("\n \n \n PPC 1 \n \n \n")
	
	if (uruns.1[1] == FALSE) { PPC.1.Pain = ppc(observations, ForPPC.1, Covs = NULL, "Pain", 1)
	} else { PPC.1.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.1[2] == FALSE) { PPC.1.Fatigue = ppc(observations, ForPPC.1, Covs = NULL, "Fatigue", 1)
	} else { PPC.1.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.1[3] == FALSE) { PPC.1.Drowsy = ppc(observations, ForPPC.1, Covs = NULL, "Drowsy", 1)
	} else { PPC.1.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.1[4] == FALSE) { PPC.1.Sleep = ppc(observations, ForPPC.1, Covs = NULL, "Sleep", 1)
	} else { PPC.1.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.1[5] == FALSE) { PPC.1.Thinking = ppc(observations, ForPPC.1, Covs = NULL, "Thinking", 1)
	} else { PPC.1.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.1[6] == FALSE) { PPC.1.Constipation = ppc(observations, ForPPC.1, Covs = NULL, "Constipation", 1)
	} else { PPC.1.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (!No_Neuropain) { 
		if (uruns.1[7] == FALSE) { PPC.1.Neuropain = ppc(observations, ForPPC.1, Covs = NULL, "Neuropain", 1)
		} else { PPC.1.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	}
	
	##################
	# Info for Model 1
	##################
	
	Model1.Info = list()
	Model1.Info[[length(Model1.Info) + 1]] = Results.1
	names(Model1.Info)[length(Model1.Info)] = "Results"
	Model1.Info[[length(Model1.Info) + 1]] = Sigs.1
	names(Model1.Info)[length(Model1.Info)] = "Sigs"
	Model1.Info[[length(Model1.Info) + 1]] = DIC.1
	names(Model1.Info)[length(Model1.Info)] = "DIC"
	Model1.Info[[length(Model1.Info) + 1]] = uruns.1
	names(Model1.Info)[length(Model1.Info)] = "uruns"
	PPC.res = list("Pain" = PPC.1.Pain$test, "Fatigue" = PPC.1.Fatigue$test, 
		"Drowsy" = PPC.1.Drowsy$test, "Sleep" = PPC.1.Sleep$test, 
		"Thinking" = PPC.1.Thinking$test, "Constipation" = PPC.1.Constipation$test)
	if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.1.Neuropain$test
		names(PPC.res)[length(PPC.res)] = "Neuropain" }
	Model1.Info[[length(Model1.Info) + 1]] = PPC.res
	names(Model1.Info)[length(Model1.Info)] = "PPC"
	
	Info = list()
	Info[[length(Info) + 1]] = Model1.Info
	names(Info)[length(Info)] = "Model1.Info"
	
	#################
	# Running Model 2
	#################
	
	nof1 = evaluate(observations, Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Results.2 = nof1$Results
	DIC.2 = nof1$DIC
	Sigs.2 = nof1$Sigs
	uruns.2 = nof1$uruns
	ForPPC.2 = nof1$ForPPC
	
	#################
	# PPC for Model 2
	#################
	
	cat("\n \n \n PPC 2 \n \n \n")
	
	if (uruns.2[1] == FALSE) { PPC.2.Pain = ppc(observations, ForPPC.2, Covs = observations$Time2, "Pain", 2)
	} else { PPC.2.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.2[2] == FALSE) { PPC.2.Fatigue = ppc(observations, ForPPC.2, Covs = observations$Time2, "Fatigue", 2)
	} else { PPC.2.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.2[3] == FALSE) { PPC.2.Drowsy = ppc(observations, ForPPC.2, Covs = observations$Time2, "Drowsy", 2)
	} else { PPC.2.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.2[4] == FALSE) { PPC.2.Sleep = ppc(observations, ForPPC.2, Covs = observations$Time2, "Sleep", 2)
	} else { PPC.2.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.2[5] == FALSE) { PPC.2.Thinking = ppc(observations, ForPPC.2, Covs = observations$Time2, "Thinking", 2)
	} else { PPC.2.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.2[6] == FALSE) { PPC.2.Constipation = ppc(observations, ForPPC.2, Covs = observations$Time2, "Constipation", 2)
	} else { PPC.2.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (!No_Neuropain) { 
		if (uruns.2[7] == FALSE) { PPC.2.Neuropain = ppc(observations, ForPPC.2, Covs = observations$Time2, "Neuropain", 2) 
		} else { PPC.2.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	}
	
	##################
	# Info for Model 2
	##################
	
	Model2.Info = list()
	Model2.Info[[length(Model2.Info) + 1]] = Results.2
	names(Model2.Info)[length(Model2.Info)] = "Results"
	Model2.Info[[length(Model2.Info) + 1]] = Sigs.2
	names(Model2.Info)[length(Model2.Info)] = "Sigs"
	Model2.Info[[length(Model2.Info) + 1]] = DIC.2
	names(Model2.Info)[length(Model2.Info)] = "DIC"
	Model2.Info[[length(Model2.Info) + 1]] = uruns.2
	names(Model2.Info)[length(Model2.Info)] = "uruns"
	PPC.res = list("Pain" = PPC.2.Pain$test, "Fatigue" = PPC.2.Fatigue$test, 
		"Drowsy" = PPC.2.Drowsy$test, "Sleep" = PPC.2.Sleep$test, 
		"Thinking" = PPC.2.Thinking$test, "Constipation" = PPC.2.Constipation$test)
	if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.2.Neuropain$test
		names(PPC.res)[length(PPC.res)] = "Neuropain" }
	Model2.Info[[length(Model2.Info) + 1]] = PPC.res
	names(Model2.Info)[length(Model2.Info)] = "PPC"
	
	Info[[length(Info) + 1]] = Model2.Info
	names(Info)[length(Info)] = "Model2.Info"
	
	#################
	# Running Model 3
	#################
	
	nof1 = evaluate(observations, Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Results.3 = nof1$Results
	DIC.3 = nof1$DIC
	Sigs.3 = nof1$Sigs
	uruns.3 = nof1$uruns
	ForPPC.3 = nof1$ForPPC
	
	#################
	# PPC for model 3
	#################
	
	cat("\n \n \n PPC 3 \n \n \n")
	
	if (uruns.3[1] == FALSE) { PPC.3.Pain = ppc(observations, ForPPC.3, Covs = Block.Covs, "Pain", 3)
	} else { PPC.3.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.3[2] == FALSE) { PPC.3.Fatigue = ppc(observations, ForPPC.3, Covs = Block.Covs, "Fatigue", 3)
	} else { PPC.3.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.3[3] == FALSE) { PPC.3.Drowsy = ppc(observations, ForPPC.3, Covs = Block.Covs, "Drowsy", 3)
	} else { PPC.3.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.3[4] == FALSE) { PPC.3.Sleep = ppc(observations, ForPPC.3, Covs = Block.Covs, "Sleep", 3)
	} else { PPC.3.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.3[5] == FALSE) { PPC.3.Thinking = ppc(observations, ForPPC.3, Covs = Block.Covs, "Thinking", 3)
	} else { PPC.3.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.3[6] == FALSE) { PPC.3.Constipation = ppc(observations, ForPPC.3, Covs = Block.Covs, "Constipation", 3)
	} else { PPC.3.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (!No_Neuropain) { 
		if (uruns.3[7] == FALSE) { PPC.3.Neuropain = ppc(observations, ForPPC.3, Covs = Block.Covs, "Neuropain", 3) 
		} else { PPC.3.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	}
	
	##################
	# Info for Model 3
	##################
	
	Model3.Info = list()
	Model3.Info[[length(Model3.Info) + 1]] = Results.3
	names(Model3.Info)[length(Model3.Info)] = "Results"
	Model3.Info[[length(Model3.Info) + 1]] = Sigs.3
	names(Model3.Info)[length(Model3.Info)] = "Sigs"
	Model3.Info[[length(Model3.Info) + 1]] = DIC.3
	names(Model3.Info)[length(Model3.Info)] = "DIC"
	Model3.Info[[length(Model3.Info) + 1]] = uruns.3
	names(Model3.Info)[length(Model3.Info)] = "uruns"
	PPC.res = list("Pain" = PPC.3.Pain$test, "Fatigue" = PPC.3.Fatigue$test, 
		"Drowsy" = PPC.3.Drowsy$test, "Sleep" = PPC.3.Sleep$test, 
		"Thinking" = PPC.3.Thinking$test, "Constipation" = PPC.3.Constipation$test)
	if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.3.Neuropain$test
		names(PPC.res)[length(PPC.res)] = "Neuropain" }
	Model3.Info[[length(Model3.Info) + 1]] = PPC.res
	names(Model3.Info)[length(Model3.Info)] = "PPC"
	
	Info[[length(Info) + 1]] = Model3.Info
	names(Info)[length(Info)] = "Model3.Info"
	
	#################
	# Running Model 4
	#################
	
	nof1 = evaluate(observations, Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Results.4 = nof1$Results
	DIC.4 = nof1$DIC
	Sigs.4 = nof1$Sigs
	uruns.4 = nof1$uruns
	ForPPC.4 = nof1$ForPPC
	
	#################
	# PPC for model 4
	#################
	
	cat("\n \n \n PPC 4 \n \n \n")
	
	if (uruns.4[1] == FALSE) { PPC.4.Pain = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 1], "Pain", 4)
	} else { PPC.4.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4[2] == FALSE) { PPC.4.Fatigue = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 2], "Fatigue", 4)
	} else { PPC.4.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4[3] == FALSE) { PPC.4.Drowsy = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 3], "Drowsy", 4)
	} else { PPC.4.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4[4] == FALSE) { PPC.4.Sleep = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 4], "Sleep", 4)
	} else { PPC.4.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4[5] == FALSE) { PPC.4.Thinking = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 5], "Thinking", 4)
	} else { PPC.4.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4[6] == FALSE) { PPC.4.Constipation = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 6], "Constipation", 4)
	} else { PPC.4.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (!No_Neuropain) { 
		if (uruns.4[7] == FALSE) { PPC.4.Neuropain = ppc(observations, ForPPC.4, Covs = Lag.Covs[ , 7], "Neuropain", 4) 
		} else { PPC.4.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	}
	
	##################
	# Info for Model 4
	##################
	
	Model4.Info = list()
	Model4.Info[[length(Model4.Info) + 1]] = Results.4
	names(Model4.Info)[length(Model4.Info)] = "Results"
	Model4.Info[[length(Model4.Info) + 1]] = Sigs.4
	names(Model4.Info)[length(Model4.Info)] = "Sigs"
	Model4.Info[[length(Model4.Info) + 1]] = DIC.4
	names(Model4.Info)[length(Model4.Info)] = "DIC"
	Model4.Info[[length(Model4.Info) + 1]] = uruns.4
	names(Model4.Info)[length(Model4.Info)] = "uruns"
	PPC.res = list("Pain" = PPC.4.Pain$test, "Fatigue" = PPC.4.Fatigue$test, 
		"Drowsy" = PPC.4.Drowsy$test, "Sleep" = PPC.4.Sleep$test, 
		"Thinking" = PPC.4.Thinking$test, "Constipation" = PPC.4.Constipation$test)
	if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.4.Neuropain$test
		names(PPC.res)[length(PPC.res)] = "Neuropain" }
	Model4.Info[[length(Model4.Info) + 1]] = PPC.res
	names(Model4.Info)[length(Model4.Info)] = "PPC"
	
	Info[[length(Info) + 1]] = Model4.Info
	names(Info)[length(Info)] = "Model4.Info"
	
	###################
	# Running Model 4.1
	###################
	
	nof1 = evaluate(observations, Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Results.4.1 = nof1$Results
	DIC.4.1 = nof1$DIC
	Sigs.4.1 = nof1$Sigs
	uruns.4.1 = nof1$uruns
	ForPPC.4.1 = nof1$ForPPC
	
	###################
	# PPC for model 4.1
	###################
	
	cat("\n \n \n PPC 4.1 \n \n \n")
	
	if (uruns.4.1[1] == FALSE) { PPC.4.1.Pain = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 1], 
		observations$Time2), "Pain", 4.1)
	} else { PPC.4.1.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.1[2] == FALSE) { PPC.4.1.Fatigue = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 2], 
		observations$Time2), "Fatigue", 4.1)
	} else { PPC.4.1.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.1[3] == FALSE) { PPC.4.1.Drowsy = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 3], 
		observations$Time2), "Drowsy", 4.1)
	} else { PPC.4.1.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.1[4] == FALSE) { PPC.4.1.Sleep = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 4], 
		observations$Time2), "Sleep", 4.1)
	} else { PPC.4.1.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.1[5] == FALSE) { PPC.4.1.Thinking = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 5], 
		observations$Time2), "Thinking", 4.1)
	} else { PPC.4.1.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.1[6] == FALSE) { PPC.4.1.Constipation = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 6], 
		observations$Time2), "Constipation", 4.1)
	} else { PPC.4.1.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (!No_Neuropain) { 
		if (uruns.4.1[7] == FALSE) { PPC.4.1.Neuropain = ppc(observations, ForPPC.4.1, Covs = cbind(Lag.Covs[ , 7], 
			observations$Time2), "Neuropain", 4.1) 
		} else { PPC.4.1.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	}
	
	####################
	# Info for Model 4.1
	####################
	
	Model4.1.Info = list()
	Model4.1.Info[[length(Model4.1.Info) + 1]] = Results.4.1
	names(Model4.1.Info)[length(Model4.1.Info)] = "Results"
	Model4.1.Info[[length(Model4.1.Info) + 1]] = Sigs.4.1
	names(Model4.1.Info)[length(Model4.1.Info)] = "Sigs"
	Model4.1.Info[[length(Model4.1.Info) + 1]] = DIC.4.1
	names(Model4.1.Info)[length(Model4.1.Info)] = "DIC"
	Model4.1.Info[[length(Model4.1.Info) + 1]] = uruns.4.1
	names(Model4.1.Info)[length(Model4.1.Info)] = "uruns"
	PPC.res = list("Pain" = PPC.4.1.Pain$test, "Fatigue" = PPC.4.1.Fatigue$test, 
		"Drowsy" = PPC.4.1.Drowsy$test, "Sleep" = PPC.4.1.Sleep$test, 
		"Thinking" = PPC.4.1.Thinking$test, "Constipation" = PPC.4.1.Constipation$test)
	if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.4.1.Neuropain$test
		names(PPC.res)[length(PPC.res)] = "Neuropain" }
	Model4.1.Info[[length(Model4.1.Info) + 1]] = PPC.res
	names(Model4.1.Info)[length(Model4.1.Info)] = "PPC"
	
	Info[[length(Info) + 1]] = Model4.1.Info
	names(Info)[length(Info)] = "Model4.1.Info"
	
	###################
	# Running Model 4.2
	###################
	
	nof1 = evaluate(observations, Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	Results.4.2 = nof1$Results
	DIC.4.2 = nof1$DIC
	Sigs.4.2 = nof1$Sigs
	uruns.4.2 = nof1$uruns
	ForPPC.4.2 = nof1$ForPPC
	
	###################
	# PPC for model 4.2
	###################
	
	cat("\n \n \n PPC 4.2 \n \n \n")
	
	if (uruns.4.2[1] == FALSE) { PPC.4.2.Pain = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 1], 
		Block.Covs), "Pain", 4.2)
	} else { PPC.4.2.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.2[2] == FALSE) { PPC.4.2.Fatigue = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 2], 
		Block.Covs), "Fatigue", 4.2)
	} else { PPC.4.2.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.2[3] == FALSE) { PPC.4.2.Drowsy = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 3], 
		Block.Covs), "Drowsy", 4.2)
	} else { PPC.4.2.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.2[4] == FALSE) { PPC.4.2.Sleep = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 4], 
		Block.Covs), "Sleep", 4.2)
	} else { PPC.4.2.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.2[5] == FALSE) { PPC.4.2.Thinking = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 5], 
		Block.Covs), "Thinking", 4.2)
	} else { PPC.4.2.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (uruns.4.2[6] == FALSE) { PPC.4.2.Constipation = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 6], 
		Block.Covs), "Constipation", 4.2)
	} else { PPC.4.2.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	
	if (!No_Neuropain) { 
		if (uruns.4.2[7] == FALSE) { PPC.4.2.Neuropain = ppc(observations, ForPPC.4.2, Covs = cbind(Lag.Covs[ , 7], 
			Block.Covs), "Neuropain", 4.2) 
		} else { PPC.4.2.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
	}
	
	####################
	# Info for Model 4.2
	####################
	
	Model4.2.Info = list()
	Model4.2.Info[[length(Model4.2.Info) + 1]] = Results.4.2
	names(Model4.2.Info)[length(Model4.2.Info)] = "Results"
	Model4.2.Info[[length(Model4.2.Info) + 1]] = Sigs.4.2
	names(Model4.2.Info)[length(Model4.2.Info)] = "Sigs"
	Model4.2.Info[[length(Model4.2.Info) + 1]] = DIC.4.2
	names(Model4.2.Info)[length(Model4.2.Info)] = "DIC"
	Model4.2.Info[[length(Model4.2.Info) + 1]] = uruns.4.2
	names(Model4.2.Info)[length(Model4.2.Info)] = "uruns"
	PPC.res = list("Pain" = PPC.4.2.Pain$test, "Fatigue" = PPC.4.2.Fatigue$test, 
		"Drowsy" = PPC.4.2.Drowsy$test, "Sleep" = PPC.4.2.Sleep$test, 
		"Thinking" = PPC.4.2.Thinking$test, "Constipation" = PPC.4.2.Constipation$test)
	if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.4.2.Neuropain$test
		names(PPC.res)[length(PPC.res)] = "Neuropain" }
	Model4.2.Info[[length(Model4.2.Info) + 1]] = PPC.res
	names(Model4.2.Info)[length(Model4.2.Info)] = "PPC"
	
	Info[[length(Info) + 1]] = Model4.2.Info
	names(Info)[length(Info)] = "Model4.2.Info"
	
	##########################
	# Selecting the Best Model
	##########################
	
	if (No_Neuropain) { nouts = 6 
	} else { nouts = 7 }

	Best.Model = rep(NA, nouts)

	for (j in 1:nouts) {
		Sig.1 = as.numeric(Model1.Info$Sigs[j, 1])
		Sig.2 = as.numeric(Model2.Info$Sigs[j, 2])
		Sig.3 = sum(as.numeric(Model3.Info$Sigs[j, 2:ncol(Model3.Info$Sigs)]))
		Sig.4 = as.numeric(Model4.Info$Sigs[j, 2])
		Sig.4.1 = as.numeric(Model4.Info$Sigs[j, 2])
		Sig.4.2 = as.numeric(Model4.Info$Sigs[j, 2])
		
		DIC.1 = Model1.Info$DIC[j]
		DIC.2 = Model2.Info$DIC[j]
		DIC.3 = Model3.Info$DIC[j]
		DIC.4 = Model4.Info$DIC[j]
		DIC.4.1 = Model4.1.Info$DIC[j]
		DIC.4.2 = Model4.2.Info$DIC[j]
		
		if (j == 1) {
			PPC.1 = sum(PPC.1.Pain$summary)
			PPC.2 = sum(PPC.2.Pain$summary)
			PPC.3 = sum(PPC.3.Pain$summary)
			PPC.4 = sum(PPC.4.Pain$summary)
			PPC.4.1 = sum(PPC.4.1.Pain$summary)
			PPC.4.2 = sum(PPC.4.2.Pain$summary)
		} else if (j == 2) {
			PPC.1 = sum(PPC.1.Fatigue$summary)
			PPC.2 = sum(PPC.2.Fatigue$summary)
			PPC.3 = sum(PPC.3.Fatigue$summary)
			PPC.4 = sum(PPC.4.Fatigue$summary)
			PPC.4.1 = sum(PPC.4.1.Fatigue$summary)
			PPC.4.2 = sum(PPC.4.2.Fatigue$summary)
		} else if (j == 3) {
			PPC.1 = sum(PPC.1.Drowsy$summary)
			PPC.2 = sum(PPC.2.Drowsy$summary)
			PPC.3 = sum(PPC.3.Drowsy$summary)
			PPC.4 = sum(PPC.4.Drowsy$summary)
			PPC.4.1 = sum(PPC.4.1.Drowsy$summary)
			PPC.4.2 = sum(PPC.4.2.Drowsy$summary)
		} else if (j == 4) {
			PPC.1 = sum(PPC.1.Sleep$summary)
			PPC.2 = sum(PPC.2.Sleep$summary)
			PPC.3 = sum(PPC.3.Sleep$summary)
			PPC.4 = sum(PPC.4.Sleep$summary)
			PPC.4.1 = sum(PPC.4.1.Sleep$summary)
			PPC.4.2 = sum(PPC.4.2.Sleep$summary)
		} else if (j == 5) {
			PPC.1 = sum(PPC.1.Thinking$summary)
			PPC.2 = sum(PPC.2.Thinking$summary)
			PPC.3 = sum(PPC.3.Thinking$summary)
			PPC.4 = sum(PPC.4.Thinking$summary)
			PPC.4.1 = sum(PPC.4.1.Thinking$summary)
			PPC.4.2 = sum(PPC.4.2.Thinking$summary)
		} else if (j == 6) {
			PPC.1 = sum(PPC.1.Constipation$summary)
			PPC.2 = sum(PPC.2.Constipation$summary)
			PPC.3 = sum(PPC.3.Constipation$summary)
			PPC.4 = sum(PPC.4.Constipation$summary)
			PPC.4.1 = sum(PPC.4.1.Constipation$summary)
			PPC.4.2 = sum(PPC.4.2.Constipation$summary)
		} else if (j == 7) {
			PPC.1 = sum(PPC.1.Neuropain$summary)
			PPC.2 = sum(PPC.2.Neuropain$summary)
			PPC.3 = sum(PPC.3.Neuropain$summary)
			PPC.4 = sum(PPC.4.Neuropain$summary)
			PPC.4.1 = sum(PPC.4.1.Neuropain$summary)
			PPC.4.2 = sum(PPC.4.2.Neuropain$summary)
		}
		
		# Testing for Time or Block effects
		BM = 1
		
		TestMod2 = c((Sig.2 == 1), (DIC.2 < DIC.1 - 3), (PPC.2 > PPC.1))
		if (sum(as.numeric(TestMod2)) >= 2) {
			BM = 2
			Cur.DIC = DIC.2
			Cur.PPC = PPC.2
		} else {
			Cur.DIC = DIC.1
			Cur.PPC = PPC.1
		}
		
		TestMod3 = c((Sig.3 >= 1), (DIC.3 < Cur.DIC - 3), (PPC.3 > Cur.PPC))
		if (sum(as.numeric(TestMod3)) >= 2) {
			BM = 3
			Cur.DIC = DIC.3
			Cur.PPC = PPC.3
		}
		
		# Testing for Lagged Effects
		
		if (BM == 1) {
			TestMod4 = c((Sig.4 == 1), (DIC.4 < Cur.DIC - 3), (PPC.4 > Cur.PPC))
			if (sum(as.numeric(TestMod4)) >= 2) {
				BM = 4
				Cur.DIC = DIC.4
				Cur.PPC = PPC.4
			}
		} else if (BM == 2) {
			TestMod4.1 = c((Sig.4.1 == 1), (DIC.4.1 < Cur.DIC - 3), (PPC.4.1 > Cur.PPC))
			if (sum(as.numeric(TestMod4.1)) >= 2) {
				BM = 4.1
				Cur.DIC = DIC.4.1
				Cur.PPC = PPC.4.1
			}
		} else if (BM == 3) {
			TestMod4.2 = c((Sig.4.2 == 1), (DIC.4.2 < Cur.DIC - 3), (PPC.4.2 > Cur.PPC))
			if (sum(as.numeric(TestMod4.2)) >= 2) {
				BM = 4.2
				Cur.DIC = DIC.4.2
				Cur.PPC = PPC.4.2
			}
		}
		
		# Carryover
		
		if (BM == 1) {
			if (!exists("Results.5.1")) {
				###################
				# Running Model 5.1
				###################
				
				nof1 = evaluate(observations, Covs = cbind(observations$car.A, observations$car.B), 
					mod.id = 5.1, No_Neuropain)
				Results.5.1 = nof1$Results
				DIC.5.1 = nof1$DIC
				Sigs.5.1 = nof1$Sigs
				uruns.5.1 = nof1$uruns
				ForPPC.5.1 = nof1$ForPPC
				
				###################
				# PPC for model 5.1
				###################
				
				cat("\n \n \n PPC 5.1 \n \n \n")
				
				if (uruns.5.1[1] == FALSE) { PPC.5.1.Pain = ppc(observations, ForPPC.5.1, Covs = 
					cbind(observations$car.A, observations$car.B), "Pain", 5.1)
				} else { PPC.5.1.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.1[2] == FALSE) { PPC.5.1.Fatigue = ppc(observations, ForPPC.5.1, Covs = 
					cbind(observations$car.A, observations$car.B), "Fatigue", 5.1)
				} else { PPC.5.1.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.1[3] == FALSE) { PPC.5.1.Drowsy = ppc(observations, ForPPC.5.1, Covs = 
					cbind(observations$car.A, observations$car.B), "Drowsy", 5.1)
				} else { PPC.5.1.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.1[4] == FALSE) { PPC.5.1.Sleep = ppc(observations, ForPPC.5.1, Covs = 
					cbind(observations$car.A, observations$car.B), "Sleep", 5.1)
				} else { PPC.5.1.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.1[5] == FALSE) { PPC.5.1.Thinking = ppc(observations, ForPPC.5.1, Covs = 
					cbind(observations$car.A, observations$car.B), "Thinking", 5.1)
				} else { PPC.5.1.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.1[6] == FALSE) { PPC.5.1.Constipation = ppc(observations, ForPPC.5.1, Covs = 
					cbind(observations$car.A, observations$car.B), "Constipation", 5.1)
				} else { PPC.5.1.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (!No_Neuropain) { 
					if (uruns.5.1[7] == FALSE) { PPC.5.1.Neuropain = ppc(observations, ForPPC.5.1, Covs = 
						cbind(observations$car.A, observations$car.B), "Neuropain", 5.1) 
					} else { PPC.5.1.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				}
				
				####################
				# Info for Model 5.1
				####################
				
				Model5.1.Info = list()
				Model5.1.Info[[length(Model5.1.Info) + 1]] = Results.5.1
				names(Model5.1.Info)[length(Model5.1.Info)] = "Results"
				Model5.1.Info[[length(Model5.1.Info) + 1]] = Sigs.5.1
				names(Model5.1.Info)[length(Model5.1.Info)] = "Sigs"
				Model5.1.Info[[length(Model5.1.Info) + 1]] = DIC.5.1
				names(Model5.1.Info)[length(Model5.1.Info)] = "DIC"
				Model5.1.Info[[length(Model5.1.Info) + 1]] = uruns.5.1
				names(Model5.1.Info)[length(Model5.1.Info)] = "uruns"
				PPC.res = list("Pain" = PPC.5.1.Pain$test, "Fatigue" = PPC.5.1.Fatigue$test, 
					"Drowsy" = PPC.5.1.Drowsy$test, "Sleep" = PPC.5.1.Sleep$test, 
					"Thinking" = PPC.5.1.Thinking$test, "Constipation" = PPC.5.1.Constipation$test)
				if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.5.1.Neuropain$test
					names(PPC.res)[length(PPC.res)] = "Neuropain" }
				Model5.1.Info[[length(Model5.1.Info) + 1]] = PPC.res
				names(Model5.1.Info)[length(Model5.1.Info)] = "PPC"
				
				Info[[length(Info) + 1]] = Model5.1.Info
				names(Info)[length(Info)] = "Model5.1.Info"
			}
			
			Sig.5.1 = Model5.1.Info$Sigs[j, 2:3]
				
			DIC.5.1 = Model5.1.Info$DIC[j]
			
			if (j == 1) { PPC.5.1 = sum(PPC.5.1.Pain$summary)
			} else if (j == 2) { PPC.5.1 = sum(PPC.5.1.Fatigue$summary)
			} else if (j == 3) { PPC.5.1 = sum(PPC.5.1.Drowsy$summary)
			} else if (j == 4) { PPC.5.1 = sum(PPC.5.1.Sleep$summary)
			} else if (j == 5) { PPC.5.1 = sum(PPC.5.1.Thinking$summary)
			} else if (j == 6) { PPC.5.1 = sum(PPC.5.1.Constipation$summary)
			} else if (j == 7) { PPC.5.1 = sum(PPC.5.1.Neuropain$summary)
			}
			
			TestMod5.1 = c((Sig.5.1 == 1), (DIC.5.1 < Cur.DIC - 3), (PPC.5.1 > Cur.PPC))
			if (sum(as.numeric(TestMod5.1)) >= 2) {
				BM = 5.1
				Cur.DIC = DIC.5.1
				Cur.PPC = PPC.5.1
			}
		
		} else if (BM == 2) {
			if (!exists("Results.5.2")) {
				###################
				# Running Model 5.2
				###################
				
				nof1 = evaluate(observations, Covs = cbind(observations$Time2, observations$car.A, observations$car.B), 
					mod.id = 5.2, No_Neuropain)
				Results.5.2 = nof1$Results
				DIC.5.2 = nof1$DIC
				Sigs.5.2 = nof1$Sigs
				uruns.5.2 = nof1$uruns
				ForPPC.5.2 = nof1$ForPPC
				
				###################
				# PPC for model 5.2
				###################
				
				cat("\n \n \n PPC 5.2 \n \n \n")
				
				if (uruns.5.2[1] == FALSE) { PPC.5.2.Pain = ppc(observations, ForPPC.5.2, Covs = 
					cbind(observations$Time2, observations$car.A, observations$car.B), "Pain", 5.2)
				} else { PPC.5.2.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.2[2] == FALSE) { PPC.5.2.Fatigue = ppc(observations, ForPPC.5.2, Covs = 
					cbind(observations$Time2, observations$car.A, observations$car.B), "Fatigue", 5.2)
				} else { PPC.5.2.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.2[3] == FALSE) { PPC.5.2.Drowsy = ppc(observations, ForPPC.5.2, Covs = 
					cbind(observations$Time2, observations$car.A, observations$car.B), "Drowsy", 5.2)
				} else { PPC.5.2.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.2[4] == FALSE) { PPC.5.2.Sleep = ppc(observations, ForPPC.5.2, Covs = 
					cbind(observations$Time2, observations$car.A, observations$car.B), "Sleep", 5.2)
				} else { PPC.5.2.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.2[5] == FALSE) { PPC.5.2.Thinking = ppc(observations, ForPPC.5.2, Covs = 
					cbind(observations$Time2, observations$car.A, observations$car.B), "Thinking", 5.2)
				} else { PPC.5.2.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.2[6] == FALSE) { PPC.5.2.Constipation = ppc(observations, ForPPC.5.2, Covs = 
					cbind(observations$Time2, observations$car.A, observations$car.B), "Constipation", 5.2)
				} else { PPC.5.2.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (!No_Neuropain) { 
					if (uruns.5.2[7] == FALSE) { PPC.5.2.Neuropain = ppc(observations, ForPPC.5.2, Covs = 
						cbind(observations$Time2, observations$car.A, observations$car.B), "Neuropain", 5.2) 
					} else { PPC.5.2.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				}
				
				####################
				# Info for Model 5.2
				####################
				
				Model5.2.Info = list()
				Model5.2.Info[[length(Model5.2.Info) + 1]] = Results.5.2
				names(Model5.2.Info)[length(Model5.2.Info)] = "Results"
				Model5.2.Info[[length(Model5.2.Info) + 1]] = Sigs.5.2
				names(Model5.2.Info)[length(Model5.2.Info)] = "Sigs"
				Model5.2.Info[[length(Model5.2.Info) + 1]] = DIC.5.2
				names(Model5.2.Info)[length(Model5.2.Info)] = "DIC"
				Model5.2.Info[[length(Model5.2.Info) + 1]] = uruns.5.2
				names(Model5.2.Info)[length(Model5.2.Info)] = "uruns"
				PPC.res = list("Pain" = PPC.5.2.Pain$test, "Fatigue" = PPC.5.2.Fatigue$test, 
					"Drowsy" = PPC.5.2.Drowsy$test, "Sleep" = PPC.5.2.Sleep$test, 
					"Thinking" = PPC.5.2.Thinking$test, "Constipation" = PPC.5.2.Constipation$test)
				if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.5.2.Neuropain$test
					names(PPC.res)[length(PPC.res)] = "Neuropain" }
				Model5.2.Info[[length(Model5.2.Info) + 1]] = PPC.res
				names(Model5.2.Info)[length(Model5.2.Info)] = "PPC"
				
				Info[[length(Info) + 1]] = Model5.2.Info
				names(Info)[length(Info)] = "Model5.2.Info"
			}
			
			Sig.5.2 = sum(Model5.2.Info$Sigs[j, 3:4])
				
			DIC.5.2 = Model5.2.Info$DIC[j]
			
			if (j == 1) { PPC.5.2 = sum(PPC.5.2.Pain$summary)
			} else if (j == 2) { PPC.5.2 = sum(PPC.5.2.Fatigue$summary)
			} else if (j == 3) { PPC.5.2 = sum(PPC.5.2.Drowsy$summary)
			} else if (j == 4) { PPC.5.2 = sum(PPC.5.2.Sleep$summary)
			} else if (j == 5) { PPC.5.2 = sum(PPC.5.2.Thinking$summary)
			} else if (j == 6) { PPC.5.2 = sum(PPC.5.2.Constipation$summary)
			} else if (j == 7) { PPC.5.2 = sum(PPC.5.2.Neuropain$summary)
			}
			
			TestMod5.2 = c((Sig.5.2 >= 1), (DIC.5.2 < Cur.DIC - 3), (PPC.5.2 > Cur.PPC))
			if (sum(as.numeric(TestMod5.2)) >= 2) {
				BM = 5.2
				Cur.DIC = DIC.5.2
				Cur.PPC = PPC.5.2
			}
		} else if (BM == 3) {
			if (!exists("Results.5.3")) {
				###################
				# Running Model 5.3
				###################
				
				nof1 = evaluate(observations, Covs = cbind(Block.Covs, observations$car.A, observations$car.B), 
					mod.id = 5.3, No_Neuropain)
				Results.5.3 = nof1$Results
				DIC.5.3 = nof1$DIC
				Sigs.5.3 = nof1$Sigs
				uruns.5.3 = nof1$uruns
				ForPPC.5.3 = nof1$ForPPC
				
				###################
				# PPC for model 5.3
				###################
				
				cat("\n \n \n PPC 5.3 \n \n \n")
				
				if (uruns.5.3[1] == FALSE) { PPC.5.3.Pain = ppc(observations, ForPPC.5.3, Covs = 
					cbind(Block.Covs, observations$car.A, observations$car.B), "Pain", 5.3)
				} else { PPC.5.3.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.3[2] == FALSE) { PPC.5.3.Fatigue = ppc(observations, ForPPC.5.3, Covs = 
					cbind(Block.Covs, observations$car.A, observations$car.B), "Fatigue", 5.3)
				} else { PPC.5.3.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.3[3] == FALSE) { PPC.5.3.Drowsy = ppc(observations, ForPPC.5.3, Covs = 
					cbind(Block.Covs, observations$car.A, observations$car.B), "Drowsy", 5.3)
				} else { PPC.5.3.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.3[4] == FALSE) { PPC.5.3.Sleep = ppc(observations, ForPPC.5.3, Covs = 
					cbind(Block.Covs, observations$car.A, observations$car.B), "Sleep", 5.3)
				} else { PPC.5.3.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.3[5] == FALSE) { PPC.5.3.Thinking = ppc(observations, ForPPC.5.3, Covs = 
					cbind(Block.Covs, observations$car.A, observations$car.B), "Thinking", 5.3)
				} else { PPC.5.3.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.3[6] == FALSE) { PPC.5.3.Constipation = ppc(observations, ForPPC.5.3, Covs = 
					cbind(Block.Covs, observations$car.A, observations$car.B), "Constipation", 5.3)
				} else { PPC.5.3.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (!No_Neuropain) { 
					if (uruns.5.3[7] == FALSE) { PPC.5.3.Neuropain = ppc(observations, ForPPC.5.3, Covs = 
						cbind(Block.Covs, observations$car.A, observations$car.B), "Neuropain", 5.3) 
					} else { PPC.5.3.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				}
				
				####################
				# Info for Model 5.3
				####################
				
				Model5.3.Info = list()
				Model5.3.Info[[length(Model5.3.Info) + 1]] = Results.5.3
				names(Model5.3.Info)[length(Model5.3.Info)] = "Results"
				Model5.3.Info[[length(Model5.3.Info) + 1]] = Sigs.5.3
				names(Model5.3.Info)[length(Model5.3.Info)] = "Sigs"
				Model5.3.Info[[length(Model5.3.Info) + 1]] = DIC.5.3
				names(Model5.3.Info)[length(Model5.3.Info)] = "DIC"
				Model5.3.Info[[length(Model5.3.Info) + 1]] = uruns.5.3
				names(Model5.3.Info)[length(Model5.3.Info)] = "uruns"
				PPC.res = list("Pain" = PPC.5.3.Pain$test, "Fatigue" = PPC.5.3.Fatigue$test, 
					"Drowsy" = PPC.5.3.Drowsy$test, "Sleep" = PPC.5.3.Sleep$test, 
					"Thinking" = PPC.5.3.Thinking$test, "Constipation" = PPC.5.3.Constipation$test)
				if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.5.3.Neuropain$test
					names(PPC.res)[length(PPC.res)] = "Neuropain" }
				Model5.3.Info[[length(Model5.3.Info) + 1]] = PPC.res
				names(Model5.3.Info)[length(Model5.3.Info)] = "PPC"
				
				Info[[length(Info) + 1]] = Model5.3.Info
				names(Info)[length(Info)] = "Model5.3.Info"
			}
			
			Sig.5.3 = sum(Model5.3.Info$Sigs[j, (ncol(Model5.3.Info$Sigs)-1):(ncol(Model5.3.Info$Sigs))])
				
			DIC.5.3 = Model5.3.Info$DIC[j]
			
			if (j == 1) { PPC.5.3 = sum(PPC.5.3.Pain$summary)
			} else if (j == 2) { PPC.5.3 = sum(PPC.5.3.Fatigue$summary)
			} else if (j == 3) { PPC.5.3 = sum(PPC.5.3.Drowsy$summary)
			} else if (j == 4) { PPC.5.3 = sum(PPC.5.3.Sleep$summary)
			} else if (j == 5) { PPC.5.3 = sum(PPC.5.3.Thinking$summary)
			} else if (j == 6) { PPC.5.3 = sum(PPC.5.3.Constipation$summary)
			} else if (j == 7) { PPC.5.3 = sum(PPC.5.3.Neuropain$summary)
			}
			
			TestMod5.3 = c((Sig.5.3 >= 1), (DIC.5.3 < Cur.DIC - 3), (PPC.5.3 > Cur.PPC))
			if (sum(as.numeric(TestMod5.3)) >= 2) {
				BM = 5.3
				Cur.DIC = DIC.5.3
				Cur.PPC = PPC.5.3
			}
		} else if (BM == 4) {
			if (!exists("Results.5.4")) {
				###################
				# Running Model 5.4
				###################
				
				nof1 = evaluate(observations, Covs = cbind(Lag.Covs, observations$car.A, observations$car.B), 
					mod.id = 5.4, No_Neuropain)
				Results.5.4 = nof1$Results
				DIC.5.4 = nof1$DIC
				Sigs.5.4 = nof1$Sigs
				uruns.5.4 = nof1$uruns
				ForPPC.5.4 = nof1$ForPPC
				
				###################
				# PPC for model 5.4
				###################
				
				cat("\n \n \n PPC 5.4 \n \n \n")
				
				if (uruns.5.4[1] == FALSE) { PPC.5.4.Pain = ppc(observations, ForPPC.5.4, Covs = 
					cbind(Lag.Covs[ , 1], observations$car.A, observations$car.B), "Pain", 5.4)
				} else { PPC.5.4.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.4[2] == FALSE) { PPC.5.4.Fatigue = ppc(observations, ForPPC.5.4, Covs = 
					cbind(Lag.Covs[ , 2], observations$car.A, observations$car.B), "Fatigue", 5.4)
				} else { PPC.5.4.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.4[3] == FALSE) { PPC.5.4.Drowsy = ppc(observations, ForPPC.5.4, Covs = 
					cbind(Lag.Covs[ , 3], observations$car.A, observations$car.B), "Drowsy", 5.4)
				} else { PPC.5.4.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.4[4] == FALSE) { PPC.5.4.Sleep = ppc(observations, ForPPC.5.4, Covs = 
					cbind(Lag.Covs[ , 4], observations$car.A, observations$car.B), "Sleep", 5.4)
				} else { PPC.5.4.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.4[5] == FALSE) { PPC.5.4.Thinking = ppc(observations, ForPPC.5.4, Covs = 
					cbind(Lag.Covs[ , 5], observations$car.A, observations$car.B), "Thinking", 5.4)
				} else { PPC.5.4.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.4[6] == FALSE) { PPC.5.4.Constipation = ppc(observations, ForPPC.5.4, Covs = 
					cbind(Lag.Covs[ , 6], observations$car.A, observations$car.B), "Constipation", 5.4)
				} else { PPC.5.4.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (!No_Neuropain) { 
					if (uruns.5.4[7] == FALSE) { PPC.5.4.Neuropain = ppc(observations, ForPPC.5.4, Covs = 
						cbind(Lag.Covs[ , 7], observations$car.A, observations$car.B), "Neuropain", 5.4) 
					} else { PPC.5.4.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				}
				
				####################
				# Info for Model 5.4
				####################
				
				Model5.4.Info = list()
				Model5.4.Info[[length(Model5.4.Info) + 1]] = Results.5.4
				names(Model5.4.Info)[length(Model5.4.Info)] = "Results"
				Model5.4.Info[[length(Model5.4.Info) + 1]] = Sigs.5.4
				names(Model5.4.Info)[length(Model5.4.Info)] = "Sigs"
				Model5.4.Info[[length(Model5.4.Info) + 1]] = DIC.5.4
				names(Model5.4.Info)[length(Model5.4.Info)] = "DIC"
				Model5.4.Info[[length(Model5.4.Info) + 1]] = uruns.5.4
				names(Model5.4.Info)[length(Model5.4.Info)] = "uruns"
				PPC.res = list("Pain" = PPC.5.4.Pain$test, "Fatigue" = PPC.5.4.Fatigue$test, 
					"Drowsy" = PPC.5.4.Drowsy$test, "Sleep" = PPC.5.4.Sleep$test, 
					"Thinking" = PPC.5.4.Thinking$test, "Constipation" = PPC.5.4.Constipation$test)
				if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.5.4.Neuropain$test
					names(PPC.res)[length(PPC.res)] = "Neuropain" }
				Model5.4.Info[[length(Model5.4.Info) + 1]] = PPC.res
				names(Model5.4.Info)[length(Model5.4.Info)] = "PPC"
				
				Info[[length(Info) + 1]] = Model5.4.Info
				names(Info)[length(Info)] = "Model5.4.Info"
			}
			
			Sig.5.4 = sum(Model5.4.Info$Sigs[j, (ncol(Model5.4.Info$Sigs)-1):(ncol(Model5.4.Info$Sigs))])
				
			DIC.5.4 = Model5.4.Info$DIC[j]
			
			if (j == 1) { PPC.5.4 = sum(PPC.5.4.Pain$summary)
			} else if (j == 2) { PPC.5.4 = sum(PPC.5.4.Fatigue$summary)
			} else if (j == 3) { PPC.5.4 = sum(PPC.5.4.Drowsy$summary)
			} else if (j == 4) { PPC.5.4 = sum(PPC.5.4.Sleep$summary)
			} else if (j == 5) { PPC.5.4 = sum(PPC.5.4.Thinking$summary)
			} else if (j == 6) { PPC.5.4 = sum(PPC.5.4.Constipation$summary)
			} else if (j == 7) { PPC.5.4 = sum(PPC.5.4.Neuropain$summary)
			}
			
			TestMod5.4 = c((Sig.5.4 >= 1), (DIC.5.4 < Cur.DIC - 3), (PPC.5.4 > Cur.PPC))
			if (sum(as.numeric(TestMod5.4)) >= 2) {
				BM = 5.4
				Cur.DIC = DIC.5.4
				Cur.PPC = PPC.5.4
			}
		} else if (BM == 4.1) {
			if (!exists("Results.5.41")) {
				####################
				# Running Model 5.41
				####################
				
				nof1 = evaluate(observations, Covs = cbind(Lag.Covs, observations$Time2, 
					observations$car.A, observations$car.B), mod.id = 5.41, No_Neuropain)
				Results.5.41 = nof1$Results
				DIC.5.41 = nof1$DIC
				Sigs.5.41 = nof1$Sigs
				uruns.5.41 = nof1$uruns
				ForPPC.5.41 = nof1$ForPPC
				
				####################
				# PPC for model 5.41
				####################
				
				cat("\n \n \n PPC 5.41 \n \n \n")
				
				if (uruns.5.41[1] == FALSE) { PPC.5.41.Pain = ppc(observations, ForPPC.5.41, Covs = 
					cbind(Lag.Covs[ , 1], observations$Time2, observations$car.A, observations$car.B), "Pain", 5.41)
				} else { PPC.5.41.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.41[2] == FALSE) { PPC.5.41.Fatigue = ppc(observations, ForPPC.5.41, Covs = 
					cbind(Lag.Covs[ , 2], observations$Time2, observations$car.A, observations$car.B), "Fatigue", 5.41)
				} else { PPC.5.41.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.41[3] == FALSE) { PPC.5.41.Drowsy = ppc(observations, ForPPC.5.41, Covs = 
					cbind(Lag.Covs[ , 3], observations$Time2, observations$car.A, observations$car.B), "Drowsy", 5.41)
				} else { PPC.5.41.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.41[4] == FALSE) { PPC.5.41.Sleep = ppc(observations, ForPPC.5.41, Covs = 
					cbind(Lag.Covs[ , 4], observations$Time2, observations$car.A, observations$car.B), "Sleep", 5.41)
				} else { PPC.5.41.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.41[5] == FALSE) { PPC.5.41.Thinking = ppc(observations, ForPPC.5.41, Covs = 
					cbind(Lag.Covs[ , 5], observations$Time2, observations$car.A, observations$car.B), "Thinking", 5.41)
				} else { PPC.5.41.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.41[6] == FALSE) { PPC.5.41.Constipation = ppc(observations, ForPPC.5.41, Covs = 
					cbind(Lag.Covs[ , 6], observations$Time2, observations$car.A, observations$car.B), "Constipation", 5.41)
				} else { PPC.5.41.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (!No_Neuropain) { 
					if (uruns.5.41[7] == FALSE) { PPC.5.41.Neuropain = ppc(observations, ForPPC.5.41, Covs = 
						cbind(Lag.Covs[ , 7], observations$Time2, observations$car.A, observations$car.B), "Neuropain", 5.41) 
					} else { PPC.5.41.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				}
				
				#####################
				# Info for Model 5.41
				#####################
				
				Model5.41.Info = list()
				Model5.41.Info[[length(Model5.41.Info) + 1]] = Results.5.41
				names(Model5.41.Info)[length(Model5.41.Info)] = "Results"
				Model5.41.Info[[length(Model5.41.Info) + 1]] = Sigs.5.41
				names(Model5.41.Info)[length(Model5.41.Info)] = "Sigs"
				Model5.41.Info[[length(Model5.41.Info) + 1]] = DIC.5.41
				names(Model5.41.Info)[length(Model5.41.Info)] = "DIC"
				Model5.41.Info[[length(Model5.41.Info) + 1]] = uruns.5.41
				names(Model5.41.Info)[length(Model5.41.Info)] = "uruns"
				PPC.res = list("Pain" = PPC.5.41.Pain$test, "Fatigue" = PPC.5.41.Fatigue$test, 
					"Drowsy" = PPC.5.41.Drowsy$test, "Sleep" = PPC.5.41.Sleep$test, 
					"Thinking" = PPC.5.41.Thinking$test, "Constipation" = PPC.5.41.Constipation$test)
				if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.5.41.Neuropain$test
					names(PPC.res)[length(PPC.res)] = "Neuropain" }
				Model5.41.Info[[length(Model5.41.Info) + 1]] = PPC.res
				names(Model5.41.Info)[length(Model5.41.Info)] = "PPC"
				
				Info[[length(Info) + 1]] = Model5.41.Info
				names(Info)[length(Info)] = "Model5.41.Info"
			}
			
			Sig.5.41 = sum(Model5.41.Info$Sigs[j, (ncol(Model5.41.Info$Sigs)-1):(ncol(Model5.41.Info$Sigs))])
				
			DIC.5.41 = Model5.41.Info$DIC[j]
			
			if (j == 1) { PPC.5.41 = sum(PPC.5.41.Pain$summary)
			} else if (j == 2) { PPC.5.41 = sum(PPC.5.41.Fatigue$summary)
			} else if (j == 3) { PPC.5.41 = sum(PPC.5.41.Drowsy$summary)
			} else if (j == 4) { PPC.5.41 = sum(PPC.5.41.Sleep$summary)
			} else if (j == 5) { PPC.5.41 = sum(PPC.5.41.Thinking$summary)
			} else if (j == 6) { PPC.5.41 = sum(PPC.5.41.Constipation$summary)
			} else if (j == 7) { PPC.5.41 = sum(PPC.5.41.Neuropain$summary)
			}
			
			TestMod5.41 = c((Sig.5.41 >= 1), (DIC.5.41 < Cur.DIC - 3), (PPC.5.41 > Cur.PPC))
			if (sum(as.numeric(TestMod5.41)) >= 2) {
				BM = 5.41
				Cur.DIC = DIC.5.41
				Cur.PPC = PPC.5.41
			}
		} else if (BM == 4.2) {
			if (!exists("Results.5.42")) {
				####################
				# Running Model 5.42
				####################
				
				nof1 = evaluate(observations, Covs = cbind(Lag.Covs, Block.Covs, 
					observations$car.A, observations$car.B), mod.id = 5.42, No_Neuropain)
				Results.5.42 = nof1$Results
				DIC.5.42 = nof1$DIC
				Sigs.5.42 = nof1$Sigs
				uruns.5.42 = nof1$uruns
				ForPPC.5.42 = nof1$ForPPC
				
				####################
				# PPC for model 5.42
				####################
				
				cat("\n \n \n PPC 5.42 \n \n \n")
				
				if (uruns.5.42[1] == FALSE) { PPC.5.42.Pain = ppc(observations, ForPPC.5.42, Covs = 
					cbind(Lag.Covs[ , 1], Block.Covs, observations$car.A, observations$car.B), "Pain", 5.42)
				} else { PPC.5.42.Pain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.42[2] == FALSE) { PPC.5.42.Fatigue = ppc(observations, ForPPC.5.42, Covs = 
					cbind(Lag.Covs[ , 2], Block.Covs, observations$car.A, observations$car.B), "Fatigue", 5.42)
				} else { PPC.5.42.Fatigue = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.42[3] == FALSE) { PPC.5.42.Drowsy = ppc(observations, ForPPC.5.42, Covs = 
					cbind(Lag.Covs[ , 3], Block.Covs, observations$car.A, observations$car.B), "Drowsy", 5.42)
				} else { PPC.5.42.Drowsy = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.42[4] == FALSE) { PPC.5.42.Sleep = ppc(observations, ForPPC.5.42, Covs = 
					cbind(Lag.Covs[ , 4], Block.Covs, observations$car.A, observations$car.B), "Sleep", 5.42)
				} else { PPC.5.42.Sleep = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.42[5] == FALSE) { PPC.5.42.Thinking = ppc(observations, ForPPC.5.42, Covs = 
					cbind(Lag.Covs[ , 5], Block.Covs, observations$car.A, observations$car.B), "Thinking", 5.42)
				} else { PPC.5.42.Thinking = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (uruns.5.42[6] == FALSE) { PPC.5.42.Constipation = ppc(observations, ForPPC.5.42, Covs = 
					cbind(Lag.Covs[ , 6], Block.Covs, observations$car.A, observations$car.B), "Constipation", 5.42)
				} else { PPC.5.42.Constipation = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				
				if (!No_Neuropain) { 
					if (uruns.5.42[7] == FALSE) { PPC.5.42.Neuropain = ppc(observations, ForPPC.5.42, Covs = 
						cbind(Lag.Covs[ , 7], Block.Covs, observations$car.A, observations$car.B), "Neuropain", 5.42) 
					} else { PPC.5.42.Neuropain = list("SimY" = NULL, "tests" = NULL, "summary" = rep(0, 11)) }
				}
				
				#####################
				# Info for Model 5.42
				#####################
				
				Model5.42.Info = list()
				Model5.42.Info[[length(Model5.42.Info) + 1]] = Results.5.42
				names(Model5.42.Info)[length(Model5.42.Info)] = "Results"
				Model5.42.Info[[length(Model5.42.Info) + 1]] = Sigs.5.42
				names(Model5.42.Info)[length(Model5.42.Info)] = "Sigs"
				Model5.42.Info[[length(Model5.42.Info) + 1]] = DIC.5.42
				names(Model5.42.Info)[length(Model5.42.Info)] = "DIC"
				Model5.42.Info[[length(Model5.42.Info) + 1]] = uruns.5.42
				names(Model5.42.Info)[length(Model5.42.Info)] = "uruns"
				PPC.res = list("Pain" = PPC.5.42.Pain$test, "Fatigue" = PPC.5.42.Fatigue$test, 
					"Drowsy" = PPC.5.42.Drowsy$test, "Sleep" = PPC.5.42.Sleep$test, 
					"Thinking" = PPC.5.42.Thinking$test, "Constipation" = PPC.5.42.Constipation$test)
				if (!No_Neuropain) { PPC.res[[length(PPC.res) + 1]] = PPC.5.42.Neuropain$test
					names(PPC.res)[length(PPC.res)] = "Neuropain" }
				Model5.42.Info[[length(Model5.42.Info) + 1]] = PPC.res
				names(Model5.42.Info)[length(Model5.42.Info)] = "PPC"
				
				Info[[length(Info) + 1]] = Model5.42.Info
				names(Info)[length(Info)] = "Model5.42.Info"
			}
			
			Sig.5.42 = sum(Model5.42.Info$Sigs[j, (ncol(Model5.42.Info$Sigs)-1):(ncol(Model5.42.Info$Sigs))])
				
			DIC.5.42 = Model5.42.Info$DIC[j]
			
			if (j == 1) { PPC.5.42 = sum(PPC.5.42.Pain$summary)
			} else if (j == 2) { PPC.5.42 = sum(PPC.5.42.Fatigue$summary)
			} else if (j == 3) { PPC.5.42 = sum(PPC.5.42.Drowsy$summary)
			} else if (j == 4) { PPC.5.42 = sum(PPC.5.42.Sleep$summary)
			} else if (j == 5) { PPC.5.42 = sum(PPC.5.42.Thinking$summary)
			} else if (j == 6) { PPC.5.42 = sum(PPC.5.42.Constipation$summary)
			} else if (j == 7) { PPC.5.42 = sum(PPC.5.42.Neuropain$summary)
			}
			
			TestMod5.42 = c((Sig.5.42 >= 1), (DIC.5.42 < Cur.DIC - 3), (PPC.5.42 > Cur.PPC))
			if (sum(as.numeric(TestMod5.42)) >= 2) {
				BM = 5.42
				Cur.DIC = DIC.5.42
				Cur.PPC = PPC.5.42
			}
		}
		Best.Model[j] = BM
	}
	
	observations.Car = observations
	for (i in nrow(observations.Car):1) {
		if (observations.Car$car.A[i] == 1 || observations.Car$car.B[i] == 1) {
			observations.Car = observations.Car[-i, ]
		}
	}
	observations.Car = observations.Car[ , -c(ncol(observations.Car)-1, ncol(observations.Car))]
	
	for (j in 1:length(Best.Model)) {
		if (Best.Model[j] == 5.1) {
			if (!exists("Results.1c")) {
				#################
				# Running Model 1
				#################
				
				nof1 = evaluate(observations.Car, Covs = NULL, mod.id = 1, No_Neuropain)
				Results.1c = nof1$Results
				DIC.1c = nof1$DIC
				Sigs.1c = nof1$Sigs
				uruns.1c = nof1$uruns
				ForPPC.1c = nof1$ForPPC
				
				##################
				# Info for Model 1
				##################
				
				Model1c.Info = list()
				Model1c.Info[[length(Model1c.Info) + 1]] = Results.1c
				names(Model1c.Info)[length(Model1c.Info)] = "Results"
				Model1c.Info[[length(Model1c.Info) + 1]] = Sigs.1c
				names(Model1c.Info)[length(Model1c.Info)] = "Sigs"
				Model1c.Info[[length(Model1c.Info) + 1]] = DIC.1c
				names(Model1c.Info)[length(Model1c.Info)] = "DIC"
				Model1c.Info[[length(Model1c.Info) + 1]] = uruns.1c
				names(Model1c.Info)[length(Model1c.Info)] = "uruns"
				
				Info[[length(Info) + 1]] = Model1c.Info
				names(Info)[length(Info)] = "Model1c.Info"
			}
		} else if (Best.Model[j] == 5.2) {
			if (!exists("Results.2c")) {
				#################
				# Running Model 2
				#################
				
				nof1 = evaluate(observations.Car, Covs = observations$Time2, mod.id = 2, No_Neuropain)
				Results.2c = nof1$Results
				DIC.2c = nof1$DIC
				Sigs.2c = nof1$Sigs
				uruns.2c = nof1$uruns
				ForPPC.2c = nof1$ForPPC
				
				##################
				# Info for Model 2
				##################
				
				Model2c.Info = list()
				Model2c.Info[[length(Model2c.Info) + 1]] = Results.2c
				names(Model2c.Info)[length(Model2c.Info)] = "Results"
				Model2c.Info[[length(Model2c.Info) + 1]] = Sigs.2c
				names(Model2c.Info)[length(Model2c.Info)] = "Sigs"
				Model2c.Info[[length(Model2c.Info) + 1]] = DIC.2c
				names(Model2c.Info)[length(Model2c.Info)] = "DIC"
				Model2c.Info[[length(Model2c.Info) + 1]] = uruns.2c
				names(Model2c.Info)[length(Model2c.Info)] = "uruns"
				
				Info[[length(Info) + 1]] = Model2c.Info
				names(Info)[length(Info)] = "Model2c.Info"
			}
		} else if (Best.Model[j] == 5.3) {
			if (!exists("Results.3c")) {
				#################
				# Running Model 3
				#################
				
				nof1 = evaluate(observations.Car, Covs = Block.Covs, mod.id = 3, No_Neuropain)
				Results.3c = nof1$Results
				DIC.3c = nof1$DIC
				Sigs.3c = nof1$Sigs
				uruns.3c = nof1$uruns
				ForPPC.3c = nof1$ForPPC
				
				##################
				# Info for Model 3
				##################
				
				Model3c.Info = list()
				Model3c.Info[[length(Model3c.Info) + 1]] = Results.3c
				names(Model3c.Info)[length(Model3c.Info)] = "Results"
				Model3c.Info[[length(Model3c.Info) + 1]] = Sigs.3c
				names(Model3c.Info)[length(Model3c.Info)] = "Sigs"
				Model3c.Info[[length(Model3c.Info) + 1]] = DIC.3c
				names(Model3c.Info)[length(Model3c.Info)] = "DIC"
				Model3c.Info[[length(Model3c.Info) + 1]] = uruns.3c
				names(Model3c.Info)[length(Model3c.Info)] = "uruns"
				
				Info[[length(Info) + 1]] = Model3c.Info
				names(Info)[length(Info)] = "Model3c.Info"
			}
		} else if (Best.Model[j] == 5.4) {
			if (!exists("Results.4c")) {
				#################
				# Running Model 4
				#################
				
				nof1 = evaluate(observations.Car, Covs = Lag.Covs, mod.id = 4, No_Neuropain)
				Results.4c = nof1$Results
				DIC.4c = nof1$DIC
				Sigs.4c = nof1$Sigs
				uruns.4c = nof1$uruns
				ForPPC.4c = nof1$ForPPC
				
				##################
				# Info for Model 4
				##################
				
				Model4c.Info = list()
				Model4c.Info[[length(Model4c.Info) + 1]] = Results.4c
				names(Model4c.Info)[length(Model4c.Info)] = "Results"
				Model4c.Info[[length(Model4c.Info) + 1]] = Sigs.4c
				names(Model4c.Info)[length(Model4c.Info)] = "Sigs"
				Model4c.Info[[length(Model4c.Info) + 1]] = DIC.4c
				names(Model4c.Info)[length(Model4c.Info)] = "DIC"
				Model4c.Info[[length(Model4c.Info) + 1]] = uruns.4c
				names(Model4c.Info)[length(Model4c.Info)] = "uruns"
				
				Info[[length(Info) + 1]] = Model4c.Info
				names(Info)[length(Info)] = "Model4c.Info"
			}
		} else if (Best.Model[j] == 5.41) {
			if (!exists("Results.4.1c")) {
				###################
				# Running Model 4.1
				###################
				
				nof1 = evaluate(observations.Car, Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
				Results.4.1c = nof1$Results
				DIC.4.1c = nof1$DIC
				Sigs.4.1c = nof1$Sigs
				uruns.4.1c = nof1$uruns
				ForPPC.4.1c = nof1$ForPPC
				
				####################
				# Info for Model 4.1
				####################
				
				Model4.1c.Info = list()
				Model4.1c.Info[[length(Model4.1c.Info) + 1]] = Results.4.1c
				names(Model4.1c.Info)[length(Model4.1c.Info)] = "Results"
				Model4.1c.Info[[length(Model4.1c.Info) + 1]] = Sigs.4.1c
				names(Model4.1c.Info)[length(Model4.1c.Info)] = "Sigs"
				Model4.1c.Info[[length(Model4.1c.Info) + 1]] = DIC.4.1c
				names(Model4.1c.Info)[length(Model4.1c.Info)] = "DIC"
				Model4.1c.Info[[length(Model4.1c.Info) + 1]] = uruns.4.1c
				names(Model4.1c.Info)[length(Model4.1c.Info)] = "uruns"
				
				Info[[length(Info) + 1]] = Model4.1c.Info
				names(Info)[length(Info)] = "Model4.1c.Info"
			}
		} else if (Best.Model[j] == 5.42) {
			if (!exists("Results.4.2c")) {
				###################
				# Running Model 4.2
				###################
				
				nof1 = evaluate(observations.Car, Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
				Results.4.2c = nof1$Results
				DIC.4.2c = nof1$DIC
				Sigs.4.2c = nof1$Sigs
				uruns.4.2c = nof1$uruns
				ForPPC.4.2c = nof1$ForPPC
				
				####################
				# Info for Model 4.2
				####################
				
				Model4.2c.Info = list()
				Model4.2c.Info[[length(Model4.2.Info) + 1]] = Results.4.2c
				names(Model4.2c.Info)[length(Model4.2.Info)] = "Results"
				Model4.2c.Info[[length(Model4.2c.Info) + 1]] = Sigs.4.2c
				names(Model4.2c.Info)[length(Model4.2c.Info)] = "Sigs"
				Model4.2c.Info[[length(Model4.2c.Info) + 1]] = DIC.4.2c
				names(Model4.2c.Info)[length(Model4.2c.Info)] = "DIC"
				Model4.2c.Info[[length(Model4.2c.Info) + 1]] = uruns.4.2c
				names(Model4.2c.Info)[length(Model4.2c.Info)] = "uruns"
				
				Info[[length(Info) + 1]] = Model4.2c.Info
				names(Info)[length(Info)] = "Model4.2c.Info"
			}
		}
	}
	
	###########################
	# Rearranging nof1 Output
	###########################

	if (Best.Model[1] == 1) { Results = Results.1
	} else if (Best.Model[1] == 2) { Results = Results.2
	} else if (Best.Model[1] == 3) { Results = Results.3
	} else if (Best.Model[1] == 4) { Results = Results.4
	} else if (Best.Model[1] == 4.1) { Results = Results.4.1
	} else if (Best.Model[1] == 4.2) { Results = Results.4.2
	} else if (Best.Model[1] == 5.1) { Results = Results.1c
	} else if (Best.Model[1] == 5.2) { Results = Results.2c
	} else if (Best.Model[1] == 5.3) { Results = Results.3c
	} else if (Best.Model[1] == 5.4) { Results = Results.4c
	} else if (Best.Model[1] == 5.41) { Results = Results.4.1c
	} else if (Best.Model[1] == 5.42) { Results = Results.4.2c
	}
	
	for (i in 2:nouts) {
		if (Best.Model[i] == 1) { Results[i, ] = Results.1[i, ]
		} else if (Best.Model[i] == 2) { Results[i, ] = Results.2[i, ]
		} else if (Best.Model[i] == 3) { Results[i, ] = Results.3[i, ]
		} else if (Best.Model[i] == 4) { Results[i, ] = Results.4[i, ]
		} else if (Best.Model[i] == 4.1) { Results[i, ] = Results.4.1[i, ]
		} else if (Best.Model[i] == 4.2) { Results[i, ] = Results.4.2[i, ]
		} else if (Best.Model[i] == 5.1 && Model1c.Info$uruns[i] == FALSE) { Results[i, ] = Results.1c[i, ]
		} else if (Best.Model[i] == 5.1 && Model1c.Info$uruns[i] == TRUE) { Results[i, ] = Results.1[i, ]
		} else if (Best.Model[i] == 5.2 && Model2c.Info$uruns[i] == FALSE) { Results[i, ] = Results.2c[i, ]
		} else if (Best.Model[i] == 5.2 && Model2c.Info$uruns[i] == TRUE) { Results[i, ] = Results.2c[i, ]
		} else if (Best.Model[i] == 5.3 && Model3c.Info$uruns[i] == FALSE) { Results[i, ] = Results.3c[i, ]
		} else if (Best.Model[i] == 5.3 && Model3c.Info$uruns[i] == TRUE) { Results[i, ] = Results.3c[i, ]
		} else if (Best.Model[i] == 5.4 && Model4c.Info$uruns[i] == FALSE) { Results[i, ] = Results.4c[i, ]
		} else if (Best.Model[i] == 5.4 && Model4c.Info$uruns[i] == TRUE) { Results[i, ] = Results.4c[i, ]
		} else if (Best.Model[i] == 5.41 && Model4.1c.Info$uruns[i] == FALSE) { Results[i, ] = Results.4.1c[i, ]
		} else if (Best.Model[i] == 5.41 && Model4.1c.Info$uruns[i] == TRUE) { Results[i, ] = Results.4.1c[i, ]
		} else if (Best.Model[i] == 5.42 && Model4.2c.Info$uruns[i] == FALSE) { Results[i, ] = Results.4.2c[i, ]
		} else if (Best.Model[i] == 5.42 && Model4.2c.Info$uruns[i] == TRUE) { Results[i, ] = Results.4.2c[i, ]
		}
	}	
	
	uruns = rep(NA, nouts)
	for (i in 1:nouts) {
		if (Best.Model[i] == 1) { uruns[i] = uruns.1[i]
		} else if (Best.Model[i] == 2) { uruns[i] = uruns.2[i]
		} else if (Best.Model[i] == 3) { uruns[i] = uruns.3[i]
		} else if (Best.Model[i] == 4) { uruns[i] = uruns.4[i]
		} else if (Best.Model[i] == 4.1) { uruns[i] = uruns.4.1[i]
		} else if (Best.Model[i] == 4.2) { uruns[i] = uruns.4.2[i]
		} else if (Best.Model[i] == 5.1) { uruns[i] = uruns.1c[i]
		} else if (Best.Model[i] == 5.2) { uruns[i] = uruns.2c[i]
		} else if (Best.Model[i] == 5.3) { uruns[i] = uruns.3c[i]
		} else if (Best.Model[i] == 5.4) { uruns[i] = uruns.4c[i]
		} else if (Best.Model[i] == 5.41) { uruns[i] = uruns.4.1c[i]
		} else if (Best.Model[i] == 5.42) { uruns[i] = uruns.4.2c[i]
		}
	}
	
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

	meta.data[[length(meta.data) + 1]] = c("1: Null Covs", "2: Time Covs", "3: Block Covs", "4: Lagged Effects", 
		"4.1: Time and Lag", "4.2: Block and Lag", "5.__: Best of Previous with Carryover")
	names(meta.data)[length(meta.data)] = "Models"
	
	meta.data[[length(meta.data) + 1]] = Best.Model
	names(meta.data)[length(meta.data)] = "Best.Model"
	
	if (No_Neuropain) {
		out = list(pain, fatigue, drowsiness, sleep_problems, thinking_problems, constipation,
			meta.data, Info, Results)
		names(out) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems", "constipation", 
			"meta.data", "Info", "Results")
	} else {
		out = list(pain, fatigue, drowsiness, sleep_problems, thinking_problems, constipation, neuropathic_pain,
			meta.data, Info, Results)
		names(out) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems", "constipation", 
			"neuropathic_pain", "meta.data", "Info", "Results")
	}
	
	return(out)	
}

