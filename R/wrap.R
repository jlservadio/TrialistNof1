wrap <- function(data, metadata) {
	cleaned = cleanData(data, metadata)
	observations = cleaned[[1]]
	Block.Covs = cleaned[[2]]
	Lag.Covs = cleaned[[3]]
	No_Neuropain = cleaned[[4]]
	meta.data = cleaned[[5]]
	insufficient_data = cleaned[[6]]
	
	observations.car = observations
	for (i in nrow(observations.car):1) {
		if (observations.car$car.A[i] == 1) { observations.car = observations.car[-i, ] }
		if (observations.car$car.B[i] == 1) { observations.car = observations.car[-i, ] }
	}
	
	###################
	# Analysis for Pain
	###################
	
	Pain.M1 = evaluate(observations, Outcome = "Pain", Covs = NULL, mod.id = 1, No_Neuropain)
	Pain.M2 = evaluate(observations, Outcome = "Pain", Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Pain.M3 = evaluate(observations, Outcome = "Pain", Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Pain.M4 = evaluate(observations, Outcome = "Pain", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Pain.M4.1 = evaluate(observations, Outcome = "Pain", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Pain.M4.2 = evaluate(observations, Outcome = "Pain", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	
	Pain.Info = list(Pain.M1, Pain.M2, Pain.M3, Pain.M4, Pain.M4.1, Pain.M4.2)
	names(Pain.Info) = c("Pain.M1", "Pain.M2", "Pain.M3", "Pain.M4", "Pain.M4.1", "Pain.M4.2")
	
	BM = 1
	Cur.DIC = Pain.M1$DIC
	
	if (Pain.M2$Sigs[2] == TRUE && Pain.M2$DIC < Pain.M1$DIC - 3) { 
		BM = 2
		Cur.DIC = Pain.M2$DIC
	} else if (Pain.M2$Sigs[2] == FALSE && Pain.M2$DIC >= Pain.M1$DIC - 3) { BM = 1
	} else {
		PPC.Pain.1 = ppc(observations, "Pain", Pain.M1$ForPPC, Covs = NULL, 1)
		Pain.M1[[length(Pain.M1) + 1]] = PPC.Pain.1
		names(Pain.M1)[length(Pain.M1)] = "PPC.Pain.1"
		PPC.Pain.2 = ppc(observations, "Pain", Pain.M2$ForPPC, observations$Time2, 2)
		Pain.M2[[length(Pain.M2) + 1]] = PPC.Pain.2
		names(Pain.M2)[length(Pain.M2)] = "PPC.Pain.2"
		if (sum(PPC.Pain.2$Summary) > sum(PPC.Pain.1$Summary)) { 
			BM = 2 
			Cur.DIC = Pain.M2$DIC
		}
	}
	
	if (sum(Pain.M3$Sigs[ , 2:ncol(Pain.M3$Sigs)]) > 0 && Pain.M3$DIC < Cur.DIC - 3) {
		BM = 3
		Cur.DIC = Pain.M3$DIC
	} else if (sum(Pain.M3$Sigs[ , 2:ncol(Pain.M3$Sigs)]) == 0 && Pain.M3$DIC >= Cur.DIC - 3) { BM = BM
	} else {
		PPC.Pain.3 = ppc(observations, "Pain", Pain.M3$ForPPC, Block.Covs, 3)
		Pain.M3[[length(Pain.M3) + 1]] = PPC.Pain.3
		names(Pain.M3)[length(Pain.M3)] = "PPC.Pain.3"
		if (BM == 1 && !exists("PPC.Pain.1")) { Cur.PPC = ppc(observations, "Pain", Pain.M1$ForPPC, Covs = NULL, 1) 
			Pain.M1[[length(Pain.M1) + 1]] = Cur.PPC
		names(Pain.M1)[length(Pain.M1)] = "PPC.Pain.1"
		} else if (BM == 1 && exists("PPC.Pain.1")) { Cur.PPC = PPC.Pain.1 }
		
		if (BM == 2 && !exists("PPC.Pain.2")) {
			Cur.PPC = ppc(observations, "Pain", Pain.M2$ForPPC, observations$Time2, 2)
			Pain.M2[[length(Pain.M2) + 1]] = Cur.PPC
		names(Pain.M2)[length(Pain.M2)] = "PPC.Pain.2"
		} else if (BM == 2 && exists("PPC.Pain.2")) { Cur.PPC = PPC.Pain.2 }
		
		if (sum(PPC.Pain.3$Summary) > sum(Cur.PPC$Summary)) {
			BM = 3
			Cur.DIC = Pain.M3$DIC
		}
	}
	
	if (BM == 1) {
		if (Pain.M4$Sigs[1, 2] == TRUE && Pain.M4$DIC < Cur.DIC - 3) {
			BM = 4
			Cur.DIC = Pain.M4$DIC
		} else if (Pain.M4$Sigs[1, 2] == FALSE && Pain.M4$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Pain.4 = ppc(observations, "Pain", Pain.M4$ForPPC, Lag.Covs[ , 1], 4)
			Pain.M4[[length(Pain.M4) + 1]] = PPC.Pain.4
			names(Pain.M4)[length(Pain.M4)] = "PPC.Pain.4"
			if (!exists("PPC.Pain.1")) { PPC.Pain.1 = ppc(observations, "Pain", Covs = NULL, 1) 
				Pain.M1[[length(Pain.M1) + 1]] = PPC.Pain.1
				names(Pain.M1)[length(Pain.M1)] = "PPC.Pain.1"
			}
			if (sum(PPC.Pain.4$Summary) > sum(PPC.Pain.1$Summary)) {
				BM = 4
				Cur.DIC = Pain.M4$DIC
			}
		}
	} else if (BM == 2) {
		if (Pain.M4.1$Sigs[1, 2] == TRUE && Pain.M4.1$DIC < Cur.DIC - 3) {
			BM = 4.1
			Cur.DIC = Pain.M4.1$DIC
		} else if (Pain.M4.1$Sigs[1, 2] == FALSE && Pain.M4.1$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Pain.4.1 = ppc(observations, "Pain", Pain.M4.1$ForPPC, cbind(Lag.Covs[ , 1], observations$Time2), 4.1)
			Pain.M1[[length(Pain.M4.1) + 1]] = PPC.Pain.4.1
			names(Pain.M4.1)[length(Pain.M4.1)] = "PPC.Pain.4.1"
			if (!exists("PPC.Pain.2")) { PPC.Pain.2 = ppc(observations, "Pain", observations$Time2, 2) 
				Pain.M2[[length(Pain.M2) + 1]] = PPC.Pain.2
				names(Pain.M2)[length(Pain.M2)] = "PPC.Pain.2"
			}
			if (sum(PPC.Pain.4.1$Summary) > sum(PPC.Pain.2$Summary)) {
				BM = 4.1
				Cur.DIC = Pain.M4.1$DIC
			}
		}
	} else if (BM == 3) {
		if (Pain.M4.2$Sigs[1, 2] == TRUE && Pain.M4.2$DIC < Cur.DIC - 3) {
			BM = 4.2
			Cur.DIC = Pain.M4.2$DIC
		} else if (Pain.M4.2$Sigs[1, 2] == FALSE && Pain.M4.2$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Pain.4.2 = ppc(observations, "Pain", Pain.M4.2$ForPPC, cbind(Lag.Covs[ , 1], Block.Covs), 4.2)
			Pain.M4.2[[length(Pain.M1) + 1]] = PPC.Pain.4.2
		names(Pain.M4.2)[length(Pain.M4.2)] = "PPC.Pain.4.2"
			if (!exists("PPC.Pain.3")) { PPC.Pain.3 = ppc(observations, "Pain", Block.Covs, 3) 
				Pain.M3[[length(Pain.M3) + 1]] = PPC.Pain.3
				names(Pain.M3)[length(Pain.M3)] = "PPC.Pain.3"
			}
			if (sum(PPC.Pain.4.2$Summary) > sum(PPC.Pain.3$Summary)) {
				BM = 4.2
				Cur.DIC = Pain.M4.2$DIC
			}
		}
	}
	
	if (BM == 1) {
		Pain.M5.1 = evaluate(observations, "Pain", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
		Pain.Info[[length(Pain.Info) + 1]] = Pain.M5.1
		names(Pain.Info)[length(Pain.Info)] = "Pain.M5.1"
		if ((Pain.M5.1$Sigs[1, 2] == TRUE || Pain.M5.1$Sigs[1, 3] == TRUE) && Pain.M5.1$DIC < Pain.M1$DIC - 3) {
			BM = 5.1
			Pain.M1c = evaluate(observations.car, "Pain", Covs = NULL, 1, No_Neuropain)
			Pain.Info[[length(Pain.Info) + 1]] = Pain.M1c
			names(Pain.Info)[length(Pain.Info)] = "Pain.M1c"
			Cur.DIC = Pain.M1c$DIC
		} else if ((Pain.M5.1$Sigs[1, 2] == FALSE || Pain.M5.1$Sigs[1, 3] == FALSE) && Pain.M5.1$DIC >= Pain.M1$DIC - 3) { BM = BM		
		} else {
			PPC.Pain.5.1 = ppc(observations, "Pain", Pain.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
			Pain.M5.1[[length(Pain.M5.1) + 1]] = PPC.Pain.5.1
			names(Pain.M5.1)[length(Pain.M5.1)] = "PPC.Pain.5.1"
			if (!exists("PPC.Pain.1")) { PPC.Pain.1 = ppc(observations, "Pain", Covs = NULL, 1) 
				Pain.M1[[length(Pain.M1) + 1]] = PPC.Pain.1
				names(Pain.M1)[length(Pain.M1)] = "PPC.Pain.1"
			}
			if (sum(PPC.Pain.5.1$Summary) > sum(PPC.Pain.1$Summary)) {
				BM = 5.1
				Pain.M1c = evaluate(observations.car, "Pain", Covs = NULL, 1, No_Neuropain)
				Pain.Info[[length(Pain.Info) + 1]] = Pain.M1c
				names(Pain.Info)[length(Pain.Info)] = "Pain.M1c"
				Cur.DIC = Pain.M1c$DIC
			}
		}
	} else if (BM == 2) {
		Pain.M5.2 = evaluate(observations, "Pain", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
		Pain.Info[[length(Pain.Info) + 1]] = Pain.M5.2
		names(Pain.Info)[length(Pain.Info)] = "Pain.M5.2"
		if ((Pain.M5.2$Sigs[1, 3] == TRUE || Pain.M5.2$Sigs[1, 4] == TRUE) && Pain.M5.2$DIC < Pain.M2$DIC - 3) {
			BM = 5.2
			Pain.M2c = evaluate(observations.car, "Pain", observations$Time2, 2, No_Neuropain)
			Pain.Info[[length(Pain.Info) + 1]] = Pain.M2c
			names(Pain.Info)[length(Pain.Info)] = "Pain.M2c"
			Cur.DIC = Pain.M2c$DIC
		} else if ((Pain.M5.2$Sigs[1, 3] == FALSE || Pain.M5.2$Sigs[1, 4] == FALSE) && Pain.M5.2$DIC >= Pain.M2$DIC - 3) { BM = BM		
		} else {
			PPC.Pain.5.2 = ppc(observations, "Pain", Pain.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
			Pain.M5.2[[length(Pain.M5.2) + 1]] = PPC.Pain.5.2
			names(Pain.M5.2)[length(Pain.M5.2)] = "PPC.Pain.5.2"
			if (!exists("PPC.Pain.2")) { PPC.Pain.2 = ppc(observations, "Pain", observations$Time2, 2) 
				Pain.M2[[length(Pain.M2) + 1]] = PPC.Pain.2
				names(Pain.M2)[length(Pain.M2)] = "PPC.Pain.2"
			}
			if (sum(PPC.Pain.5.2$Summary) > sum(PPC.Pain.2$Summary)) {
				BM = 5.2
				Pain.M2c = evaluate(observations.car, "Pain", observations$Time2, 2, No_Neuropain)
				Pain.Info[[length(Pain.Info) + 1]] = Pain.M2c
				names(Pain.Info)[length(Pain.Info)] = "Pain.M2c"
				Cur.DIC = Pain.M2c$DIC
			}
		}
	} else if (BM == 3) {
		Pain.M5.3 = evaluate(observations, "Pain", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
		Pain.Info[[length(Pain.Info) + 1]] = Pain.M5.3
		names(Pain.Info)[length(Pain.Info)] = "Pain.M5.3"
		if ((Pain.M5.3$Sigs[1, ncol(Pain.M5.3$Sigs)-1] == TRUE || Pain.M5.3$Sigs[1, ncol(Pain.M5.3$Sigs)] == TRUE) && 
			Pain.M5.3$DIC < Pain.M3$DIC - 3) {
			BM = 5.3
			Pain.M3c = evaluate(observations.car, "Pain", Block.Covs, 3, No_Neuropain)
			Pain.Info[[length(Pain.Info) + 1]] = Pain.M3c
			names(Pain.Info)[length(Pain.Info)] = "Pain.M3c"
			Cur.DIC = Pain.M3c$DIC
		} else if ((Pain.M5.3$Sigs[1, ncol(Pain.M5.3$Sigs)-1] == FALSE || Pain.M5.3$Sigs[1, ncol(Pain.M5.3$Sigs)] == FALSE) && 
			Pain.M5.3$DIC >= Pain.M3$DIC - 3) { BM = BM		
		} else {
			PPC.Pain.5.3 = ppc(observations, "Pain", Pain.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
			Pain.M5.3[[length(Pain.M5.3) + 1]] = PPC.Pain.5.3
			names(Pain.M5.3)[length(Pain.M5.3)] = "PPC.Pain.5.3"
			if (!exists("PPC.Pain.3")) { PPC.Pain.3 = ppc(observations, "Pain", Block.Covs, 3) 
				Pain.M3[[length(Pain.M3) + 1]] = PPC.Pain.3
				names(Pain.M3)[length(Pain.M3)] = "PPC.Pain.3"
			}
			if (sum(PPC.Pain.5.3$Summary) > sum(PPC.Pain.3$Summary)) {
				BM = 5.3
				Pain.M3c = evaluate(observations.car, "Pain", Block.Covs, 3, No_Neuropain)
				Pain.Info[[length(Pain.Info) + 1]] = Pain.M3c
				names(Pain.Info)[length(Pain.Info)] = "Pain.M3c"
				Cur.DIC = Pain.M3c$DIC
			}
		}
	} else if (BM == 4) {
		Pain.M5.4 = evaluate(observations, "Pain", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
		Pain.Info[[length(Pain.Info) + 1]] = Pain.M5.4
		names(Pain.Info)[length(Pain.Info)] = "Pain.M5.4"
		if ((Pain.M5.4$Sigs[1, ncol(Pain.M5.4$Sigs)-1] == TRUE || Pain.M5.4$Sigs[1, ncol(Pain.M5.4$Sigs)] == TRUE) && 
			Pain.M5.4$DIC < Pain.M4$DIC - 3) {
			BM = 5.4
			Pain.M4c = evaluate(observations.car, "Pain", Lag.Covs, 4, No_Neuropain)
			Pain.Info[[length(Pain.Info) + 1]] = Pain.M4c
			names(Pain.Info)[length(Pain.Info)] = "Pain.M4c"
			Cur.DIC = Pain.M4c$DIC
		} else if ((Pain.M5.4$Sigs[1, ncol(Pain.M5.4$Sigs)-1] == FALSE && Pain.M5.4$Sigs[1, ncol(Pain.M5.4$Sigs)] == FALSE) && 
			Pain.M5.4$DIC >= Pain.M4$DIC - 3) { BM = BM		
		} else {
			PPC.Pain.5.4 = ppc(observations, "Pain", Pain.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
			Pain.M5.4[[length(Pain.M5.4) + 1]] = PPC.Pain.5.4
			names(Pain.M5.4)[length(Pain.M5.4)] = "PPC.Pain.5.4"
			if (!exists("PPC.Pain.4")) { PPC.Pain.4 = ppc(observations, "Pain", Lag.Covs[ , 1], 4) 
				Pain.M4[[length(Pain.M4) + 1]] = PPC.Pain.4
				names(Pain.M4)[length(Pain.M4)] = "PPC.Pain.4"
			}
			if (sum(PPC.Pain.5.4$Summary) > sum(PPC.Pain.4$Summary)) {
				BM = 5.4
				Pain.M4c = evaluate(observations.car, "Pain", Lag.Covs, 4, No_Neuropain)
				Pain.Info[[length(Pain.Info) + 1]] = Pain.M4c
				names(Pain.Info)[length(Pain.Info)] = "Pain.M4c"
				Cur.DIC = Pain.M4c$DIC
			}
		}
	} else if (BM == 4.1) {
		Pain.M5.41 = evaluate(observations, "Pain", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
			5.41, No_Neuropain)
		Pain.Info[[length(Pain.Info) + 1]] = Pain.M5.41
		names(Pain.Info)[length(Pain.Info)] = "Pain.M5.41"
		if ((Pain.M5.41$Sigs[1, ncol(Pain.M5.41$Sigs)-1] == TRUE || Pain.M5.41$Sigs[1, ncol(Pain.M5.41$Sigs)] == TRUE) && 
			Pain.M5.41$DIC < Pain.M4.1$DIC - 3) {
			BM = 5.41
			Pain.M4.1c = evaluate(observations.car, "Pain", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
			Pain.Info[[length(Pain.Info) + 1]] = Pain.M4.1c
			names(Pain.Info)[length(Pain.Info)] = "Pain.M4.1c"
			Cur.DIC = Pain.M4.1c$DIC
		} else if ((Pain.M5.41$Sigs[1, ncol(Pain.M5.41$Sigs)-1] == FALSE && Pain.M5.41$Sigs[1, ncol(Pain.M5.41$Sigs)] == FALSE) && 
			Pain.M5.41$DIC >= Pain.M4.1$DIC - 3) { BM = BM		
		} else {
			PPC.Pain.5.41 = ppc(observations, "Pain", Pain.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
				observations$car.B), 5.41)
			Pain.M5.41[[length(Pain.M5.41) + 1]] = PPC.Pain.5.41
			names(Pain.M5.41)[length(Pain.M5.41)] = "PPC.Pain.5.41"
			if (!exists("PPC.Pain.4.1")) { PPC.Pain.4.1 = ppc(observations, "Pain", cbind(Lag.Covs[ , 1], observations$Time2), 4.1) 
				Pain.M4.1[[length(Pain.M4.1) + 1]] = PPC.Pain.4.1
				names(Pain.M4.1)[length(Pain.M4.1)] = "PPC.Pain.4.1"
			}
			if (sum(PPC.Pain.5.41$Summary) > sum(PPC.Pain.4.1$Summary)) {
				BM = 5.41
				Pain.M4.1c = evaluate(observations.car, "Pain", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Pain.Info[[length(Pain.Info) + 1]] = Pain.M4.1c
				names(Pain.Info)[length(Pain.Info)] = "Pain.M4.1c"
				Cur.DIC = Pain.M4.1c$DIC
			}
		}
	} else if (BM == 4.2) {
		Pain.M5.42 = evaluate(observations, "Pain", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
			5.42, No_Neuropain)
		Pain.Info[[length(Pain.Info) + 1]] = Pain.M5.42
		names(Pain.Info)[length(Pain.Info)] = "Pain.M5.42"
		if ((Pain.M5.42$Sigs[1, ncol(Pain.M5.42$Sigs)-1] == TRUE || Pain.M5.42$Sigs[1, ncol(Pain.M5.42$Sigs)] == TRUE) && 
			Pain.M5.42$DIC < Pain.M4.2$DIC - 3) {
			BM = 5.42
			Pain.M4.2c = evaluate(observations.car, "Pain", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
			Pain.Info[[length(Pain.Info) + 1]] = Pain.M4.2c
			names(Pain.Info)[length(Pain.Info)] = "Pain.M4.2c"
			Cur.DIC = Pain.M4.2c$DIC
		} else if ((Pain.M5.42$Sigs[1, ncol(Pain.M5.42$Sigs)-1] == FALSE && Pain.M5.42$Sigs[1, ncol(Pain.M5.42$Sigs)] == FALSE) && 
			Pain.M5.42$DIC >= Pain.M4.2$DIC - 3) { BM = BM		
		} else {
			PPC.Pain.5.42 = ppc(observations, "Pain", Pain.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
				observations$car.B), 5.42)
			Pain.M5.42[[length(Pain.M5.42) + 1]] = PPC.Pain.5.42
			names(Pain.M5.42)[length(Pain.M5.42)] = "PPC.Pain.5.42"
			if (!exists("PPC.Pain.4.2")) { PPC.Pain.4.2 = ppc(observations, "Pain", cbind(Lag.Covs[ , 1], Block.Covs), 4.2) 
				Pain.M4.2[[length(Pain.M4.2) + 1]] = PPC.Pain.4.2
				names(Pain.M4.2)[length(Pain.M4.2)] = "PPC.Pain.4.2"
			}
			if (sum(PPC.Pain.5.42$Summary) > sum(PPC.Pain.4.2$Summary)) {
				BM = 5.42
				Pain.M4.2c = evaluate(observations.car, "Pain", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Pain.Info[[length(Pain.Info) + 1]] = Pain.M4.2c
				names(Pain.Info)[length(Pain.Info)] = "Pain.M4.2c"
				Cur.DIC = Pain.M4.2c$DIC
			}
		}
	}
	
	Best.Model = BM
	
	######################
	# Analysis for Fatigue
	######################
	
	Fatigue.M1 = evaluate(observations, Outcome = "Fatigue", Covs = NULL, mod.id = 1, No_Neuropain)
	Fatigue.M2 = evaluate(observations, Outcome = "Fatigue", Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Fatigue.M3 = evaluate(observations, Outcome = "Fatigue", Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Fatigue.M4 = evaluate(observations, Outcome = "Fatigue", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Fatigue.M4.1 = evaluate(observations, Outcome = "Fatigue", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Fatigue.M4.2 = evaluate(observations, Outcome = "Fatigue", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	
	Fatigue.Info = list(Fatigue.M1, Fatigue.M2, Fatigue.M3, Fatigue.M4, Fatigue.M4.1, Fatigue.M4.2)
	names(Fatigue.Info) = c("Fatigue.M1", "Fatigue.M2", "Fatigue.M3", "Fatigue.M4", "Fatigue.M4.1", "Fatigue.M4.2")
	
	BM = 1
	Cur.DIC = Fatigue.M1$DIC
	
	if (Fatigue.M2$Sigs[2] == TRUE && Fatigue.M2$DIC < Fatigue.M1$DIC - 3) { 
		BM = 2
		Cur.DIC = Fatigue.M2$DIC
	} else if (Fatigue.M2$Sigs[2] == FALSE && Fatigue.M2$DIC >= Fatigue.M1$DIC - 3) { BM = 1
	} else {
		PPC.Fatigue.1 = ppc(observations, "Fatigue", Fatigue.M1$ForPPC, Covs = NULL, 1)
		Fatigue.M1[[length(Fatigue.M1) + 1]] = PPC.Fatigue.1
		names(Fatigue.M1)[length(Fatigue.M1)] = "PPC.Fatigue.1"
		PPC.Fatigue.2 = ppc(observations, "Fatigue", Fatigue.M2$ForPPC, observations$Time2, 2)
		Fatigue.M2[[length(Fatigue.M2) + 1]] = PPC.Fatigue.2
		names(Fatigue.M2)[length(Fatigue.M2)] = "PPC.Fatigue.2"
		if (sum(PPC.Fatigue.2$Summary) > sum(PPC.Fatigue.1$Summary)) { 
			BM = 2 
			Cur.DIC = Fatigue.M2$DIC
		}
	}
	
	if (sum(Fatigue.M3$Sigs[ , 2:ncol(Fatigue.M3$Sigs)]) > 0 && Fatigue.M3$DIC < Cur.DIC - 3) {
		BM = 3
		Cur.DIC = Fatigue.M3$DIC
	} else if (sum(Fatigue.M3$Sigs[ , 2:ncol(Fatigue.M3$Sigs)]) == 0 && Fatigue.M3$DIC >= Cur.DIC - 3) { BM = BM
	} else {
		PPC.Fatigue.3 = ppc(observations, "Fatigue", Fatigue.M3$ForPPC, Block.Covs, 3)
		Fatigue.M3[[length(Fatigue.M3) + 1]] = PPC.Fatigue.3
		names(Fatigue.M3)[length(Fatigue.M3)] = "PPC.Fatigue.3"
		if (BM == 1 && !exists("PPC.Fatigue.1")) { Cur.PPC = ppc(observations, "Fatigue", Fatigue.M1$ForPPC, Covs = NULL, 1) 
			Fatigue.M1[[length(Fatigue.M1) + 1]] = Cur.PPC
		names(Fatigue.M1)[length(Fatigue.M1)] = "PPC.Fatigue.1"
		} else if (BM == 1 && exists("PPC.Fatigue.1")) { Cur.PPC = PPC.Fatigue.1 }
		
		if (BM == 2 && !exists("PPC.Fatigue.2")) {
			Cur.PPC = ppc(observations, "Fatigue", Fatigue.M2$ForPPC, observations$Time2, 2)
			Fatigue.M2[[length(Fatigue.M2) + 1]] = Cur.PPC
		names(Fatigue.M2)[length(Fatigue.M2)] = "PPC.Fatigue.2"
		} else if (BM == 2 && exists("PPC.Fatigue.2")) { Cur.PPC = PPC.Fatigue.2 }
		
		if (sum(PPC.Fatigue.3$Summary) > sum(Cur.PPC$Summary)) {
			BM = 3
			Cur.DIC = Fatigue.M3$DIC
		}
	}
	
	if (BM == 1) {
		if (Fatigue.M4$Sigs[1, 2] == TRUE && Fatigue.M4$DIC < Cur.DIC - 3) {
			BM = 4
			Cur.DIC = Fatigue.M4$DIC
		} else if (Fatigue.M4$Sigs[1, 2] == FALSE && Fatigue.M4$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Fatigue.4 = ppc(observations, "Fatigue", Fatigue.M4$ForPPC, Lag.Covs[ , 2], 4)
			Fatigue.M4[[length(Fatigue.M4) + 1]] = PPC.Fatigue.4
			names(Fatigue.M4)[length(Fatigue.M4)] = "PPC.Fatigue.4"
			if (!exists("PPC.Fatigue.1")) { PPC.Fatigue.1 = ppc(observations, "Fatigue", Covs = NULL, 1) 
				Fatigue.M1[[length(Fatigue.M1) + 1]] = PPC.Fatigue.1
				names(Fatigue.M1)[length(Fatigue.M1)] = "PPC.Fatigue.1"
			}
			if (sum(PPC.Fatigue.4$Summary) > sum(PPC.Fatigue.1$Summary)) {
				BM = 4
				Cur.DIC = Fatigue.M4$DIC
			}
		}
	} else if (BM == 2) {
		if (Fatigue.M4.1$Sigs[1, 2] == TRUE && Fatigue.M4.1$DIC < Cur.DIC - 3) {
			BM = 4.1
			Cur.DIC = Fatigue.M4.1$DIC
		} else if (Fatigue.M4.1$Sigs[1, 2] == FALSE && Fatigue.M4.1$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Fatigue.4.1 = ppc(observations, "Fatigue", Fatigue.M4.1$ForPPC, cbind(Lag.Covs[ , 2], observations$Time2), 4.1)
			Fatigue.M1[[length(Fatigue.M4.1) + 1]] = PPC.Fatigue.4.1
			names(Fatigue.M4.1)[length(Fatigue.M4.1)] = "PPC.Fatigue.4.1"
			if (!exists("PPC.Fatigue.2")) { PPC.Fatigue.2 = ppc(observations, "Fatigue", observations$Time2, 2) 
				Fatigue.M2[[length(Fatigue.M2) + 1]] = PPC.Fatigue.2
				names(Fatigue.M2)[length(Fatigue.M2)] = "PPC.Fatigue.2"
			}
			if (sum(PPC.Fatigue.4.1$Summary) > sum(PPC.Fatigue.2$Summary)) {
				BM = 4.1
				Cur.DIC = Fatigue.M4.1$DIC
			}
		}
	} else if (BM == 3) {
		if (Fatigue.M4.2$Sigs[1, 2] == TRUE && Fatigue.M4.2$DIC < Cur.DIC - 3) {
			BM = 4.2
			Cur.DIC = Fatigue.M4.2$DIC
		} else if (Fatigue.M4.2$Sigs[1, 2] == FALSE && Fatigue.M4.2$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Fatigue.4.2 = ppc(observations, "Fatigue", Fatigue.M4.2$ForPPC, cbind(Lag.Covs[ , 2], Block.Covs), 4.2)
			Fatigue.M4.2[[length(Fatigue.M1) + 1]] = PPC.Fatigue.4.2
		names(Fatigue.M4.2)[length(Fatigue.M4.2)] = "PPC.Fatigue.4.2"
			if (!exists("PPC.Fatigue.3")) { PPC.Fatigue.3 = ppc(observations, "Fatigue", Block.Covs, 3) 
				Fatigue.M3[[length(Fatigue.M3) + 1]] = PPC.Fatigue.3
				names(Fatigue.M3)[length(Fatigue.M3)] = "PPC.Fatigue.3"
			}
			if (sum(PPC.Fatigue.4.2$Summary) > sum(PPC.Fatigue.3$Summary)) {
				BM = 4.2
				Cur.DIC = Fatigue.M4.2$DIC
			}
		}
	}
	
	if (BM == 1) {
		Fatigue.M5.1 = evaluate(observations, "Fatigue", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
		Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M5.1
		names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M5.1"
		if ((Fatigue.M5.1$Sigs[1, 2] == TRUE || Fatigue.M5.1$Sigs[1, 3] == TRUE) && Fatigue.M5.1$DIC < Fatigue.M1$DIC - 3) {
			BM = 5.1
			Fatigue.M1c = evaluate(observations.car, "Fatigue", Covs = NULL, 1, No_Neuropain)
			Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M1c
			names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M1c"
			Cur.DIC = Fatigue.M1c$DIC
		} else if ((Fatigue.M5.1$Sigs[1, 2] == FALSE || Fatigue.M5.1$Sigs[1, 3] == FALSE) && Fatigue.M5.1$DIC >= Fatigue.M1$DIC - 3) { BM = BM		
		} else {
			PPC.Fatigue.5.1 = ppc(observations, "Fatigue", Fatigue.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
			Fatigue.M5.1[[length(Fatigue.M5.1) + 1]] = PPC.Fatigue.5.1
			names(Fatigue.M5.1)[length(Fatigue.M5.1)] = "PPC.Fatigue.5.1"
			if (!exists("PPC.Fatigue.1")) { PPC.Fatigue.1 = ppc(observations, "Fatigue", Covs = NULL, 1) 
				Fatigue.M1[[length(Fatigue.M1) + 1]] = PPC.Fatigue.1
				names(Fatigue.M1)[length(Fatigue.M1)] = "PPC.Fatigue.1"
			}
			if (sum(PPC.Fatigue.5.1$Summary) > sum(PPC.Fatigue.1$Summary)) {
				BM = 5.1
				Fatigue.M1c = evaluate(observations.car, "Fatigue", Covs = NULL, 1, No_Neuropain)
				Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M1c
				names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M1c"
				Cur.DIC = Fatigue.M1c$DIC
			}
		}
	} else if (BM == 2) {
		Fatigue.M5.2 = evaluate(observations, "Fatigue", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
		Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M5.2
		names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M5.2"
		if ((Fatigue.M5.2$Sigs[1, 3] == TRUE || Fatigue.M5.2$Sigs[1, 4] == TRUE) && Fatigue.M5.2$DIC < Fatigue.M2$DIC - 3) {
			BM = 5.2
			Fatigue.M2c = evaluate(observations.car, "Fatigue", observations$Time2, 2, No_Neuropain)
			Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M2c
			names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M2c"
			Cur.DIC = Fatigue.M2c$DIC
		} else if ((Fatigue.M5.2$Sigs[1, 3] == FALSE || Fatigue.M5.2$Sigs[1, 4] == FALSE) && Fatigue.M5.2$DIC >= Fatigue.M2$DIC - 3) { BM = BM		
		} else {
			PPC.Fatigue.5.2 = ppc(observations, "Fatigue", Fatigue.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
			Fatigue.M5.2[[length(Fatigue.M5.2) + 1]] = PPC.Fatigue.5.2
			names(Fatigue.M5.2)[length(Fatigue.M5.2)] = "PPC.Fatigue.5.2"
			if (!exists("PPC.Fatigue.2")) { PPC.Fatigue.2 = ppc(observations, "Fatigue", observations$Time2, 2) 
				Fatigue.M2[[length(Fatigue.M2) + 1]] = PPC.Fatigue.2
				names(Fatigue.M2)[length(Fatigue.M2)] = "PPC.Fatigue.2"
			}
			if (sum(PPC.Fatigue.5.2$Summary) > sum(PPC.Fatigue.2$Summary)) {
				BM = 5.2
				Fatigue.M2c = evaluate(observations.car, "Fatigue", observations$Time2, 2, No_Neuropain)
				Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M2c
				names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M2c"
				Cur.DIC = Fatigue.M2c$DIC
			}
		}
	} else if (BM == 3) {
		Fatigue.M5.3 = evaluate(observations, "Fatigue", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
		Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M5.3
		names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M5.3"
		if ((Fatigue.M5.3$Sigs[1, ncol(Fatigue.M5.3$Sigs)-1] == TRUE || Fatigue.M5.3$Sigs[1, ncol(Fatigue.M5.3$Sigs)] == TRUE) && 
			Fatigue.M5.3$DIC < Fatigue.M3$DIC - 3) {
			BM = 5.3
			Fatigue.M3c = evaluate(observations.car, "Fatigue", Block.Covs, 3, No_Neuropain)
			Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M3c
			names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M3c"
			Cur.DIC = Fatigue.M3c$DIC
		} else if ((Fatigue.M5.3$Sigs[1, ncol(Fatigue.M5.3$Sigs)-1] == FALSE || Fatigue.M5.3$Sigs[1, ncol(Fatigue.M5.3$Sigs)] == FALSE) && 
			Fatigue.M5.3$DIC >= Fatigue.M3$DIC - 3) { BM = BM		
		} else {
			PPC.Fatigue.5.3 = ppc(observations, "Fatigue", Fatigue.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
			Fatigue.M5.3[[length(Fatigue.M5.3) + 1]] = PPC.Fatigue.5.3
			names(Fatigue.M5.3)[length(Fatigue.M5.3)] = "PPC.Fatigue.5.3"
			if (!exists("PPC.Fatigue.3")) { PPC.Fatigue.3 = ppc(observations, "Fatigue", Block.Covs, 3) 
				Fatigue.M3[[length(Fatigue.M3) + 1]] = PPC.Fatigue.3
				names(Fatigue.M3)[length(Fatigue.M3)] = "PPC.Fatigue.3"
			}
			if (sum(PPC.Fatigue.5.3$Summary) > sum(PPC.Fatigue.3$Summary)) {
				BM = 5.3
				Fatigue.M3c = evaluate(observations.car, "Fatigue", Block.Covs, 3, No_Neuropain)
				Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M3c
				names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M3c"
				Cur.DIC = Fatigue.M3c$DIC
			}
		}
	} else if (BM == 4) {
		Fatigue.M5.4 = evaluate(observations, "Fatigue", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
		Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M5.4
		names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M5.4"
		if ((Fatigue.M5.4$Sigs[1, ncol(Fatigue.M5.4$Sigs)-1] == TRUE || Fatigue.M5.4$Sigs[1, ncol(Fatigue.M5.4$Sigs)] == TRUE) && 
			Fatigue.M5.4$DIC < Fatigue.M4$DIC - 3) {
			BM = 5.4
			Fatigue.M4c = evaluate(observations.car, "Fatigue", Lag.Covs, 4, No_Neuropain)
			Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M4c
			names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M4c"
			Cur.DIC = Fatigue.M4c$DIC
		} else if ((Fatigue.M5.4$Sigs[1, ncol(Fatigue.M5.4$Sigs)-1] == FALSE && Fatigue.M5.4$Sigs[1, ncol(Fatigue.M5.4$Sigs)] == FALSE) && 
			Fatigue.M5.4$DIC >= Fatigue.M4$DIC - 3) { BM = BM		
		} else {
			PPC.Fatigue.5.4 = ppc(observations, "Fatigue", Fatigue.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
			Fatigue.M5.4[[length(Fatigue.M5.4) + 1]] = PPC.Fatigue.5.4
			names(Fatigue.M5.4)[length(Fatigue.M5.4)] = "PPC.Fatigue.5.4"
			if (!exists("PPC.Fatigue.4")) { PPC.Fatigue.4 = ppc(observations, "Fatigue", Lag.Covs[ , 2], 4) 
				Fatigue.M4[[length(Fatigue.M4) + 1]] = PPC.Fatigue.4
				names(Fatigue.M4)[length(Fatigue.M4)] = "PPC.Fatigue.4"
			}
			if (sum(PPC.Fatigue.5.4$Summary) > sum(PPC.Fatigue.4$Summary)) {
				BM = 5.4
				Fatigue.M4c = evaluate(observations.car, "Fatigue", Lag.Covs, 4, No_Neuropain)
				Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M4c
				names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M4c"
				Cur.DIC = Fatigue.M4c$DIC
			}
		}
	} else if (BM == 4.1) {
		Fatigue.M5.41 = evaluate(observations, "Fatigue", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
			5.41, No_Neuropain)
		Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M5.41
		names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M5.41"
		if ((Fatigue.M5.41$Sigs[1, ncol(Fatigue.M5.41$Sigs)-1] == TRUE || Fatigue.M5.41$Sigs[1, ncol(Fatigue.M5.41$Sigs)] == TRUE) && 
			Fatigue.M5.41$DIC < Fatigue.M4.1$DIC - 3) {
			BM = 5.41
			Fatigue.M4.1c = evaluate(observations.car, "Fatigue", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
			Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M4.1c
			names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M4.1c"
			Cur.DIC = Fatigue.M4.1c$DIC
		} else if ((Fatigue.M5.41$Sigs[1, ncol(Fatigue.M5.41$Sigs)-1] == FALSE && Fatigue.M5.41$Sigs[1, ncol(Fatigue.M5.41$Sigs)] == FALSE) && 
			Fatigue.M5.41$DIC >= Fatigue.M4.1$DIC - 3) { BM = BM		
		} else {
			PPC.Fatigue.5.41 = ppc(observations, "Fatigue", Fatigue.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
				observations$car.B), 5.41)
			Fatigue.M5.41[[length(Fatigue.M5.41) + 1]] = PPC.Fatigue.5.41
			names(Fatigue.M5.41)[length(Fatigue.M5.41)] = "PPC.Fatigue.5.41"
			if (!exists("PPC.Fatigue.4.1")) { PPC.Fatigue.4.1 = ppc(observations, "Fatigue", cbind(Lag.Covs[ , 2], observations$Time2), 4.1) 
				Fatigue.M4.1[[length(Fatigue.M4.1) + 1]] = PPC.Fatigue.4.1
				names(Fatigue.M4.1)[length(Fatigue.M4.1)] = "PPC.Fatigue.4.1"
			}
			if (sum(PPC.Fatigue.5.41$Summary) > sum(PPC.Fatigue.4.1$Summary)) {
				BM = 5.41
				Fatigue.M4.1c = evaluate(observations.car, "Fatigue", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M4.1c
				names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M4.1c"
				Cur.DIC = Fatigue.M4.1c$DIC
			}
		}
	} else if (BM == 4.2) {
		Fatigue.M5.42 = evaluate(observations, "Fatigue", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
			5.42, No_Neuropain)
		Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M5.42
		names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M5.42"
		if ((Fatigue.M5.42$Sigs[1, ncol(Fatigue.M5.42$Sigs)-1] == TRUE || Fatigue.M5.42$Sigs[1, ncol(Fatigue.M5.42$Sigs)] == TRUE) && 
			Fatigue.M5.42$DIC < Fatigue.M4.2$DIC - 3) {
			BM = 5.42
			Fatigue.M4.2c = evaluate(observations.car, "Fatigue", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
			Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M4.2c
			names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M4.2c"
			Cur.DIC = Fatigue.M4.2c$DIC
		} else if ((Fatigue.M5.42$Sigs[1, ncol(Fatigue.M5.42$Sigs)-1] == FALSE && Fatigue.M5.42$Sigs[1, ncol(Fatigue.M5.42$Sigs)] == FALSE) && 
			Fatigue.M5.42$DIC >= Fatigue.M4.2$DIC - 3) { BM = BM		
		} else {
			PPC.Fatigue.5.42 = ppc(observations, "Fatigue", Fatigue.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
				observations$car.B), 5.42)
			Fatigue.M5.42[[length(Fatigue.M5.42) + 1]] = PPC.Fatigue.5.42
			names(Fatigue.M5.42)[length(Fatigue.M5.42)] = "PPC.Fatigue.5.42"
			if (!exists("PPC.Fatigue.4.2")) { PPC.Fatigue.4.2 = ppc(observations, "Fatigue", cbind(Lag.Covs[ , 2], Block.Covs), 4.2) 
				Fatigue.M4.2[[length(Fatigue.M4.2) + 1]] = PPC.Fatigue.4.2
				names(Fatigue.M4.2)[length(Fatigue.M4.2)] = "PPC.Fatigue.4.2"
			}
			if (sum(PPC.Fatigue.5.42$Summary) > sum(PPC.Fatigue.4.2$Summary)) {
				BM = 5.42
				Fatigue.M4.2c = evaluate(observations.car, "Fatigue", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Fatigue.Info[[length(Fatigue.Info) + 1]] = Fatigue.M4.2c
				names(Fatigue.Info)[length(Fatigue.Info)] = "Fatigue.M4.2c"
				Cur.DIC = Fatigue.M4.2c$DIC
			}
		}
	}
	
	Best.Model = cbind(Best.Model, BM)
	
	#####################
	# Analysis for Drowsy
	#####################
	
	Drowsy.M1 = evaluate(observations, Outcome = "Drowsy", Covs = NULL, mod.id = 1, No_Neuropain)
	Drowsy.M2 = evaluate(observations, Outcome = "Drowsy", Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Drowsy.M3 = evaluate(observations, Outcome = "Drowsy", Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Drowsy.M4 = evaluate(observations, Outcome = "Drowsy", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Drowsy.M4.1 = evaluate(observations, Outcome = "Drowsy", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Drowsy.M4.2 = evaluate(observations, Outcome = "Drowsy", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	
	Drowsy.Info = list(Drowsy.M1, Drowsy.M2, Drowsy.M3, Drowsy.M4, Drowsy.M4.1, Drowsy.M4.2)
	names(Drowsy.Info) = c("Drowsy.M1", "Drowsy.M2", "Drowsy.M3", "Drowsy.M4", "Drowsy.M4.1", "Drowsy.M4.2")
	
	BM = 1
	Cur.DIC = Drowsy.M1$DIC
	
	if (Drowsy.M2$Sigs[2] == TRUE && Drowsy.M2$DIC < Drowsy.M1$DIC - 3) { 
		BM = 2
		Cur.DIC = Drowsy.M2$DIC
	} else if (Drowsy.M2$Sigs[2] == FALSE && Drowsy.M2$DIC >= Drowsy.M1$DIC - 3) { BM = 1
	} else {
		PPC.Drowsy.1 = ppc(observations, "Drowsy", Drowsy.M1$ForPPC, Covs = NULL, 1)
		Drowsy.M1[[length(Drowsy.M1) + 1]] = PPC.Drowsy.1
		names(Drowsy.M1)[length(Drowsy.M1)] = "PPC.Drowsy.1"
		PPC.Drowsy.2 = ppc(observations, "Drowsy", Drowsy.M2$ForPPC, observations$Time2, 2)
		Drowsy.M2[[length(Drowsy.M2) + 1]] = PPC.Drowsy.2
		names(Drowsy.M2)[length(Drowsy.M2)] = "PPC.Drowsy.2"
		if (sum(PPC.Drowsy.2$Summary) > sum(PPC.Drowsy.1$Summary)) { 
			BM = 2 
			Cur.DIC = Drowsy.M2$DIC
		}
	}
	
	if (sum(Drowsy.M3$Sigs[ , 2:ncol(Drowsy.M3$Sigs)]) > 0 && Drowsy.M3$DIC < Cur.DIC - 3) {
		BM = 3
		Cur.DIC = Drowsy.M3$DIC
	} else if (sum(Drowsy.M3$Sigs[ , 2:ncol(Drowsy.M3$Sigs)]) == 0 && Drowsy.M3$DIC >= Cur.DIC - 3) { BM = BM
	} else {
		PPC.Drowsy.3 = ppc(observations, "Drowsy", Drowsy.M3$ForPPC, Block.Covs, 3)
		Drowsy.M3[[length(Drowsy.M3) + 1]] = PPC.Drowsy.3
		names(Drowsy.M3)[length(Drowsy.M3)] = "PPC.Drowsy.3"
		if (BM == 1 && !exists("PPC.Drowsy.1")) { Cur.PPC = ppc(observations, "Drowsy", Drowsy.M1$ForPPC, Covs = NULL, 1) 
			Drowsy.M1[[length(Drowsy.M1) + 1]] = Cur.PPC
		names(Drowsy.M1)[length(Drowsy.M1)] = "PPC.Drowsy.1"
		} else if (BM == 1 && exists("PPC.Drowsy.1")) { Cur.PPC = PPC.Drowsy.1 }
		
		if (BM == 2 && !exists("PPC.Drowsy.2")) {
			Cur.PPC = ppc(observations, "Drowsy", Drowsy.M2$ForPPC, observations$Time2, 2)
			Drowsy.M2[[length(Drowsy.M2) + 1]] = Cur.PPC
		names(Drowsy.M2)[length(Drowsy.M2)] = "PPC.Drowsy.2"
		} else if (BM == 2 && exists("PPC.Drowsy.2")) { Cur.PPC = PPC.Drowsy.2 }
		
		if (sum(PPC.Drowsy.3$Summary) > sum(Cur.PPC$Summary)) {
			BM = 3
			Cur.DIC = Drowsy.M3$DIC
		}
	}
	
	if (BM == 1) {
		if (Drowsy.M4$Sigs[1, 2] == TRUE && Drowsy.M4$DIC < Cur.DIC - 3) {
			BM = 4
			Cur.DIC = Drowsy.M4$DIC
		} else if (Drowsy.M4$Sigs[1, 2] == FALSE && Drowsy.M4$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Drowsy.4 = ppc(observations, "Drowsy", Drowsy.M4$ForPPC, Lag.Covs[ , 3], 4)
			Drowsy.M4[[length(Drowsy.M4) + 1]] = PPC.Drowsy.4
			names(Drowsy.M4)[length(Drowsy.M4)] = "PPC.Drowsy.4"
			if (!exists("PPC.Drowsy.1")) { PPC.Drowsy.1 = ppc(observations, "Drowsy", Covs = NULL, 1) 
				Drowsy.M1[[length(Drowsy.M1) + 1]] = PPC.Drowsy.1
				names(Drowsy.M1)[length(Drowsy.M1)] = "PPC.Drowsy.1"
			}
			if (sum(PPC.Drowsy.4$Summary) > sum(PPC.Drowsy.1$Summary)) {
				BM = 4
				Cur.DIC = Drowsy.M4$DIC
			}
		}
	} else if (BM == 2) {
		if (Drowsy.M4.1$Sigs[1, 2] == TRUE && Drowsy.M4.1$DIC < Cur.DIC - 3) {
			BM = 4.1
			Cur.DIC = Drowsy.M4.1$DIC
		} else if (Drowsy.M4.1$Sigs[1, 2] == FALSE && Drowsy.M4.1$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Drowsy.4.1 = ppc(observations, "Drowsy", Drowsy.M4.1$ForPPC, cbind(Lag.Covs[ , 3], observations$Time2), 4.1)
			Drowsy.M1[[length(Drowsy.M4.1) + 1]] = PPC.Drowsy.4.1
			names(Drowsy.M4.1)[length(Drowsy.M4.1)] = "PPC.Drowsy.4.1"
			if (!exists("PPC.Drowsy.2")) { PPC.Drowsy.2 = ppc(observations, "Drowsy", observations$Time2, 2) 
				Drowsy.M2[[length(Drowsy.M2) + 1]] = PPC.Drowsy.2
				names(Drowsy.M2)[length(Drowsy.M2)] = "PPC.Drowsy.2"
			}
			if (sum(PPC.Drowsy.4.1$Summary) > sum(PPC.Drowsy.2$Summary)) {
				BM = 4.1
				Cur.DIC = Drowsy.M4.1$DIC
			}
		}
	} else if (BM == 3) {
		if (Drowsy.M4.2$Sigs[1, 2] == TRUE && Drowsy.M4.2$DIC < Cur.DIC - 3) {
			BM = 4.2
			Cur.DIC = Drowsy.M4.2$DIC
		} else if (Drowsy.M4.2$Sigs[1, 2] == FALSE && Drowsy.M4.2$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Drowsy.4.2 = ppc(observations, "Drowsy", Drowsy.M4.2$ForPPC, cbind(Lag.Covs[ , 3], Block.Covs), 4.2)
			Drowsy.M4.2[[length(Drowsy.M1) + 1]] = PPC.Drowsy.4.2
		names(Drowsy.M4.2)[length(Drowsy.M4.2)] = "PPC.Drowsy.4.2"
			if (!exists("PPC.Drowsy.3")) { PPC.Drowsy.3 = ppc(observations, "Drowsy", Block.Covs, 3) 
				Drowsy.M3[[length(Drowsy.M3) + 1]] = PPC.Drowsy.3
				names(Drowsy.M3)[length(Drowsy.M3)] = "PPC.Drowsy.3"
			}
			if (sum(PPC.Drowsy.4.2$Summary) > sum(PPC.Drowsy.3$Summary)) {
				BM = 4.2
				Cur.DIC = Drowsy.M4.2$DIC
			}
		}
	}
	
	if (BM == 1) {
		Drowsy.M5.1 = evaluate(observations, "Drowsy", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
		Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M5.1
		names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M5.1"
		if ((Drowsy.M5.1$Sigs[1, 2] == TRUE || Drowsy.M5.1$Sigs[1, 3] == TRUE) && Drowsy.M5.1$DIC < Drowsy.M1$DIC - 3) {
			BM = 5.1
			Drowsy.M1c = evaluate(observations.car, "Drowsy", Covs = NULL, 1, No_Neuropain)
			Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M1c
			names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M1c"
			Cur.DIC = Drowsy.M1c$DIC
		} else if ((Drowsy.M5.1$Sigs[1, 2] == FALSE || Drowsy.M5.1$Sigs[1, 3] == FALSE) && Drowsy.M5.1$DIC >= Drowsy.M1$DIC - 3) { BM = BM		
		} else {
			PPC.Drowsy.5.1 = ppc(observations, "Drowsy", Drowsy.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
			Drowsy.M5.1[[length(Drowsy.M5.1) + 1]] = PPC.Drowsy.5.1
			names(Drowsy.M5.1)[length(Drowsy.M5.1)] = "PPC.Drowsy.5.1"
			if (!exists("PPC.Drowsy.1")) { PPC.Drowsy.1 = ppc(observations, "Drowsy", Covs = NULL, 1) 
				Drowsy.M1[[length(Drowsy.M1) + 1]] = PPC.Drowsy.1
				names(Drowsy.M1)[length(Drowsy.M1)] = "PPC.Drowsy.1"
			}
			if (sum(PPC.Drowsy.5.1$Summary) > sum(PPC.Drowsy.1$Summary)) {
				BM = 5.1
				Drowsy.M1c = evaluate(observations.car, "Drowsy", Covs = NULL, 1, No_Neuropain)
				Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M1c
				names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M1c"
				Cur.DIC = Drowsy.M1c$DIC
			}
		}
	} else if (BM == 2) {
		Drowsy.M5.2 = evaluate(observations, "Drowsy", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
		Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M5.2
		names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M5.2"
		if ((Drowsy.M5.2$Sigs[1, 3] == TRUE || Drowsy.M5.2$Sigs[1, 4] == TRUE) && Drowsy.M5.2$DIC < Drowsy.M2$DIC - 3) {
			BM = 5.2
			Drowsy.M2c = evaluate(observations.car, "Drowsy", observations$Time2, 2, No_Neuropain)
			Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M2c
			names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M2c"
			Cur.DIC = Drowsy.M2c$DIC
		} else if ((Drowsy.M5.2$Sigs[1, 3] == FALSE || Drowsy.M5.2$Sigs[1, 4] == FALSE) && Drowsy.M5.2$DIC >= Drowsy.M2$DIC - 3) { BM = BM		
		} else {
			PPC.Drowsy.5.2 = ppc(observations, "Drowsy", Drowsy.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
			Drowsy.M5.2[[length(Drowsy.M5.2) + 1]] = PPC.Drowsy.5.2
			names(Drowsy.M5.2)[length(Drowsy.M5.2)] = "PPC.Drowsy.5.2"
			if (!exists("PPC.Drowsy.2")) { PPC.Drowsy.2 = ppc(observations, "Drowsy", observations$Time2, 2) 
				Drowsy.M2[[length(Drowsy.M2) + 1]] = PPC.Drowsy.2
				names(Drowsy.M2)[length(Drowsy.M2)] = "PPC.Drowsy.2"
			}
			if (sum(PPC.Drowsy.5.2$Summary) > sum(PPC.Drowsy.2$Summary)) {
				BM = 5.2
				Drowsy.M2c = evaluate(observations.car, "Drowsy", observations$Time2, 2, No_Neuropain)
				Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M2c
				names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M2c"
				Cur.DIC = Drowsy.M2c$DIC
			}
		}
	} else if (BM == 3) {
		Drowsy.M5.3 = evaluate(observations, "Drowsy", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
		Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M5.3
		names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M5.3"
		if ((Drowsy.M5.3$Sigs[1, ncol(Drowsy.M5.3$Sigs)-1] == TRUE || Drowsy.M5.3$Sigs[1, ncol(Drowsy.M5.3$Sigs)] == TRUE) && 
			Drowsy.M5.3$DIC < Drowsy.M3$DIC - 3) {
			BM = 5.3
			Drowsy.M3c = evaluate(observations.car, "Drowsy", Block.Covs, 3, No_Neuropain)
			Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M3c
			names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M3c"
			Cur.DIC = Drowsy.M3c$DIC
		} else if ((Drowsy.M5.3$Sigs[1, ncol(Drowsy.M5.3$Sigs)-1] == FALSE || Drowsy.M5.3$Sigs[1, ncol(Drowsy.M5.3$Sigs)] == FALSE) && 
			Drowsy.M5.3$DIC >= Drowsy.M3$DIC - 3) { BM = BM		
		} else {
			PPC.Drowsy.5.3 = ppc(observations, "Drowsy", Drowsy.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
			Drowsy.M5.3[[length(Drowsy.M5.3) + 1]] = PPC.Drowsy.5.3
			names(Drowsy.M5.3)[length(Drowsy.M5.3)] = "PPC.Drowsy.5.3"
			if (!exists("PPC.Drowsy.3")) { PPC.Drowsy.3 = ppc(observations, "Drowsy", Block.Covs, 3) 
				Drowsy.M3[[length(Drowsy.M3) + 1]] = PPC.Drowsy.3
				names(Drowsy.M3)[length(Drowsy.M3)] = "PPC.Drowsy.3"
			}
			if (sum(PPC.Drowsy.5.3$Summary) > sum(PPC.Drowsy.3$Summary)) {
				BM = 5.3
				Drowsy.M3c = evaluate(observations.car, "Drowsy", Block.Covs, 3, No_Neuropain)
				Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M3c
				names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M3c"
				Cur.DIC = Drowsy.M3c$DIC
			}
		}
	} else if (BM == 4) {
		Drowsy.M5.4 = evaluate(observations, "Drowsy", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
		Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M5.4
		names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M5.4"
		if ((Drowsy.M5.4$Sigs[1, ncol(Drowsy.M5.4$Sigs)-1] == TRUE || Drowsy.M5.4$Sigs[1, ncol(Drowsy.M5.4$Sigs)] == TRUE) && 
			Drowsy.M5.4$DIC < Drowsy.M4$DIC - 3) {
			BM = 5.4
			Drowsy.M4c = evaluate(observations.car, "Drowsy", Lag.Covs, 4, No_Neuropain)
			Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M4c
			names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M4c"
			Cur.DIC = Drowsy.M4c$DIC
		} else if ((Drowsy.M5.4$Sigs[1, ncol(Drowsy.M5.4$Sigs)-1] == FALSE && Drowsy.M5.4$Sigs[1, ncol(Drowsy.M5.4$Sigs)] == FALSE) && 
			Drowsy.M5.4$DIC >= Drowsy.M4$DIC - 3) { BM = BM		
		} else {
			PPC.Drowsy.5.4 = ppc(observations, "Drowsy", Drowsy.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
			Drowsy.M5.4[[length(Drowsy.M5.4) + 1]] = PPC.Drowsy.5.4
			names(Drowsy.M5.4)[length(Drowsy.M5.4)] = "PPC.Drowsy.5.4"
			if (!exists("PPC.Drowsy.4")) { PPC.Drowsy.4 = ppc(observations, "Drowsy", Lag.Covs[ , 3], 4) 
				Drowsy.M4[[length(Drowsy.M4) + 1]] = PPC.Drowsy.4
				names(Drowsy.M4)[length(Drowsy.M4)] = "PPC.Drowsy.4"
			}
			if (sum(PPC.Drowsy.5.4$Summary) > sum(PPC.Drowsy.4$Summary)) {
				BM = 5.4
				Drowsy.M4c = evaluate(observations.car, "Drowsy", Lag.Covs, 4, No_Neuropain)
				Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M4c
				names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M4c"
				Cur.DIC = Drowsy.M4c$DIC
			}
		}
	} else if (BM == 4.1) {
		Drowsy.M5.41 = evaluate(observations, "Drowsy", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
			5.41, No_Neuropain)
		Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M5.41
		names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M5.41"
		if ((Drowsy.M5.41$Sigs[1, ncol(Drowsy.M5.41$Sigs)-1] == TRUE || Drowsy.M5.41$Sigs[1, ncol(Drowsy.M5.41$Sigs)] == TRUE) && 
			Drowsy.M5.41$DIC < Drowsy.M4.1$DIC - 3) {
			BM = 5.41
			Drowsy.M4.1c = evaluate(observations.car, "Drowsy", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
			Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M4.1c
			names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M4.1c"
			Cur.DIC = Drowsy.M4.1c$DIC
		} else if ((Drowsy.M5.41$Sigs[1, ncol(Drowsy.M5.41$Sigs)-1] == FALSE && Drowsy.M5.41$Sigs[1, ncol(Drowsy.M5.41$Sigs)] == FALSE) && 
			Drowsy.M5.41$DIC >= Drowsy.M4.1$DIC - 3) { BM = BM		
		} else {
			PPC.Drowsy.5.41 = ppc(observations, "Drowsy", Drowsy.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
				observations$car.B), 5.41)
			Drowsy.M5.41[[length(Drowsy.M5.41) + 1]] = PPC.Drowsy.5.41
			names(Drowsy.M5.41)[length(Drowsy.M5.41)] = "PPC.Drowsy.5.41"
			if (!exists("PPC.Drowsy.4.1")) { PPC.Drowsy.4.1 = ppc(observations, "Drowsy", cbind(Lag.Covs[ , 3], observations$Time2), 4.1) 
				Drowsy.M4.1[[length(Drowsy.M4.1) + 1]] = PPC.Drowsy.4.1
				names(Drowsy.M4.1)[length(Drowsy.M4.1)] = "PPC.Drowsy.4.1"
			}
			if (sum(PPC.Drowsy.5.41$Summary) > sum(PPC.Drowsy.4.1$Summary)) {
				BM = 5.41
				Drowsy.M4.1c = evaluate(observations.car, "Drowsy", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M4.1c
				names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M4.1c"
				Cur.DIC = Drowsy.M4.1c$DIC
			}
		}
	} else if (BM == 4.2) {
		Drowsy.M5.42 = evaluate(observations, "Drowsy", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
			5.42, No_Neuropain)
		Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M5.42
		names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M5.42"
		if ((Drowsy.M5.42$Sigs[1, ncol(Drowsy.M5.42$Sigs)-1] == TRUE || Drowsy.M5.42$Sigs[1, ncol(Drowsy.M5.42$Sigs)] == TRUE) && 
			Drowsy.M5.42$DIC < Drowsy.M4.2$DIC - 3) {
			BM = 5.42
			Drowsy.M4.2c = evaluate(observations.car, "Drowsy", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
			Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M4.2c
			names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M4.2c"
			Cur.DIC = Drowsy.M4.2c$DIC
		} else if ((Drowsy.M5.42$Sigs[1, ncol(Drowsy.M5.42$Sigs)-1] == FALSE && Drowsy.M5.42$Sigs[1, ncol(Drowsy.M5.42$Sigs)] == FALSE) && 
			Drowsy.M5.42$DIC >= Drowsy.M4.2$DIC - 3) { BM = BM		
		} else {
			PPC.Drowsy.5.42 = ppc(observations, "Drowsy", Drowsy.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
				observations$car.B), 5.42)
			Drowsy.M5.42[[length(Drowsy.M5.42) + 1]] = PPC.Drowsy.5.42
			names(Drowsy.M5.42)[length(Drowsy.M5.42)] = "PPC.Drowsy.5.42"
			if (!exists("PPC.Drowsy.4.2")) { PPC.Drowsy.4.2 = ppc(observations, "Drowsy", cbind(Lag.Covs[ , 3], Block.Covs), 4.2) 
				Drowsy.M4.2[[length(Drowsy.M4.2) + 1]] = PPC.Drowsy.4.2
				names(Drowsy.M4.2)[length(Drowsy.M4.2)] = "PPC.Drowsy.4.2"
			}
			if (sum(PPC.Drowsy.5.42$Summary) > sum(PPC.Drowsy.4.2$Summary)) {
				BM = 5.42
				Drowsy.M4.2c = evaluate(observations.car, "Drowsy", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Drowsy.Info[[length(Drowsy.Info) + 1]] = Drowsy.M4.2c
				names(Drowsy.Info)[length(Drowsy.Info)] = "Drowsy.M4.2c"
				Cur.DIC = Drowsy.M4.2c$DIC
			}
		}
	}
	
	Best.Model = cbind(Best.Model, BM)
		
	####################
	# Analysis for Sleep
	####################
	
	Sleep.M1 = evaluate(observations, Outcome = "Sleep", Covs = NULL, mod.id = 1, No_Neuropain)
	Sleep.M2 = evaluate(observations, Outcome = "Sleep", Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Sleep.M3 = evaluate(observations, Outcome = "Sleep", Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Sleep.M4 = evaluate(observations, Outcome = "Sleep", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Sleep.M4.1 = evaluate(observations, Outcome = "Sleep", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Sleep.M4.2 = evaluate(observations, Outcome = "Sleep", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	
	Sleep.Info = list(Sleep.M1, Sleep.M2, Sleep.M3, Sleep.M4, Sleep.M4.1, Sleep.M4.2)
	names(Sleep.Info) = c("Sleep.M1", "Sleep.M2", "Sleep.M3", "Sleep.M4", "Sleep.M4.1", "Sleep.M4.2")
	
	BM = 1
	Cur.DIC = Sleep.M1$DIC
	
	if (Sleep.M2$Sigs[2] == TRUE && Sleep.M2$DIC < Sleep.M1$DIC - 3) { 
		BM = 2
		Cur.DIC = Sleep.M2$DIC
	} else if (Sleep.M2$Sigs[2] == FALSE && Sleep.M2$DIC >= Sleep.M1$DIC - 3) { BM = 1
	} else {
		PPC.Sleep.1 = ppc(observations, "Sleep", Sleep.M1$ForPPC, Covs = NULL, 1)
		Sleep.M1[[length(Sleep.M1) + 1]] = PPC.Sleep.1
		names(Sleep.M1)[length(Sleep.M1)] = "PPC.Sleep.1"
		PPC.Sleep.2 = ppc(observations, "Sleep", Sleep.M2$ForPPC, observations$Time2, 2)
		Sleep.M2[[length(Sleep.M2) + 1]] = PPC.Sleep.2
		names(Sleep.M2)[length(Sleep.M2)] = "PPC.Sleep.2"
		if (sum(PPC.Sleep.2$Summary) > sum(PPC.Sleep.1$Summary)) { 
			BM = 2 
			Cur.DIC = Sleep.M2$DIC
		}
	}
	
	if (sum(Sleep.M3$Sigs[ , 2:ncol(Sleep.M3$Sigs)]) > 0 && Sleep.M3$DIC < Cur.DIC - 3) {
		BM = 3
		Cur.DIC = Sleep.M3$DIC
	} else if (sum(Sleep.M3$Sigs[ , 2:ncol(Sleep.M3$Sigs)]) == 0 && Sleep.M3$DIC >= Cur.DIC - 3) { BM = BM
	} else {
		PPC.Sleep.3 = ppc(observations, "Sleep", Sleep.M3$ForPPC, Block.Covs, 3)
		Sleep.M3[[length(Sleep.M3) + 1]] = PPC.Sleep.3
		names(Sleep.M3)[length(Sleep.M3)] = "PPC.Sleep.3"
		if (BM == 1 && !exists("PPC.Sleep.1")) { Cur.PPC = ppc(observations, "Sleep", Sleep.M1$ForPPC, Covs = NULL, 1) 
			Sleep.M1[[length(Sleep.M1) + 1]] = Cur.PPC
		names(Sleep.M1)[length(Sleep.M1)] = "PPC.Sleep.1"
		} else if (BM == 1 && exists("PPC.Sleep.1")) { Cur.PPC = PPC.Sleep.1 }
		
		if (BM == 2 && !exists("PPC.Sleep.2")) {
			Cur.PPC = ppc(observations, "Sleep", Sleep.M2$ForPPC, observations$Time2, 2)
			Sleep.M2[[length(Sleep.M2) + 1]] = Cur.PPC
		names(Sleep.M2)[length(Sleep.M2)] = "PPC.Sleep.2"
		} else if (BM == 2 && exists("PPC.Sleep.2")) { Cur.PPC = PPC.Sleep.2 }
		
		if (sum(PPC.Sleep.3$Summary) > sum(Cur.PPC$Summary)) {
			BM = 3
			Cur.DIC = Sleep.M3$DIC
		}
	}
	
	if (BM == 1) {
		if (Sleep.M4$Sigs[1, 2] == TRUE && Sleep.M4$DIC < Cur.DIC - 3) {
			BM = 4
			Cur.DIC = Sleep.M4$DIC
		} else if (Sleep.M4$Sigs[1, 2] == FALSE && Sleep.M4$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Sleep.4 = ppc(observations, "Sleep", Sleep.M4$ForPPC, Lag.Covs[ , 4], 4)
			Sleep.M4[[length(Sleep.M4) + 1]] = PPC.Sleep.4
			names(Sleep.M4)[length(Sleep.M4)] = "PPC.Sleep.4"
			if (!exists("PPC.Sleep.1")) { PPC.Sleep.1 = ppc(observations, "Sleep", Covs = NULL, 1) 
				Sleep.M1[[length(Sleep.M1) + 1]] = PPC.Sleep.1
				names(Sleep.M1)[length(Sleep.M1)] = "PPC.Sleep.1"
			}
			if (sum(PPC.Sleep.4$Summary) > sum(PPC.Sleep.1$Summary)) {
				BM = 4
				Cur.DIC = Sleep.M4$DIC
			}
		}
	} else if (BM == 2) {
		if (Sleep.M4.1$Sigs[1, 2] == TRUE && Sleep.M4.1$DIC < Cur.DIC - 3) {
			BM = 4.1
			Cur.DIC = Sleep.M4.1$DIC
		} else if (Sleep.M4.1$Sigs[1, 2] == FALSE && Sleep.M4.1$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Sleep.4.1 = ppc(observations, "Sleep", Sleep.M4.1$ForPPC, cbind(Lag.Covs[ , 4], observations$Time2), 4.1)
			Sleep.M1[[length(Sleep.M4.1) + 1]] = PPC.Sleep.4.1
			names(Sleep.M4.1)[length(Sleep.M4.1)] = "PPC.Sleep.4.1"
			if (!exists("PPC.Sleep.2")) { PPC.Sleep.2 = ppc(observations, "Sleep", observations$Time2, 2) 
				Sleep.M2[[length(Sleep.M2) + 1]] = PPC.Sleep.2
				names(Sleep.M2)[length(Sleep.M2)] = "PPC.Sleep.2"
			}
			if (sum(PPC.Sleep.4.1$Summary) > sum(PPC.Sleep.2$Summary)) {
				BM = 4.1
				Cur.DIC = Sleep.M4.1$DIC
			}
		}
	} else if (BM == 3) {
		if (Sleep.M4.2$Sigs[1, 2] == TRUE && Sleep.M4.2$DIC < Cur.DIC - 3) {
			BM = 4.2
			Cur.DIC = Sleep.M4.2$DIC
		} else if (Sleep.M4.2$Sigs[1, 2] == FALSE && Sleep.M4.2$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Sleep.4.2 = ppc(observations, "Sleep", Sleep.M4.2$ForPPC, cbind(Lag.Covs[ , 4], Block.Covs), 4.2)
			Sleep.M4.2[[length(Sleep.M1) + 1]] = PPC.Sleep.4.2
		names(Sleep.M4.2)[length(Sleep.M4.2)] = "PPC.Sleep.4.2"
			if (!exists("PPC.Sleep.3")) { PPC.Sleep.3 = ppc(observations, "Sleep", Block.Covs, 3) 
				Sleep.M3[[length(Sleep.M3) + 1]] = PPC.Sleep.3
				names(Sleep.M3)[length(Sleep.M3)] = "PPC.Sleep.3"
			}
			if (sum(PPC.Sleep.4.2$Summary) > sum(PPC.Sleep.3$Summary)) {
				BM = 4.2
				Cur.DIC = Sleep.M4.2$DIC
			}
		}
	}
	
	if (BM == 1) {
		Sleep.M5.1 = evaluate(observations, "Sleep", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
		Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M5.1
		names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M5.1"
		if ((Sleep.M5.1$Sigs[1, 2] == TRUE || Sleep.M5.1$Sigs[1, 3] == TRUE) && Sleep.M5.1$DIC < Sleep.M1$DIC - 3) {
			BM = 5.1
			Sleep.M1c = evaluate(observations.car, "Sleep", Covs = NULL, 1, No_Neuropain)
			Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M1c
			names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M1c"
			Cur.DIC = Sleep.M1c$DIC
		} else if ((Sleep.M5.1$Sigs[1, 2] == FALSE || Sleep.M5.1$Sigs[1, 3] == FALSE) && Sleep.M5.1$DIC >= Sleep.M1$DIC - 3) { BM = BM		
		} else {
			PPC.Sleep.5.1 = ppc(observations, "Sleep", Sleep.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
			Sleep.M5.1[[length(Sleep.M5.1) + 1]] = PPC.Sleep.5.1
			names(Sleep.M5.1)[length(Sleep.M5.1)] = "PPC.Sleep.5.1"
			if (!exists("PPC.Sleep.1")) { PPC.Sleep.1 = ppc(observations, "Sleep", Sleep.M1$ForPPC, Covs = NULL, 1) 
				Sleep.M1[[length(Sleep.M1) + 1]] = PPC.Sleep.1
				names(Sleep.M1)[length(Sleep.M1)] = "PPC.Sleep.1"
			}
			if (sum(PPC.Sleep.5.1$Summary) > sum(PPC.Sleep.1$Summary)) {
				BM = 5.1
				Sleep.M1c = evaluate(observations.car, "Sleep", Covs = NULL, 1, No_Neuropain)
				Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M1c
				names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M1c"
				Cur.DIC = Sleep.M1c$DIC
			}
		}
	} else if (BM == 2) {
		Sleep.M5.2 = evaluate(observations, "Sleep", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
		Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M5.2
		names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M5.2"
		if ((Sleep.M5.2$Sigs[1, 3] == TRUE || Sleep.M5.2$Sigs[1, 4] == TRUE) && Sleep.M5.2$DIC < Sleep.M2$DIC - 3) {
			BM = 5.2
			Sleep.M2c = evaluate(observations.car, "Sleep", observations$Time2, 2, No_Neuropain)
			Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M2c
			names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M2c"
			Cur.DIC = Sleep.M2c$DIC
		} else if ((Sleep.M5.2$Sigs[1, 3] == FALSE || Sleep.M5.2$Sigs[1, 4] == FALSE) && Sleep.M5.2$DIC >= Sleep.M2$DIC - 3) { BM = BM		
		} else {
			PPC.Sleep.5.2 = ppc(observations, "Sleep", Sleep.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
			Sleep.M5.2[[length(Sleep.M5.2) + 1]] = PPC.Sleep.5.2
			names(Sleep.M5.2)[length(Sleep.M5.2)] = "PPC.Sleep.5.2"
			if (!exists("PPC.Sleep.2")) { PPC.Sleep.2 = ppc(observations, "Sleep", Sleep.M2$ForPPC, observations$Time2, 2) 
				Sleep.M2[[length(Sleep.M2) + 1]] = PPC.Sleep.2
				names(Sleep.M2)[length(Sleep.M2)] = "PPC.Sleep.2"
			}
			if (sum(PPC.Sleep.5.2$Summary) > sum(PPC.Sleep.2$Summary)) {
				BM = 5.2
				Sleep.M2c = evaluate(observations.car, "Sleep", observations$Time2, 2, No_Neuropain)
				Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M2c
				names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M2c"
				Cur.DIC = Sleep.M2c$DIC
			}
		}
	} else if (BM == 3) {
		Sleep.M5.3 = evaluate(observations, "Sleep", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
		Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M5.3
		names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M5.3"
		if ((Sleep.M5.3$Sigs[1, ncol(Sleep.M5.3$Sigs)-1] == TRUE || Sleep.M5.3$Sigs[1, ncol(Sleep.M5.3$Sigs)] == TRUE) && 
			Sleep.M5.3$DIC < Sleep.M3$DIC - 3) {
			BM = 5.3
			Sleep.M3c = evaluate(observations.car, "Sleep", Block.Covs, 3, No_Neuropain)
			Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M3c
			names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M3c"
			Cur.DIC = Sleep.M3c$DIC
		} else if ((Sleep.M5.3$Sigs[1, ncol(Sleep.M5.3$Sigs)-1] == FALSE || Sleep.M5.3$Sigs[1, ncol(Sleep.M5.3$Sigs)] == FALSE) && 
			Sleep.M5.3$DIC >= Sleep.M3$DIC - 3) { BM = BM		
		} else {
			PPC.Sleep.5.3 = ppc(observations, "Sleep", Sleep.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
			Sleep.M5.3[[length(Sleep.M5.3) + 1]] = PPC.Sleep.5.3
			names(Sleep.M5.3)[length(Sleep.M5.3)] = "PPC.Sleep.5.3"
			if (!exists("PPC.Sleep.3")) { PPC.Sleep.3 = ppc(observations, "Sleep", Sleep.M3$ForPPC, Block.Covs, 3) 
				Sleep.M3[[length(Sleep.M3) + 1]] = PPC.Sleep.3
				names(Sleep.M3)[length(Sleep.M3)] = "PPC.Sleep.3"
			}
			if (sum(PPC.Sleep.5.3$Summary) > sum(PPC.Sleep.3$Summary)) {
				BM = 5.3
				Sleep.M3c = evaluate(observations.car, "Sleep", Block.Covs, 3, No_Neuropain)
				Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M3c
				names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M3c"
				Cur.DIC = Sleep.M3c$DIC
			}
		}
	} else if (BM == 4) {
		Sleep.M5.4 = evaluate(observations, "Sleep", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
		Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M5.4
		names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M5.4"
		if ((Sleep.M5.4$Sigs[1, ncol(Sleep.M5.4$Sigs)-1] == TRUE || Sleep.M5.4$Sigs[1, ncol(Sleep.M5.4$Sigs)] == TRUE) && 
			Sleep.M5.4$DIC < Sleep.M4$DIC - 3) {
			BM = 5.4
			Sleep.M4c = evaluate(observations.car, "Sleep", Lag.Covs, 4, No_Neuropain)
			Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M4c
			names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M4c"
			Cur.DIC = Sleep.M4c$DIC
		} else if ((Sleep.M5.4$Sigs[1, ncol(Sleep.M5.4$Sigs)-1] == FALSE && Sleep.M5.4$Sigs[1, ncol(Sleep.M5.4$Sigs)] == FALSE) && 
			Sleep.M5.4$DIC >= Sleep.M4$DIC - 3) { BM = BM		
		} else {
			PPC.Sleep.5.4 = ppc(observations, "Sleep", Sleep.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
			Sleep.M5.4[[length(Sleep.M5.4) + 1]] = PPC.Sleep.5.4
			names(Sleep.M5.4)[length(Sleep.M5.4)] = "PPC.Sleep.5.4"
			if (!exists("PPC.Sleep.4")) { PPC.Sleep.4 = ppc(observations, "Sleep", Sleep.M4$ForPPC, Lag.Covs[ , 4], 4) 
				Sleep.M4[[length(Sleep.M4) + 1]] = PPC.Sleep.4
				names(Sleep.M4)[length(Sleep.M4)] = "PPC.Sleep.4"
			}
			if (sum(PPC.Sleep.5.4$Summary) > sum(PPC.Sleep.4$Summary)) {
				BM = 5.4
				Sleep.M4c = evaluate(observations.car, "Sleep", Lag.Covs, 4, No_Neuropain)
				Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M4c
				names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M4c"
				Cur.DIC = Sleep.M4c$DIC
			}
		}
	} else if (BM == 4.1) {
		Sleep.M5.41 = evaluate(observations, "Sleep", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
			5.41, No_Neuropain)
		Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M5.41
		names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M5.41"
		if ((Sleep.M5.41$Sigs[1, ncol(Sleep.M5.41$Sigs)-1] == TRUE || Sleep.M5.41$Sigs[1, ncol(Sleep.M5.41$Sigs)] == TRUE) && 
			Sleep.M5.41$DIC < Sleep.M4.1$DIC - 3) {
			BM = 5.41
			Sleep.M4.1c = evaluate(observations.car, "Sleep", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
			Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M4.1c
			names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M4.1c"
			Cur.DIC = Sleep.M4.1c$DIC
		} else if ((Sleep.M5.41$Sigs[1, ncol(Sleep.M5.41$Sigs)-1] == FALSE && Sleep.M5.41$Sigs[1, ncol(Sleep.M5.41$Sigs)] == FALSE) && 
			Sleep.M5.41$DIC >= Sleep.M4.1$DIC - 3) { BM = BM		
		} else {
			PPC.Sleep.5.41 = ppc(observations, "Sleep", Sleep.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
				observations$car.B), 5.41)
			Sleep.M5.41[[length(Sleep.M5.41) + 1]] = PPC.Sleep.5.41
			names(Sleep.M5.41)[length(Sleep.M5.41)] = "PPC.Sleep.5.41"
			if (!exists("PPC.Sleep.4.1")) { PPC.Sleep.4.1 = ppc(observations, "Sleep", Sleep.M4.1$ForPPC, cbind(Lag.Covs[ , 4], observations$Time2), 4.1) 
				Sleep.M4.1[[length(Sleep.M4.1) + 1]] = PPC.Sleep.4.1
				names(Sleep.M4.1)[length(Sleep.M4.1)] = "PPC.Sleep.4.1"
			}
			if (sum(PPC.Sleep.5.41$Summary) > sum(PPC.Sleep.4.1$Summary)) {
				BM = 5.41
				Sleep.M4.1c = evaluate(observations.car, "Sleep", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M4.1c
				names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M4.1c"
				Cur.DIC = Sleep.M4.1c$DIC
			}
		}
	} else if (BM == 4.2) {
		Sleep.M5.42 = evaluate(observations, "Sleep", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
			5.42, No_Neuropain)
		Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M5.42
		names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M5.42"
		if ((Sleep.M5.42$Sigs[1, ncol(Sleep.M5.42$Sigs)-1] == TRUE || Sleep.M5.42$Sigs[1, ncol(Sleep.M5.42$Sigs)] == TRUE) && 
			Sleep.M5.42$DIC < Sleep.M4.2$DIC - 3) {
			BM = 5.42
			Sleep.M4.2c = evaluate(observations.car, "Sleep", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
			Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M4.2c
			names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M4.2c"
			Cur.DIC = Sleep.M4.2c$DIC
		} else if ((Sleep.M5.42$Sigs[1, ncol(Sleep.M5.42$Sigs)-1] == FALSE && Sleep.M5.42$Sigs[1, ncol(Sleep.M5.42$Sigs)] == FALSE) && 
			Sleep.M5.42$DIC >= Sleep.M4.2$DIC - 3) { BM = BM		
		} else {
			PPC.Sleep.5.42 = ppc(observations, "Sleep", Sleep.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
				observations$car.B), 5.42)
			Sleep.M5.42[[length(Sleep.M5.42) + 1]] = PPC.Sleep.5.42
			names(Sleep.M5.42)[length(Sleep.M5.42)] = "PPC.Sleep.5.42"
			if (!exists("PPC.Sleep.4.2")) { PPC.Sleep.4.2 = ppc(observations, "Sleep", Sleep.M4.2$ForPPC, cbind(Lag.Covs[ , 4], Block.Covs), 4.2) 
				Sleep.M4.2[[length(Sleep.M4.2) + 1]] = PPC.Sleep.4.2
				names(Sleep.M4.2)[length(Sleep.M4.2)] = "PPC.Sleep.4.2"
			}
			if (sum(PPC.Sleep.5.42$Summary) > sum(PPC.Sleep.4.2$Summary)) {
				BM = 5.42
				Sleep.M4.2c = evaluate(observations.car, "Sleep", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Sleep.Info[[length(Sleep.Info) + 1]] = Sleep.M4.2c
				names(Sleep.Info)[length(Sleep.Info)] = "Sleep.M4.2c"
				Cur.DIC = Sleep.M4.2c$DIC
			}
		}
	}
	
	Best.Model = cbind(Best.Model, BM)
	
	#######################
	# Analysis for Thinking
	#######################
	
	Thinking.M1 = evaluate(observations, Outcome = "Thinking", Covs = NULL, mod.id = 1, No_Neuropain)
	Thinking.M2 = evaluate(observations, Outcome = "Thinking", Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Thinking.M3 = evaluate(observations, Outcome = "Thinking", Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Thinking.M4 = evaluate(observations, Outcome = "Thinking", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Thinking.M4.1 = evaluate(observations, Outcome = "Thinking", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Thinking.M4.2 = evaluate(observations, Outcome = "Thinking", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	
	Thinking.Info = list(Thinking.M1, Thinking.M2, Thinking.M3, Thinking.M4, Thinking.M4.1, Thinking.M4.2)
	names(Thinking.Info) = c("Thinking.M1", "Thinking.M2", "Thinking.M3", "Thinking.M4", "Thinking.M4.1", "Thinking.M4.2")
	
	BM = 1
	Cur.DIC = Thinking.M1$DIC
	
	if (Thinking.M2$Sigs[2] == TRUE && Thinking.M2$DIC < Thinking.M1$DIC - 3) { 
		BM = 2
		Cur.DIC = Thinking.M2$DIC
	} else if (Thinking.M2$Sigs[2] == FALSE && Thinking.M2$DIC >= Thinking.M1$DIC - 3) { BM = 1
	} else {
		PPC.Thinking.1 = ppc(observations, "Thinking", Thinking.M1$ForPPC, Covs = NULL, 1)
		Thinking.M1[[length(Thinking.M1) + 1]] = PPC.Thinking.1
		names(Thinking.M1)[length(Thinking.M1)] = "PPC.Thinking.1"
		PPC.Thinking.2 = ppc(observations, "Thinking", Thinking.M2$ForPPC, observations$Time2, 2)
		Thinking.M2[[length(Thinking.M2) + 1]] = PPC.Thinking.2
		names(Thinking.M2)[length(Thinking.M2)] = "PPC.Thinking.2"
		if (sum(PPC.Thinking.2$Summary) > sum(PPC.Thinking.1$Summary)) { 
			BM = 2 
			Cur.DIC = Thinking.M2$DIC
		}
	}
	
	if (sum(Thinking.M3$Sigs[ , 2:ncol(Thinking.M3$Sigs)]) > 0 && Thinking.M3$DIC < Cur.DIC - 3) {
		BM = 3
		Cur.DIC = Thinking.M3$DIC
	} else if (sum(Thinking.M3$Sigs[ , 2:ncol(Thinking.M3$Sigs)]) == 0 && Thinking.M3$DIC >= Cur.DIC - 3) { BM = BM
	} else {
		PPC.Thinking.3 = ppc(observations, "Thinking", Thinking.M3$ForPPC, Block.Covs, 3)
		Thinking.M3[[length(Thinking.M3) + 1]] = PPC.Thinking.3
		names(Thinking.M3)[length(Thinking.M3)] = "PPC.Thinking.3"
		if (BM == 1 && !exists("PPC.Thinking.1")) { Cur.PPC = ppc(observations, "Thinking", Thinking.M1$ForPPC, Covs = NULL, 1) 
			Thinking.M1[[length(Thinking.M1) + 1]] = Cur.PPC
		names(Thinking.M1)[length(Thinking.M1)] = "PPC.Thinking.1"
		} else if (BM == 1 && exists("PPC.Thinking.1")) { Cur.PPC = PPC.Thinking.1 }
		
		if (BM == 2 && !exists("PPC.Thinking.2")) {
			Cur.PPC = ppc(observations, "Thinking", Thinking.M2$ForPPC, observations$Time2, 2)
			Thinking.M2[[length(Thinking.M2) + 1]] = Cur.PPC
		names(Thinking.M2)[length(Thinking.M2)] = "PPC.Thinking.2"
		} else if (BM == 2 && exists("PPC.Thinking.2")) { Cur.PPC = PPC.Thinking.2 }
		
		if (sum(PPC.Thinking.3$Summary) > sum(Cur.PPC$Summary)) {
			BM = 3
			Cur.DIC = Thinking.M3$DIC
		}
	}
	
	if (BM == 1) {
		if (Thinking.M4$Sigs[1, 2] == TRUE && Thinking.M4$DIC < Cur.DIC - 3) {
			BM = 4
			Cur.DIC = Thinking.M4$DIC
		} else if (Thinking.M4$Sigs[1, 2] == FALSE && Thinking.M4$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Thinking.4 = ppc(observations, "Thinking", Thinking.M4$ForPPC, Lag.Covs[ , 5], 4)
			Thinking.M4[[length(Thinking.M4) + 1]] = PPC.Thinking.4
			names(Thinking.M4)[length(Thinking.M4)] = "PPC.Thinking.4"
			if (!exists("PPC.Thinking.1")) { PPC.Thinking.1 = ppc(observations, "Thinking", Covs = NULL, 1) 
				Thinking.M1[[length(Thinking.M1) + 1]] = PPC.Thinking.1
				names(Thinking.M1)[length(Thinking.M1)] = "PPC.Thinking.1"
			}
			if (sum(PPC.Thinking.4$Summary) > sum(PPC.Thinking.1$Summary)) {
				BM = 4
				Cur.DIC = Thinking.M4$DIC
			}
		}
	} else if (BM == 2) {
		if (Thinking.M4.1$Sigs[1, 2] == TRUE && Thinking.M4.1$DIC < Cur.DIC - 3) {
			BM = 4.1
			Cur.DIC = Thinking.M4.1$DIC
		} else if (Thinking.M4.1$Sigs[1, 2] == FALSE && Thinking.M4.1$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Thinking.4.1 = ppc(observations, "Thinking", Thinking.M4.1$ForPPC, cbind(Lag.Covs[ , 5], observations$Time2), 4.1)
			Thinking.M1[[length(Thinking.M4.1) + 1]] = PPC.Thinking.4.1
			names(Thinking.M4.1)[length(Thinking.M4.1)] = "PPC.Thinking.4.1"
			if (!exists("PPC.Thinking.2")) { PPC.Thinking.2 = ppc(observations, "Thinking", observations$Time2, 2) 
				Thinking.M2[[length(Thinking.M2) + 1]] = PPC.Thinking.2
				names(Thinking.M2)[length(Thinking.M2)] = "PPC.Thinking.2"
			}
			if (sum(PPC.Thinking.4.1$Summary) > sum(PPC.Thinking.2$Summary)) {
				BM = 4.1
				Cur.DIC = Thinking.M4.1$DIC
			}
		}
	} else if (BM == 3) {
		if (Thinking.M4.2$Sigs[1, 2] == TRUE && Thinking.M4.2$DIC < Cur.DIC - 3) {
			BM = 4.2
			Cur.DIC = Thinking.M4.2$DIC
		} else if (Thinking.M4.2$Sigs[1, 2] == FALSE && Thinking.M4.2$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Thinking.4.2 = ppc(observations, "Thinking", Thinking.M4.2$ForPPC, cbind(Lag.Covs[ , 5], Block.Covs), 4.2)
			Thinking.M4.2[[length(Thinking.M1) + 1]] = PPC.Thinking.4.2
		names(Thinking.M4.2)[length(Thinking.M4.2)] = "PPC.Thinking.4.2"
			if (!exists("PPC.Thinking.3")) { PPC.Thinking.3 = ppc(observations, "Thinking", Block.Covs, 3) 
				Thinking.M3[[length(Thinking.M3) + 1]] = PPC.Thinking.3
				names(Thinking.M3)[length(Thinking.M3)] = "PPC.Thinking.3"
			}
			if (sum(PPC.Thinking.4.2$Summary) > sum(PPC.Thinking.3$Summary)) {
				BM = 4.2
				Cur.DIC = Thinking.M4.2$DIC
			}
		}
	}
	
	if (BM == 1) {
		Thinking.M5.1 = evaluate(observations, "Thinking", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
		Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M5.1
		names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M5.1"
		if ((Thinking.M5.1$Sigs[1, 2] == TRUE || Thinking.M5.1$Sigs[1, 3] == TRUE) && Thinking.M5.1$DIC < Thinking.M1$DIC - 3) {
			BM = 5.1
			Thinking.M1c = evaluate(observations.car, "Thinking", Covs = NULL, 1, No_Neuropain)
			Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M1c
			names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M1c"
			Cur.DIC = Thinking.M1c$DIC
		} else if ((Thinking.M5.1$Sigs[1, 2] == FALSE || Thinking.M5.1$Sigs[1, 3] == FALSE) && Thinking.M5.1$DIC >= Thinking.M1$DIC - 3) { BM = BM		
		} else {
			PPC.Thinking.5.1 = ppc(observations, "Thinking", Thinking.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
			Thinking.M5.1[[length(Thinking.M5.1) + 1]] = PPC.Thinking.5.1
			names(Thinking.M5.1)[length(Thinking.M5.1)] = "PPC.Thinking.5.1"
			if (!exists("PPC.Thinking.1")) { PPC.Thinking.1 = ppc(observations, "Thinking", Covs = NULL, 1) 
				Thinking.M1[[length(Thinking.M1) + 1]] = PPC.Thinking.1
				names(Thinking.M1)[length(Thinking.M1)] = "PPC.Thinking.1"
			}
			if (sum(PPC.Thinking.5.1$Summary) > sum(PPC.Thinking.1$Summary)) {
				BM = 5.1
				Thinking.M1c = evaluate(observations.car, "Thinking", Covs = NULL, 1, No_Neuropain)
				Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M1c
				names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M1c"
				Cur.DIC = Thinking.M1c$DIC
			}
		}
	} else if (BM == 2) {
		Thinking.M5.2 = evaluate(observations, "Thinking", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
		Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M5.2
		names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M5.2"
		if ((Thinking.M5.2$Sigs[1, 3] == TRUE || Thinking.M5.2$Sigs[1, 4] == TRUE) && Thinking.M5.2$DIC < Thinking.M2$DIC - 3) {
			BM = 5.2
			Thinking.M2c = evaluate(observations.car, "Thinking", observations$Time2, 2, No_Neuropain)
			Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M2c
			names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M2c"
			Cur.DIC = Thinking.M2c$DIC
		} else if ((Thinking.M5.2$Sigs[1, 3] == FALSE || Thinking.M5.2$Sigs[1, 4] == FALSE) && Thinking.M5.2$DIC >= Thinking.M2$DIC - 3) { BM = BM		
		} else {
			PPC.Thinking.5.2 = ppc(observations, "Thinking", Thinking.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
			Thinking.M5.2[[length(Thinking.M5.2) + 1]] = PPC.Thinking.5.2
			names(Thinking.M5.2)[length(Thinking.M5.2)] = "PPC.Thinking.5.2"
			if (!exists("PPC.Thinking.2")) { PPC.Thinking.2 = ppc(observations, "Thinking", observations$Time2, 2) 
				Thinking.M2[[length(Thinking.M2) + 1]] = PPC.Thinking.2
				names(Thinking.M2)[length(Thinking.M2)] = "PPC.Thinking.2"
			}
			if (sum(PPC.Thinking.5.2$Summary) > sum(PPC.Thinking.2$Summary)) {
				BM = 5.2
				Thinking.M2c = evaluate(observations.car, "Thinking", observations$Time2, 2, No_Neuropain)
				Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M2c
				names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M2c"
				Cur.DIC = Thinking.M2c$DIC
			}
		}
	} else if (BM == 3) {
		Thinking.M5.3 = evaluate(observations, "Thinking", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
		Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M5.3
		names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M5.3"
		if ((Thinking.M5.3$Sigs[1, ncol(Thinking.M5.3$Sigs)-1] == TRUE || Thinking.M5.3$Sigs[1, ncol(Thinking.M5.3$Sigs)] == TRUE) && 
			Thinking.M5.3$DIC < Thinking.M3$DIC - 3) {
			BM = 5.3
			Thinking.M3c = evaluate(observations.car, "Thinking", Block.Covs, 3, No_Neuropain)
			Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M3c
			names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M3c"
			Cur.DIC = Thinking.M3c$DIC
		} else if ((Thinking.M5.3$Sigs[1, ncol(Thinking.M5.3$Sigs)-1] == FALSE || Thinking.M5.3$Sigs[1, ncol(Thinking.M5.3$Sigs)] == FALSE) && 
			Thinking.M5.3$DIC >= Thinking.M3$DIC - 3) { BM = BM		
		} else {
			PPC.Thinking.5.3 = ppc(observations, "Thinking", Thinking.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
			Thinking.M5.3[[length(Thinking.M5.3) + 1]] = PPC.Thinking.5.3
			names(Thinking.M5.3)[length(Thinking.M5.3)] = "PPC.Thinking.5.3"
			if (!exists("PPC.Thinking.3")) { PPC.Thinking.3 = ppc(observations, "Thinking", Block.Covs, 3) 
				Thinking.M3[[length(Thinking.M3) + 1]] = PPC.Thinking.3
				names(Thinking.M3)[length(Thinking.M3)] = "PPC.Thinking.3"
			}
			if (sum(PPC.Thinking.5.3$Summary) > sum(PPC.Thinking.3$Summary)) {
				BM = 5.3
				Thinking.M3c = evaluate(observations.car, "Thinking", Block.Covs, 3, No_Neuropain)
				Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M3c
				names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M3c"
				Cur.DIC = Thinking.M3c$DIC
			}
		}
	} else if (BM == 4) {
		Thinking.M5.4 = evaluate(observations, "Thinking", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
		Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M5.4
		names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M5.4"
		if ((Thinking.M5.4$Sigs[1, ncol(Thinking.M5.4$Sigs)-1] == TRUE || Thinking.M5.4$Sigs[1, ncol(Thinking.M5.4$Sigs)] == TRUE) && 
			Thinking.M5.4$DIC < Thinking.M4$DIC - 3) {
			BM = 5.4
			Thinking.M4c = evaluate(observations.car, "Thinking", Lag.Covs, 4, No_Neuropain)
			Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M4c
			names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M4c"
			Cur.DIC = Thinking.M4c$DIC
		} else if ((Thinking.M5.4$Sigs[1, ncol(Thinking.M5.4$Sigs)-1] == FALSE && Thinking.M5.4$Sigs[1, ncol(Thinking.M5.4$Sigs)] == FALSE) && 
			Thinking.M5.4$DIC >= Thinking.M4$DIC - 3) { BM = BM		
		} else {
			PPC.Thinking.5.4 = ppc(observations, "Thinking", Thinking.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
			Thinking.M5.4[[length(Thinking.M5.4) + 1]] = PPC.Thinking.5.4
			names(Thinking.M5.4)[length(Thinking.M5.4)] = "PPC.Thinking.5.4"
			if (!exists("PPC.Thinking.4")) { PPC.Thinking.4 = ppc(observations, "Thinking", Lag.Covs[ , 5], 4) 
				Thinking.M4[[length(Thinking.M4) + 1]] = PPC.Thinking.4
				names(Thinking.M4)[length(Thinking.M4)] = "PPC.Thinking.4"
			}
			if (sum(PPC.Thinking.5.4$Summary) > sum(PPC.Thinking.4$Summary)) {
				BM = 5.4
				Thinking.M4c = evaluate(observations.car, "Thinking", Lag.Covs, 4, No_Neuropain)
				Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M4c
				names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M4c"
				Cur.DIC = Thinking.M4c$DIC
			}
		}
	} else if (BM == 4.1) {
		Thinking.M5.41 = evaluate(observations, "Thinking", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
			5.41, No_Neuropain)
		Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M5.41
		names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M5.41"
		if ((Thinking.M5.41$Sigs[1, ncol(Thinking.M5.41$Sigs)-1] == TRUE || Thinking.M5.41$Sigs[1, ncol(Thinking.M5.41$Sigs)] == TRUE) && 
			Thinking.M5.41$DIC < Thinking.M4.1$DIC - 3) {
			BM = 5.41
			Thinking.M4.1c = evaluate(observations.car, "Thinking", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
			Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M4.1c
			names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M4.1c"
			Cur.DIC = Thinking.M4.1c$DIC
		} else if ((Thinking.M5.41$Sigs[1, ncol(Thinking.M5.41$Sigs)-1] == FALSE && Thinking.M5.41$Sigs[1, ncol(Thinking.M5.41$Sigs)] == FALSE) && 
			Thinking.M5.41$DIC >= Thinking.M4.1$DIC - 3) { BM = BM		
		} else {
			PPC.Thinking.5.41 = ppc(observations, "Thinking", Thinking.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
				observations$car.B), 5.41)
			Thinking.M5.41[[length(Thinking.M5.41) + 1]] = PPC.Thinking.5.41
			names(Thinking.M5.41)[length(Thinking.M5.41)] = "PPC.Thinking.5.41"
			if (!exists("PPC.Thinking.4.1")) { PPC.Thinking.4.1 = ppc(observations, "Thinking", cbind(Lag.Covs[ , 5], observations$Time2), 4.1) 
				Thinking.M4.1[[length(Thinking.M4.1) + 1]] = PPC.Thinking.4.1
				names(Thinking.M4.1)[length(Thinking.M4.1)] = "PPC.Thinking.4.1"
			}
			if (sum(PPC.Thinking.5.41$Summary) > sum(PPC.Thinking.4.1$Summary)) {
				BM = 5.41
				Thinking.M4.1c = evaluate(observations.car, "Thinking", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M4.1c
				names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M4.1c"
				Cur.DIC = Thinking.M4.1c$DIC
			}
		}
	} else if (BM == 4.2) {
		Thinking.M5.42 = evaluate(observations, "Thinking", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
			5.42, No_Neuropain)
		Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M5.42
		names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M5.42"
		if ((Thinking.M5.42$Sigs[1, ncol(Thinking.M5.42$Sigs)-1] == TRUE || Thinking.M5.42$Sigs[1, ncol(Thinking.M5.42$Sigs)] == TRUE) && 
			Thinking.M5.42$DIC < Thinking.M4.2$DIC - 3) {
			BM = 5.42
			Thinking.M4.2c = evaluate(observations.car, "Thinking", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
			Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M4.2c
			names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M4.2c"
			Cur.DIC = Thinking.M4.2c$DIC
		} else if ((Thinking.M5.42$Sigs[1, ncol(Thinking.M5.42$Sigs)-1] == FALSE && Thinking.M5.42$Sigs[1, ncol(Thinking.M5.42$Sigs)] == FALSE) && 
			Thinking.M5.42$DIC >= Thinking.M4.2$DIC - 3) { BM = BM		
		} else {
			PPC.Thinking.5.42 = ppc(observations, "Thinking", Thinking.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
				observations$car.B), 5.42)
			Thinking.M5.42[[length(Thinking.M5.42) + 1]] = PPC.Thinking.5.42
			names(Thinking.M5.42)[length(Thinking.M5.42)] = "PPC.Thinking.5.42"
			if (!exists("PPC.Thinking.4.2")) { PPC.Thinking.4.2 = ppc(observations, "Thinking", cbind(Lag.Covs[ , 5], Block.Covs), 4.2) 
				Thinking.M4.2[[length(Thinking.M4.2) + 1]] = PPC.Thinking.4.2
				names(Thinking.M4.2)[length(Thinking.M4.2)] = "PPC.Thinking.4.2"
			}
			if (sum(PPC.Thinking.5.42$Summary) > sum(PPC.Thinking.4.2$Summary)) {
				BM = 5.42
				Thinking.M4.2c = evaluate(observations.car, "Thinking", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Thinking.Info[[length(Thinking.Info) + 1]] = Thinking.M4.2c
				names(Thinking.Info)[length(Thinking.Info)] = "Thinking.M4.2c"
				Cur.DIC = Thinking.M4.2c$DIC
			}
		}
	}
	
	Best.Model = cbind(Best.Model, BM)
	
	###########################
	# Analysis for Constipation
	###########################
	
	Constipation.M1 = evaluate(observations, Outcome = "Constipation", Covs = NULL, mod.id = 1, No_Neuropain)
	Constipation.M2 = evaluate(observations, Outcome = "Constipation", Covs = observations$Time2, mod.id = 2, No_Neuropain)
	Constipation.M3 = evaluate(observations, Outcome = "Constipation", Covs = Block.Covs, mod.id = 3, No_Neuropain)
	Constipation.M4 = evaluate(observations, Outcome = "Constipation", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
	Constipation.M4.1 = evaluate(observations, Outcome = "Constipation", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
	Constipation.M4.2 = evaluate(observations, Outcome = "Constipation", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
	
	Constipation.Info = list(Constipation.M1, Constipation.M2, Constipation.M3, Constipation.M4, Constipation.M4.1, Constipation.M4.2)
	names(Constipation.Info) = c("Constipation.M1", "Constipation.M2", "Constipation.M3", "Constipation.M4", "Constipation.M4.1", "Constipation.M4.2")
	
	BM = 1
	Cur.DIC = Constipation.M1$DIC
	
	if (Constipation.M2$Sigs[2] == TRUE && Constipation.M2$DIC < Constipation.M1$DIC - 3) { 
		BM = 2
		Cur.DIC = Constipation.M2$DIC
	} else if (Constipation.M2$Sigs[2] == FALSE && Constipation.M2$DIC >= Constipation.M1$DIC - 3) { BM = 1
	} else {
		PPC.Constipation.1 = ppc(observations, "Constipation", Constipation.M1$ForPPC, Covs = NULL, 1)
		Constipation.M1[[length(Constipation.M1) + 1]] = PPC.Constipation.1
		names(Constipation.M1)[length(Constipation.M1)] = "PPC.Constipation.1"
		PPC.Constipation.2 = ppc(observations, "Constipation", Constipation.M2$ForPPC, observations$Time2, 2)
		Constipation.M2[[length(Constipation.M2) + 1]] = PPC.Constipation.2
		names(Constipation.M2)[length(Constipation.M2)] = "PPC.Constipation.2"
		if (sum(PPC.Constipation.2$Summary) > sum(PPC.Constipation.1$Summary)) { 
			BM = 2 
			Cur.DIC = Constipation.M2$DIC
		}
	}
	
	if (sum(Constipation.M3$Sigs[ , 2:ncol(Constipation.M3$Sigs)]) > 0 && Constipation.M3$DIC < Cur.DIC - 3) {
		BM = 3
		Cur.DIC = Constipation.M3$DIC
	} else if (sum(Constipation.M3$Sigs[ , 2:ncol(Constipation.M3$Sigs)]) == 0 && Constipation.M3$DIC >= Cur.DIC - 3) { BM = BM
	} else {
		PPC.Constipation.3 = ppc(observations, "Constipation", Constipation.M3$ForPPC, Block.Covs, 3)
		Constipation.M3[[length(Constipation.M3) + 1]] = PPC.Constipation.3
		names(Constipation.M3)[length(Constipation.M3)] = "PPC.Constipation.3"
		if (BM == 1 && !exists("PPC.Constipation.1")) { Cur.PPC = ppc(observations, "Constipation", Constipation.M1$ForPPC, Covs = NULL, 1) 
			Constipation.M1[[length(Constipation.M1) + 1]] = Cur.PPC
		names(Constipation.M1)[length(Constipation.M1)] = "PPC.Constipation.1"
		} else if (BM == 1 && exists("PPC.Constipation.1")) { Cur.PPC = PPC.Constipation.1 }
		
		if (BM == 2 && !exists("PPC.Constipation.2")) {
			Cur.PPC = ppc(observations, "Constipation", Constipation.M2$ForPPC, observations$Time2, 2)
			Constipation.M2[[length(Constipation.M2) + 1]] = Cur.PPC
		names(Constipation.M2)[length(Constipation.M2)] = "PPC.Constipation.2"
		} else if (BM == 2 && exists("PPC.Constipation.2")) { Cur.PPC = PPC.Constipation.2 }
		
		if (sum(PPC.Constipation.3$Summary) > sum(Cur.PPC$Summary)) {
			BM = 3
			Cur.DIC = Constipation.M3$DIC
		}
	}
	
	if (BM == 1) {
		if (Constipation.M4$Sigs[1, 2] == TRUE && Constipation.M4$DIC < Cur.DIC - 3) {
			BM = 4
			Cur.DIC = Constipation.M4$DIC
		} else if (Constipation.M4$Sigs[1, 2] == FALSE && Constipation.M4$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Constipation.4 = ppc(observations, "Constipation", Constipation.M4$ForPPC, Lag.Covs[ , 6], 4)
			Constipation.M4[[length(Constipation.M4) + 1]] = PPC.Constipation.4
			names(Constipation.M4)[length(Constipation.M4)] = "PPC.Constipation.4"
			if (!exists("PPC.Constipation.1")) { PPC.Constipation.1 = ppc(observations, "Constipation", Covs = NULL, 1) 
				Constipation.M1[[length(Constipation.M1) + 1]] = PPC.Constipation.1
				names(Constipation.M1)[length(Constipation.M1)] = "PPC.Constipation.1"
			}
			if (sum(PPC.Constipation.4$Summary) > sum(PPC.Constipation.1$Summary)) {
				BM = 4
				Cur.DIC = Constipation.M4$DIC
			}
		}
	} else if (BM == 2) {
		if (Constipation.M4.1$Sigs[1, 2] == TRUE && Constipation.M4.1$DIC < Cur.DIC - 3) {
			BM = 4.1
			Cur.DIC = Constipation.M4.1$DIC
		} else if (Constipation.M4.1$Sigs[1, 2] == FALSE && Constipation.M4.1$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Constipation.4.1 = ppc(observations, "Constipation", Constipation.M4.1$ForPPC, cbind(Lag.Covs[ , 6], observations$Time2), 4.1)
			Constipation.M1[[length(Constipation.M4.1) + 1]] = PPC.Constipation.4.1
			names(Constipation.M4.1)[length(Constipation.M4.1)] = "PPC.Constipation.4.1"
			if (!exists("PPC.Constipation.2")) { PPC.Constipation.2 = ppc(observations, "Constipation", observations$Time2, 2) 
				Constipation.M2[[length(Constipation.M2) + 1]] = PPC.Constipation.2
				names(Constipation.M2)[length(Constipation.M2)] = "PPC.Constipation.2"
			}
			if (sum(PPC.Constipation.4.1$Summary) > sum(PPC.Constipation.2$Summary)) {
				BM = 4.1
				Cur.DIC = Constipation.M4.1$DIC
			}
		}
	} else if (BM == 3) {
		if (Constipation.M4.2$Sigs[1, 2] == TRUE && Constipation.M4.2$DIC < Cur.DIC - 3) {
			BM = 4.2
			Cur.DIC = Constipation.M4.2$DIC
		} else if (Constipation.M4.2$Sigs[1, 2] == FALSE && Constipation.M4.2$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Constipation.4.2 = ppc(observations, "Constipation", Constipation.M4.2$ForPPC, cbind(Lag.Covs[ , 6], Block.Covs), 4.2)
			Constipation.M4.2[[length(Constipation.M1) + 1]] = PPC.Constipation.4.2
		names(Constipation.M4.2)[length(Constipation.M4.2)] = "PPC.Constipation.4.2"
			if (!exists("PPC.Constipation.3")) { PPC.Constipation.3 = ppc(observations, "Constipation", Block.Covs, 3) 
				Constipation.M3[[length(Constipation.M3) + 1]] = PPC.Constipation.3
				names(Constipation.M3)[length(Constipation.M3)] = "PPC.Constipation.3"
			}
			if (sum(PPC.Constipation.4.2$Summary) > sum(PPC.Constipation.3$Summary)) {
				BM = 4.2
				Cur.DIC = Constipation.M4.2$DIC
			}
		}
	}
	
	if (BM == 1) {
		Constipation.M5.1 = evaluate(observations, "Constipation", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
		Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M5.1
		names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M5.1"
		if ((Constipation.M5.1$Sigs[1, 2] == TRUE || Constipation.M5.1$Sigs[1, 3] == TRUE) && Constipation.M5.1$DIC < Constipation.M1$DIC - 3) {
			BM = 5.1
			Constipation.M1c = evaluate(observations.car, "Constipation", Covs = NULL, 1, No_Neuropain)
			Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M1c
			names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M1c"
			Cur.DIC = Constipation.M1c$DIC
		} else if ((Constipation.M5.1$Sigs[1, 2] == FALSE || Constipation.M5.1$Sigs[1, 3] == FALSE) && Constipation.M5.1$DIC >= Constipation.M1$DIC - 3) { BM = BM		
		} else {
			PPC.Constipation.5.1 = ppc(observations, "Constipation", Constipation.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
			Constipation.M5.1[[length(Constipation.M5.1) + 1]] = PPC.Constipation.5.1
			names(Constipation.M5.1)[length(Constipation.M5.1)] = "PPC.Constipation.5.1"
			if (!exists("PPC.Constipation.1")) { PPC.Constipation.1 = ppc(observations, "Constipation", Covs = NULL, 1) 
				Constipation.M1[[length(Constipation.M1) + 1]] = PPC.Constipation.1
				names(Constipation.M1)[length(Constipation.M1)] = "PPC.Constipation.1"
			}
			if (sum(PPC.Constipation.5.1$Summary) > sum(PPC.Constipation.1$Summary)) {
				BM = 5.1
				Constipation.M1c = evaluate(observations.car, "Constipation", Covs = NULL, 1, No_Neuropain)
				Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M1c
				names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M1c"
				Cur.DIC = Constipation.M1c$DIC
			}
		}
	} else if (BM == 2) {
		Constipation.M5.2 = evaluate(observations, "Constipation", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
		Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M5.2
		names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M5.2"
		if ((Constipation.M5.2$Sigs[1, 3] == TRUE || Constipation.M5.2$Sigs[1, 4] == TRUE) && Constipation.M5.2$DIC < Constipation.M2$DIC - 3) {
			BM = 5.2
			Constipation.M2c = evaluate(observations.car, "Constipation", observations$Time2, 2, No_Neuropain)
			Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M2c
			names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M2c"
			Cur.DIC = Constipation.M2c$DIC
		} else if ((Constipation.M5.2$Sigs[1, 3] == FALSE || Constipation.M5.2$Sigs[1, 4] == FALSE) && Constipation.M5.2$DIC >= Constipation.M2$DIC - 3) { BM = BM		
		} else {
			PPC.Constipation.5.2 = ppc(observations, "Constipation", Constipation.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
			Constipation.M5.2[[length(Constipation.M5.2) + 1]] = PPC.Constipation.5.2
			names(Constipation.M5.2)[length(Constipation.M5.2)] = "PPC.Constipation.5.2"
			if (!exists("PPC.Constipation.2")) { PPC.Constipation.2 = ppc(observations, "Constipation", observations$Time2, 2) 
				Constipation.M2[[length(Constipation.M2) + 1]] = PPC.Constipation.2
				names(Constipation.M2)[length(Constipation.M2)] = "PPC.Constipation.2"
			}
			if (sum(PPC.Constipation.5.2$Summary) > sum(PPC.Constipation.2$Summary)) {
				BM = 5.2
				Constipation.M2c = evaluate(observations.car, "Constipation", observations$Time2, 2, No_Neuropain)
				Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M2c
				names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M2c"
				Cur.DIC = Constipation.M2c$DIC
			}
		}
	} else if (BM == 3) {
		Constipation.M5.3 = evaluate(observations, "Constipation", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
		Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M5.3
		names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M5.3"
		if ((Constipation.M5.3$Sigs[1, ncol(Constipation.M5.3$Sigs)-1] == TRUE || Constipation.M5.3$Sigs[1, ncol(Constipation.M5.3$Sigs)] == TRUE) && 
			Constipation.M5.3$DIC < Constipation.M3$DIC - 3) {
			BM = 5.3
			Constipation.M3c = evaluate(observations.car, "Constipation", Block.Covs, 3, No_Neuropain)
			Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M3c
			names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M3c"
			Cur.DIC = Constipation.M3c$DIC
		} else if ((Constipation.M5.3$Sigs[1, ncol(Constipation.M5.3$Sigs)-1] == FALSE || Constipation.M5.3$Sigs[1, ncol(Constipation.M5.3$Sigs)] == FALSE) && 
			Constipation.M5.3$DIC >= Constipation.M3$DIC - 3) { BM = BM		
		} else {
			PPC.Constipation.5.3 = ppc(observations, "Constipation", Constipation.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
			Constipation.M5.3[[length(Constipation.M5.3) + 1]] = PPC.Constipation.5.3
			names(Constipation.M5.3)[length(Constipation.M5.3)] = "PPC.Constipation.5.3"
			if (!exists("PPC.Constipation.3")) { PPC.Constipation.3 = ppc(observations, "Constipation", Block.Covs, 3) 
				Constipation.M3[[length(Constipation.M3) + 1]] = PPC.Constipation.3
				names(Constipation.M3)[length(Constipation.M3)] = "PPC.Constipation.3"
			}
			if (sum(PPC.Constipation.5.3$Summary) > sum(PPC.Constipation.3$Summary)) {
				BM = 5.3
				Constipation.M3c = evaluate(observations.car, "Constipation", Block.Covs, 3, No_Neuropain)
				Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M3c
				names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M3c"
				Cur.DIC = Constipation.M3c$DIC
			}
		}
	} else if (BM == 4) {
		Constipation.M5.4 = evaluate(observations, "Constipation", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
		Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M5.4
		names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M5.4"
		if ((Constipation.M5.4$Sigs[1, ncol(Constipation.M5.4$Sigs)-1] == TRUE || Constipation.M5.4$Sigs[1, ncol(Constipation.M5.4$Sigs)] == TRUE) && 
			Constipation.M5.4$DIC < Constipation.M4$DIC - 3) {
			BM = 5.4
			Constipation.M4c = evaluate(observations.car, "Constipation", Lag.Covs, 4, No_Neuropain)
			Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M4c
			names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M4c"
			Cur.DIC = Constipation.M4c$DIC
		} else if ((Constipation.M5.4$Sigs[1, ncol(Constipation.M5.4$Sigs)-1] == FALSE && Constipation.M5.4$Sigs[1, ncol(Constipation.M5.4$Sigs)] == FALSE) && 
			Constipation.M5.4$DIC >= Constipation.M4$DIC - 3) { BM = BM		
		} else {
			PPC.Constipation.5.4 = ppc(observations, "Constipation", Constipation.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
			Constipation.M5.4[[length(Constipation.M5.4) + 1]] = PPC.Constipation.5.4
			names(Constipation.M5.4)[length(Constipation.M5.4)] = "PPC.Constipation.5.4"
			if (!exists("PPC.Constipation.4")) { PPC.Constipation.4 = ppc(observations, "Constipation", Lag.Covs[ , 6], 4) 
				Constipation.M4[[length(Constipation.M4) + 1]] = PPC.Constipation.4
				names(Constipation.M4)[length(Constipation.M4)] = "PPC.Constipation.4"
			}
			if (sum(PPC.Constipation.5.4$Summary) > sum(PPC.Constipation.4$Summary)) {
				BM = 5.4
				Constipation.M4c = evaluate(observations.car, "Constipation", Lag.Covs, 4, No_Neuropain)
				Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M4c
				names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M4c"
				Cur.DIC = Constipation.M4c$DIC
			}
		}
	} else if (BM == 4.1) {
		Constipation.M5.41 = evaluate(observations, "Constipation", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
			5.41, No_Neuropain)
		Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M5.41
		names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M5.41"
		if ((Constipation.M5.41$Sigs[1, ncol(Constipation.M5.41$Sigs)-1] == TRUE || Constipation.M5.41$Sigs[1, ncol(Constipation.M5.41$Sigs)] == TRUE) && 
			Constipation.M5.41$DIC < Constipation.M4.1$DIC - 3) {
			BM = 5.41
			Constipation.M4.1c = evaluate(observations.car, "Constipation", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
			Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M4.1c
			names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M4.1c"
			Cur.DIC = Constipation.M4.1c$DIC
		} else if ((Constipation.M5.41$Sigs[1, ncol(Constipation.M5.41$Sigs)-1] == FALSE && Constipation.M5.41$Sigs[1, ncol(Constipation.M5.41$Sigs)] == FALSE) && 
			Constipation.M5.41$DIC >= Constipation.M4.1$DIC - 3) { BM = BM		
		} else {
			PPC.Constipation.5.41 = ppc(observations, "Constipation", Constipation.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
				observations$car.B), 5.41)
			Constipation.M5.41[[length(Constipation.M5.41) + 1]] = PPC.Constipation.5.41
			names(Constipation.M5.41)[length(Constipation.M5.41)] = "PPC.Constipation.5.41"
			if (!exists("PPC.Constipation.4.1")) { PPC.Constipation.4.1 = ppc(observations, "Constipation", cbind(Lag.Covs[ , 6], observations$Time2), 4.1) 
				Constipation.M4.1[[length(Constipation.M4.1) + 1]] = PPC.Constipation.4.1
				names(Constipation.M4.1)[length(Constipation.M4.1)] = "PPC.Constipation.4.1"
			}
			if (sum(PPC.Constipation.5.41$Summary) > sum(PPC.Constipation.4.1$Summary)) {
				BM = 5.41
				Constipation.M4.1c = evaluate(observations.car, "Constipation", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M4.1c
				names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M4.1c"
				Cur.DIC = Constipation.M4.1c$DIC
			}
		}
	} else if (BM == 4.2) {
		Constipation.M5.42 = evaluate(observations, "Constipation", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
			5.42, No_Neuropain)
		Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M5.42
		names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M5.42"
		if ((Constipation.M5.42$Sigs[1, ncol(Constipation.M5.42$Sigs)-1] == TRUE || Constipation.M5.42$Sigs[1, ncol(Constipation.M5.42$Sigs)] == TRUE) && 
			Constipation.M5.42$DIC < Constipation.M4.2$DIC - 3) {
			BM = 5.42
			Constipation.M4.2c = evaluate(observations.car, "Constipation", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
			Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M4.2c
			names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M4.2c"
			Cur.DIC = Constipation.M4.2c$DIC
		} else if ((Constipation.M5.42$Sigs[1, ncol(Constipation.M5.42$Sigs)-1] == FALSE && Constipation.M5.42$Sigs[1, ncol(Constipation.M5.42$Sigs)] == FALSE) && 
			Constipation.M5.42$DIC >= Constipation.M4.2$DIC - 3) { BM = BM		
		} else {
			PPC.Constipation.5.42 = ppc(observations, "Constipation", Constipation.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
				observations$car.B), 5.42)
			Constipation.M5.42[[length(Constipation.M5.42) + 1]] = PPC.Constipation.5.42
			names(Constipation.M5.42)[length(Constipation.M5.42)] = "PPC.Constipation.5.42"
			if (!exists("PPC.Constipation.4.2")) { PPC.Constipation.4.2 = ppc(observations, "Constipation", cbind(Lag.Covs[ , 6], Block.Covs), 4.2) 
				Constipation.M4.2[[length(Constipation.M4.2) + 1]] = PPC.Constipation.4.2
				names(Constipation.M4.2)[length(Constipation.M4.2)] = "PPC.Constipation.4.2"
			}
			if (sum(PPC.Constipation.5.42$Summary) > sum(PPC.Constipation.4.2$Summary)) {
				BM = 5.42
				Constipation.M4.2c = evaluate(observations.car, "Constipation", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Constipation.Info[[length(Constipation.Info) + 1]] = Constipation.M4.2c
				names(Constipation.Info)[length(Constipation.Info)] = "Constipation.M4.2c"
				Cur.DIC = Constipation.M4.2c$DIC
			}
		}
	}
	
	Best.Model = cbind(Best.Model, BM)
	
	########################
	# Analysis for Neuropain
	########################
	
	if (!No_Neuropain) {
		Neuropain.M1 = evaluate(observations, Outcome = "Neuropain", Covs = NULL, mod.id = 1, No_Neuropain)
		Neuropain.M2 = evaluate(observations, Outcome = "Neuropain", Covs = observations$Time2, mod.id = 2, No_Neuropain)
		Neuropain.M3 = evaluate(observations, Outcome = "Neuropain", Covs = Block.Covs, mod.id = 3, No_Neuropain)
		Neuropain.M4 = evaluate(observations, Outcome = "Neuropain", Covs = Lag.Covs, mod.id = 4, No_Neuropain)
		Neuropain.M4.1 = evaluate(observations, Outcome = "Neuropain", Covs = cbind(Lag.Covs, observations$Time2), mod.id = 4.1, No_Neuropain)
		Neuropain.M4.2 = evaluate(observations, Outcome = "Neuropain", Covs = cbind(Lag.Covs, Block.Covs), mod.id = 4.2, No_Neuropain)
		
		Neuropain.Info = list(Neuropain.M1, Neuropain.M2, Neuropain.M3, Neuropain.M4, Neuropain.M4.1, Neuropain.M4.2)
		names(Neuropain.Info) = c("Neuropain.M1", "Neuropain.M2", "Neuropain.M3", "Neuropain.M4", "Neuropain.M4.1", "Neuropain.M4.2")
		
		BM = 1
		Cur.DIC = Neuropain.M1$DIC
		
		if (Neuropain.M2$Sigs[2] == TRUE && Neuropain.M2$DIC < Neuropain.M1$DIC - 3) { 
			BM = 2
			Cur.DIC = Neuropain.M2$DIC
		} else if (Neuropain.M2$Sigs[2] == FALSE && Neuropain.M2$DIC >= Neuropain.M1$DIC - 3) { BM = 1
		} else {
			PPC.Neuropain.1 = ppc(observations, "Neuropain", Neuropain.M1$ForPPC, Covs = NULL, 1)
			Neuropain.M1[[length(Neuropain.M1) + 1]] = PPC.Neuropain.1
			names(Neuropain.M1)[length(Neuropain.M1)] = "PPC.Neuropain.1"
			PPC.Neuropain.2 = ppc(observations, "Neuropain", Neuropain.M2$ForPPC, observations$Time2, 2)
			Neuropain.M2[[length(Neuropain.M2) + 1]] = PPC.Neuropain.2
			names(Neuropain.M2)[length(Neuropain.M2)] = "PPC.Neuropain.2"
			if (sum(PPC.Neuropain.2$Summary) > sum(PPC.Neuropain.1$Summary)) { 
				BM = 2 
				Cur.DIC = Neuropain.M2$DIC
			}
		}
		
		if (sum(Neuropain.M3$Sigs[ , 2:ncol(Neuropain.M3$Sigs)]) > 0 && Neuropain.M3$DIC < Cur.DIC - 3) {
			BM = 3
			Cur.DIC = Neuropain.M3$DIC
		} else if (sum(Neuropain.M3$Sigs[ , 2:ncol(Neuropain.M3$Sigs)]) == 0 && Neuropain.M3$DIC >= Cur.DIC - 3) { BM = BM
		} else {
			PPC.Neuropain.3 = ppc(observations, "Neuropain", Neuropain.M3$ForPPC, Block.Covs, 3)
			Neuropain.M3[[length(Neuropain.M3) + 1]] = PPC.Neuropain.3
			names(Neuropain.M3)[length(Neuropain.M3)] = "PPC.Neuropain.3"
			if (BM == 1 && !exists("PPC.Neuropain.1")) { Cur.PPC = ppc(observations, "Neuropain", Neuropain.M1$ForPPC, Covs = NULL, 1) 
				Neuropain.M1[[length(Neuropain.M1) + 1]] = Cur.PPC
			names(Neuropain.M1)[length(Neuropain.M1)] = "PPC.Neuropain.1"
			} else if (BM == 1 && exists("PPC.Neuropain.1")) { Cur.PPC = PPC.Neuropain.1 }
			
			if (BM == 2 && !exists("PPC.Neuropain.2")) {
				Cur.PPC = ppc(observations, "Neuropain", Neuropain.M2$ForPPC, observations$Time2, 2)
				Neuropain.M2[[length(Neuropain.M2) + 1]] = Cur.PPC
			names(Neuropain.M2)[length(Neuropain.M2)] = "PPC.Neuropain.2"
			} else if (BM == 2 && exists("PPC.Neuropain.2")) { Cur.PPC = PPC.Neuropain.2 }
			
			if (sum(PPC.Neuropain.3$Summary) > sum(Cur.PPC$Summary)) {
				BM = 3
				Cur.DIC = Neuropain.M3$DIC
			}
		}
		
		if (BM == 1) {
			if (Neuropain.M4$Sigs[1, 2] == TRUE && Neuropain.M4$DIC < Cur.DIC - 3) {
				BM = 4
				Cur.DIC = Neuropain.M4$DIC
			} else if (Neuropain.M4$Sigs[1, 2] == FALSE && Neuropain.M4$DIC >= Cur.DIC - 3) { BM = BM
			} else {
				PPC.Neuropain.4 = ppc(observations, "Neuropain", Neuropain.M4$ForPPC, Lag.Covs[ , 7], 4)
				Neuropain.M4[[length(Neuropain.M4) + 1]] = PPC.Neuropain.4
				names(Neuropain.M4)[length(Neuropain.M4)] = "PPC.Neuropain.4"
				if (!exists("PPC.Neuropain.1")) { PPC.Neuropain.1 = ppc(observations, "Neuropain", Covs = NULL, 1) 
					Neuropain.M1[[length(Neuropain.M1) + 1]] = PPC.Neuropain.1
					names(Neuropain.M1)[length(Neuropain.M1)] = "PPC.Neuropain.1"
				}
				if (sum(PPC.Neuropain.4$Summary) > sum(PPC.Neuropain.1$Summary)) {
					BM = 4
					Cur.DIC = Neuropain.M4$DIC
				}
			}
		} else if (BM == 2) {
			if (Neuropain.M4.1$Sigs[1, 2] == TRUE && Neuropain.M4.1$DIC < Cur.DIC - 3) {
				BM = 4.1
				Cur.DIC = Neuropain.M4.1$DIC
			} else if (Neuropain.M4.1$Sigs[1, 2] == FALSE && Neuropain.M4.1$DIC >= Cur.DIC - 3) { BM = BM
			} else {
				PPC.Neuropain.4.1 = ppc(observations, "Neuropain", Neuropain.M4.1$ForPPC, cbind(Lag.Covs[ , 7], observations$Time2), 4.1)
				Neuropain.M1[[length(Neuropain.M4.1) + 1]] = PPC.Neuropain.4.1
				names(Neuropain.M4.1)[length(Neuropain.M4.1)] = "PPC.Neuropain.4.1"
				if (!exists("PPC.Neuropain.2")) { PPC.Neuropain.2 = ppc(observations, "Neuropain", observations$Time2, 2) 
					Neuropain.M2[[length(Neuropain.M2) + 1]] = PPC.Neuropain.2
					names(Neuropain.M2)[length(Neuropain.M2)] = "PPC.Neuropain.2"
				}
				if (sum(PPC.Neuropain.4.1$Summary) > sum(PPC.Neuropain.2$Summary)) {
					BM = 4.1
					Cur.DIC = Neuropain.M4.1$DIC
				}
			}
		} else if (BM == 3) {
			if (Neuropain.M4.2$Sigs[1, 2] == TRUE && Neuropain.M4.2$DIC < Cur.DIC - 3) {
				BM = 4.2
				Cur.DIC = Neuropain.M4.2$DIC
			} else if (Neuropain.M4.2$Sigs[1, 2] == FALSE && Neuropain.M4.2$DIC >= Cur.DIC - 3) { BM = BM
			} else {
				PPC.Neuropain.4.2 = ppc(observations, "Neuropain", Neuropain.M4.2$ForPPC, cbind(Lag.Covs[ , 7], Block.Covs), 4.2)
				Neuropain.M4.2[[length(Neuropain.M1) + 1]] = PPC.Neuropain.4.2
			names(Neuropain.M4.2)[length(Neuropain.M4.2)] = "PPC.Neuropain.4.2"
				if (!exists("PPC.Neuropain.3")) { PPC.Neuropain.3 = ppc(observations, "Neuropain", Block.Covs, 3) 
					Neuropain.M3[[length(Neuropain.M3) + 1]] = PPC.Neuropain.3
					names(Neuropain.M3)[length(Neuropain.M3)] = "PPC.Neuropain.3"
				}
				if (sum(PPC.Neuropain.4.2$Summary) > sum(PPC.Neuropain.3$Summary)) {
					BM = 4.2
					Cur.DIC = Neuropain.M4.2$DIC
				}
			}
		}
		
		if (BM == 1) {
			Neuropain.M5.1 = evaluate(observations, "Neuropain", cbind(observations$car.A, observations$car.B), 5.1, No_Neuropain)
			Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M5.1
			names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M5.1"
			if ((Neuropain.M5.1$Sigs[1, 2] == TRUE || Neuropain.M5.1$Sigs[1, 3] == TRUE) && Neuropain.M5.1$DIC < Neuropain.M1$DIC - 3) {
				BM = 5.1
				Neuropain.M1c = evaluate(observations.car, "Neuropain", Covs = NULL, 1, No_Neuropain)
				Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M1c
				names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M1c"
				Cur.DIC = Neuropain.M1c$DIC
			} else if ((Neuropain.M5.1$Sigs[1, 2] == FALSE || Neuropain.M5.1$Sigs[1, 3] == FALSE) && Neuropain.M5.1$DIC >= Neuropain.M1$DIC - 3) { BM = BM		
			} else {
				PPC.Neuropain.5.1 = ppc(observations, "Neuropain", Neuropain.M5.1$ForPPC, cbind(observations$car.A, observations$car.B), 5.1)
				Neuropain.M5.1[[length(Neuropain.M5.1) + 1]] = PPC.Neuropain.5.1
				names(Neuropain.M5.1)[length(Neuropain.M5.1)] = "PPC.Neuropain.5.1"
				if (!exists("PPC.Neuropain.1")) { PPC.Neuropain.1 = ppc(observations, "Neuropain", Covs = NULL, 1) 
					Neuropain.M1[[length(Neuropain.M1) + 1]] = PPC.Neuropain.1
					names(Neuropain.M1)[length(Neuropain.M1)] = "PPC.Neuropain.1"
				}
				if (sum(PPC.Neuropain.5.1$Summary) > sum(PPC.Neuropain.1$Summary)) {
					BM = 5.1
					Neuropain.M1c = evaluate(observations.car, "Neuropain", Covs = NULL, 1, No_Neuropain)
					Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M1c
					names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M1c"
					Cur.DIC = Neuropain.M1c$DIC
				}
			}
		} else if (BM == 2) {
			Neuropain.M5.2 = evaluate(observations, "Neuropain", cbind(observations$Time2, observations$car.A, observations$car.B), 5.2, No_Neuropain)
			Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M5.2
			names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M5.2"
			if ((Neuropain.M5.2$Sigs[1, 3] == TRUE || Neuropain.M5.2$Sigs[1, 4] == TRUE) && Neuropain.M5.2$DIC < Neuropain.M2$DIC - 3) {
				BM = 5.2
				Neuropain.M2c = evaluate(observations.car, "Neuropain", observations$Time2, 2, No_Neuropain)
				Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M2c
				names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M2c"
				Cur.DIC = Neuropain.M2c$DIC
			} else if ((Neuropain.M5.2$Sigs[1, 3] == FALSE || Neuropain.M5.2$Sigs[1, 4] == FALSE) && Neuropain.M5.2$DIC >= Neuropain.M2$DIC - 3) { BM = BM		
			} else {
				PPC.Neuropain.5.2 = ppc(observations, "Neuropain", Neuropain.M5.2$ForPPC, cbind(observations$Time2, observations$car.A, observations$car.B), 5.2)
				Neuropain.M5.2[[length(Neuropain.M5.2) + 1]] = PPC.Neuropain.5.2
				names(Neuropain.M5.2)[length(Neuropain.M5.2)] = "PPC.Neuropain.5.2"
				if (!exists("PPC.Neuropain.2")) { PPC.Neuropain.2 = ppc(observations, "Neuropain", observations$Time2, 2) 
					Neuropain.M2[[length(Neuropain.M2) + 1]] = PPC.Neuropain.2
					names(Neuropain.M2)[length(Neuropain.M2)] = "PPC.Neuropain.2"
				}
				if (sum(PPC.Neuropain.5.2$Summary) > sum(PPC.Neuropain.2$Summary)) {
					BM = 5.2
					Neuropain.M2c = evaluate(observations.car, "Neuropain", observations$Time2, 2, No_Neuropain)
					Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M2c
					names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M2c"
					Cur.DIC = Neuropain.M2c$DIC
				}
			}
		} else if (BM == 3) {
			Neuropain.M5.3 = evaluate(observations, "Neuropain", cbind(Block.Covs, observations$car.A, observations$car.B), 5.3, No_Neuropain)
			Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M5.3
			names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M5.3"
			if ((Neuropain.M5.3$Sigs[1, ncol(Neuropain.M5.3$Sigs)-1] == TRUE || Neuropain.M5.3$Sigs[1, ncol(Neuropain.M5.3$Sigs)] == TRUE) && 
				Neuropain.M5.3$DIC < Neuropain.M3$DIC - 3) {
				BM = 5.3
				Neuropain.M3c = evaluate(observations.car, "Neuropain", Block.Covs, 3, No_Neuropain)
				Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M3c
				names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M3c"
				Cur.DIC = Neuropain.M3c$DIC
			} else if ((Neuropain.M5.3$Sigs[1, ncol(Neuropain.M5.3$Sigs)-1] == FALSE || Neuropain.M5.3$Sigs[1, ncol(Neuropain.M5.3$Sigs)] == FALSE) && 
				Neuropain.M5.3$DIC >= Neuropain.M3$DIC - 3) { BM = BM		
			} else {
				PPC.Neuropain.5.3 = ppc(observations, "Neuropain", Neuropain.M5.3$ForPPC, cbind(Block.Covs, observations$car.A, observations$car.B), 5.3)
				Neuropain.M5.3[[length(Neuropain.M5.3) + 1]] = PPC.Neuropain.5.3
				names(Neuropain.M5.3)[length(Neuropain.M5.3)] = "PPC.Neuropain.5.3"
				if (!exists("PPC.Neuropain.3")) { PPC.Neuropain.3 = ppc(observations, "Neuropain", Block.Covs, 3) 
					Neuropain.M3[[length(Neuropain.M3) + 1]] = PPC.Neuropain.3
					names(Neuropain.M3)[length(Neuropain.M3)] = "PPC.Neuropain.3"
				}
				if (sum(PPC.Neuropain.5.3$Summary) > sum(PPC.Neuropain.3$Summary)) {
					BM = 5.3
					Neuropain.M3c = evaluate(observations.car, "Neuropain", Block.Covs, 3, No_Neuropain)
					Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M3c
					names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M3c"
					Cur.DIC = Neuropain.M3c$DIC
				}
			}
		} else if (BM == 4) {
			Neuropain.M5.4 = evaluate(observations, "Neuropain", cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4, No_Neuropain)
			Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M5.4
			names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M5.4"
			if ((Neuropain.M5.4$Sigs[1, ncol(Neuropain.M5.4$Sigs)-1] == TRUE || Neuropain.M5.4$Sigs[1, ncol(Neuropain.M5.4$Sigs)] == TRUE) && 
				Neuropain.M5.4$DIC < Neuropain.M4$DIC - 3) {
				BM = 5.4
				Neuropain.M4c = evaluate(observations.car, "Neuropain", Lag.Covs, 4, No_Neuropain)
				Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M4c
				names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M4c"
				Cur.DIC = Neuropain.M4c$DIC
			} else if ((Neuropain.M5.4$Sigs[1, ncol(Neuropain.M5.4$Sigs)-1] == FALSE && Neuropain.M5.4$Sigs[1, ncol(Neuropain.M5.4$Sigs)] == FALSE) && 
				Neuropain.M5.4$DIC >= Neuropain.M4$DIC - 3) { BM = BM		
			} else {
				PPC.Neuropain.5.4 = ppc(observations, "Neuropain", Neuropain.M5.4$ForPPC, cbind(Lag.Covs, observations$car.A, observations$car.B), 5.4)
				Neuropain.M5.4[[length(Neuropain.M5.4) + 1]] = PPC.Neuropain.5.4
				names(Neuropain.M5.4)[length(Neuropain.M5.4)] = "PPC.Neuropain.5.4"
				if (!exists("PPC.Neuropain.4")) { PPC.Neuropain.4 = ppc(observations, "Neuropain", Lag.Covs[ , 7], 4) 
					Neuropain.M4[[length(Neuropain.M4) + 1]] = PPC.Neuropain.4
					names(Neuropain.M4)[length(Neuropain.M4)] = "PPC.Neuropain.4"
				}
				if (sum(PPC.Neuropain.5.4$Summary) > sum(PPC.Neuropain.4$Summary)) {
					BM = 5.4
					Neuropain.M4c = evaluate(observations.car, "Neuropain", Lag.Covs, 4, No_Neuropain)
					Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M4c
					names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M4c"
					Cur.DIC = Neuropain.M4c$DIC
				}
			}
		} else if (BM == 4.1) {
			Neuropain.M5.41 = evaluate(observations, "Neuropain", cbind(Lag.Covs, observations$Time2, observations$car.A, observations$car.B), 
				5.41, No_Neuropain)
			Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M5.41
			names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M5.41"
			if ((Neuropain.M5.41$Sigs[1, ncol(Neuropain.M5.41$Sigs)-1] == TRUE || Neuropain.M5.41$Sigs[1, ncol(Neuropain.M5.41$Sigs)] == TRUE) && 
				Neuropain.M5.41$DIC < Neuropain.M4.1$DIC - 3) {
				BM = 5.41
				Neuropain.M4.1c = evaluate(observations.car, "Neuropain", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
				Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M4.1c
				names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M4.1c"
				Cur.DIC = Neuropain.M4.1c$DIC
			} else if ((Neuropain.M5.41$Sigs[1, ncol(Neuropain.M5.41$Sigs)-1] == FALSE && Neuropain.M5.41$Sigs[1, ncol(Neuropain.M5.41$Sigs)] == FALSE) && 
				Neuropain.M5.41$DIC >= Neuropain.M4.1$DIC - 3) { BM = BM		
			} else {
				PPC.Neuropain.5.41 = ppc(observations, "Neuropain", Neuropain.M5.41$ForPPC, cbind(Lag.Covs, observations$Time2, observations$car.A, 
					observations$car.B), 5.41)
				Neuropain.M5.41[[length(Neuropain.M5.41) + 1]] = PPC.Neuropain.5.41
				names(Neuropain.M5.41)[length(Neuropain.M5.41)] = "PPC.Neuropain.5.41"
				if (!exists("PPC.Neuropain.4.1")) { PPC.Neuropain.4.1 = ppc(observations, "Neuropain", cbind(Lag.Covs[ , 7], observations$Time2), 4.1) 
					Neuropain.M4.1[[length(Neuropain.M4.1) + 1]] = PPC.Neuropain.4.1
					names(Neuropain.M4.1)[length(Neuropain.M4.1)] = "PPC.Neuropain.4.1"
				}
				if (sum(PPC.Neuropain.5.41$Summary) > sum(PPC.Neuropain.4.1$Summary)) {
					BM = 5.41
					Neuropain.M4.1c = evaluate(observations.car, "Neuropain", cbind(Lag.Covs, observations$Time2), 4.1, No_Neuropain)
					Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M4.1c
					names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M4.1c"
					Cur.DIC = Neuropain.M4.1c$DIC
				}
			}
		} else if (BM == 4.2) {
			Neuropain.M5.42 = evaluate(observations, "Neuropain", cbind(Lag.Covs, Block.Covs, observations$car.A, observations$car.B), 
				5.42, No_Neuropain)
			Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M5.42
			names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M5.42"
			if ((Neuropain.M5.42$Sigs[1, ncol(Neuropain.M5.42$Sigs)-1] == TRUE || Neuropain.M5.42$Sigs[1, ncol(Neuropain.M5.42$Sigs)] == TRUE) && 
				Neuropain.M5.42$DIC < Neuropain.M4.2$DIC - 3) {
				BM = 5.42
				Neuropain.M4.2c = evaluate(observations.car, "Neuropain", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
				Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M4.2c
				names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M4.2c"
				Cur.DIC = Neuropain.M4.2c$DIC
			} else if ((Neuropain.M5.42$Sigs[1, ncol(Neuropain.M5.42$Sigs)-1] == FALSE && Neuropain.M5.42$Sigs[1, ncol(Neuropain.M5.42$Sigs)] == FALSE) && 
				Neuropain.M5.42$DIC >= Neuropain.M4.2$DIC - 3) { BM = BM		
			} else {
				PPC.Neuropain.5.42 = ppc(observations, "Neuropain", Neuropain.M5.42$ForPPC, cbind(Lag.Covs, Block.Covs, observations$car.A, 
					observations$car.B), 5.42)
				Neuropain.M5.42[[length(Neuropain.M5.42) + 1]] = PPC.Neuropain.5.42
				names(Neuropain.M5.42)[length(Neuropain.M5.42)] = "PPC.Neuropain.5.42"
				if (!exists("PPC.Neuropain.4.2")) { PPC.Neuropain.4.2 = ppc(observations, "Neuropain", cbind(Lag.Covs[ , 7], Block.Covs), 4.2) 
					Neuropain.M4.2[[length(Neuropain.M4.2) + 1]] = PPC.Neuropain.4.2
					names(Neuropain.M4.2)[length(Neuropain.M4.2)] = "PPC.Neuropain.4.2"
				}
				if (sum(PPC.Neuropain.5.42$Summary) > sum(PPC.Neuropain.4.2$Summary)) {
					BM = 5.42
					Neuropain.M4.2c = evaluate(observations.car, "Neuropain", cbind(Lag.Covs, Block.Covs), 4.2, No_Neuropain)
					Neuropain.Info[[length(Neuropain.Info) + 1]] = Neuropain.M4.2c
					names(Neuropain.Info)[length(Neuropain.Info)] = "Neuropain.M4.2c"
					Cur.DIC = Neuropain.M4.2c$DIC
				}
			}
		}
		
		Best.Model = cbind(Best.Model, BM)
	}
	
	##################
	# Compiling Output
	##################
	
	if (ncol(Best.Model) == 6) {
		colnames(Best.Model) = c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation")
	} else if (ncol(Best.Model) == 7) {
		colnames(Best.Model) = c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")
	}
	
	graph5_names = c("lower_bound", "lower_bound_regimen", "median_effect", "more_effective_regimen", 
		"upper_bound", "upper_bound_regimen")
	graph6_names = c("P(< -0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
	
	if (Best.Model[1] == 1) { 
		Results = Pain.M1$Results 
		graph5 = list(abs(Pain.M1$Results[1]), as.numeric(Pain.M1$Results[1] < 0), abs(Pain.M1$Results[2]), 
			as.numeric(Pain.M1$Results[2] < 0), abs(Pain.M1$Results[3]), as.numeric(Pain.M1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M1$Results[4], Pain.M1$Results[5], Pain.M1$Results[6], Pain.M1$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M1$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 2) { 
		Results = Pain.M2$Results
		graph5 = list(abs(Pain.M2$Results[1]), as.numeric(Pain.M2$Results[1] < 0), abs(Pain.M2$Results[2]), 
			as.numeric(Pain.M2$Results[2] < 0), abs(Pain.M2$Results[3]), as.numeric(Pain.M2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M2$Results[4], Pain.M2$Results[5], Pain.M2$Results[6], Pain.M2$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M2$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 3) { 
		Results = Pain.M3$Results
		graph5 = list(abs(Pain.M3$Results[1]), as.numeric(Pain.M3$Results[1] < 0), abs(Pain.M3$Results[2]), 
			as.numeric(Pain.M3$Results[2] < 0), abs(Pain.M3$Results[3]), as.numeric(Pain.M3$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M3$Results[4], Pain.M3$Results[5], Pain.M3$Results[6], Pain.M3$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M3$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 4) { 
		Results = Pain.M4$Results
		graph5 = list(abs(Pain.M4$Results[1]), as.numeric(Pain.M4$Results[1] < 0), abs(Pain.M4$Results[2]), 
			as.numeric(Pain.M4$Results[2] < 0), abs(Pain.M4$Results[3]), as.numeric(Pain.M4$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M4$Results[4], Pain.M4$Results[5], Pain.M4$Results[6], Pain.M4$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M4$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 4.1) { 
		Results = Pain.M4.1$Results
		graph5 = list(abs(Pain.M4.1$Results[1]), as.numeric(Pain.M4.1$Results[1] < 0), abs(Pain.M4.1$Results[2]), 
			as.numeric(Pain.M4.1$Results[2] < 0), abs(Pain.M4.1$Results[3]), as.numeric(Pain.M4.1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M4.1$Results[4], Pain.M4.1$Results[5], Pain.M4.1$Results[6], Pain.M4.1$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M4.1$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 4.2) { 
		Results = Pain.M4.2$Results
		graph5 = list(abs(Pain.M4.2$Results[1]), as.numeric(Pain.M4.2$Results[1] < 0), abs(Pain.M4.2$Results[2]), 
			as.numeric(Pain.M4.2$Results[2] < 0), abs(Pain.M4.2$Results[3]), as.numeric(Pain.M4.2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M4.2$Results[4], Pain.M4.2$Results[5], Pain.M4.2$Results[6], Pain.M4.2$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M4.2$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 5.1) { 
		Results = Pain.M1c$Results
		graph5 = list(abs(Pain.M1c$Results[1]), as.numeric(Pain.M1c$Results[1] < 0), abs(Pain.M1c$Results[2]), 
			as.numeric(Pain.M1c$Results[2] < 0), abs(Pain.M1c$Results[3]), as.numeric(Pain.M1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M1c$Results[4], Pain.M1c$Results[5], Pain.M1c$Results[6], Pain.M1c$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M1c$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 5.2) { 
		Results = Pain.M2c$Results
		graph5 = list(abs(Pain.M2c$Results[1]), as.numeric(Pain.M2c$Results[1] < 0), abs(Pain.M2c$Results[2]), 
			as.numeric(Pain.M2c$Results[2] < 0), abs(Pain.M2c$Results[3]), as.numeric(Pain.M2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M2c$Results[4], Pain.M2c$Results[5], Pain.M2c$Results[6], Pain.M2c$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M2c$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 5.3) { 
		Results = Pain.M3c$Results
		graph5 = list(abs(Pain.M3c$Results[1]), as.numeric(Pain.M3c$Results[1] < 0), abs(Pain.M3c$Results[2]), 
			as.numeric(Pain.M3c$Results[2] < 0), abs(Pain.M3c$Results[3]), as.numeric(Pain.M3c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M3c$Results[4], Pain.M3c$Results[5], Pain.M3c$Results[6], Pain.M3c$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M3c$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 5.4) { 
		Results = Pain.M4c$Results
		graph5 = list(abs(Pain.M4c$Results[1]), as.numeric(Pain.M4c$Results[1] < 0), abs(Pain.M4c$Results[2]), 
			as.numeric(Pain.M4c$Results[2] < 0), abs(Pain.M4c$Results[3]), as.numeric(Pain.M4c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M4c$Results[4], Pain.M4c$Results[5], Pain.M4c$Results[6], Pain.M4c$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M4c$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 5.41) { 
		Results = Pain.M4.1c$Results
		graph5 = list(abs(Pain.M4.1c$Results[1]), as.numeric(Pain.M4.1c$Results[1] < 0), abs(Pain.M4.1c$Results[2]), 
			as.numeric(Pain.M4.1c$Results[2] < 0), abs(Pain.M4.1c$Results[3]), as.numeric(Pain.M4.1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M4.1c$Results[4], Pain.M4.1c$Results[5], Pain.M4.1c$Results[6], Pain.M4.1c$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M4.1c$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[1] == 5.42) { 
		Results = Pain.M4.2c$Results
		graph5 = list(abs(Pain.M4.2c$Results[1]), as.numeric(Pain.M4.2c$Results[1] < 0), abs(Pain.M4.2c$Results[2]), 
			as.numeric(Pain.M4.2c$Results[2] < 0), abs(Pain.M4.2c$Results[3]), as.numeric(Pain.M4.2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Pain.M4.2c$Results[4], Pain.M4.2c$Results[5], Pain.M4.2c$Results[6], Pain.M4.2c$Results[7])
		names(graph6) = graph6_names
		pain = list(as.logical(1 - Pain.M4.2c$urun), graph5, graph6)
		names(pain) = c("successful_run", "graph_5", "graph_6")
	}
	
	if (Best.Model[2] == 1) { 
		Results = rbind(Results, Fatigue.M1$Results) 
		graph5 = list(abs(Fatigue.M1$Results[1]), as.numeric(Fatigue.M1$Results[1] < 0), abs(Fatigue.M1$Results[2]), 
			as.numeric(Fatigue.M1$Results[2] < 0), abs(Fatigue.M1$Results[3]), as.numeric(Fatigue.M1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M1$Results[4], Fatigue.M1$Results[5], Fatigue.M1$Results[6], Fatigue.M1$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M1$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 2) { 
		Results = rbind(Results, Fatigue.M2$Results)
		graph5 = list(abs(Fatigue.M2$Results[1]), as.numeric(Fatigue.M2$Results[1] < 0), abs(Fatigue.M2$Results[2]), 
			as.numeric(Fatigue.M2$Results[2] < 0), abs(Fatigue.M2$Results[3]), as.numeric(Fatigue.M2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M2$Results[4], Fatigue.M2$Results[5], Fatigue.M2$Results[6], Fatigue.M2$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M2$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 3) { 
		Results = rbind(Results, Fatigue.M3$Results)
		graph5 = list(abs(Fatigue.M3$Results[1]), as.numeric(Fatigue.M3$Results[1] < 0), abs(Fatigue.M3$Results[2]), 
			as.numeric(Fatigue.M3$Results[2] < 0), abs(Fatigue.M3$Results[3]), as.numeric(Fatigue.M3$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M3$Results[4], Fatigue.M3$Results[5], Fatigue.M3$Results[6], Fatigue.M3$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M3$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 4) { 
		Results = rbind(Results, Fatigue.M4$Results)
		graph5 = list(abs(Fatigue.M4$Results[1]), as.numeric(Fatigue.M4$Results[1] < 0), abs(Fatigue.M4$Results[2]), 
			as.numeric(Fatigue.M4$Results[2] < 0), abs(Fatigue.M4$Results[3]), as.numeric(Fatigue.M4$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M4$Results[4], Fatigue.M4$Results[5], Fatigue.M4$Results[6], Fatigue.M4$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M4$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 4.1) { 
		Results = rbind(Results, Fatigue.M4.1$Results)
		graph5 = list(abs(Fatigue.M4.1$Results[1]), as.numeric(Fatigue.M4.1$Results[1] < 0), abs(Fatigue.M4.1$Results[2]), 
			as.numeric(Fatigue.M4.1$Results[2] < 0), abs(Fatigue.M4.1$Results[3]), as.numeric(Fatigue.M4.1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M4.1$Results[4], Fatigue.M4.1$Results[5], Fatigue.M4.1$Results[6], Fatigue.M4.1$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M4.1$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 4.2) { 
		Results = rbind(Results, Fatigue.M4.2$Results)
		graph5 = list(abs(Fatigue.M4.2$Results[1]), as.numeric(Fatigue.M4.2$Results[1] < 0), abs(Fatigue.M4.2$Results[2]), 
			as.numeric(Fatigue.M4.2$Results[2] < 0), abs(Fatigue.M4.2$Results[3]), as.numeric(Fatigue.M4.2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M4.2$Results[4], Fatigue.M4.2$Results[5], Fatigue.M4.2$Results[6], Fatigue.M4.2$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M4.2$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 5.1) { 
		Results = rbind(Results, Fatigue.M1c$Results)
		graph5 = list(abs(Fatigue.M1c$Results[1]), as.numeric(Fatigue.M1c$Results[1] < 0), abs(Fatigue.M1c$Results[2]), 
			as.numeric(Fatigue.M1c$Results[2] < 0), abs(Fatigue.M1c$Results[3]), as.numeric(Fatigue.M1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M1c$Results[4], Fatigue.M1c$Results[5], Fatigue.M1c$Results[6], Fatigue.M1c$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M1c$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 5.2) { 
		Results = rbind(Results, Fatigue.M2c$Results)
		graph5 = list(abs(Fatigue.M2c$Results[1]), as.numeric(Fatigue.M2c$Results[1] < 0), abs(Fatigue.M2c$Results[2]), 
			as.numeric(Fatigue.M2c$Results[2] < 0), abs(Fatigue.M2c$Results[3]), as.numeric(Fatigue.M2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M2c$Results[4], Fatigue.M2c$Results[5], Fatigue.M2c$Results[6], Fatigue.M2c$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M2c$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 5.3) { 
		Results = rbind(Results, Fatigue.M3c$Results)
		graph5 = list(abs(Fatigue.M3c$Results[1]), as.numeric(Fatigue.M3c$Results[1] < 0), abs(Fatigue.M3c$Results[2]), 
			as.numeric(Fatigue.M3c$Results[2] < 0), abs(Fatigue.M3c$Results[3]), as.numeric(Fatigue.M3c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M3c$Results[4], Fatigue.M3c$Results[5], Fatigue.M3c$Results[6], Fatigue.M3c$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M3c$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 5.4) { 
		Results = rbind(Results, Fatigue.M4c$Results)
		graph5 = list(abs(Fatigue.M4c$Results[1]), as.numeric(Fatigue.M4c$Results[1] < 0), abs(Fatigue.M4c$Results[2]), 
			as.numeric(Fatigue.M4c$Results[2] < 0), abs(Fatigue.M4c$Results[3]), as.numeric(Fatigue.M4c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M4c$Results[4], Fatigue.M4c$Results[5], Fatigue.M4c$Results[6], Fatigue.M4c$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M4c$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 5.41) { 
		Results = rbind(Results, Fatigue.M4.1c$Results)
		graph5 = list(abs(Fatigue.M4.1c$Results[1]), as.numeric(Fatigue.M4.1c$Results[1] < 0), abs(Fatigue.M4.1c$Results[2]), 
			as.numeric(Fatigue.M4.1c$Results[2] < 0), abs(Fatigue.M4.1c$Results[3]), as.numeric(Fatigue.M4.1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M4.1c$Results[4], Fatigue.M4.1c$Results[5], Fatigue.M4.1c$Results[6], Fatigue.M4.1c$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M4.1c$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[2] == 5.42) { 
		Results = rbind(Results, Fatigue.M4.2c$Results)
		graph5 = list(abs(Fatigue.M4.2c$Results[1]), as.numeric(Fatigue.M4.2c$Results[1] < 0), abs(Fatigue.M4.2c$Results[2]), 
			as.numeric(Fatigue.M4.2c$Results[2] < 0), abs(Fatigue.M4.2c$Results[3]), as.numeric(Fatigue.M4.2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Fatigue.M4.2c$Results[4], Fatigue.M4.2c$Results[5], Fatigue.M4.2c$Results[6], Fatigue.M4.2c$Results[7])
		names(graph6) = graph6_names
		fatigue = list(as.logical(1 - Fatigue.M4.2c$urun), graph5, graph6)
		names(fatigue) = c("successful_run", "graph_5", "graph_6")
	}
	
	if (Best.Model[3] == 1) { 
		Results = rbind(Results, Drowsy.M1$Results) 
		graph5 = list(abs(Drowsy.M1$Results[1]), as.numeric(Drowsy.M1$Results[1] < 0), abs(Drowsy.M1$Results[2]), 
			as.numeric(Drowsy.M1$Results[2] < 0), abs(Drowsy.M1$Results[3]), as.numeric(Drowsy.M1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M1$Results[4], Drowsy.M1$Results[5], Drowsy.M1$Results[6], Drowsy.M1$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M1$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 2) { 
		Results = rbind(Results, Drowsy.M2$Results)
		graph5 = list(abs(Drowsy.M2$Results[1]), as.numeric(Drowsy.M2$Results[1] < 0), abs(Drowsy.M2$Results[2]), 
			as.numeric(Drowsy.M2$Results[2] < 0), abs(Drowsy.M2$Results[3]), as.numeric(Drowsy.M2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M2$Results[4], Drowsy.M2$Results[5], Drowsy.M2$Results[6], Drowsy.M2$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M2$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 3) { 
		Results = rbind(Results, Drowsy.M3$Results)
		graph5 = list(abs(Drowsy.M3$Results[1]), as.numeric(Drowsy.M3$Results[1] < 0), abs(Drowsy.M3$Results[2]), 
			as.numeric(Drowsy.M3$Results[2] < 0), abs(Drowsy.M3$Results[3]), as.numeric(Drowsy.M3$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M3$Results[4], Drowsy.M3$Results[5], Drowsy.M3$Results[6], Drowsy.M3$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M3$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 4) { 
		Results = rbind(Results, Drowsy.M4$Results)
		graph5 = list(abs(Drowsy.M4$Results[1]), as.numeric(Drowsy.M4$Results[1] < 0), abs(Drowsy.M4$Results[2]), 
			as.numeric(Drowsy.M4$Results[2] < 0), abs(Drowsy.M4$Results[3]), as.numeric(Drowsy.M4$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M4$Results[4], Drowsy.M4$Results[5], Drowsy.M4$Results[6], Drowsy.M4$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M4$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 4.1) { 
		Results = rbind(Results, Drowsy.M4.1$Results)
		graph5 = list(abs(Drowsy.M4.1$Results[1]), as.numeric(Drowsy.M4.1$Results[1] < 0), abs(Drowsy.M4.1$Results[2]), 
			as.numeric(Drowsy.M4.1$Results[2] < 0), abs(Drowsy.M4.1$Results[3]), as.numeric(Drowsy.M4.1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M4.1$Results[4], Drowsy.M4.1$Results[5], Drowsy.M4.1$Results[6], Drowsy.M4.1$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M4.1$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 4.2) { 
		Results = rbind(Results, Drowsy.M4.2$Results)
		graph5 = list(abs(Drowsy.M4.2$Results[1]), as.numeric(Drowsy.M4.2$Results[1] < 0), abs(Drowsy.M4.2$Results[2]), 
			as.numeric(Drowsy.M4.2$Results[2] < 0), abs(Drowsy.M4.2$Results[3]), as.numeric(Drowsy.M4.2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M4.2$Results[4], Drowsy.M4.2$Results[5], Drowsy.M4.2$Results[6], Drowsy.M4.2$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M4.2$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 5.1) { 
		Results = rbind(Results, Drowsy.M1c$Results)
		graph5 = list(abs(Drowsy.M1c$Results[1]), as.numeric(Drowsy.M1c$Results[1] < 0), abs(Drowsy.M1c$Results[2]), 
			as.numeric(Drowsy.M1c$Results[2] < 0), abs(Drowsy.M1c$Results[3]), as.numeric(Drowsy.M1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M1c$Results[4], Drowsy.M1c$Results[5], Drowsy.M1c$Results[6], Drowsy.M1c$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M1c$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 5.2) { 
		Results = rbind(Results, Drowsy.M2c$Results)
		graph5 = list(abs(Drowsy.M2c$Results[1]), as.numeric(Drowsy.M2c$Results[1] < 0), abs(Drowsy.M2c$Results[2]), 
			as.numeric(Drowsy.M2c$Results[2] < 0), abs(Drowsy.M2c$Results[3]), as.numeric(Drowsy.M2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M2c$Results[4], Drowsy.M2c$Results[5], Drowsy.M2c$Results[6], Drowsy.M2c$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M2c$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 5.3) { 
		Results = rbind(Results, Drowsy.M3c$Results)
		graph5 = list(abs(Drowsy.M3c$Results[1]), as.numeric(Drowsy.M3c$Results[1] < 0), abs(Drowsy.M3c$Results[2]), 
			as.numeric(Drowsy.M3c$Results[2] < 0), abs(Drowsy.M3c$Results[3]), as.numeric(Drowsy.M3c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M3c$Results[4], Drowsy.M3c$Results[5], Drowsy.M3c$Results[6], Drowsy.M3c$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M3c$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 5.4) { 
		Results = rbind(Results, Drowsy.M4c$Results)
		graph5 = list(abs(Drowsy.M4c$Results[1]), as.numeric(Drowsy.M4c$Results[1] < 0), abs(Drowsy.M4c$Results[2]), 
			as.numeric(Drowsy.M4c$Results[2] < 0), abs(Drowsy.M4c$Results[3]), as.numeric(Drowsy.M4c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M4c$Results[4], Drowsy.M4c$Results[5], Drowsy.M4c$Results[6], Drowsy.M4c$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M4c$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 5.41) { 
		Results = rbind(Results, Drowsy.M4.1c$Results)
		graph5 = list(abs(Drowsy.M4.1c$Results[1]), as.numeric(Drowsy.M4.1c$Results[1] < 0), abs(Drowsy.M4.1c$Results[2]), 
			as.numeric(Drowsy.M4.1c$Results[2] < 0), abs(Drowsy.M4.1c$Results[3]), as.numeric(Drowsy.M4.1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M4.1c$Results[4], Drowsy.M4.1c$Results[5], Drowsy.M4.1c$Results[6], Drowsy.M4.1c$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M4.1c$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[3] == 5.42) { 
		Results = rbind(Results, Drowsy.M4.2c$Results)
		graph5 = list(abs(Drowsy.M4.2c$Results[1]), as.numeric(Drowsy.M4.2c$Results[1] < 0), abs(Drowsy.M4.2c$Results[2]), 
			as.numeric(Drowsy.M4.2c$Results[2] < 0), abs(Drowsy.M4.2c$Results[3]), as.numeric(Drowsy.M4.2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Drowsy.M4.2c$Results[4], Drowsy.M4.2c$Results[5], Drowsy.M4.2c$Results[6], Drowsy.M4.2c$Results[7])
		names(graph6) = graph6_names
		drowsiness = list(as.logical(1 - Drowsy.M4.2c$urun), graph5, graph6)
		names(drowsiness) = c("successful_run", "graph_5", "graph_6")
	}
	
	if (Best.Model[4] == 1) { 
		Results = rbind(Results, Sleep.M1$Results) 
		graph5 = list(abs(Sleep.M1$Results[1]), as.numeric(Sleep.M1$Results[1] < 0), abs(Sleep.M1$Results[2]), 
			as.numeric(Sleep.M1$Results[2] < 0), abs(Sleep.M1$Results[3]), as.numeric(Sleep.M1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M1$Results[4], Sleep.M1$Results[5], Sleep.M1$Results[6], Sleep.M1$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M1$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 2) { 
		Results = rbind(Results, Sleep.M2$Results)
		graph5 = list(abs(Sleep.M2$Results[1]), as.numeric(Sleep.M2$Results[1] < 0), abs(Sleep.M2$Results[2]), 
			as.numeric(Sleep.M2$Results[2] < 0), abs(Sleep.M2$Results[3]), as.numeric(Sleep.M2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M2$Results[4], Sleep.M2$Results[5], Sleep.M2$Results[6], Sleep.M2$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M2$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 3) { 
		Results = rbind(Results, Sleep.M3$Results)
		graph5 = list(abs(Sleep.M3$Results[1]), as.numeric(Sleep.M3$Results[1] < 0), abs(Sleep.M3$Results[2]), 
			as.numeric(Sleep.M3$Results[2] < 0), abs(Sleep.M3$Results[3]), as.numeric(Sleep.M3$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M3$Results[4], Sleep.M3$Results[5], Sleep.M3$Results[6], Sleep.M3$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M3$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 4) { 
		Results = rbind(Results, Sleep.M4$Results)
		graph5 = list(abs(Sleep.M4$Results[1]), as.numeric(Sleep.M4$Results[1] < 0), abs(Sleep.M4$Results[2]), 
			as.numeric(Sleep.M4$Results[2] < 0), abs(Sleep.M4$Results[3]), as.numeric(Sleep.M4$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M4$Results[4], Sleep.M4$Results[5], Sleep.M4$Results[6], Sleep.M4$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M4$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 4.1) { 
		Results = rbind(Results, Sleep.M4.1$Results)
		graph5 = list(abs(Sleep.M4.1$Results[1]), as.numeric(Sleep.M4.1$Results[1] < 0), abs(Sleep.M4.1$Results[2]), 
			as.numeric(Sleep.M4.1$Results[2] < 0), abs(Sleep.M4.1$Results[3]), as.numeric(Sleep.M4.1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M4.1$Results[4], Sleep.M4.1$Results[5], Sleep.M4.1$Results[6], Sleep.M4.1$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M4.1$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 4.2) { 
		Results = rbind(Results, Sleep.M4.2$Results)
		graph5 = list(abs(Sleep.M4.2$Results[1]), as.numeric(Sleep.M4.2$Results[1] < 0), abs(Sleep.M4.2$Results[2]), 
			as.numeric(Sleep.M4.2$Results[2] < 0), abs(Sleep.M4.2$Results[3]), as.numeric(Sleep.M4.2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M4.2$Results[4], Sleep.M4.2$Results[5], Sleep.M4.2$Results[6], Sleep.M4.2$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M4.2$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 5.1) { 
		Results = rbind(Results, Sleep.M1c$Results)
		graph5 = list(abs(Sleep.M1c$Results[1]), as.numeric(Sleep.M1c$Results[1] < 0), abs(Sleep.M1c$Results[2]), 
			as.numeric(Sleep.M1c$Results[2] < 0), abs(Sleep.M1c$Results[3]), as.numeric(Sleep.M1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M1c$Results[4], Sleep.M1c$Results[5], Sleep.M1c$Results[6], Sleep.M1c$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M1c$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 5.2) { 
		Results = rbind(Results, Sleep.M2c$Results)
		graph5 = list(abs(Sleep.M2c$Results[1]), as.numeric(Sleep.M2c$Results[1] < 0), abs(Sleep.M2c$Results[2]), 
			as.numeric(Sleep.M2c$Results[2] < 0), abs(Sleep.M2c$Results[3]), as.numeric(Sleep.M2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M2c$Results[4], Sleep.M2c$Results[5], Sleep.M2c$Results[6], Sleep.M2c$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M2c$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 5.3) { 
		Results = rbind(Results, Sleep.M3c$Results)
		graph5 = list(abs(Sleep.M3c$Results[1]), as.numeric(Sleep.M3c$Results[1] < 0), abs(Sleep.M3c$Results[2]), 
			as.numeric(Sleep.M3c$Results[2] < 0), abs(Sleep.M3c$Results[3]), as.numeric(Sleep.M3c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M3c$Results[4], Sleep.M3c$Results[5], Sleep.M3c$Results[6], Sleep.M3c$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M3c$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 5.4) { 
		Results = rbind(Results, Sleep.M4c$Results)
		graph5 = list(abs(Sleep.M4c$Results[1]), as.numeric(Sleep.M4c$Results[1] < 0), abs(Sleep.M4c$Results[2]), 
			as.numeric(Sleep.M4c$Results[2] < 0), abs(Sleep.M4c$Results[3]), as.numeric(Sleep.M4c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M4c$Results[4], Sleep.M4c$Results[5], Sleep.M4c$Results[6], Sleep.M4c$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M4c$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 5.41) { 
		Results = rbind(Results, Sleep.M4.1c$Results)
		graph5 = list(abs(Sleep.M4.1c$Results[1]), as.numeric(Sleep.M4.1c$Results[1] < 0), abs(Sleep.M4.1c$Results[2]), 
			as.numeric(Sleep.M4.1c$Results[2] < 0), abs(Sleep.M4.1c$Results[3]), as.numeric(Sleep.M4.1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M4.1c$Results[4], Sleep.M4.1c$Results[5], Sleep.M4.1c$Results[6], Sleep.M4.1c$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M4.1c$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[4] == 5.42) { 
		Results = rbind(Results, Sleep.M4.2c$Results)
		graph5 = list(abs(Sleep.M4.2c$Results[1]), as.numeric(Sleep.M4.2c$Results[1] < 0), abs(Sleep.M4.2c$Results[2]), 
			as.numeric(Sleep.M4.2c$Results[2] < 0), abs(Sleep.M4.2c$Results[3]), as.numeric(Sleep.M4.2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Sleep.M4.2c$Results[4], Sleep.M4.2c$Results[5], Sleep.M4.2c$Results[6], Sleep.M4.2c$Results[7])
		names(graph6) = graph6_names
		sleep_problems = list(as.logical(1 - Sleep.M4.2c$urun), graph5, graph6)
		names(sleep_problems) = c("successful_run", "graph_5", "graph_6")
	}
	
	if (Best.Model[5] == 1) { 
		Results = rbind(Results, Thinking.M1$Results) 
		graph5 = list(abs(Thinking.M1$Results[1]), as.numeric(Thinking.M1$Results[1] < 0), abs(Thinking.M1$Results[2]), 
			as.numeric(Thinking.M1$Results[2] < 0), abs(Thinking.M1$Results[3]), as.numeric(Thinking.M1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M1$Results[4], Thinking.M1$Results[5], Thinking.M1$Results[6], Thinking.M1$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M1$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 2) { 
		Results = rbind(Results, Thinking.M2$Results)
		graph5 = list(abs(Thinking.M2$Results[1]), as.numeric(Thinking.M2$Results[1] < 0), abs(Thinking.M2$Results[2]), 
			as.numeric(Thinking.M2$Results[2] < 0), abs(Thinking.M2$Results[3]), as.numeric(Thinking.M2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M2$Results[4], Thinking.M2$Results[5], Thinking.M2$Results[6], Thinking.M2$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M2$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 3) { 
		Results = rbind(Results, Thinking.M3$Results)
		graph5 = list(abs(Thinking.M3$Results[1]), as.numeric(Thinking.M3$Results[1] < 0), abs(Thinking.M3$Results[2]), 
			as.numeric(Thinking.M3$Results[2] < 0), abs(Thinking.M3$Results[3]), as.numeric(Thinking.M3$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M3$Results[4], Thinking.M3$Results[5], Thinking.M3$Results[6], Thinking.M3$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M3$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 4) { 
		Results = rbind(Results, Thinking.M4$Results)
		graph5 = list(abs(Thinking.M4$Results[1]), as.numeric(Thinking.M4$Results[1] < 0), abs(Thinking.M4$Results[2]), 
			as.numeric(Thinking.M4$Results[2] < 0), abs(Thinking.M4$Results[3]), as.numeric(Thinking.M4$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M4$Results[4], Thinking.M4$Results[5], Thinking.M4$Results[6], Thinking.M4$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M4$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 4.1) { 
		Results = rbind(Results, Thinking.M4.1$Results)
		graph5 = list(abs(Thinking.M4.1$Results[1]), as.numeric(Thinking.M4.1$Results[1] < 0), abs(Thinking.M4.1$Results[2]), 
			as.numeric(Thinking.M4.1$Results[2] < 0), abs(Thinking.M4.1$Results[3]), as.numeric(Thinking.M4.1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M4.1$Results[4], Thinking.M4.1$Results[5], Thinking.M4.1$Results[6], Thinking.M4.1$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M4.1$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 4.2) { 
		Results = rbind(Results, Thinking.M4.2$Results)
		graph5 = list(abs(Thinking.M4.2$Results[1]), as.numeric(Thinking.M4.2$Results[1] < 0), abs(Thinking.M4.2$Results[2]), 
			as.numeric(Thinking.M4.2$Results[2] < 0), abs(Thinking.M4.2$Results[3]), as.numeric(Thinking.M4.2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M4.2$Results[4], Thinking.M4.2$Results[5], Thinking.M4.2$Results[6], Thinking.M4.2$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M4.2$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 5.1) { 
		Results = rbind(Results, Thinking.M1c$Results)
		graph5 = list(abs(Thinking.M1c$Results[1]), as.numeric(Thinking.M1c$Results[1] < 0), abs(Thinking.M1c$Results[2]), 
			as.numeric(Thinking.M1c$Results[2] < 0), abs(Thinking.M1c$Results[3]), as.numeric(Thinking.M1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M1c$Results[4], Thinking.M1c$Results[5], Thinking.M1c$Results[6], Thinking.M1c$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M1c$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 5.2) { 
		Results = rbind(Results, Thinking.M2c$Results)
		graph5 = list(abs(Thinking.M2c$Results[1]), as.numeric(Thinking.M2c$Results[1] < 0), abs(Thinking.M2c$Results[2]), 
			as.numeric(Thinking.M2c$Results[2] < 0), abs(Thinking.M2c$Results[3]), as.numeric(Thinking.M2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M2c$Results[4], Thinking.M2c$Results[5], Thinking.M2c$Results[6], Thinking.M2c$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M2c$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 5.3) { 
		Results = rbind(Results, Thinking.M3c$Results)
		graph5 = list(abs(Thinking.M3c$Results[1]), as.numeric(Thinking.M3c$Results[1] < 0), abs(Thinking.M3c$Results[2]), 
			as.numeric(Thinking.M3c$Results[2] < 0), abs(Thinking.M3c$Results[3]), as.numeric(Thinking.M3c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M3c$Results[4], Thinking.M3c$Results[5], Thinking.M3c$Results[6], Thinking.M3c$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M3c$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 5.4) { 
		Results = rbind(Results, Thinking.M4c$Results)
		graph5 = list(abs(Thinking.M4c$Results[1]), as.numeric(Thinking.M4c$Results[1] < 0), abs(Thinking.M4c$Results[2]), 
			as.numeric(Thinking.M4c$Results[2] < 0), abs(Thinking.M4c$Results[3]), as.numeric(Thinking.M4c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M4c$Results[4], Thinking.M4c$Results[5], Thinking.M4c$Results[6], Thinking.M4c$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M4c$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 5.41) { 
		Results = rbind(Results, Thinking.M4.1c$Results)
		graph5 = list(abs(Thinking.M4.1c$Results[1]), as.numeric(Thinking.M4.1c$Results[1] < 0), abs(Thinking.M4.1c$Results[2]), 
			as.numeric(Thinking.M4.1c$Results[2] < 0), abs(Thinking.M4.1c$Results[3]), as.numeric(Thinking.M4.1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M4.1c$Results[4], Thinking.M4.1c$Results[5], Thinking.M4.1c$Results[6], Thinking.M4.1c$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M4.1c$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[5] == 5.42) { 
		Results = rbind(Results, Thinking.M4.2c$Results)
		graph5 = list(abs(Thinking.M4.2c$Results[1]), as.numeric(Thinking.M4.2c$Results[1] < 0), abs(Thinking.M4.2c$Results[2]), 
			as.numeric(Thinking.M4.2c$Results[2] < 0), abs(Thinking.M4.2c$Results[3]), as.numeric(Thinking.M4.2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Thinking.M4.2c$Results[4], Thinking.M4.2c$Results[5], Thinking.M4.2c$Results[6], Thinking.M4.2c$Results[7])
		names(graph6) = graph6_names
		thinking_problems = list(as.logical(1 - Thinking.M4.2c$urun), graph5, graph6)
		names(thinking_problems) = c("successful_run", "graph_5", "graph_6")
	}
	
	if (Best.Model[6] == 1) { 
		Results = rbind(Results, Constipation.M1$Results) 
		graph5 = list(abs(Constipation.M1$Results[1]), as.numeric(Constipation.M1$Results[1] < 0), abs(Constipation.M1$Results[2]), 
			as.numeric(Constipation.M1$Results[2] < 0), abs(Constipation.M1$Results[3]), as.numeric(Constipation.M1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M1$Results[4], Constipation.M1$Results[5], Constipation.M1$Results[6], Constipation.M1$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M1$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 2) { 
		Results = rbind(Results, Constipation.M2$Results)
		graph5 = list(abs(Constipation.M2$Results[1]), as.numeric(Constipation.M2$Results[1] < 0), abs(Constipation.M2$Results[2]), 
			as.numeric(Constipation.M2$Results[2] < 0), abs(Constipation.M2$Results[3]), as.numeric(Constipation.M2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M2$Results[4], Constipation.M2$Results[5], Constipation.M2$Results[6], Constipation.M2$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M2$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 3) { 
		Results = rbind(Results, Constipation.M3$Results)
		graph5 = list(abs(Constipation.M3$Results[1]), as.numeric(Constipation.M3$Results[1] < 0), abs(Constipation.M3$Results[2]), 
			as.numeric(Constipation.M3$Results[2] < 0), abs(Constipation.M3$Results[3]), as.numeric(Constipation.M3$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M3$Results[4], Constipation.M3$Results[5], Constipation.M3$Results[6], Constipation.M3$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M3$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 4) { 
		Results = rbind(Results, Constipation.M4$Results)
		graph5 = list(abs(Constipation.M4$Results[1]), as.numeric(Constipation.M4$Results[1] < 0), abs(Constipation.M4$Results[2]), 
			as.numeric(Constipation.M4$Results[2] < 0), abs(Constipation.M4$Results[3]), as.numeric(Constipation.M4$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M4$Results[4], Constipation.M4$Results[5], Constipation.M4$Results[6], Constipation.M4$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M4$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 4.1) { 
		Results = rbind(Results, Constipation.M4.1$Results)
		graph5 = list(abs(Constipation.M4.1$Results[1]), as.numeric(Constipation.M4.1$Results[1] < 0), abs(Constipation.M4.1$Results[2]), 
			as.numeric(Constipation.M4.1$Results[2] < 0), abs(Constipation.M4.1$Results[3]), as.numeric(Constipation.M4.1$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M4.1$Results[4], Constipation.M4.1$Results[5], Constipation.M4.1$Results[6], Constipation.M4.1$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M4.1$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 4.2) { 
		Results = rbind(Results, Constipation.M4.2$Results)
		graph5 = list(abs(Constipation.M4.2$Results[1]), as.numeric(Constipation.M4.2$Results[1] < 0), abs(Constipation.M4.2$Results[2]), 
			as.numeric(Constipation.M4.2$Results[2] < 0), abs(Constipation.M4.2$Results[3]), as.numeric(Constipation.M4.2$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M4.2$Results[4], Constipation.M4.2$Results[5], Constipation.M4.2$Results[6], Constipation.M4.2$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M4.2$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 5.1) { 
		Results = rbind(Results, Constipation.M1c$Results)
		graph5 = list(abs(Constipation.M1c$Results[1]), as.numeric(Constipation.M1c$Results[1] < 0), abs(Constipation.M1c$Results[2]), 
			as.numeric(Constipation.M1c$Results[2] < 0), abs(Constipation.M1c$Results[3]), as.numeric(Constipation.M1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M1c$Results[4], Constipation.M1c$Results[5], Constipation.M1c$Results[6], Constipation.M1c$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M1c$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 5.2) { 
		Results = rbind(Results, Constipation.M2c$Results)
		graph5 = list(abs(Constipation.M2c$Results[1]), as.numeric(Constipation.M2c$Results[1] < 0), abs(Constipation.M2c$Results[2]), 
			as.numeric(Constipation.M2c$Results[2] < 0), abs(Constipation.M2c$Results[3]), as.numeric(Constipation.M2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M2c$Results[4], Constipation.M2c$Results[5], Constipation.M2c$Results[6], Constipation.M2c$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M2c$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 5.3) { 
		Results = rbind(Results, Constipation.M3c$Results)
		graph5 = list(abs(Constipation.M3c$Results[1]), as.numeric(Constipation.M3c$Results[1] < 0), abs(Constipation.M3c$Results[2]), 
			as.numeric(Constipation.M3c$Results[2] < 0), abs(Constipation.M3c$Results[3]), as.numeric(Constipation.M3c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M3c$Results[4], Constipation.M3c$Results[5], Constipation.M3c$Results[6], Constipation.M3c$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M3c$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 5.4) { 
		Results = rbind(Results, Constipation.M4c$Results)
		graph5 = list(abs(Constipation.M4c$Results[1]), as.numeric(Constipation.M4c$Results[1] < 0), abs(Constipation.M4c$Results[2]), 
			as.numeric(Constipation.M4c$Results[2] < 0), abs(Constipation.M4c$Results[3]), as.numeric(Constipation.M4c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M4c$Results[4], Constipation.M4c$Results[5], Constipation.M4c$Results[6], Constipation.M4c$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M4c$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 5.41) { 
		Results = rbind(Results, Constipation.M4.1c$Results)
		graph5 = list(abs(Constipation.M4.1c$Results[1]), as.numeric(Constipation.M4.1c$Results[1] < 0), abs(Constipation.M4.1c$Results[2]), 
			as.numeric(Constipation.M4.1c$Results[2] < 0), abs(Constipation.M4.1c$Results[3]), as.numeric(Constipation.M4.1c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M4.1c$Results[4], Constipation.M4.1c$Results[5], Constipation.M4.1c$Results[6], Constipation.M4.1c$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M4.1c$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	} else if (Best.Model[6] == 5.42) { 
		Results = rbind(Results, Constipation.M4.2c$Results)
		graph5 = list(abs(Constipation.M4.2c$Results[1]), as.numeric(Constipation.M4.2c$Results[1] < 0), abs(Constipation.M4.2c$Results[2]), 
			as.numeric(Constipation.M4.2c$Results[2] < 0), abs(Constipation.M4.2c$Results[3]), as.numeric(Constipation.M4.2c$Results[3] < 0))
		for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
		names(graph5) = graph5_names
		graph6 = list(Constipation.M4.2c$Results[4], Constipation.M4.2c$Results[5], Constipation.M4.2c$Results[6], Constipation.M4.2c$Results[7])
		names(graph6) = graph6_names
		constipation = list(as.logical(1 - Constipation.M4.2c$urun), graph5, graph6)
		names(constipation) = c("successful_run", "graph_5", "graph_6")
	}
	
	if (!No_Neuropain) {
		if (Best.Model[7] == 1) { 
			Results = rbind(Results, Neuropain.M1$Results) 
			graph5 = list(abs(Neuropain.M1$Results[1]), as.numeric(Neuropain.M1$Results[1] < 0), abs(Neuropain.M1$Results[2]), 
				as.numeric(Neuropain.M1$Results[2] < 0), abs(Neuropain.M1$Results[3]), as.numeric(Neuropain.M1$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M1$Results[4], Neuropain.M1$Results[5], Neuropain.M1$Results[6], Neuropain.M1$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M1$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 2) { 
			Results = rbind(Results, Neuropain.M2$Results)
			graph5 = list(abs(Neuropain.M2$Results[1]), as.numeric(Neuropain.M2$Results[1] < 0), abs(Neuropain.M2$Results[2]), 
				as.numeric(Neuropain.M2$Results[2] < 0), abs(Neuropain.M2$Results[3]), as.numeric(Neuropain.M2$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M2$Results[4], Neuropain.M2$Results[5], Neuropain.M2$Results[6], Neuropain.M2$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M2$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 3) { 
			Results = rbind(Results, Neuropain.M3$Results)
			graph5 = list(abs(Neuropain.M3$Results[1]), as.numeric(Neuropain.M3$Results[1] < 0), abs(Neuropain.M3$Results[2]), 
				as.numeric(Neuropain.M3$Results[2] < 0), abs(Neuropain.M3$Results[3]), as.numeric(Neuropain.M3$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M3$Results[4], Neuropain.M3$Results[5], Neuropain.M3$Results[6], Neuropain.M3$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M3$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 4) { 
			Results = rbind(Results, Neuropain.M4$Results)
			graph5 = list(abs(Neuropain.M4$Results[1]), as.numeric(Neuropain.M4$Results[1] < 0), abs(Neuropain.M4$Results[2]), 
				as.numeric(Neuropain.M4$Results[2] < 0), abs(Neuropain.M4$Results[3]), as.numeric(Neuropain.M4$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M4$Results[4], Neuropain.M4$Results[5], Neuropain.M4$Results[6], Neuropain.M4$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M4$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 4.1) { 
			Results = rbind(Results, Neuropain.M4.1$Results)
			graph5 = list(abs(Neuropain.M4.1$Results[1]), as.numeric(Neuropain.M4.1$Results[1] < 0), abs(Neuropain.M4.1$Results[2]), 
				as.numeric(Neuropain.M4.1$Results[2] < 0), abs(Neuropain.M4.1$Results[3]), as.numeric(Neuropain.M4.1$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M4.1$Results[4], Neuropain.M4.1$Results[5], Neuropain.M4.1$Results[6], Neuropain.M4.1$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M4.1$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 4.2) { 
			Results = rbind(Results, Neuropain.M4.2$Results)
			graph5 = list(abs(Neuropain.M4.2$Results[1]), as.numeric(Neuropain.M4.2$Results[1] < 0), abs(Neuropain.M4.2$Results[2]), 
				as.numeric(Neuropain.M4.2$Results[2] < 0), abs(Neuropain.M4.2$Results[3]), as.numeric(Neuropain.M4.2$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M4.2$Results[4], Neuropain.M4.2$Results[5], Neuropain.M4.2$Results[6], Neuropain.M4.2$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M4.2$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 5.1) { 
			Results = rbind(Results, Neuropain.M1c$Results)
			graph5 = list(abs(Neuropain.M1c$Results[1]), as.numeric(Neuropain.M1c$Results[1] < 0), abs(Neuropain.M1c$Results[2]), 
				as.numeric(Neuropain.M1c$Results[2] < 0), abs(Neuropain.M1c$Results[3]), as.numeric(Neuropain.M1c$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M1c$Results[4], Neuropain.M1c$Results[5], Neuropain.M1c$Results[6], Neuropain.M1c$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M1c$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 5.2) { 
			Results = rbind(Results, Neuropain.M2c$Results)
			graph5 = list(abs(Neuropain.M2c$Results[1]), as.numeric(Neuropain.M2c$Results[1] < 0), abs(Neuropain.M2c$Results[2]), 
				as.numeric(Neuropain.M2c$Results[2] < 0), abs(Neuropain.M2c$Results[3]), as.numeric(Neuropain.M2c$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M2c$Results[4], Neuropain.M2c$Results[5], Neuropain.M2c$Results[6], Neuropain.M2c$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M2c$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 5.3) { 
			Results = rbind(Results, Neuropain.M3c$Results)
			graph5 = list(abs(Neuropain.M3c$Results[1]), as.numeric(Neuropain.M3c$Results[1] < 0), abs(Neuropain.M3c$Results[2]), 
				as.numeric(Neuropain.M3c$Results[2] < 0), abs(Neuropain.M3c$Results[3]), as.numeric(Neuropain.M3c$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M3c$Results[4], Neuropain.M3c$Results[5], Neuropain.M3c$Results[6], Neuropain.M3c$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M3c$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 5.4) { 
			Results = rbind(Results, Neuropain.M4c$Results)
			graph5 = list(abs(Neuropain.M4c$Results[1]), as.numeric(Neuropain.M4c$Results[1] < 0), abs(Neuropain.M4c$Results[2]), 
				as.numeric(Neuropain.M4c$Results[2] < 0), abs(Neuropain.M4c$Results[3]), as.numeric(Neuropain.M4c$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M4c$Results[4], Neuropain.M4c$Results[5], Neuropain.M4c$Results[6], Neuropain.M4c$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M4c$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 5.41) { 
			Results = rbind(Results, Neuropain.M4.1c$Results)
			graph5 = list(abs(Neuropain.M4.1c$Results[1]), as.numeric(Neuropain.M4.1c$Results[1] < 0), abs(Neuropain.M4.1c$Results[2]), 
				as.numeric(Neuropain.M4.1c$Results[2] < 0), abs(Neuropain.M4.1c$Results[3]), as.numeric(Neuropain.M4.1c$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M4.1c$Results[4], Neuropain.M4.1c$Results[5], Neuropain.M4.1c$Results[6], Neuropain.M4.1c$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M4.1c$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		} else if (Best.Model[7] == 5.42) { 
			Results = rbind(Results, Neuropain.M4.2c$Results)
			graph5 = list(abs(Neuropain.M4.2c$Results[1]), as.numeric(Neuropain.M4.2c$Results[1] < 0), abs(Neuropain.M4.2c$Results[2]), 
				as.numeric(Neuropain.M4.2c$Results[2] < 0), abs(Neuropain.M4.2c$Results[3]), as.numeric(Neuropain.M4.2c$Results[3] < 0))
			for (i in c(2, 4, 6)) { if (graph5[[i]] == 0) { graph5[[i]] = "A" } else { graph5[[i]] = "B" } }
			names(graph5) = graph5_names
			graph6 = list(Neuropain.M4.2c$Results[4], Neuropain.M4.2c$Results[5], Neuropain.M4.2c$Results[6], Neuropain.M4.2c$Results[7])
			names(graph6) = graph6_names
			neuropathic_pain = list(as.logical(1 - Neuropain.M4.2c$urun), graph5, graph6)
			names(neuropathic_pain) = c("successful_run", "graph_5", "graph_6")
		}
	}
	
	colnames(Results) = c("P025", "Median", "P975", "P(< -0.2)", "P(-0.2 - 0)", "P(0 - 0.2)", "P(> 0.2)")
	if (nrow(Results) == 6) {
		rownames(Results) = c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation")
	} else if (nrow(Results) == 7) {
		rownames(Results) = c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", "Constipation", "Neuropain")
	}
	
	out = list(pain, fatigue, drowsiness, sleep_problems, thinking_problems, constipation)
	names(out) = c("pain", "fatigue", "drowsiness", "sleep_problems", "thinking_problems", "constipation")
	
	if (!No_Neuropain) { out[[length(out) + 1]] = neuropathic_pain 
		names(out)[length(out)] = "neuropathic_pain"
	}
	
	meta.data[[length(meta.data) + 1]] = Best.Model
	names(meta.data)[length(meta.data)] = "Best.Model"
	
	out[[length(out) + 1]] = Pain.Info
	names(out)[length(out)] = "Pain.Info"
	out[[length(out) + 1]] = Fatigue.Info
	names(out)[length(out)] = "Fatigue.Info"
	out[[length(out) + 1]] = Drowsy.Info
	names(out)[length(out)] = "Drowsy.Info"
	out[[length(out) + 1]] = Sleep.Info
	names(out)[length(out)] = "Sleep.Info"
	out[[length(out) + 1]] = Thinking.Info
	names(out)[length(out)] = "Thinking.Info"
	out[[length(out) + 1]] = Constipation.Info
	names(out)[length(out)] = "Constipation.Info"
	
	if (!No_Neuropain) {
		out[[length(out) + 1]] = Neuropain.Info
		names(out)[length(out)] = "Neuropain.Info"
	}
	
	out[[length(out) + 1]] = meta.data
	names(out)[length(out)] = "meta.data"
	
	out[[length(out) + 1]] = Results
	names(out)[length(out)] = "Results"
	
	return(out)
}
