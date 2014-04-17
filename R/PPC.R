PPC <- function(observations, ForPPC, Covs, Outcome) {

	if (Outcome == "Pain") { index = 1
	} else if (Outcome == "Fatigue") { index = 2
	} else if (Outcome == "Drowsy") { index = 3
	} else if (Outcome == "Sleep") { index = 4
	} else if (Outcome == "Thinking") { index = 5
	} else if (Outcome == "Constipation") { index = 6
	} else if (Outcome == "Neuropain") { index = 7
	}
	
	SimY = matrix(NA, nrow(observations), length(ForPPC[[index]][[1]]))
	
	if (index == 1 || index == 7) {
		if (is.null(Covs)) {
			Covs = rep(0, length(observations$Treat2))
			ForPPC[[index]][[4]] = rep(0, length(ForPPC[[index]][[1]]))
		}
		for (i in 1:ncol(SimY)) {
			SimY[ , i] = ForPPC[[index]][[1]][i] + 
				ForPPC[[index]][[2]][i] * observations$Treat2 + 
				ForPPC[[index]][[4]][i] * Covs
		}
	} else if (index > 1 && index < 7){
		if (is.null(Covs)) {
			Covs = rep(0, length(observations$Treat2))
			ForPPC[[index]][[2]] = rep(0, length(ForPPC[[index]][[1]]))
		}
		for (i in 1:ncol(SimY)) {
			SimY[ , i] = ForPPC[[index]][[1]][i] * observations$Treat2 + 
				ForPPC[[index]][[2]][i] * Covs
		}
	}
		
	# mean
	test = rep(NA, ncol(SimY))
	lessthan = rep(0, length(test))
	for (i in 1:length(test)) {	
		test[i] = mean(SimY[ , i]) 
		if (test[i] <= mean(observations$Pain2[!is.na(observations$Pain2)])) { lessthan[i] = 1 }
	}
	Pain.1.mean.prob = sum(lessthan) / length(lessthan)
	
	# max
	
	lessthan = rep(0, length(test))
	for (i in 1:length(test)) {
		test[i] = max(SimY[ , i])
		if (test[i] <= max(observations$Pain2[!is.na(observations$Pain2)])) { lessthan[i] = 1 }
	}
	Pain.1.max.prob = sum(lessthan) / length(lessthan)
	
	# min

	lessthan = rep(0, length(test))
	for (i in 1:length(test)) {
		test[i] = min(SimY[ , i])
		if (test[i] <= min(observations$Pain2[!is.na(observations$Pain2)])) { lessthan[i] = 1 }
	}
	Pain.1.min.prob = sum(lessthan) / length(lessthan)
	
	# median

	lessthan = rep(0, length(test))
	for (i in 1:length(test)) {
		test[i] = median(SimY[ , i])
		if (test[i] <= median(observations$Pain2[!is.na(observations$Pain2)])) { lessthan[i] = 1 }
	}
	Pain.1.median.prob = sum(lessthan) / length(lessthan)
	
	out = list("mean" = Pain.1.median.prob, "min" = Pain.1.min.prob, "max" = Pain.1.max.prob, "median" = Pain.1.median.prob)
	return(out)
	
}

