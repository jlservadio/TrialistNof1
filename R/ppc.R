ppc <- function(observations, Outcome, ForPPC, Covs, mod.id) {

	cat(mod.id, " *")
	flush.console()

	if (Outcome == "Pain") { index = 1
	} else if (Outcome == "Fatigue") { index = 2
	} else if (Outcome == "Drowsy") { index = 3
	} else if (Outcome == "Sleep") { index = 4
	} else if (Outcome == "Thinking") { index = 5
	} else if (Outcome == "Constipation") { index = 6
	} else if (Outcome == "Neuropain") { index = 7
	}
	
	#########################
	# Creating Simulated Data
	#########################
	
	if (!is.null(Covs)) { Covs = as.matrix(Covs) }
	
	if (mod.id == 2 || mod.id == 4.1 || mod.id == 5.2 || mod.id == 5.41) {
		Covs = as.matrix(Covs)
		if (mod.id == 2 || mod.id == 4.1) { tm = ncol(as.matrix(Covs))
		} else if (mod.id == 5.2 || mod.id == 5.41) { tm = 1 }
	
		x = as.matrix(Covs)[ , tm]
		k = l = x
		
		if (is.na(k[1])) { k[1] = 1 }
		if (is.na(l[length(l)])) { l[length(l)] = l[length(l) - 1] + 1 }
		
		for (ix in 2:length(x)) {
			if (is.na(k[ix])) { k[ix] = k[ix - 1] }
		}

		for (ix in (length(x) - 1):1) {
			if (is.na(l[ix])) { l[ix] = l[ix + 1] }
		}
		
		for (i in (length(k) - 1):1) {
			if (is.na(k[i])) { k[i] = k[i + 1] - 1 }
		}
		
		for (i in 2:length(l)) {
			if (is.na(l[i])) { l[i] = l[i - 1] + 1 }
		}
		
		k = k - .05
		l = l + .05		
		
		for (i in 1:length(x)) {
			if (is.na(x[i])) { x[i] = runif(1, k[i], l[i]) }
		}
		
		Covs[ , tm] = x
	}
	
	if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42) && index %in% c(1, 7)) {
		if (is.na(Covs[1, 1])) { Covs[1, 1] = rnorm(1, mean(Covs[ , 1], na.rm = TRUE), 2) }
		while (sum(is.na(Covs)) != 0) {
			for (i in 2:nrow(Covs)) {
				if (is.na(Covs[i, 1]) && !is.na(Covs[i-1, 1])) {
					Covs[i, 1] = round(mean(ForPPC[["alpha"]]) + mean(ForPPC[["beta"]]) * observations$Treat2[i] + 
					mean(ForPPC[["Slope"]][ , , 1]) * Covs[i-1, 1])
				}
			}
		}
	}
	
	intercept = matrix(1, nrow = nrow(observations), ncol = 1)
	
	SimY0 = SimY = matrix(NA, nrow(observations), length(ForPPC[["beta"]]))
		
		if (index == 1 || index == 7) {
			if (mod.id %in% c(4.1, 4.2, 5.2, 5.4, 5.1)) {
				slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
				slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
				
				SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
					as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
					Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2))
			} else if (mod.id == 5.41 || mod.id == 5.42) {
				slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
				slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
				slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
				
				SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
					as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
					Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
					Covs[ , 3] %*% t(as.matrix(slope3))
			} else if (mod.id == 3) {
				if (ncol(Covs) == 1) {
					slope = ForPPC[["Slope"]]
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs %*% t(as.matrix(slope))
				} else if (ncol(Covs) == 2) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2))
				} else if (ncol(Covs) == 3) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3))
				}	
			} else if (mod.id == 5.3) {
				if (ncol(Covs) == 3) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3))
				} else if (ncol(Covs) == 4) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					slope4 = as.numeric(ForPPC[["Slope"]][ , , 4])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3)) + Covs[ , 4] %*% t(as.matrix(slope4))
				} else if (ncol(Covs) == 5) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					slope4 = as.numeric(ForPPC[["Slope"]][ , , 4])
					slope5 = as.numeric(ForPPC[["Slope"]][ , , 5])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3)) + Covs[ , 4] %*% t(as.matrix(slope4)) + 
						Covs[ , 5] %*% t(as.matrix(slope5))
				}
			} else if (mod.id == 5.42) {
				if (ncol(Covs) == 4) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					slope4 = as.numeric(ForPPC[["Slope"]][ , , 4])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3)) + Covs[ , 4] %*% t(as.matrix(slope4))
				} else if (ncol(Covs) == 5) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					slope4 = as.numeric(ForPPC[["Slope"]][ , , 4])
					slope5 = as.numeric(ForPPC[["Slope"]][ , , 5])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3)) + Covs[ , 4] %*% t(as.matrix(slope4)) + 
						Covs[ , 5] %*% t(as.matrix(slope5))
				} else if (ncol(Covs) == 6) {
					slope1 = as.numeric(ForPPC[["Slope"]][ , , 1])
					slope2 = as.numeric(ForPPC[["Slope"]][ , , 2])
					slope3 = as.numeric(ForPPC[["Slope"]][ , , 3])
					slope4 = as.numeric(ForPPC[["Slope"]][ , , 4])
					slope5 = as.numeric(ForPPC[["Slope"]][ , , 5])
					slope6 = as.numeric(ForPPC[["Slope"]][ , , 6])
					
					SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
						as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]])) + 
						Covs[ , 1] %*% t(as.matrix(slope1)) + Covs[ , 2] %*% t(as.matrix(slope2)) + 
						Covs[ , 3] %*% t(as.matrix(slope3)) + Covs[ , 4] %*% t(as.matrix(slope4)) + 
						Covs[ , 5] %*% t(as.matrix(slope5)) + Covs[ , 6] %*% t(as.matrix(slope6))
				}
			} else {
				if (is.null(Covs)) {
					Covs = rep(0, length(observations$Treat2))
					ForPPC[["Slope"]] = rep(0, length(ForPPC[["alpha"]]))
				}
				
				SimY0 = intercept %*% t(as.matrix(ForPPC[["alpha"]])) + 
					as.matrix(observations$Treat2) %*% t(as.matrix(ForPPC[["beta"]]))
			}
		} else if (index > 1 && index < 7){
			if (is.null(Covs)) {
				Covs = rep(0, length(observations$Treat2))
				ForPPC[["Slope"]] = rep(0, length(ForPPC[["beta"]]))
			}

			for (i in 1:ncol(SimY0)) {
				for (j in 1:nrow(SimY0)) {
					SimY0[j, i] = sample(1:ncol(ForPPC[["p"]][1, , ]), 1, prob = 
						abs(ForPPC[["p"]][i, j, ]))
				}
			}
		}	
	
	if (index == 1 || index == 7) {
	
		for (j in 1:ncol(SimY0)) {
			SimY[ , j] = rnorm(SimY0[ , j], ForPPC[["Sd"]][j])
		}
	} else {
	
		for (j in 1:ncol(SimY0)) {
			SimY[ , j] = rnorm(SimY0[ , j], sd(SimY0[ , j]))
		}
	}
	
	SimY = round(SimY)
	
	cat("+")
	flush.console()

	#######
	# PPC's
	#######

	# Goodness of fit Chi-Square
	
	test0 = SimY - SimY0 / SimY0
	expected = rowMeans(SimY0)
	test = colSums(test0)
	obs0 = observations[[index+2]] - expected / expected
	obs = sum(obs0, na.rm = TRUE)
	
	lessthan = greaterthan = 0
	for (i in 1:length(test)) {
		if (test[i] > obs) { greaterthan = greaterthan + 1
		} else if (test[i] < obs) { lessthan = lessthan + 1 }
	}
	greaterthan = greaterthan / length(test)
	lessthan = lessthan / length(test)
	Goodness.of.fit = cbind(lessthan, greaterthan)
	rownames(Goodness.of.fit) = "Goodness.of.fit"
	
	# Minimum
	
	test = colMins(SimY, na.rm = TRUE)
	obs.min = min(observations[ , index+2][!is.na(observations[ , index+2])])
	min.greaterthan = min.lessthan = 0
	for (i in 1:length(test)) {
		if(obs.min > test[i]) { min.greaterthan = min.greaterthan + 1
		} else if (obs.min < test[i]) { min.lessthan = min.lessthan + 1 }
	}
	min.greaterthan = min.greaterthan/length(test)
	min.lessthan = min.lessthan/length(test)
	min.test = cbind(min.greaterthan, min.lessthan)
	rownames(min.test) = "min.test"
		
	# Maximum
	
	test = colMaxs(SimY, na.rm = TRUE)
	obs.max = max(observations[ , index+2][!is.na(observations[ , index+2])])
	max.greaterthan = max.lessthan = 0
	for (i in 1:length(test)) {
		if(obs.max > test[i]) { max.greaterthan = max.greaterthan + 1
		} else if (obs.max < test[i]) { max.lessthan = max.lessthan + 1 }
	}
	max.greaterthan = max.greaterthan/length(test)
	max.lessthan = max.lessthan/length(test)
	max.test = cbind(max.greaterthan, max.lessthan)
	rownames(max.test) = "max.test"
	
	# Mean
	
	test = colMeans(SimY, na.rm = TRUE)
	obs.mean = mean(observations[ , index+2][!is.na(observations[ , index+2])])
	mean.greaterthan = mean.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.mean > test[i]) { mean.greaterthan = mean.greaterthan + 1 
		} else if (obs.mean < test[i]) { mean.lessthan = mean.lessthan + 1 }
	}
	mean.greaterthan = mean.greaterthan/length(test)
	mean.lessthan = mean.lessthan/length(test)
	mean.test = cbind(mean.greaterthan, mean.lessthan)
	rownames(mean.test) = "mean.test"
	
	# Maximum Change on a Treatment Switch
	
	SimY2 = SimY3 = NULL
	for (i in 2:nrow(SimY)) {
		if (observations$Treat2[i] != observations$Treat2[i - 1]) {
			SimY2 = rbind(SimY2, SimY[i, ] - SimY[i - 1, ])
			SimY3 = rbind(SimY3, observations[i, index+2] - observations[i - 1, index+2])
		}
	}
	
	test = colMaxs(SimY2, na.rm = TRUE)
	obs.max.change = colMaxs(SimY3, na.rm = TRUE)
	max.change.greaterthan = max.change.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.max.change > test[i]) { max.change.greaterthan = max.change.greaterthan + 1
		} else if (obs.max.change < test[i]) { max.change.lessthan = max.change.lessthan + 1 }
	}
	max.change.greaterthan = max.change.greaterthan/length(test)
	max.change.lessthan = max.change.lessthan/length(test)
	max.change.test = cbind(max.change.greaterthan, max.change.lessthan)
	rownames(max.change.test) = "max.change.test"
	
	# Maximum Day-to-Day Change (absolute)
	
	SimY2 = SimY3 = NULL
	for (i in nrow(SimY):2) {
		SimY2 = rbind(SimY[i, ] - SimY[i-1, ], SimY2)
		SimY3 = rbind(observations[i, index+2] - observations[i-1, index+2], SimY3)
	}
	test = colMaxs(abs(SimY2), na.rm = TRUE)
	obs.max.dtd.change = max(abs(SimY3), na.rm = TRUE)
	max.dtd.change.greaterthan = max.dtd.change.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.max.dtd.change > test[i]) {
			max.dtd.change.greaterthan = max.dtd.change.greaterthan + 1
		} else if (obs.max.dtd.change < test[i]) {
			max.dtd.change.lessthan = max.dtd.change.lessthan + 1
		}
	}
	max.dtd.change.greaterthan = max.dtd.change.greaterthan / length(test)
	max.dtd.change.lessthan = max.dtd.change.lessthan / length(test)
	max.dtd.change.test = cbind(max.dtd.change.greaterthan, max.dtd.change.lessthan)
	rownames(max.dtd.change.test) = "max.dtd.change.test"
	
	# Number of Day-to-Day increases
	
	SimY2 = SimY.inc = SimY.dec = SimY.same = SimY
	SimY2[1, ] = SimY.inc[1, ] = SimY.dec[1, ] = SimY.same[1, ] = rep(NA, ncol(SimY))
	for (i in 2:nrow(SimY)) {
		SimY2[i, ] = SimY[i, ] - SimY[i - 1, ]
		SimY.inc[i, ] = SimY.dec[i, ] = SimY.same[i, ] = SimY2[i, ]
		for (j in 1:ncol(SimY)) {
			if (!is.na(SimY2[i, j]) && SimY2[i, j] < 0) { 
				SimY.inc[i, j] = 0
				SimY.dec[i, j] = 1
				SimY.same[i, j] = 0
			} else if (!is.na(SimY2[i, j]) && SimY2[i, j] > 0) { 
				SimY.inc[i, j] = 1
				SimY.dec[i, j] = 0
				SimY.same[i, j] = 0
			} else if (!is.na(SimY2[i, j]) && SimY2[i, j] == 0) {
				SimY.inc[i, j] = 0
				SimY.dec[i, j] = 0
				SimY.same[i, j] = 1
			}
		}
	}
	
	Y2 = Y = observations[ , index+2]
	for (i in 2:length(Y)) {
		Y2[i] = Y[i] - Y[i - 1]
	}
	Y2[1] = NA
	inc = dec = same = rep(NA, length(Y))
	for (i in 1:length(Y)) {
		if (!is.na(Y2[i]) && Y2[i] < 0) {
			inc[i] = 0
			dec[i] = 1
			same[i] = 0
		} else if (!is.na(Y2[i]) && Y2[i] > 0) {
			inc[i] = 1
			dec[i] = 0
			same[i] = 0
		} else if (!is.na(Y2[i]) && Y2[i] == 0) {
			inc[i] = 0
			dec[i] = 0
			same[i] = 1
		}
	}
	
	test = colSums(SimY.inc, na.rm = TRUE)
	obs.inc = sum(inc, na.rm = TRUE)
	inc.greaterthan = inc.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.inc > test[i]) { inc.greaterthan = inc.greaterthan + 1 
		} else if (obs.inc < test[i]) {inc.lessthan = inc.lessthan + 1 }
	}
	inc.greaterthan = inc.greaterthan / length(test)
	inc.lessthan = inc.lessthan / length(test)
	inc.test = cbind(inc.greaterthan, inc.lessthan)
	rownames(inc.test) = "inc.test"
	
	# Number of Day-to-Day decreases
	
	test = colSums(SimY.dec, na.rm = TRUE)
	obs.dec = sum(dec, na.rm = TRUE)
	dec.greaterthan = dec.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.dec > test[i]) { dec.greaterthan = dec.greaterthan + 1
		} else if (obs.dec < test[i]) { dec.lessthan = dec.lessthan + 1 }
	}
	dec.greaterthan = dec.greaterthan / length(test)
	dec.lessthan = dec.lessthan / length(test)
	dec.test = cbind(dec.greaterthan, dec.lessthan)
	rownames(dec.test) = "dec.test"
	
	# Number of Day-to-Day Increments without Change
	
	test = colSums(SimY.same, na.rm = TRUE)
	obs.same = sum(same, na.rm = TRUE)
	same.greaterthan = same.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.same > test[i]) { same.greaterthan = same.greaterthan + 1
		} else if (obs.same < test[i]) { same.lessthan = same.lessthan + 1 }
	}
	same.greaterthan = same.greaterthan / length(test)
	same.lessthan = same.lessthan / length(test)
	same.test = cbind(same.greaterthan, same.lessthan)
	rownames(same.test) = "same.test"
	
	# Changes from Beginning to End of Treatment Period Within Block
	
	Treat.bounds = rep(0, length(observations$Treat2))
	Treat.bounds[1] = 1
	for (i in 2:length(observations$Treat2)) {
		if (observations$Treat2[i] != observations$Treat2[i - 1]) {
			Treat.bounds[i] = 1
			Treat.bounds[i - 1] = 1
		}
		if (observations$Block2[i] != observations$Block2[i - 1]) {
			Treat.bounds[i] = 1
			Treat.bounds[i - 1] = 1
		}
	}
	Treat.bounds[length(Treat.bounds)] = 1
	
	if (dim(table(Treat.bounds)) == 1) {
		for (i in seq(from = 2, to = length(Treat.bounds), by = 3)) {
			Treat.bounds[i] = 0
		}
	}
	
	while ((sum(Treat.bounds) / 2) != (round(sum(Treat.bounds) / 2))) {
		j = round(runif(1, min = 1, max = length(Treat.bounds)))
		Treat.bounds[j] = 0
	}
	
	a = 1
	
	while(a < 5) {
	
		j = length(Treat.bounds)
		while (is.na(observations[j, 3])) {
			Treat.bounds[j] = 0
			j = j - 1
			Treat.bounds[j] = 1
		}
		
		k = 1
		while (is.na(observations[k, 3])) {
			Treat.bounds[k] = 0
			k = k + 1
			Treat.bounds[k] = 1
		}
		if (k == 1) { k = 2 }
		if (j == nrow(SimY)) { j = nrow(SimY) - 1 }
		
		for (i in k:j) {
			if (Treat.bounds[i] == 1 && Treat.bounds[i - 1] == 0) {
				if (is.na(observations[i, 3])) {
					Treat.bounds[i] = 0
					Treat.bounds[i - 1] = 1
				}
			} else if (Treat.bounds[i] == 1 && Treat.bounds[i + 1] == 0) {
				if (is.na(observations[i, 3])) {
					Treat.bounds[i] = 0
					Treat.bounds[i + 1] = 1
				}
			}
		}
	
		a = a + 1
	}

	SimY2 = obs2 = NULL
	for (i in 1:length(Treat.bounds)) {
		if (Treat.bounds[i] == 1) { 
			SimY2 = rbind(SimY2, SimY[i, ]) 
			obs2 = rbind(obs2, observations[i, ])
		}
	}
	
	if (length(Treat.bounds) <= 16) {
		obs2[1, 3:9] = NA
	}
	
	if (sum(is.na(obs2)) != 0) { Fail = TRUE } else { Fail = FALSE }
	if (sum(is.na(SimY2)) != 0) { Fail = TRUE }
	
	if (!Fail) {
		obs.start = seq(from = 1, to = nrow(SimY2), by = 2)
		obs.end = obs.start + 1
		
		test = obs.diff = NULL
		for (i in 1:length(obs.start)) {
			test = rbind(test, SimY2[obs.end[i], ] - SimY2[obs.start[i], ])
			obs.diff = rbind(obs.diff, obs2[obs.end[i], ] - obs2[obs.start[i], ])
		}
		
		obs.diff = obs.diff[ , index+2]
		for (i in 1:length(obs.diff)) {
			if (is.na(obs.diff[i])) { obs.diff[i] = 0 }
		}
		
		greaterthan = lessthan = rep(0, 4)
		
		for (i in 1:ncol(test)) {
			if (obs.diff[1] > test[1, i]) { greaterthan = greaterthan + c(1, 0, 0, 0) 
			} else if (obs.diff[1] < test[1, i]) { lessthan = lessthan + c(1, 0, 0, 0) }
			if (obs.diff[2] > test[2, i]) { greaterthan = greaterthan + c(0, 1, 0, 0) 
			} else if (obs.diff[2] < test[2, i]) { lessthan = lessthan + c(0, 1, 0, 0) }
			if (obs.diff[3] > test[3, i]) { greaterthan = greaterthan + c(0, 0, 1, 0) 
			} else if (obs.diff[3] < test[3, i]) { lessthan = lessthan + c(0, 0, 1, 0) }
			if (obs.diff[4] > test[4, i]) { greaterthan = greaterthan + c(0, 0, 0, 1) 
			} else if (obs.diff[4] < test[4, i]) { lessthan = lessthan + c(0, 0, 0, 1) }
		}
		
		greaterthan = greaterthan / ncol(test)
		lessthan = lessthan / ncol(test)
		
		treat.diff.test = cbind(greaterthan, lessthan)
		rownames(treat.diff.test) = rep("treat.diff.test", nrow(treat.diff.test))
	} else {
		treat.diff.test = t(as.matrix(c(0, 1)))
		rownames(treat.diff.test) = "treat.diff.test"
	}
	
	
	# Range of Outcomes
	
	test = colMaxs(SimY, na.rm = TRUE) - colMins(SimY, na.rm = TRUE)
	obs.range = max(observations[ , index+2], na.rm = TRUE) - 
		min(observations[ , index+2], na.rm = TRUE)
	
	range.greaterthan = range.lessthan = 0
	for (i in 1:length(test)) {
		if (obs.range > test[i]) { range.greaterthan = range.greaterthan + 1
		} else if (obs.range < test[i]) {range.lessthan = range.lessthan + 1 }
	}
	range.greaterthan = range.greaterthan / length(test)
	range.lessthan = range.lessthan / length(test)
	range.test = cbind(range.greaterthan, range.lessthan)	
	rownames(range.test) = "range.test"
	
	# Last Day's observation
	
	test = observations[[index+2]][length(observations[[index+2]])]
	if (is.na(test)) { test = -100 }
	last.greaterthan = last.lessthan = 0
	for (i in 1:ncol(SimY)) {
		if (SimY[nrow(SimY), i] > test) { last.greaterthan = last.greaterthan + 1 
		} else if (SimY[nrow(SimY), i] < test) { last.lessthan = last.lessthan + 1 }
	}
	last.greaterthan = last.greaterthan / ncol(SimY)
	last.lessthan = last.lessthan / ncol(SimY)
	
	last.test = cbind(last.greaterthan, last.lessthan)
	rownames(last.test) = "last.test"
	
	# Maximum Consecutive Increases
	
	max.inc = rep(NA, ncol(SimY.inc))
	for (i in 1:ncol(SimY.inc)) {
		max.inc[i] = max(SimY.inc[ , i] * unlist(lapply(rle(SimY.inc[ , i])$lengths, seq_len)), na.rm = TRUE)
	}
	test = max(inc * unlist(lapply(rle(inc)$lengths, seq_len)), na.rm = TRUE)
	max.cons.inc.greaterthan = max.cons.inc.lessthan = 0
	for (i in 1:length(max.inc)) {
		if (max.inc[i] > test) { max.cons.inc.greaterthan = max.cons.inc.greaterthan + 1
		} else if (max.inc[i] < test) { max.cons.inc.lessthan = max.cons.inc.lessthan + 1 }
	}
	max.cons.inc.greaterthan = max.cons.inc.greaterthan / length(max.inc)
	max.cons.inc.lessthan = max.cons.inc.lessthan / length(max.inc)
	max.cons.inc.test = cbind(max.cons.inc.greaterthan, max.cons.inc.lessthan)
	rownames(max.cons.inc.test) = "max.cons.inc.test"
	
	# Maximum Consecutive Decreases
	
	max.dec = rep(NA, ncol(SimY.dec))
	for (i in 1:ncol(SimY.dec)) {
		max.dec[i] = max(SimY.dec[ , i] * unlist(lapply(rle(SimY.dec[ , i])$lengths, seq_len)), na.rm = TRUE)
	}
	test = max(dec * unlist(lapply(rle(dec)$lengths, seq_len)), na.rm = TRUE)
	max.cons.dec.greaterthan = max.cons.dec.lessthan = 0
	for (i in 1:length(max.dec)) {
		if (max.dec[i] > test) { max.cons.dec.greaterthan = max.cons.dec.greaterthan + 1
		} else if (max.dec[i] < test) { max.cons.dec.lessthan = max.cons.dec.lessthan + 1 }
	}
	max.cons.dec.greaterthan = max.cons.dec.greaterthan / length(max.dec)
	max.cons.dec.lessthan = max.cons.dec.lessthan / length(max.dec)
	max.cons.dec.test = cbind(max.cons.dec.greaterthan, max.cons.dec.lessthan)
	rownames(max.cons.dec.test) = "max.cons.dec.test"
	
	# Maximum Consecutive Same Observations
	
	max.same = rep(NA, ncol(SimY.same))
	for (i in 1:ncol(SimY.same)) {
		max.same[i] = max(SimY.same[ , i] * unlist(lapply(rle(SimY.same[ , i])$lengths, seq_len)), na.rm = TRUE)
	}
	test = max(same * unlist(lapply(rle(same)$lengths, seq_len)), na.rm = TRUE)
	max.cons.same.greaterthan = max.cons.same.lessthan = 0
	for (i in 1:length(max.same)) {
		if (max.same[i] > test) { max.cons.same.greaterthan = max.cons.same.greaterthan + 1
		} else if (max.same[i] < test) { max.cons.same.lessthan = max.cons.same.lessthan + 1 }
	}
	max.cons.same.greaterthan = max.cons.same.greaterthan / length(max.same)
	max.cons.same.lessthan = max.cons.same.lessthan / length(max.same)
	max.cons.same.test = cbind(max.cons.same.greaterthan, max.cons.same.lessthan)
	rownames(max.cons.same.test) = "max.cons.same.test"
	
	cat("+")
	flush.console()
	
	# Maximum variance within cycle
	
	per.starts = rep(NA, 1)
	per.starts[1] = 1
	for (i in 2:length(observations$Block2)) {
		if (observations$Block2[i] != observations$Block2[i-1] ||
		observations$Treat2[i] != observations$Treat2[i-1]) {
			per.starts[length(per.starts)+1] = i
		}
	}
	per.starts[length(per.starts)+1] = nrow(SimY) + 1
	per.vars = matrix(NA, nrow = length(per.starts)-1, ncol = ncol(SimY))
	for (j in 1:ncol(per.vars)) {
		for (i in 1:nrow(per.vars)) {
			per.vars[i, j] = var(SimY[per.starts[i]:(per.starts[i+1]-1), j])
		}
	}
	test = colMaxs(per.vars)
	
	obs.vars = rep(NA, nrow(per.vars))
	for (i in 1:length(obs.vars)) {
		obs.vars[i] = var(observations[[index+2]][per.starts[i]:(per.starts[i+1]-1)], na.rm = TRUE)
	}
	
	max.vars.greaterthan = max.vars.lessthan = 0
	for (i in 1:length(test)) {
		if (test[i] > max(obs.vars, na.rm = TRUE)) { max.vars.greaterthan = max.vars.greaterthan + 1 
		} else if (test[i] < max(obs.vars, na.rm = TRUE)) { max.vars.lessthan = max.vars.lessthan + 1 }
	}
	max.vars.greaterthan = max.vars.greaterthan / length(test)
	max.vars.lessthan = max.vars.lessthan / length(test)
	max.vars.test = cbind(max.vars.greaterthan, max.vars.lessthan)
	rownames(max.vars.test) = "max.vars.test"
	
	# Minimum Variance within Cycle
	
	test = colMins(per.vars)
	min.vars.greaterthan = min.vars.lessthan = 0
	for (i in 1:length(test)) {
		if (test[i] > min(obs.vars, na.rm = TRUE)) { min.vars.greaterthan = min.vars.greaterthan + 1
		} else if (test[i] > min(obs.vars, na.rm = TRUE)) { min.vars.lessthan = min.vars.lessthan + 1 }
	}
	min.vars.greaterthan = min.vars.greaterthan / length(test)
	min.vars.lessthan = min.vars.lessthan / length(test)
	min.vars.test = cbind(min.vars.greaterthan, min.vars.lessthan)
	rownames(min.vars.test) = "min.vars.test"
	
	# Number of Outcome Categories not observed per period
	
	if (index == 1 || index == 7) { ncats = 30 }
	if (index > 1 && index < 7) { ncats = 5 }
	if (index == 3) { ncats = 6 }
	
	obs.zero.per = rep(NA, length(per.starts) - 1)
	for (i in (1:(length(per.starts) - 1))) {
		obs.zero.per[i] = ncats - dim(table(observations[[index+2]][per.starts[i]:(per.starts[i+1]-1)]))
	}
	obs.zero = sum(obs.zero.per)
	
	test = rep(0, ncol(SimY))
	for (j in 1:length(test)) {
		if (j == 12000) { cat("+")
			flush.console() }
		zero.per = rep(NA, length(obs.zero.per))
		for (i in (1:(length(per.starts) - 1))) {
			zero.per[i] = ncats - dim(table(SimY[per.starts[i]:(per.starts[i+1]-1), j]))
		}
		test[j] = sum(zero.per)
	}
	
	out.cat.zero.greaterthan = out.cat.zero.lessthan = 0
	for (i in 1:length(test)) {
		if (test[i] > obs.zero) { out.cat.zero.greaterthan = out.cat.zero.greaterthan + 1
		} else if (test[i] < obs.zero) { out.cat.zero.lessthan = out.cat.zero.lessthan + 1 }
	}
	out.cat.zero.greaterthan = out.cat.zero.greaterthan / length(test)
	out.cat.zero.lessthan = out.cat.zero.lessthan / length(test)

	out.cat.zero.test = cbind(out.cat.zero.greaterthan, out.cat.zero.lessthan)
	rownames(out.cat.zero.test) = "out.cat.zero.test"
	
	# Number of Days where A is better than B
	
	obs.A = observations[observations$Treat2 == 0, index+2]
	obs.B = observations[observations$Treat2 == 1, index+2]
	obs.A = obs.A[!is.na(obs.A)]
	obs.B = obs.B[!is.na(obs.B)]
	
	obs.A.lt.B = 0
	for (i in 1:length(obs.A)) {
		for (j in 1:length(obs.B)) {
			if (obs.A[i] < obs.B[j]) { obs.A.lt.B = obs.A.lt.B + 1 }
		}
	}
	
	SimY = cbind(SimY, observations$Treat2)
	SimY.A = SimY[SimY[, ncol(SimY)] == 0, ]
	SimY.B = SimY[SimY[, ncol(SimY)] == 1, ]
	SimY = SimY[ , -ncol(SimY)]
	SimY.A = SimY.A[ , -ncol(SimY.A)]
	SimY.B = SimY.B[ , -ncol(SimY.B)]
	
	for (i in nrow(observations):1) {
		if (is.na(observations[[index+2]][i])) {
			SimY.A = SimY.A[-i, ]
			SimY.B = SimY.B[-i, ]
		}
	}
	
	test = rep(0, ncol(SimY))
	for (i in 1:length(test)) {
		for (j in 1:nrow(SimY.A)) {
			for (k in 1:nrow(SimY.B)) {
				if (SimY.A[j, i] < SimY.B[k, i]) { test[i] = test[i] + 1 }
			}
		}
	}
	
	A.lt.B.greaterthan = A.lt.B.lessthan = 0
	for (i in 1:length(test)) {
		if (test[i] > obs.A.lt.B) { A.lt.B.greaterthan = A.lt.B.greaterthan + 1
		} else if (test[i] < obs.A.lt.B) { A.lt.B.lessthan = A.lt.B.lessthan + 1 }
	}
	A.lt.B.greaterthan = A.lt.B.greaterthan / length(test)
	A.lt.B.lessthan = A.lt.B.lessthan / length(test)
	
	A.lt.B.test = cbind(A.lt.B.greaterthan, A.lt.B.lessthan)
	rownames(A.lt.B.test) = "A.lt.B.test"
	
	##################
	# Compiling Output
	##################
	
	cat("\n")
	
	tests = rbind(Goodness.of.fit, min.test, max.test, mean.test, max.change.test, max.dtd.change.test, inc.test, 
		dec.test, same.test, treat.diff.test, range.test, last.test, max.cons.inc.test, max.cons.dec.test, 
		max.cons.same.test, max.vars.test, min.vars.test, out.cat.zero.test, A.lt.B.test)
	colnames(tests) = c("greaterthan", "lessthan")
	
	summary = tests[ , 1]
	for (i in 1:length(summary)) {
		summary[i] = 1
		if (tests[i, 1] < 0.01 || tests[i, 1] > 0.99 || tests[i, 2] < 0.01 || tests[i, 2] > 0.99) { summary[i] = 0 }
	}
	summary = as.matrix(summary)
	
	out = list("SimY" = SimY, "tests" = tests, "Summary" = summary)
		
	return(out)
	
}
