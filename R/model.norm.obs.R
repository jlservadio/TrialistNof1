model.norm.obs <- function (nobs, Covs, prior, path, mod.id) 
{
	if (mod.id %in% c(1, 2, 3, 5.1, 5.2, 5.3)) {
		cat("
		for (i in 1:", nobs, ") {", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42)) {
		cat("
		for (i in 2:", nobs, ") {", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
		
	if (mod.id == 1) {
		cat("
		mu[i] <- alpha + beta*Treat[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 2) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope*x[i]
		x[i] ~ dunif(j[i], l[i])", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 3) {
		if (ncol(Covs) == 1) {
			cat("
			mu[i] <- alpha + beta*Treat[i] + slope*x1[i]", 
			file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		} else if (ncol(Covs == 2)) {
			cat("
			mu[i] <- alpha + beta*Treat[i] + slope1*x1[i] + slope2*x2[i]", 
			file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		} else if (ncol(Covs) == 3) {
			cat("
			mu[i] <- alpha + beta*Treat[i] + slope1*x1[i] + slope2*x2[i] + slope3*x3[i]", 
			file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		}
	} else if (mod.id == 4) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope*Y[i-1]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.1) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x[i]
		x[i] ~ dunif(j[i], l[i])", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 2) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x1[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 3) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x1[i] + slope3*x2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 4) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x1[i] + slope3*x2[i] + slope4*x3[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.1) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*z1[i] + slope2*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.2) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*x[i] + slope2*z1[i] + slope3*z2[i]
		x[i] ~ dunif(j[i], l[i])", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.3 && ncol(Covs) == 3) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*x1[i] + slope2*z1[i] + slope3*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.3 && ncol(Covs) == 4) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*x1[i] + slope2*x2[i] + slope3*z1[i] + slope4*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.3 && ncol(Covs) == 5) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*x1[i] + slope2*x2[i] + slope3*x3[i] + slope4*z1[i] + slope5*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.4) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*z1[i] + slope3*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.41) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x[i] + slope3*z1[i] + slope4*z2[i]
		x[i] ~ dunif(j[i], l[i])", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 4) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x1[i] + slope3*z1[i] + slope4*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 5) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x1[i] + slope3*x2[i] + slope4*z1[i] + slope5*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 6) {
		cat("
		mu[i] <- alpha + beta*Treat[i] + slope1*Y[i-1] + slope2*x1[i] + slope3*x2[i] + slope4*x3[i] + slope5*z1[i] + slope6*z2[i]", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
	
	cat("
	Y[i] ~ dnorm(mu[i],prec)

	}
	alpha ~ ", prior$Prior.alpha, "
	beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	
	if (mod.id %in% c(2, 4) || (mod.id == 3 && ncol(Covs) == 1)) {
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id %in% c(4.1, 5.1) || (mod.id %in% c(3, 4.2) && ncol(Covs) == 2)) {
		cat("
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id %in% c(5.2, 5.4) || (mod.id %in% c(3, 4.2, 5.3) && ncol(Covs) == 3)) {
		cat("
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.41 || (mod.id %in% c(4.2, 5.3, 5.42) && ncol(Covs) == 4)) {
		cat("
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id %in% c(5.3, 5.42) && ncol(Covs) == 5) {
		cat("
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, "
		slope5 ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 6) {
		cat("
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, "
		slope5 ~ ", prior$Prior.slope, "
		slope6 ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
	
	if (mod.id == 4) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope*Y0

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.1) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x[1]

		x[1] ~ dunif(j[1], l[1])
		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 2) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x1[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 3) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x1[1] + slope3*x2[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 4) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x1[1] + slope3*x2[1] + slope4*x3[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.4) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*z1[1] + slope3*z2[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.41) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)

		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x[1] + slope3*z1[1] + slope4*z2[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 4) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x1[1] + slope3*z1[1] + slope4*z2[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 5) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x1[1] + slope3*x2[1] + slope4*z1[1] + slope5*z2[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 6) {
		cat("
		Y[1] ~ dnorm(mu[1], prec)
		mu[1] <- alpha + beta*Treat[1] + slope1*Y0 + slope2*x1[1] + slope3*x2[1] + slope4*x3[1] + slope5*z1[1] + slope6*z2[1]

		Y0 ~ dnorm(v, 2)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
	
}
