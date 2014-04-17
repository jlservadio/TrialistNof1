model.norm.obs <- function (nobs, Covs, prior, path, i) 
{
	if (i == 1) {
		cat("
		for (i in 1:", nobs, ") {
		   mu[i] <- alpha + beta*Treat[i]
		   Y[i] ~ dnorm(mu[i],prec)
		}", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		alpha ~ ", prior$Prior.alpha, file = paste(path, "model.txt", sep = ""), append = T, sep = "") 
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (i == 2){
		cat("
		for (i in 1:", nobs, ") {
		   mu[i] <- alpha + beta*Treat[i] + slope*x[i]
		   Y[i] ~ dnorm(mu[i],prec)
		   x[i] ~ dunif(j[i], l[i])
		}", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		alpha ~ ", prior$Prior.alpha, file = paste(path, "model.txt", sep = ""), append = T, sep = "") 
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (i == 3){
		cat("
		for (i in 1:", nobs, ") {
		   mu[i] <- alpha + beta*Treat[i] + slope*x[i]
		   Y[i] ~ dnorm(mu[i],prec)
		}", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		alpha ~ ", prior$Prior.alpha, file = paste(path, "model.txt", sep = ""), append = T, sep = "") 
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (i == 4) {
		cat("
		for (i in 2:", nobs, ") {
		   mu[i] <- alpha + beta*Treat[i] + slope*Y[i-1]
		   Y[i] ~ dnorm(mu[i],prec)
		}", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		alpha ~ ", prior$Prior.alpha, file = paste(path, "model.txt", sep = ""), append = T, sep = "") 
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
}
