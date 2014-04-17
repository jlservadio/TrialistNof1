model.ord <- function (Covs, prior, path, i) 
{
	if (i == 1) {
		cat("model\n{", file = paste(path, "model.txt", sep = ""))
		cat("
		for (i in 1:nobs) {
		   p[i,1] <- 1 - Q[i,1]
		   for (r in 2:(ncat-1)) {
			  p[i,r] <- Q[i,r-1] - Q[i,r]
		   }
		   p[i,ncat] <- Q[i,(ncat-1)]
		   for (r in 1:(ncat-1)) {
			  logit(Q[i,r]) <- beta*Treat[i] - c[r]
		   }
		   Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[i] ~", prior$Prior.dc, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
		   c[i] <- c[i-1] + dc[i]
		}",file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[1] ~", prior$Prior.c1, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		or <- exp(beta)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (i == 2) {
		cat("model\n{", file = paste(path, "model.txt", sep = ""))
		cat("
		for (i in 1:nobs) {
		   p[i,1] <- 1 - Q[i,1]
		   for (r in 2:(ncat-1)) {
			  p[i,r] <- Q[i,r-1] - Q[i,r]
		   }
		   p[i,ncat] <- Q[i,(ncat-1)]
		   for (r in 1:(ncat-1)) {
			  logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope*x[i]
		   }
		   Y[i] ~ dcat(p[i,])
		   x[i] ~ dunif(j[i], l[i])
		}
		for (i in 2:(ncat-1)) {", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[i] ~", prior$Prior.dc, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
		   c[i] <- c[i-1] + dc[i]
		}",file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[1] ~", prior$Prior.c1, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		or <- exp(beta)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (i == 3) {
		cat("model\n{", file = paste(path, "model.txt", sep = ""))
		cat("
		for (i in 1:nobs) {
		   p[i,1] <- 1 - Q[i,1]
		   for (r in 2:(ncat-1)) {
			  p[i,r] <- Q[i,r-1] - Q[i,r]
		   }
		   p[i,ncat] <- Q[i,(ncat-1)]
		   for (r in 1:(ncat-1)) {
			  logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope*x[i]
		   }
		   Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[i] ~", prior$Prior.dc, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
		   c[i] <- c[i-1] + dc[i]
		}",file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[1] ~", prior$Prior.c1, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		or <- exp(beta)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (i == 4) {
		cat("model\n{", file = paste(path, "model.txt", sep = ""))
		cat("
		for (i in 2:nobs) {
		   p[i,1] <- 1 - Q[i,1]
		   for (r in 2:(ncat-1)) {
			  p[i,r] <- Q[i,r-1] - Q[i,r]
		   }
		   p[i,ncat] <- Q[i,(ncat-1)]
		   for (r in 1:(ncat-1)) {
			  logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope*Y[i - 1]
		   }
		   Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[i] ~", prior$Prior.dc, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
		   c[i] <- c[i-1] + dc[i]
		}",file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		   dc[1] ~", prior$Prior.c1, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		slope ~ ", prior$Prior.slope, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("
		or <- exp(beta)", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
		cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
}
