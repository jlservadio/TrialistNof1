model.ord <- function (Covs, prior, path, mod.id) 
{

	cat("model\n{", file = paste(path, "model.txt", sep = ""))

	if (mod.id == 1) {
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
		for (i in 2:(ncat-1)) {
			dc[i] ~ ", prior$Prior.dc, "
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
		   c[i] <- c[i-1] + dc[i]
		}", 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 2) {
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
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope ~ ", prior$Prior.slope,  
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")	
	} else if (mod.id == 3 && ncol(Covs) == 1) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope*x1[i]		
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 3 && ncol(Covs) == 2) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope1*x1[i] + slope2*x2[i]	
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 3 && ncol(Covs) == 3) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope1*x1[i] + slope2*x2[i]	+ slope3*x3[i]
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope,
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4) {	
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope*logit(Q[i - 1, r])
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}		
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope*logit(Q0)
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.1) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		for (i in 1:nobs) {
			x[i] ~ dunif(j[i], l[i])
		}
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope,
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 2) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x1[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x1[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope,
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 3) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x1[i] + slope3*x2[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x1[1] + slope3*x2[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 4.2 && ncol(Covs) == 4) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x1[i] + slope3*x2[i] + slope4*x3[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x1[1] + slope3*x2[1] + slope4*x3[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope,
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.1) {	
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r]  + slope1*z1[i] + slope2*z2[i]
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ ", prior$Prior.dc, "
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
		   c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope,
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.2) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope1*x[i] + slope2*z1[i] + slope3*z2[i]
			}
			Y[i] ~ dcat(p[i,])
			x[i] ~ dunif(j[i], l[i])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope,
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.3 && ncol(Covs) == 3) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope1*x1[i] + slope2*z1[i] + slope3*z2[i]		
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.3 && ncol(Covs) == 4) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope1*x1[i] + slope2*x2[i]	+ slope3*z1[i] + slope4*z2[i]
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.3 && ncol(Covs == 5)) {
		cat("
		for (i in 1:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i,r]) <- beta*Treat[i] - c[r] + slope1*x1[i] + slope2*x2[i]	+ slope3*x3[i] + slope4*z1[i] + slope5*z2[i]
			}
			Y[i] ~ dcat(p[i,])
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, "
		slope5 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.4) {	
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*z1[i] + slope3*z2[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*z1[1] + slope3*z2[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.41) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x[i] + slope3*z1[i] + slope4*z2[i]
			}
		}




		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		for (i in 1:nobs) {
			x[i] ~ dunif(j[i], l[i])
		}
		for (i in 2:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x[1] + slope3*z1[1] + slope4*z2[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 4) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x1[i] + slope3*z1[i] + slope4*z2[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[i] - c[r] + slope1*logit(Q0) + slope2*x1[1] + slope3*z1[1] + slope4*z2[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 5) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x1[i] + slope3*x2[i] + slope4*z1[i] + slope5*z2[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
		
		for (i in 2:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x1[1] + slope3*x2[1] + slope4*z1[1] + slope5*z2[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, "
		slope5 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	} else if (mod.id == 5.42 && ncol(Covs) == 6) {
		cat("
		for (i in 2:nobs) {
			p[i,1] <- 1 - Q[i,1]
			for (r in 2:(ncat-1)) {
				p[i,r] <- Q[i,r-1] - Q[i,r]
			}
			p[i,ncat] <- Q[i,(ncat-1)]
			for (r in 1:(ncat-1)) {
				logit(Q[i, r]) <- beta*Treat[i] - c[r] + slope1*logit(Q[i - 1, r]) + slope2*x1[i] + slope3*x2[i] + slope4*x3[i] + 
					slope5*z1[i] + slope6*z2[i]
			}
		}
		



		for (r in 1:ncat) {
			p[1, r] <- p[2, r]
		}
		
		for (i in 2:(ncat-1)) {
			dc[i] ~ dunif(lower.dc, upper.dc)
		}
		c[1] <- dc[1]
		for (i in 2:(ncat-1)) {
			c[i] <- c[i-1] + dc[i]
		}
	
		for (i in 1:(ncat-1)) {
			logit(Q[1, i]) <- beta*Treat[1] - c[i] + slope1*logit(Q0) + slope2*x1[1] + slope3*x2[1] + slope4*x3[1] + 
				slope5*z1[1] + slope6*z2[1]
		}
		Q0 ~ dunif(0, 0.2)
		for (i in 1:nobs) {
			Y[i] ~ dcat(v)
		}
		slope1 ~ ", prior$Prior.slope, "
		slope2 ~ ", prior$Prior.slope, "
		slope3 ~ ", prior$Prior.slope, "
		slope4 ~ ", prior$Prior.slope, "
		slope5 ~ ", prior$Prior.slope, "
		slope6 ~ ", prior$Prior.slope, 
		file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	}
	
	cat("
	dc[1] ~", prior$Prior.c1, "
	beta ~ ", prior$Prior.beta, "
	or <- exp(beta)
	}", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
	
}
