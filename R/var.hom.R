var.hom <- function (prior, varprior, path) 
{
    if (varprior[[1]] == "prec") {
        cat("\n            prec ~ ", prior$Prior.prec, "\n            sigma <- 1/prec", file = paste(path, "model.txt", 
			sep = ""), append = T, sep = "")
    }
    else if (varprior[[1]] == "Sd") {
        cat("\n            Sd ~ ", prior$Prior.prec, "\n            prec <- 1/pow(Sd,2)\n            Sigma <- 1/prec", 
			file = paste(path, "model.txt", sep = ""), append = T, sep = "")
    }
}

