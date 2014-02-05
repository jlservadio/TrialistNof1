model.norm.obs <-
function (nobs, Covs, prior, path) 
{
    cat("
    for (i in 1:", nobs, ") {
       mu[i] <- alpha + beta*Treat[i]
       Y[i] ~ dnorm(mu[i],prec)
    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
    cat("
    alpha ~ ", prior$Prior.alpha, file = paste(path, "model.txt", sep = ""), append = T, sep = "") 
    cat("
    beta ~ ", prior$Prior.beta, file = paste(path, "model.txt", sep = ""), append = T, sep = "")
}
