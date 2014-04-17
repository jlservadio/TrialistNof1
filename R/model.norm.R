model.norm <- function (nobs, Covs, prior, varprior, path, i) 
{
    cat("model\n{", file = paste(path, "model.txt", sep = ""))
    model.norm.obs(nobs, Covs, prior, path, i)
    var.hom(prior, varprior, path)
    cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
}
