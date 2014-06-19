model.norm <- function (nobs, Covs, prior, varprior, path, mod.id) 
{
    cat("model\n{", file = paste(path, "model.txt", sep = ""))
    model.norm.obs(nobs, Covs, prior, path, mod.id)
    var.hom(prior, varprior, path)
    cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
}
