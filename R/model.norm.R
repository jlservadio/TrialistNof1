model.norm <-
function (nobs, Covs, prior, varprior, path) 
{
    cat("model\n{", file = paste(path, "model.txt", sep = ""))
    model.norm.obs(nobs, Covs, prior, path)
    var.hom(prior, varprior, path)
    cat("\n    }", file = paste(path, "model.txt", sep = ""), append = T, sep = "")
}
