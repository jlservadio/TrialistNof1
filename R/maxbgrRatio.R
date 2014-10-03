maxbgrRatio <- function (x) 
{
    t1 = mcmc(x[, 1, drop = F])
    t2 = mcmc(x[, 2, drop = F])
    t3 = mcmc(x[, 3, drop = F])
    cols.to.drop = seq(dim(t1)[2])[apply(t1, 2, var) == 0]
    if (length(cols.to.drop > 0)) {
        t1 = t1[, -cols.to.drop, drop = F]
        t2 = t2[, -cols.to.drop, drop = F]
        t3 = t3[, -cols.to.drop, drop = F]
    }
    max.bgrRatio = max(gelman.diag(list(t1, t2, t3), autoburnin = F)[[1]][, 1])
    return(max.bgrRatio)
}

