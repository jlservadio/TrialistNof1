treat.diff.change <-
function (treat.diff, score.range) 
{
    treat.diff.change = treat.diff/score.range
    treat.diff.change.1 = as.vector(c(quantile(treat.diff.change, 0.025), median(treat.diff.change), quantile(treat.diff.change, 0.975)))
    treat.diff.change.2 = as.numeric(table(cut(treat.diff.change, breaks = c(-1, -0.2, 0, 0.2, 1))))/length(treat.diff.change)
    return(list(treat.diff.change.1, treat.diff.change.2))
}
