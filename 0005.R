this.prob <- "0005"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0005 = function(min=1, max=20) {
    # smallest positive number evenly divisible by all numbers from min to max
    n = max
    while (n) {
        divs = 0
        for (i in min:max) if (n %% i == 0) divs = divs + 1
        if (divs == max - min + 1) return(n)
        n = n + max
    }
    return(-1)
}

