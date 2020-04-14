this.prob <- "0020"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")   # big_mult(s, f)


p.0020 = function(n = 100) {
    # sum of digits in n!
    v = 1
    for (i in 2:n)
        v = big_mult(v, i)
    cat(n, "! has", length(v), "digits, sum digits =", sum(v), " val =", paste(collapse="", rev(v)), "\n")
}
