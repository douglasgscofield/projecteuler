this.prob <- "0010"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")


p.0010 = function(n = 2000000) {
    # sum of all primes less than n
    # integer overflow, so put them in a 64-bit int
    vp = generate_primes(max = n - 1)
    sm = integer64(1)
    for (p in vp) sm = sm + p
    sm
}

