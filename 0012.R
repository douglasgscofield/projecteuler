this.prob <- "0012"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")
# tri(i)      : i-th triangular number
# divisors(x) : integer divisors of x


p.0012 = function(n = 500) {
    # value of first triangle number i (sum(1:i)) with *over* n divisors
    i = 1
    while (TRUE) {
        d = divisors(tri(i))
        if (i %% 100 == 0)
            cat("tri", i, " val =", tri(i), " n div =", length(d), "\n")
        if (length(d) > n)
            break
        i = i + 1
    }
    tri(i)
}

