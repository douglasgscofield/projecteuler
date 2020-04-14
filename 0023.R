this.prob <- "0023"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")


p.0023 = function(n = 28123) {
    # find all numbers <= n that *cannot* be written as sum of two abundant numbers
    # > 28123, all can be so written
    # identify abundant numbers in this range
    # 'abundant' vector set by compute.abundant()
    # a bug in proper.divisors / divisors: I was keeping all terms of the factor tree
    # rather than simplifying, so was calling 16 an abundant number; downloaded
    # https://oeis.org/A005101/b005101.txt which are the first 10000 abundant numbers
    # and found the bug that way
    proper.divisors = function(x) {
        if (x == 1) return(NA)
        divs = unique(divisors(x))  # bug exposed by problem 23, '16' is not abundant
        return(divs[divs < x])
    }
    number.type = function(x) {  #0 = perfect, -1 = deficient, +1 = abundant
        if (x == 1) return(NA)
        spd = sum(proper.divisors(x))
        ifelse(spd == x, 0, ifelse(spd < x, -1, +1))
    }
    compute.abundant = function(n=28123) {
        ab = 2:n
        ab[sapply(ab, number.type) > 0]
    }

    na_sum = 0
    abundant <- compute.abundant(n)
    for (i in 1:n) {
        #if (i %% 1000 == 0) cat("p.0023: working on", i, "\n")
        j = i - abundant
        if (! any(j %in% abundant))
            na_sum = na_sum + i
    }
    na_sum
}
