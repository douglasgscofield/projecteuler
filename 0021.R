this.prob <- "0021"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")  # for divisors()


p.0021 = function(n = 10000) {
    # Let d(n) be defined as the sum of proper divisors of n (numbers less than n
    # which divide evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a
    # and b are an amicable pair and each of a and b are called amicable numbers.
    # 
    # Evaluate the sum of all the amicable numbers under n=10000.
    #
    # already have divisors() from p.0012(), make it global
    is.amicable = logical(n)
    d = function(x) {
        divs = divisors(x)
        return(sum(divs[divs < x]))
    }
    for (a in 2:(n - 1)) {
        if (is.amicable[a]) next
        b = d(a)
        if (b != a && b < n && b > 1) { # candidate
            if (d(b) == a) {
                is.amicable[c(a, b)] = TRUE
                cat(a, "and", b, "amicable: d(a) =", d(a), " d(b) =", d(b), "\n")
            }
        }
    }
    sum(which(is.amicable))
}

