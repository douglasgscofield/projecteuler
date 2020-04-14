this.prob <- "0007"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


library(primes)  # generate_primes(max=...)


p.0007 = function(n=10001) {
    # what is the n-th prime number? starting with 2
    # Prime Number Theorem expects x/log(x) primes less than x
    # first, approximate power of 10 needed
    p = 1
    while (1) { 
        x = 10^p
        if (x/log(x) > n) {
            primes = generate_primes(max=x)
            if (length(primes) >= n)
                return(primes[n])
        }
        p = p + 1
    }
    
}

