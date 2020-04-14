this.prob <- "0003"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")  # for generate_primes() from library(primes)


p.0003 = function(n=10403) {#600851475143) {
    f = integer(0)
    if (n < 2) return(f)
    #primes = prime_sieve(floor(sqrt(n)))
    primes = generate_primes(max=floor(sqrt(n)))  # instead use primes package
    for (p in primes) {
        if (p * p > n) break
        while (n %% p == 0) {
            f = c(f, p)
            n = floor(n / p)
        }
    }
    if (n > 1)
        f = c(f, n)
    f
}
