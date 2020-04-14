this.prob <- "0024"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))



p.0024 = function(digits=0:9, term=1000000) {
    # term-th lexicographical permutation of digits?
    # create permutations in sorted order
    digits = sort(digits)
    # abcd in sorted order, permutations in sorted order are
    # abcd
    # abdc
    # acbd
    # acdb
    # adbc
    # adcb
    # bacd
    # badc
    # bcad
    # bcda
    # bdac
    # bdca
    # ....
}

