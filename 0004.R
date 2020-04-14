this.prob <- "0004"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


source("utilities.R")  # for library(stringi) and stri_reverse()


p.0004 = function(d1 = 3, d2 = 3) {
    # largest palindrome made from product of d1- and d2-digit numbers
    # palindrome product minus its reverse is 0
    p = integer(0)
    for (i in seq(10^(d1 - 1), 10^d1 - 1)) {
        for (j in seq(10^(d2 - 1), 10^d2 - 1)) {
            if (i*j - as.integer(stri_reverse(as.character(i*j))) == 0) {
                p = c(p, i*j)
            }
        }
    }
    max(p)
}

