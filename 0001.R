this.prob <- "0001"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0001 = function (mx=999) {
    # sum of multiples of 3 or 5 less than 1000
    threes = seq(3, mx, by=3)
    fives = seq(3, mx, by=5)
    a = unique(sort(c(threes, fives)))
    ans = sum(a)
}

