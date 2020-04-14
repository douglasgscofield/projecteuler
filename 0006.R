this.prob <- "0006"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0006 = function(n=10) {
    # difference between sum of squares 1..n and square of sum
    sq_sum = ((n*(n+1))/2)^2
    sq_sum - sum((1:n)^2)
}

