this.prob <- "0009"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0009 = function(n = 1000) {
    for (b in 2:((n / 2) - 1)) {
        for (a in 1:(b - 1)) {
            c = n - b - a
            if (c <= b) break
            if (a^2 + b^2 == c^2) {
                cat("a =", a, "b =", b, "c =", c, "\n")
                return(a * b * c)
            }
        }
    }
    paste("last values which failed were a =", a, "b =", b, "c =", c, "\n")
}

