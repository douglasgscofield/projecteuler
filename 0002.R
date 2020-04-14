this.prob <- "0002"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0002 = function(mx=4000000, v=FALSE) {
    # sum of even terms of fibonacci sequence <= 4000000
    even.sum = 0
    p1 = 1; p2 = 1
    while (p2 <= mx) {
        if (p2 %% 2 == 0) {
            even.sum = even.sum + p2
        }
        p3 = p1 + p2
        p1 = p2
        p2 = p3
        if (v) cat("end of loop: p1 p2 even.sum", p1, p2, even.sum, "\n")
    }
    even.sum
}

