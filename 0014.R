this.prob <- "0014"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0014 = function(n=1000000L, progress=floor(n/10)) {
    # which Collatz sequence, starting from less than n, produces the longest sequence?
    # x even: x = n / 2
    # x odd : x = 3*n + 1
    # don't use collatz() from utilities.R, use our own faster version, because once
    # we've hit a precomputed number, we know how long the remaining sequence is
    pre = integer(n-1)
    l.collatz = function(x) {
        ans = as.integer(x)
        while (x != 1) {
            if (x <= n-1 && pre[x] > 0) {
                #cat("found pre[",x,"] =", pre[x], "\n")
                return(length(ans) - 1 + pre[x])
            }
            x = ifelse(x %% 2 == 0, x / 2, 3*x + 1)
            ans = c(ans, x)
        }
        length(ans)
    }
    for (s in 1:(n - 1)) {
        l = l.collatz(s)
        pre[s] = l
        if (s %% progress == 0) cat("start =", s, " len =", l, " max =", max(pre), "\n")
    }
    max.s = which(pre == max(pre))
    cat("max start =", max.s, " length =", max(pre), "\n")
    max.s
}
