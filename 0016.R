this.prob <- "0016"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0016 = function(n=1000) {
    # what is the sum of the digits in 2^n?  question has n = 1000
    # at first i thought the sum of the digits of a number is equal to the sum
    # of its components, e.g.,
    #     sumd(56) = sumd(41) + sumd(15)
    # but that didn't hold.  so i implemented a 2-multiplier.  much cleaner
    #
    # an even cleaner solution is with the gmp package.
    #     library(gmp)
    #     sum(as.integer(strsplit(as.character(as.bigz(2^1000)), NULL)[[1]]))
    #
    mult2 = function(s) {
        # s is a vector of digits of a base-10 number we want to multiply by 2 s[1]
        # is the 1s place (10^0), s[2] is the 10s place (10^1), etc.  If it grows
        # in size, it can only grow by one digit, and if so, carry can only be 1
        final.p = length(s) + 1
        ans = integer(final.p)
        carry = 0
        for (p in 1:length(s)) {
            np = s[p] * 2 + carry
            if (np > 9) { np = np %% 10; carry = 1 }
            else { carry = 0 }
            ans[p] = np
        }
        if (carry)
            ans[final.p] = 1
        else ans = ans[-final.p]
        ans
    }
    ans = 1
    for (i in 1:n)
        ans = mult2(ans)
    cat("2 ^", n, " ", length(ans), "digits, sum digits =", sum(ans), " val =", paste(collapse="", rev(ans)), "\n")
    sum(ans)
}

