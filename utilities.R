library(primes)   # generate_primes()
library(stringi)  # stri_reverse()
library(bit64)    # various 64-bit integer routines


# coll(v)     : print vector v as collapsed string
coll = function(v) paste(collapse="", v)


# tri(i)      : i-th triangular number 1+2+3+...+i
tri = function(i) sum(1:i)


# divisors(x) : sorted list of integer divisors of x, including 1 and x
divisors = function(x) {
    if (x == 1) return(1)
    ans = c(1L, x)  # all numbers have at least this
    if (x < 4) return(ans)
    # when we find a lower divisor i, the max novel divisor we can have is less than x / i
    i = 2L
    max.cand = as.integer(floor(x/2))
    while (i < max.cand) {
        if (x %% i == 0) {
            ans = c(ans, i, x / i)
            max.cand = x / i
        }
        i = i + 1
    }
    sort(ans)
}


# collatz(x)  : Collatz sequence for number x
#     https://esolangs.org/wiki/Collatz_sequence
#     https://en.wikipedia.org/wiki/Collatz_conjecture

collatz = function(x) {
    ans = x
    while (x != 1) {
        x = ifelse(x %% 2 == 0, x/2, 3*x + 1)
        ans = c(ans, x)
    }
    ans
}


# prime_sieve(n): all primes up to n using Sieve of Sundaram
#     https://en.wikipedia.org/wiki/Sieve_of_Sundaram
#
#### instead use generate_primes(max=...) from library(primes)
#
# prime_sieve = function(n) {
#     u = floor(n / 2)
#     sv = rep(TRUE, u)
#     for (j in 1:u) {
#         for (i in 1:j) {
#             cand = i + j + 2*i*j; if (cand <= u) sv[cand] = FALSE
#         }
#     }
#     c(2, (which(sv) * 2) + 1)
# }



# big_mult(s, f): multiply big int s by regular int f
big_mult = function(s, f) {  # big integer multiplication (s is the big int)
    # s is vector of base-10 digits, s[1] is 10^0 place and s[i] is 10^(i-1) place
    # f is a 'standard' integer which is converted to s-format
    # answer is returned in vector format
    #cat("mult: multiplying", coll(rev(s)), "by", f, "\n")
    big_mult9 = function(s, single_f) {
        # s is multiplied by single-digit single_f
        # generalised mult2() from p.0016
        final.p = length(s) + 1
        ans = integer(final.p)
        carry = 0
        for (p in 1:length(s)) {
            np = s[p] * single_f + carry
            carry = np %/% 10; np = np %% 10
            ans[p] = np
        }
        if (carry) ans[final.p] = carry
        else ans = ans[-final.p]
        ans
    }
    ff = as.integer(rev(strsplit(as.character(f), NULL)[[1]]))  # split f
    new.s = integer(0)
    for (i in 1:length(ff)) {
        #cat(i, ": calculating for", ff[i], "\n")
        this.s = big_mult9(s, ff[i])
        if (i > 1)
            this.s = c(rep(0, i-1), this.s)
        new.s = big_plus(new.s, this.s)
    }
    return(new.s)
}

# big_plus(a, b):  add big integers a and b
big_plus = function(a, b) {  # big integer addition
    # a and b are vectors of base-10 digits, a[i] is 10^(i-1) place
    # answer is returned in vector format
    final.a = length(a); final.b = length(b)
    final.p = max(final.a, final.b) + 1
    stopifnot(final.p > 1)
    ans = integer(final.p)
    carry = 0
    for (p in 1:(final.p - 1)) {
        np = carry
        if (p <= final.a) np = np + a[p]
        if (p <= final.b) np = np + b[p]
        carry = np %/% 10; np = np %% 10
        ans[p] = np
    }
    if (carry) ans[final.p] = carry
    else ans = ans[-final.p]
    ans
}

