this.prob <- "0017"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0017 = function(n=1000) {
    # what is the total number of letters in the British English version of the
    # numbers from 1 to n?  excluding spaces and hyphens, but including "and"
    stopifnot(n <= 1000)
    as.english = function(x) {
        e = character(0)
        # leave out spaces and hyphens
        s = rev(as.integer(strsplit(as.character(x), NULL)[[1]]))
        as.ones = function(d1) {
            switch(as.character(d1),
                   "0" = "",    "1" = "one",   "2" = "two",   "3" = "three", "4" = "four", "5" = "five",
                   "6" = "six", "7" = "seven", "8" = "eight", "9" = "nine",  default = stop("unhandled as.ones")) 
        }
        as.tens = function(d1, d2, add=TRUE) {
            ans = if (add && d1+d2 > 0) "and" else character(0)
            if      (d2 == 0) ans = c(ans, as.ones(d1))
            else if (d2 == 1) {
                s = switch(as.character(d1), 
                           "0" = "ten",      "1" = "eleven",  "2" = "twelve",  "3" = "thirteen",
                           "4" = "fourteen", "5" = "fifteen", "6" = "sixteen", "7" = "seventeen",
                           "8" = "eighteen", "9" = "nineteen", default = stop("unhandled as.teens")) 
                ans = c(ans, s)
            } else {
                s = switch(as.character(d2),
                           "2" = "twenty", "3" = "thirty",  "4" = "forty",  "5" = "fifty",
                           "6" = "sixty",  "7" = "seventy", "8" = "eighty", "9" = "ninety",
                           default = stop("unhandled as.tens"))
                ans = c(ans, s)
                ans = c(ans, as.ones(d1))
            }
            ans
        }
        as.hundreds = function(d1, d2, d3) {
            ans = character(0)
            if (d3 > 0) {
                ans = c(ans, as.ones(d3))
                ans = c(ans, "hundred")
            }
            ans = c(ans, as.tens(d1, d2))
        }
        if (length(s) == 4)      e = c(e, "one", "thousand")
        if (length(s) >= 3)      e = c(e, as.hundreds(s[1], s[2], s[3]))
        else if (length(s) == 2) e = c(e, as.tens(s[1], s[2], add=FALSE))
        else                     e = c(e, as.tens(s[1], 0, add=FALSE))
        e
    }
    ans = character(n)
    for (i in 1:n)
        ans[i] = paste(collapse="", as.english(i))
    sum(nchar(ans))
}

