this.prob <- "0019"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0019 = function(dom = 1, dow = 6) {
    dim = function(y, m) {  # calculate the number of days in month m of year y
        d = c(31, NA, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        if (! is.na(d[m])) return(d[m])
        if      (y %% 400 == 0) return(29)
        else if (y %% 100 == 0) return(28)
        else if (y %% 4 == 0)   return(29)
        else                    return(28)
    }
    diy = function(y) sum(sapply(1:12, function(m) dim(y, m)))
    #
    day.of.week = ((diy(1900)+1):sum(sapply(1900:2000, diy)) - 1) %% 7
    day.of.month = unlist(sapply(1901:2000, function(y) sapply(1:12, function(m) 1:dim(y, m))))
    stopifnot(length(day.of.week) == length(day.of.month))
    sum(day.of.week == dow & day.of.month == dom)
}

