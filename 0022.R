this.prob <- "0022"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0022 = function(file = '0022_names.txt') {
    # sum of name character values where A = 1, multiplied by position in sorted list
    # one of the names is "NA", which is the default NA string in R, so careful...
    nms = scan(file=file, sep=",", what=character(), na.strings="")
    strToSum = function(s) {
        sum(as.integer(sapply(strsplit(s, NULL)[[1]], charToRaw)) - as.integer(charToRaw("A")) + 1)
    }
    v = setNames(sapply(nms, strToSum) * rank(nms), nms) # setNames not required but nice for checking v["COLIN"]
    sum(v)
}

