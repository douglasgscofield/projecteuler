this.prob <- "0015"
options(prompt=paste0(this.prob, "> "))
reload = function(doit=FALSE) if (doit) source(paste(sep=".", this.prob, "R"))


p.0015 = function(n=20) {
    # in an nxn square, how many right-or-down-only routes along the edges are
    # there from upper left to bottom right?
    #
    # * always 2*n moves
    # * 2 possibilities each move, but must have n moves D, n moves R
    # * now many orderings of n R and n D are there?
    # * the routes reflect along the diagonal by swapping R and D
    # * i thought of runs, but it didn't help
    #
    # better to think of orderings of indistinguishable objects (the
    # combinatorics of which I had to look up), should have gotten here sooner.
    # number of orderings of 6 objects of 2 different kinds, 2 of one and 4 of
    # the other, is 6!/(2! x 4!) so here, our answer is (2*n)!/(n! x n!).

    # now I see this (after writing the combinatorics chapter for my Math for
    # Bioinformatics book) that this is counting the Catalan number C_n

    factorial(2*n)/(factorial(n) * factorial(n))
}

