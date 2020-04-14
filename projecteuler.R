options(prompt="euler> ")
reload = function(doit=FALSE) if (doit) source("projecteuler.R")
library(primes)  # generate_primes(max=...)
# library(Kmisc)   # str_rev(x) reverse a string
# library(bit64)  # 64-bit ints
coll = function(s) paste(collapse="", s)
# other functions defined outside problems (big_mult(), big_plus(), etc.)

p.0023 = function(digits=0:9, term=1000000) {
    # term-th lexicographical permutation of digits?
    # create permutations in sorted order
    digits = sort(digits)
    # abcd in sorted order, permutations in sorted order are
    # abcd
    # abdc
    # acbd
    # acdb
    # adbc
    # adcb
    # bacd
    # badc
    # bcad
    # bcda
    # bdac
    # bdca
    # ....
}

p.0023 = function(n = 28123) {
    # find all numbers <= n that *cannot* be written as sum of two abundant numbers
    # > 28123, all can be so written
    # identify abundant numbers in this range
    # 'abundant' vector set by compute.abundant()
    # a bug in proper.divisors / divisors: I was keeping all terms of the factor tree
    # rather than simplifying, so was calling 16 an abundant number; downloaded
    # https://oeis.org/A005101/b005101.txt which are the first 10000 abundant numbers
    # and found the bug that way
    na_sum = 0
    for (i in 1:n) {
        #if (i %% 1000 == 0) cat("p.0023: working on", i, "\n")
        j = i - abundant
        if (! any(j %in% abundant))
            na_sum = na_sum + i
    }
    na_sum
}
compute.abundant = function(n=28123) {
    ab = 2:n
    ab = ab[sapply(ab, number.type) > 0]
    abundant <<- ab
}
number.type = function(x) {  #0 = perfect, -1 = deficient, +1 = abundant
    if (x == 1) return(NA)
    spd = sum(proper.divisors(x))
    ifelse(spd == x, 0, ifelse(spd < x, -1, +1))
}
proper.divisors = function(x) {
    if (x == 1) return(NA)
    divs = unique(divisors(x))  # bug exposed by problem 23, '16' is not abundant
    return(divs[divs < x])
}

p.0022 = function(file = 'p022_names.txt') {
    # sum of name character values where A = 1, multiplied by position in sorted list
    # one of the names is "NA", which is the default NA string in R, so careful...
    nms = scan(file=file, sep=",", what=character(), na.strings="")
    strToSum = function(s) {
        sum(as.integer(sapply(strsplit(s, NULL)[[1]], charToRaw)) - as.integer(charToRaw("A")) + 1)
    }
    v = setNames(sapply(nms, strToSum) * rank(nms), nms) # setNames not required but nice for checking v["COLIN"]
    sum(v)
}

p.0021 = function(n = 10000) {
    # Let d(n) be defined as the sum of proper divisors of n (numbers less than n
    # which divide evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a
    # and b are an amicable pair and each of a and b are called amicable numbers.
    # 
    # Evaluate the sum of all the amicable numbers under n=10000.
    #
    # already have divisors() from p.0012(), make it global
    is.amicable = logical(n)
    d = function(x) {
        divs = divisors(x)
        return(sum(divs[divs < x]))
    }
    for (a in 2:(n - 1)) {
        if (is.amicable[a]) next
        b = d(a)
        if (b != a && b < n && b > 1) { # candidate
            if (d(b) == a) {
                is.amicable[c(a, b)] = TRUE
                cat(a, "and", b, "amicable: d(a) =", d(a), " d(b) =", d(b), "\n")
            }
        }
    }
    sum(which(is.amicable))
}

p.0020 = function(n = 100) {
    # sum of digits in n!
    v = 1
    for (i in 2:n)
        v = big_mult(v, i)
    cat(n, "! has", length(v), "digits, sum digits =", sum(v), " val =", paste(collapse="", rev(v)), "\n")
}
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

p.0018 = function(g="75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23") {
    g = as.integer(strsplit(paste(collapse=" ", strwrap(g)), " ")[[1]])
    nr = Re(polyroot(c(-2*length(g), 1, 1))[1])  # number of rows
    tri = list()
    for (i in 1:nr) {  # load triangle
        gp = 1 + (i*(i - 1)/2)
        tri[[i]] = g[gp:(gp + i - 1)]
    }
    for (i in 2:nr) {
        new.r = integer(i)
        for (j in 1:i) {
            if (j > 1)
                new.r[j] = tri[[i - 1]][j - 1] + tri[[i]][j]
            if (j < i)
                new.r[j] = max(c(new.r[j], tri[[i - 1]][j] + tri[[i]][j]))
        }
        tri[[i]] = new.r
    }
    max(tri[[nr]])
}

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

    factorial(2*n)/(factorial(n) * factorial(n))
}

p.0014 = function(n=1000000L, progress=floor(n/10)) {
    # which Collatz sequence, starting from less than n, produces the longest sequence?
    # x even: x = n / 2
    # x odd : x = 3*n + 1
    # once we've hit a precomputed number, we know how long the remaining sequence is
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
collatz = function(x) {
    ans = x
    while (x != 1) {
        x = ifelse(x %% 2 == 0, x/2, 3*x + 1)
        ans = c(ans, x)
    }
    ans
}

p.0013 = function(g="37107287533902102798797998220837590246510135740250 \
46376937677490009712648124896970078050417018260538 \
74324986199524741059474233309513058123726617309629 \
91942213363574161572522430563301811072406154908250 \
23067588207539346171171980310421047513778063246676 \
89261670696623633820136378418383684178734361726757 \
28112879812849979408065481931592621691275889832738 \
44274228917432520321923589422876796487670272189318 \
47451445736001306439091167216856844588711603153276 \
70386486105843025439939619828917593665686757934951 \
62176457141856560629502157223196586755079324193331 \
64906352462741904929101432445813822663347944758178 \
92575867718337217661963751590579239728245598838407 \
58203565325359399008402633568948830189458628227828 \
80181199384826282014278194139940567587151170094390 \
35398664372827112653829987240784473053190104293586 \
86515506006295864861532075273371959191420517255829 \
71693888707715466499115593487603532921714970056938 \
54370070576826684624621495650076471787294438377604 \
53282654108756828443191190634694037855217779295145 \
36123272525000296071075082563815656710885258350721 \
45876576172410976447339110607218265236877223636045 \
17423706905851860660448207621209813287860733969412 \
81142660418086830619328460811191061556940512689692 \
51934325451728388641918047049293215058642563049483 \
62467221648435076201727918039944693004732956340691 \
15732444386908125794514089057706229429197107928209 \
55037687525678773091862540744969844508330393682126 \
18336384825330154686196124348767681297534375946515 \
80386287592878490201521685554828717201219257766954 \
78182833757993103614740356856449095527097864797581 \
16726320100436897842553539920931837441497806860984 \
48403098129077791799088218795327364475675590848030 \
87086987551392711854517078544161852424320693150332 \
59959406895756536782107074926966537676326235447210 \
69793950679652694742597709739166693763042633987085 \
41052684708299085211399427365734116182760315001271 \
65378607361501080857009149939512557028198746004375 \
35829035317434717326932123578154982629742552737307 \
94953759765105305946966067683156574377167401875275 \
88902802571733229619176668713819931811048770190271 \
25267680276078003013678680992525463401061632866526 \
36270218540497705585629946580636237993140746255962 \
24074486908231174977792365466257246923322810917141 \
91430288197103288597806669760892938638285025333403 \
34413065578016127815921815005561868836468420090470 \
23053081172816430487623791969842487255036638784583 \
11487696932154902810424020138335124462181441773470 \
63783299490636259666498587618221225225512486764533 \
67720186971698544312419572409913959008952310058822 \
95548255300263520781532296796249481641953868218774 \
76085327132285723110424803456124867697064507995236 \
37774242535411291684276865538926205024910326572967 \
23701913275725675285653248258265463092207058596522 \
29798860272258331913126375147341994889534765745501 \
18495701454879288984856827726077713721403798879715 \
38298203783031473527721580348144513491373226651381 \
34829543829199918180278916522431027392251122869539 \
40957953066405232632538044100059654939159879593635 \
29746152185502371307642255121183693803580388584903 \
41698116222072977186158236678424689157993532961922 \
62467957194401269043877107275048102390895523597457 \
23189706772547915061505504953922979530901129967519 \
86188088225875314529584099251203829009407770775672 \
11306739708304724483816533873502340845647058077308 \
82959174767140363198008187129011875491310547126581 \
97623331044818386269515456334926366572897563400500 \
42846280183517070527831839425882145521227251250327 \
55121603546981200581762165212827652751691296897789 \
32238195734329339946437501907836945765883352399886 \
75506164965184775180738168837861091527357929701337 \
62177842752192623401942399639168044983993173312731 \
32924185707147349566916674687634660915035914677504 \
99518671430235219628894890102423325116913619626622 \
73267460800591547471830798392868535206946944540724 \
76841822524674417161514036427982273348055556214818 \
97142617910342598647204516893989422179826088076852 \
87783646182799346313767754307809363333018982642090 \
10848802521674670883215120185883543223812876952786 \
71329612474782464538636993009049310363619763878039 \
62184073572399794223406235393808339651327408011116 \
66627891981488087797941876876144230030984490851411 \
60661826293682836764744779239180335110989069790714 \
85786944089552990653640447425576083659976645795096 \
66024396409905389607120198219976047599490197230297 \
64913982680032973156037120041377903785566085089252 \
16730939319872750275468906903707539413042652315011 \
94809377245048795150954100921645863754710598436791 \
78639167021187492431995700641917969777599028300699 \
15368713711936614952811305876380278410754449733078 \
40789923115535562561142322423255033685442488917353 \
44889911501440648020369068063960672322193204149535 \
41503128880339536053299340368006977710650566631954 \
81234880673210146739058568557934581403627822703280 \
82616570773948327592232845941706525094512325230608 \
22918802058777319719839450180888072429661980811197 \
77158542502016545090413245809786882778948721859617 \
72107838435069186155435662884062257473692284509516 \
20849603980134001723930671666823555245252804609722 \
53503534226472524250874054075591789781264330331690", dig=10) {
    library(bit64)
    # value of the first 10 digits of the sum of the 100 given 50-digit numbers
    g = strsplit(paste(collapse=" ", strwrap(g)), " ")[[1]]
    cat("length(g) =", length(g), "\n")
    # the maximum digits in a sum of 100 n-digit numbers is n+2
    # so, no digits from 13-50 can influence the 10th digit apart from carry
    # carry will only matter insofar as the digits left of the rightmost are 9
    # because cascading carry can only be 1 left of that digit
    sm = sum(as.integer64(substr(g, 1, 12)))
    cat("sum of 12 leftmost =", sm, "\n")
    # 55373762303860
    # the last digit is 0, so any potential carry from the sum of 13-50 will be
    # swallowed by that; the first 10 digits are stable
    substr(as.character(sm), 1, 10)
}

p.0012 = function(n = 500) {
    # value of first triangle number i (sum(1:i)) with *over* n divisors
    tri = function(i) sum(1:i)
    i = 1
    while (TRUE) {
        d = divisors(tri(i))
        if (i %% 100 == 0)
            cat("tri", i, " val =", tri(i), " n div =", length(d), "\n")
        if (length(d) > n)
            break
        i = i + 1
    }
    tri(i)
}
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

p.0011 = function(g="08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48", nr=20, nc=nr, stride=4) {
    g = matrix(as.integer(strsplit(g, " ")[[1]]), nrow=nr, ncol=nc, byrow=TRUE)
    # wherever we can calculate both a row and column product, we can calculate a diagonal product
    # remember to calculate both diagonal products... for a while I neglected upper-right to lower-left :-(
    offs = 0:(stride - 1) # offsets into cells
    ans = 0
    rans = cans = dansl = dansr = g
    rans[] = cans[] = dansl[] = dansr[] = 0L
    for (i in 1:nr) {
        for (j in 1:nc) {
            has.rp = j <= nc - stride + 1 # can we calculate a row product?
            has.cp = i <= nr - stride + 1 # can we calculate a column product?
            if (has.rp)
                rans[i, j] = prod(g[i, j+offs])
            if (has.cp)
                cans[i, j] = prod(g[i+offs, j])
            if (has.rp && has.cp) {
                # left diagonal
                dansl[i, j] = prod(diag(g[i+offs, j+offs]))
                # right diagonal
                dansr[i, j] = prod(diag(t(apply(g[i+offs, j+offs], 2, rev))))
            }
        }
    }
    ans = max(max(rans), max(cans), max(dansl), max(dansr))
    if (any(rans == ans))  cat("max", ans, "row   @ ", which(rans == ans, arr.ind=TRUE), "\n")
    if (any(cans == ans))  cat("max", ans, "col   @ ", which(cans == ans, arr.ind=TRUE), "\n")
    if (any(dansl == ans)) cat("max", ans, "ldiag @ ", which(dansl == ans, arr.ind=TRUE), "\n")
    if (any(dansr == ans)) cat("max", ans, "rdiag @ ", which(dansr == ans, arr.ind=TRUE), "\n")
    ans
}

p.0010 = function(n = 2000000) {
    library(bit64)
    # sum of all primes less than n
    # integer overflow, so put them in a 64-bit int
    vp = generate_primes(max = n - 1)
    sm = integer64(1)
    for (p in vp) sm = sm + p
    sm
}

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

p.0008 = function(str="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450", stride=13) {
    l = nchar(str)
    vstr = integer(l - stride + 1)
    for (s in 1:(l - stride + 1))
        vstr[s] = prod(as.integer(strsplit(substr(str, s, s + stride - 1), NULL)[[1]]))
    max(vstr)
}

p.0007 = function(n=10001) {
    # what is the n-th prime number? starting with 2
    # Prime Number Theorem expects x/log(x) primes less than x
    # first, approximate power of 10 needed
    p = 1
    while (1) { 
        x = 10^p
        if (x/log(x) > n) {
            primes = generate_primes(max=x)
            if (length(primes) >= n)
                return(primes[n])
        }
        p = p + 1
    }
    
}

p.0006 = function(n=10) {
    # difference between sum of squares 1..n and square of sum
    sq_sum = ((n*(n+1))/2)^2
    sq_sum - sum((1:n)^2)
}

p.0005 = function(min=1, max=20) {
    # smallest positive number evenly divisible by all numbers from min to max
    n = max
    while (n) {
        divs = 0
        for (i in min:max) if (n %% i == 0) divs = divs + 1
        if (divs == max - min + 1) return(n)
        n = n + max
    }
    return(-1)
}

p.0004 = function(d1 = 3, d2 = 3) {
    # largest palindrome made from product of d1- and d2-digit numbers
    # palindrome product minus its reverse is 0
    library(Kmisc)   # str_rev(x) reverse a string
    p = integer(0)
    for (i in seq(10^(d1 - 1), 10^d1 - 1)) {
        for (j in seq(10^(d2 - 1), 10^d2 - 1)) {
            if (i*j - as.integer(str_rev(as.character(i*j))) == 0) {
                p = c(p, i*j)
            }
        }
    }
    max(p)
}

p.0003 = function(n=10403) {#600851475143) {
    # using trial division because I suck :-)
    # https://en.wikipedia.org/wiki/Trial_division
    prime_sieve = function(n) {
        # https://en.wikipedia.org/wiki/Sieve_of_Sundaram
        u = floor(n / 2)
        sv = rep(TRUE, u)
        for (j in 1:u) {
            for (i in 1:j) {
                cand = i + j + 2*i*j; if (cand <= u) sv[cand] = FALSE
            }
        }
        c(2, (which(sv) * 2) + 1)
    }
    f = integer(0)
    if (n < 2) return(f)
    #primes = prime_sieve(floor(sqrt(n)))
    primes = generate_primes(max=floor(sqrt(n)))  # instead use primes package
    for (p in primes) {
        if (p * p > n) break
        while (n %% p == 0) {
            f = c(f, p)
            n = floor(n / p)
        }
    }
    if (n > 1)
        f = c(f, n)
    f
}

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

p.0001 = function (mx=999) {
    # sum of multiples of 3 or 5 less than 1000
    threes = seq(3, mx, by=3)
    fives = seq(3, mx, by=5)
    a = unique(sort(c(threes, fives)))
    ans = sum(a)
}

