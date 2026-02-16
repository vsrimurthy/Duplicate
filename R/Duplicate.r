
#' fcn.to.comments
#' 
#' returns the comment section
#' @param x = a string (function name)
#' @keywords fcn.to.comments
#' @export
#' @family fcn
#' @import utils

fcn.to.comments <- function (x) 
{
    y <- fcn.to.txt(x, T, T)
    z <- all(!is.element(txt.right(y, 1), c(" ", "\t")))
    if (!z) 
        cat(x, "has lines with trailing whitespace!\n")
    if (z & !grepl("^function\\(", y[1])) {
        cat(x, "has a first line with non-canonical leading characters!\n")
        z <- F
    }
    if (z & any(!is.element(txt.left(y[-1], 1), c("#", "\t", 
        "}")))) {
        cat(x, "has lines with non-canonical leading characters!\n")
        z <- F
    }
    comment.delimiter <- paste("#", strrep("-", 65))
    w <- y == comment.delimiter
    if (z & sum(w) != 2) {
        cat(x, "does not have precisely two comment delimiters!\n")
        z <- F
    }
    w <- seq(1, length(y))[w]
    if (z & w[1] != 2) {
        cat(x, "does not have a proper beginning comment delimiter!\n")
        z <- F
    }
    if (z & w[2] - w[1] < 5) {
        cat(x, "has an ending too close to the beginning comment delimiter!\n")
        z <- F
    }
    if (z & length(y) - w[2] > 2) {
        z <- is.element(tail(y, 2)[1], c("\tz", "\tinvisible()"))
        if (!z) 
            cat(x, "returns a non-canonical variable!\n")
    }
    if (z) 
        z <- y[seq(w[1] + 1, w[2] - 1)]
    else z <- NULL
    z
}

#' level
#' 
#' responder's response to 1m
#' @param x = an integer (point count)
#' @param y = an integer vector (level thresholds)
#' @keywords level
#' @export
#' @import stats

level <- function (x, y) 
{
    approx(c(0, y), 0:length(y), x, method = "constant", rule = 2)[["y"]]
}

#' auction
#' 
#' full auction assuming no competition
#' @param x = internal representation of a bridge hand
#' @param y = internal representation of a bridge hand
#' @keywords auction
#' @export

auction <- function (x, y) 
{
    r <- open(x) == "P"
    if (r) {
        z <- x
        x <- y
        y <- z
    }
    x <- opener(x)
    y <- responder(y, x[["bid"]])
    y <- list(y)
    names(y) <- x[["bid"]]
    z <- x[["bid"]]
    while (any(names(y) == x[["bid"]])) {
        h <- y[[x[["bid"]]]]
        y <- x
        x <- h
        z <- c(z, x[["bid"]])
    }
    if (tail(z, 1) != "P") 
        z <- c(z, "P")
    if (r) 
        z <- c("P", z)
    z
}

#' bal
#' 
#' T/F depending on whether hand is balanced
#' @param x = internal representation of a bridge hand
#' @keywords bal
#' @export

bal <- function (x) 
{
    grepl("^(4333|4432|5332)$", shape(x))
}

#' both.hands.random
#' 
#' internal representation of a bridge hand
#' @keywords both.hands.random
#' @export

both.hands.random <- function () 
{
    z <- sample.int(52, 26)
    z <- list(open = z[1:13], resp = z[14:26])
    z <- lapply(z, function(z) sort(z - 1, decreasing = T))
    z
}

#' cheapest
#' 
#' cheapest bid in strain <y> after bid <x>
#' @param x = existing bid
#' @param y = new strain
#' @keywords cheapest
#' @export

cheapest <- function (x, y) 
{
    w <- is.element(y, 1:4)
    if (any(w)) 
        y[w] <- suits()[as.numeric(y[w])]
    z <- c("N", suits())
    z <- match(y, z) < match(gsub("^\\d", "", x), z)
    z <- as.numeric(gsub(".$", "", x)) + as.numeric(!z)
    z <- paste0(z, y)
    z
}

#' example
#' 
#' hand examples
#' @param fcn = function that determines when to stop
#' @param fun = type of hand you want to focus on
#' @keywords example
#' @export

example <- function (fcn, fun) 
{
    z <- list()
    proceed <- T
    while (proceed) {
        v <- both.hands.random()
        while (!fun(v)) v <- both.hands.random()
        u <- paste(auction(v[["open"]], v[["resp"]]), collapse = " ")
        if (all(names(z) != u)) {
            v[["count"]] <- 1
            z[[u]] <- v
            cat(sum(sapply(z, function(z) z[["count"]])), length(z), 
                "..\n")
        }
        else {
            z[[u]][["count"]] <- 1 + z[[u]][["count"]]
        }
        proceed <- fcn(z)
    }
    z
}

#' fcn.all.roxygenize
#' 
#' roxygenizes all functions
#' @param x = the output file
#' @keywords fcn.all.roxygenize
#' @export
#' @family fcn

fcn.all.roxygenize <- function (x) 
{
    n <- grep("\\.", fcn.list(), value = T)
    n <- gsub("\\..*$", "", n)
    n <- aggregate(x = n, by = list(grp = n), FUN = length)
    y <- n[, "grp"]
    n <- n[, "x"]
    names(n) <- y
    n <- names(n)[n > 1]
    y <- "fcn.to.comments"
    names(y) <- "utils"
    y["stats"] <- "level"
    z <- NULL
    for (w in names(y)) z <- c(z, "", fcn.roxygenize(y[w], w, 
        n))
    y <- setdiff(fcn.list(), y)
    for (w in y) z <- c(z, "", fcn.roxygenize(w, , n))
    writeLines(z, x)
    invisible()
}

#' fcn.comments.parse
#' 
#' extracts information from the comments
#' @param x = a string vector (comments section of a function)
#' @keywords fcn.comments.parse
#' @export
#' @family fcn

fcn.comments.parse <- function (x) 
{
    z <- list(canonical = !is.null(x))
    if (z$canonical) {
        if (!grepl("^# Name\t\t: ", x[1])) {
            cat("Problem with NAME!\n")
            z$canonical <- F
        }
        else {
            z$name <- gsub("^.{10}", "", x[1])
            x <- x[-1]
        }
    }
    if (z$canonical) {
        if (!grepl("^# Author\t: ", x[1])) {
            cat("Problem with AUTHOR!\n")
            z$canonical <- F
        }
        else {
            z$author <- gsub("^.{11}", "", x[1])
            x <- x[-1]
        }
    }
    if (z$canonical) {
        if (!grepl("^# Date\t\t: ", x[1])) {
            cat("Problem with DATE!\n")
            z$canonical <- F
        }
        else {
            z$date <- gsub("^.{10}", "", x[1])
            x <- x[-1]
            while (length(x) > 0 & grepl("^#\t\t: ", x[1])) {
                z$date <- paste0(z$date, gsub("^.{5}", "", x[1]))
                x <- x[-1]
            }
        }
    }
    if (z$canonical) {
        if (!grepl("^# Args\t\t: ", x[1])) {
            cat("Problem with ARGS!\n")
            z$canonical <- F
        }
        else {
            z$detl.args <- x[1]
            x <- x[-1]
            while (length(x) > 0 & grepl("^(#\t\t:\t|#\t\t: )", 
                x[1])) {
                z$detl.args <- c(z$detl.args, x[1])
                x <- x[-1]
            }
            z$detl.args <- fcn.extract.args(z$detl.args)
            if (z$detl.args[1] != "none") 
                z$args <- gsub(" =.*$", "", z$detl.args)
        }
    }
    z$missing <- grepl("[ (]can be missing)", z$detl.args)
    if (z$canonical) {
        if (!grepl("^# Output\t: ", x[1])) {
            cat("Problem with OUTPUT!\n")
            z$canonical <- F
        }
        else {
            z$out <- x[1]
            x <- x[-1]
            while (length(x) > 0 & grepl("^(#\t\t:\t|#\t\t: )", 
                x[1])) {
                z$out <- c(z$out, x[1])
                x <- x[-1]
            }
            z$out <- fcn.extract.out(z$out)
        }
    }
    if (z$canonical & length(x) > 0) {
        if (grepl("^# Notes\t\t: ", x[1])) {
            x <- x[-1]
            while (length(x) > 0 & grepl("^(#\t\t:\t|#\t\t: )", 
                x[1])) x <- x[-1]
        }
    }
    if (z$canonical & length(x) > 0) {
        if (grepl("^# Example\t: ", x[1])) {
            z$example <- gsub("^.{12}", "", x[1])
            x <- x[-1]
        }
    }
    if (z$canonical & length(x) > 0) {
        if (grepl("^# Import\t: ", x[1])) {
            z$import <- gsub("^.{11}", "", x[1])
            x <- x[-1]
        }
    }
    if (z$canonical & length(x) > 0) {
        cat("Other bizarre problem!\n")
        z$canonical <- F
    }
    z
}

#' fcn.extract.args
#' 
#' vector of arguments with explanations
#' @param x = a string vector (argument section of comments)
#' @keywords fcn.extract.args
#' @export
#' @family fcn

fcn.extract.args <- function (x) 
{
    n <- length(x)
    x <- gsub("^(# Args\t\t: |#\t\t: )", "", x)
    if (n > 1) {
        w <- grepl("=", x)
        while (any(w[-n] & !w[-1])) {
            i <- 2:n - 1
            i <- i[w[-n] & !w[-1]][1]
            j <- i:n + 1
            j <- j[c(w, T)[j]][1] - 1
            x[i] <- paste(txt.trim(x[i:j], "\t"), collapse = " ")
            while (j > i) {
                x <- x[-j]
                w <- w[-j]
                j <- j - 1
                n <- n - 1
            }
        }
    }
    z <- x
    z
}

#' fcn.extract.out
#' 
#' extracts output
#' @param x = a string vector (output section of comments)
#' @keywords fcn.extract.out
#' @export
#' @family fcn

fcn.extract.out <- function (x) 
{
    paste(gsub("^(# Output\t: |#\t\t: )", "", x), collapse = " ")
}

#' fcn.list
#' 
#' Returns the names of objects that are or are not functions
#' @param x = a string (regular expression)
#' @keywords fcn.list
#' @export
#' @family fcn

fcn.list <- function (x = "*") 
{
    w <- globalenv()
    while (!is.element("fcn.list", ls(envir = w))) w <- parent.env(w)
    z <- ls(envir = w, all.names = T, pattern = x)
    w <- is.element(z, as.character(lsf.str(envir = w, all.names = T)))
    z <- z[w]
    z
}

#' fcn.order
#' 
#' functions in alphabetical order
#' @keywords fcn.order
#' @export
#' @family fcn

fcn.order <- function () 
{
    x <- fcn.list()
    x <- split(x, x)
    fcn <- function(z) paste(z, "<-", fcn.to.txt(z, T, F))
    x <- sapply(x, fcn)
    writeLines(x, fcn.path())
    invisible()
}

#' fcn.path
#' 
#' path to function source file
#' @keywords fcn.path
#' @export
#' @family fcn

fcn.path <- function () 
{
    "C:\\temp\\Cards\\Personal\\Duplicate\\Duplicate.r"
}

#' fcn.roxygenize
#' 
#' roxygenized function format
#' @param x = a string (function name)
#' @param y = a string (library to import)
#' @param n = vector of function families
#' @keywords fcn.roxygenize
#' @export
#' @family fcn

fcn.roxygenize <- function (x, y, n) 
{
    w <- fcn.to.comments(x)
    w <- gsub("([\\%])", "\\\\\\1", w)
    w <- gsub("@", "@@", w)
    w <- fcn.comments.parse(w)
    z <- c(w$name, "", w$out)
    if (any(names(w) == "args")) 
        z <- c(z, paste("@param", w$detl.args))
    z <- c(z, paste("@keywords", w$name), "@export")
    if (!missing(n)) {
        if (any(x == n) | any(txt.left(x, nchar(n) + 1) == paste0(n, 
            "."))) {
            z <- c(z, paste("@family", gsub("\\..*$", "", x)))
        }
    }
    if (!missing(y)) {
        z <- c(z, paste("@import", y))
    }
    else if (any(names(w) == "import")) 
        z <- c(z, w$import)
    if (any(names(w) == "example")) 
        z <- c(z, "@examples", w$example)
    z <- c(paste("#'", z), "")
    x <- fcn.to.txt(x, F, T)
    x[1] <- paste(w$name, "<-", x[1])
    z <- c(z, x)
    z
}

#' fcn.to.txt
#' 
#' represents <x> as a string or string vector
#' @param x = a string (function name)
#' @param y = a boolean (do/don't return comments)
#' @param n = a boolean (return string/vector)
#' @keywords fcn.to.txt
#' @export
#' @family fcn

fcn.to.txt <- function (x, y = F, n = F) 
{
    x <- get(x)
    if (y) 
        z <- deparse(x, control = "useSource")
    else z <- deparse(x)
    if (!n) 
        z <- paste(z, collapse = "\n")
    z
}

#' good.suit
#' 
#' T/F depending on whether suit is good
#' @param x = internal representation of a bridge hand
#' @param y = T/F depending on whether both criteria are used
#' @keywords good.suit
#' @export

good.suit <- function (x, y = T) 
{
    x <- split(x%%13, x%/%13)
    z <- sapply(x, function(z) sum(z > 9) > 1)
    if (y) 
        z <- z | sapply(x, function(z) sum(z > 7) > 2)
    z <- z[match(3:0, names(z))]
    z <- as.logical(ifelse(is.na(z), F, z))
    z
}

#' greater
#' 
#' T/F depending on whether <x> is greater than <y>
#' @param x = a bid
#' @param y = a bid
#' @keywords greater
#' @export

greater <- function (x, y) 
{
    gsub("S", "J", x) > gsub("S", "J", y)
}

#' hand.random
#' 
#' internal representation of a bridge hand
#' @keywords hand.random
#' @export
#' @family hand

hand.random <- function () 
{
    sort(sample.int(52, 13) - 1, decreasing = T)
}

#' hand.read
#' 
#' internal representation of a bridge hand
#' @param x = a string
#' @keywords hand.read
#' @export
#' @family hand

hand.read <- function (x) 
{
    z <- strsplit(x, " ", fixed = T)[[1]]
    z <- split(z, suits()[seq_along(z)])
    z <- lapply(z, function(z) strsplit(z, "")[[1]])
    y <- strsplit("AKQJT98765432", "")[[1]]
    z <- lapply(z, function(z) 13 - match(z, y))
    z[["D"]] <- z[["D"]] + 13
    z[["H"]] <- z[["H"]] + 26
    z[["S"]] <- z[["S"]] + 39
    z <- sort(Reduce(c, z), decreasing = T)
    z
}

#' hand.show
#' 
#' a string
#' @param x = internal representation of a bridge hand
#' @keywords hand.show
#' @export
#' @family hand

hand.show <- function (x) 
{
    z <- split(x%%13, x%/%13)
    y <- strsplit("AKQJT98765432", "")[[1]]
    z <- lapply(z, function(z) y[13 - z])
    z <- sapply(z, function(z) paste(z, collapse = ""))
    z <- z[order(as.numeric(names(z)), decreasing = T)]
    z <- paste0(suit.symbols(names(z)), z)
    z <- paste(z, collapse = " ")
    z
}

#' hcp
#' 
#' number of high-card points
#' @param x = internal representation of a bridge hand
#' @keywords hcp
#' @export

hcp <- function (x) 
{
    z <- x%%13 - 8
    z <- sum(z[z > 0])
    z
}

#' len
#' 
#' length points
#' @param x = internal representation of a bridge hand
#' @keywords len
#' @export

len <- function (x) 
{
    z <- strsplit(shape(x), "")[[1]]
    z <- as.numeric(z) - 4
    z <- sum(z[z > 0])
    z
}

#' open
#' 
#' opening bid
#' @param x = internal representation of a bridge hand
#' @keywords open
#' @export

open <- function (x) 
{
    y <- bal(x)
    z <- shape.detl(x)
    w <- good.suit(x)
    x <- hcp(x) + len(x)
    if (x > 21) {
        z <- "2C"
    }
    else if (x < 13 & any(z > 6 & w)) {
        w <- max(z)
        z <- paste0(w - 4, head(suits()[z == w], 1))
    }
    else if (x < 13 & any(z[-4] == 6 & w[-4])) {
        z <- paste0("2", head(suits()[z == 6 & w], 1))
    }
    else if (x < 13) {
        z <- "P"
    }
    else if (y & is.element(x, 15:17)) {
        z <- "1N"
    }
    else if (y & is.element(x, 20:21)) {
        z <- "2N"
    }
    else if (max(z) > 4) {
        z <- suits()[z == max(z)]
        z <- paste0("1", head(z, 1))
    }
    else if (all(z[3:4] == 3:2)) {
        z <- "1D"
    }
    else if (all(z[3:4] == 2:3)) {
        z <- "1C"
    }
    else if (all(z[3:4] == 3)) {
        z <- "1C"
    }
    else if (all(z[3:4] == 4)) {
        z <- "1D"
    }
    else {
        z <- suits()[z == max(z)]
        z <- paste0("1", tail(z, 1))
    }
    z
}

#' opener
#' 
#' responder's response to 1N
#' @param x = internal representation of a bridge hand
#' @keywords opener
#' @export
#' @family opener

opener <- function (x) 
{
    z <- list(bid = open(x))
    if (z[["bid"]] == "2C") {
        z <- c(z, opener.2C(x))
    }
    else if (grepl("^(2[DHS]|3[^N])$", z[["bid"]])) {
        z <- c(z, opener.preempt(x))
    }
    else if (z[["bid"]] == "1N") {
        z <- c(z, opener.1N(x))
    }
    else if (z[["bid"]] == "2N") {
        z <- c(z, opener.2N(x))
    }
    else if (grepl("^1(H|S)$", z[["bid"]])) {
        z <- c(z, opener.1minorajor(x, z[["bid"]]))
    }
    else if (grepl("^1(C|D)$", z[["bid"]])) {
        z <- c(z, opener.1minor(x, z[["bid"]]))
    }
    z
}

#' opener.1minor
#' 
#' opener's subsequent bids after 1m
#' @param x = internal representation of a bridge hand
#' @param y = opening bid
#' @keywords opener.1minor
#' @export
#' @family opener

opener.1minor <- function (x, y) 
{
    v <- match(gsub("^\\d", "", y), suits())
    n <- shape.detl(x)
    m <- hcp(x) + len(x)
    r <- n > 1
    if (sum(!r) == 1 & all(n > 0)) {
        r <- 4 - which(!r)
        r <- (x%%13)[x%/%13 == r] > 10
    }
    else {
        r <- all(r)
    }
    w <- stopped.suits(x)
    w[v] <- T
    if (all(w[1:2])) 
        r <- T
    u <- m + n[v] - shortness(n, y) - len(x)
    z <- list()
    if (r & m > 18) {
        for (j in 2:3) z[[gsub("^1", j, y)]] <- list(bid = "3N")
    }
    else if (r & m > 15) {
        for (j in 2:3) z[[gsub("^1", j, y)]] <- list(bid = paste0(j, 
            "N"))
    }
    else if (r & m == 15) {
        z[[gsub("^1", "3", y)]] <- list(bid = "3N")
    }
    else if (all(!w[suits() != gsub("^\\d", "", y)])) {
        if (u > 19) 
            z[[gsub("^1", "2", y)]] <- list(bid = gsub("^1", 
                4 + level(u, 23), y))
        if (u > 17) 
            z[[gsub("^1", "3", y)]] <- list(bid = gsub("^1", 
                4 + level(u, 19), y))
    }
    else if (y == "1D" & all(!w[1:2])) {
        if (m > 15) {
            bid <- list(bid = "3C")
            if (u > 22) 
                bid[["3D"]] <- list(bid = "5D")
            if (u > 17) 
                bid[["4D"]] <- list(bid = "5D")
            z[["2D"]] <- bid
        }
        if (u > 17) 
            z[["3D"]] <- list(bid = paste0(4 + level(u, 19), 
                "D"))
    }
    else if (m > 14) {
        j <- (2:v - 1)[w[2:v - 1]]
        bid <- list(bid = paste0(3, tail(suits()[j], 1)))
        if (u > 18) 
            bid[[gsub("^1", 4, y)]] <- list(bid = gsub("^1", 
                5, y))
        if (tail(j, 1) == 3) 
            for (j in 1:2) if (all((w | seq_along(w) == j)[1:2])) {
                bid[[paste0(3, suits()[j])]] <- list(bid = "3N")
            }
            else {
                bid[[paste0(3, suits()[j])]] <- list(bid = gsub("^1", 
                  4 + level(u, 19), y))
            }
        z[[gsub("^1", "3", y)]] <- bid
        if (m > 15) {
            j <- (2:v - 1)[w[2:v - 1]]
            bid <- list(bid = paste0(2, tail(suits()[j], 1)))
            if (u > 17) 
                bid[[gsub("^1", 4, y)]] <- list(bid = gsub("^1", 
                  5, y))
            if (tail(j, 1) == 3) 
                for (j in 1:2) if (all((w | seq_along(w) == j)[1:2])) {
                  bid[[paste0(2, suits()[j])]] <- list(bid = paste0(2 + 
                    level(m, 19), "N"))
                }
                else {
                  bid[[paste0(2, suits()[j])]] <- list(bid = gsub("^1", 
                    3 + level(u, c(20, 23)), y))
                }
            if (m > 18) 
                bid[["2N"]] <- list(bid = "3N")
            if (u > 19) 
                bid[[gsub("^1", "3", y)]] <- list(bid = gsub("^1", 
                  4 + level(u, 23), y))
            z[[gsub("^1", "2", y)]] <- bid
        }
    }
    z <- c(z, opener.1Xresp(x, y))
    z <- c(z, opener.2Nresp(x, y))
    z <- c(z, opener.1Nresp(x, y))
    z <- c(z, opener.21(x, y))
    z
}

#' opener.1minorajor
#' 
#' opener's subsequent bids after 1M
#' @param x = internal representation of a bridge hand
#' @param y = opening bid
#' @keywords opener.1minorajor
#' @export
#' @family opener

opener.1minorajor <- function (x, y) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    m <- hcp(x)
    r <- len(x)
    u <- shortness(n, y)
    h <- n[as.numeric(y == "1H") + 1]
    z <- list()
    if (m + h - u > 18) {
        z[[gsub("^1", "2", y)]] <- list(bid = gsub("^1", "4", 
            y))
        z[[gsub("^1", "3", y)]] <- list(bid = gsub("^1", "4", 
            y))
    }
    else if (m + h - u > 15) {
        z[[gsub("^1", "2", y)]] <- list(bid = gsub("^1", "3", 
            y))
        z[[gsub("^1", "3", y)]] <- list(bid = gsub("^1", "4", 
            y))
    }
    else if (m + h - u == 15) {
        z[[gsub("^1", "3", y)]] <- list(bid = gsub("^1", "4", 
            y))
    }
    if (y != "1S") 
        z <- c(z, opener.1Xresp(x, y))
    z <- c(z, opener.2Nresp(x, y))
    z <- c(z, opener.1Nresp(x, y))
    z <- c(z, opener.21(x, y))
    z
}

#' opener.1N
#' 
#' opener's subsequent bids after 1N
#' @param x = internal representation of a bridge hand
#' @keywords opener.1N
#' @export
#' @family opener

opener.1N <- function (x) 
{
    n <- shape.detl(x)
    m <- hcp(x) + len(x) > 15
    z <- list()
    z[["2C"]] <- list(bid = stayman(x))
    if (m) {
        z[["2N"]] <- list(bid = "3N")
        z[["2C"]][["2N"]] <- z[["2N"]]
    }
    for (j in suits(2)) {
        if (n[match(j, suits())] > 2) {
            z[[paste0("3", j)]] <- list(bid = paste0("4", j))
        }
        else {
            z[[paste0("3", j)]] <- list(bid = "3N")
        }
        if (m) 
            z[["2C"]][[paste0("3", j)]] <- z[[paste0("3", j)]]
    }
    if (z[["2C"]][["bid"]] == "2D") 
        for (j in suits(2)) if (n[match(j, suits())] < 3) {
            z[["2C"]][[paste0("2", j)]] <- list(bid = paste0(2 + 
                as.numeric(m), "N"))
            z[["2C"]][[paste0("3", j)]] <- list(bid = "3N")
        }
        else {
            z[["2C"]][[paste0("3", j)]] <- list(bid = paste0("4", 
                j))
            if (m) 
                z[["2C"]][[paste0("2", j)]] <- list(bid = paste0("4", 
                  j))
        }
    if (z[["2C"]][["bid"]] == "2S" & m) 
        z[["2C"]][["3H"]] <- list(bid = "4H")
    z
}

#' opener.1Nresp
#' 
#' opener's subsequent bids after 1M
#' @param x = internal representation of a bridge hand
#' @param y = opening bid
#' @keywords opener.1Nresp
#' @export
#' @family opener

opener.1Nresp <- function (x, y) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    m <- hcp(x)
    r <- len(x)
    v <- match(gsub("\\d", "", y), suits())
    u <- max(setdiff(ord[1:2], v))
    z <- list()
    if (bal(x) & m + r > 17) {
        z[["1N"]] <- list(bid = "3N")
    }
    else if (bal(x)) {
        z[["1N"]] <- list(bid = "P")
    }
    else if (m + r > 18 & n[u] < 4) {
        z[["1N"]] <- list(bid = "3N")
    }
    else if (n[v] > 5 & m + r > 15 & (n[u] < 4 | is.element(m + 
        r, 16:18))) {
        z[["1N"]] <- list(bid = gsub("^1", 3, y))
    }
    else if (n[u] < 4) {
        z[["1N"]] <- list(bid = gsub("^1", 2, y))
    }
    else if (v < u) {
        z[["1N"]] <- list(bid = paste0(2 + level(m + r, 19), 
            suits()[u]))
        for (k in c("2N", paste0(3, suits()[v:3 + 1]))) {
            l <- match(k, paste0(3, suits()))
            if (is.na(l)) {
                if (m + r > 15) 
                  z[["1N"]][[k]] <- list(bid = "3N")
            }
            else if (l == 2 & n[2] > 2) {
                if (m + n[2] - min(n) > 15) 
                  z[["1N"]][[k]] <- list(bid = "4H")
            }
            else {
                if (m + r > 15) 
                  z[["1N"]][[k]] <- list(bid = "3N")
            }
        }
    }
    else if (m + r > 15) {
        z[["1N"]] <- list(bid = paste0(2, suits()[u]))
        if (m + r > 18) 
            z[["1N"]][["2N"]] <- list(bid = "3N")
    }
    else if (n[ord][3] == 4) {
        z[["1N"]] <- list(bid = paste0(2, suits()[ord][3]))
    }
    else {
        z[["1N"]] <- list(bid = gsub("^1", 2, y))
    }
    z
}

#' opener.1Xresp
#' 
#' opener's subsequent bids after 1M
#' @param x = internal representation of a bridge hand
#' @param y = opening bid
#' @keywords opener.1Xresp
#' @export
#' @family opener

opener.1Xresp <- function (x, y) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    m <- hcp(x)
    r <- len(x)
    v <- match(gsub("\\d", "", y), suits())
    g <- as.logical(stopped.suits(x))
    g <- g | n > 2
    z <- list()
    if (v > 1) 
        for (j in 2:v - 1) {
            u <- setdiff(ord, c(v, j))[1]
            k <- setdiff(1:4, c(v, u, j))
            if (j < 3 & n[j] > 3) {
                bid <- list(bid = paste0(2 + level(m + n[j] - 
                  min(n), c(16, 19)), suits()[j]))
                if (m + n[j] - min(n) == 15) 
                  bid[[paste0(3, suits()[j])]] <- list(bid = paste0(4, 
                    suits()[j]))
                z[[paste0(1, suits()[j])]] <- bid
            }
            else if (bal(x)) {
                bid <- list(bid = paste0(1 + level(m + r, 18), 
                  "N"))
                if (m + r == 14) 
                  bid[["2N"]] <- list(bid = "3N")
                z[[paste0(1, suits()[j])]] <- bid
            }
            else if (u < j & n[u] > 3) {
                bid <- list(bid = paste0(1 + level(m + r, 19), 
                  suits()[u]))
                bid <- response.wrapper(bid, k, n, g[k], c(u, 
                  j, v, k), c(5, 3, 5 + as.numeric(g[k]), 4))
                if (m + r > 18) {
                  bid <- response(bid, v, "N", 3)
                  if (n[j] > 1 & j < 2) {
                    bid <- response(bid, j, j, 4)
                  }
                  else {
                    bid <- response(bid, j, "N", 3)
                  }
                }
                if (m + r > 14) 
                  bid[[paste0(3, suits()[u])]] <- list(bid = paste0(4, 
                    suits()[u]))
                if (m + r > 15) {
                  bid[["2N"]] <- list(bid = "3N")
                  bid[[paste0(2, suits()[u])]] <- list(bid = paste0(3 + 
                    level(m + 3, 19), suits()[u]))
                  bid[[paste0(3, suits()[v])]] <- list(bid = "3N")
                  if (n[j] > 1 & j < 2) {
                    bid[[paste0(3, suits()[j])]] <- list(bid = paste0(4, 
                      suits()[j]))
                  }
                  else {
                    bid[[paste0(3, suits()[j])]] <- list(bid = "3N")
                  }
                }
                z[[paste0(1, suits()[j])]] <- bid
            }
            else if (m + r > 18 & n[u] < 4) {
                z[[paste0(1, suits()[j])]] <- list(bid = "3N")
            }
            else if (n[v] > 5 & m + r > 15 & (n[u] < 4 | is.element(m + 
                r, 16:18))) {
                z[[paste0(1, suits()[j])]] <- list(bid = gsub("^1", 
                  3, y))
            }
            else if (n[j] > 3 & m + r > 15 & (n[u] < 4 | is.element(m + 
                r, 16:18))) {
                z[[paste0(1, suits()[j])]] <- list(bid = paste0(3, 
                  suits()[j]))
            }
            else if (n[j] > 3) {
                bid <- list(bid = paste0(2, suits()[j]))
                if (j > 1) 
                  for (k in 2:j - 1) {
                    bid <- response.wrapper(bid, k, n, g[setdiff(1:4, 
                      c(v, j, k))], c(j, k, v), c(4, 4, 6), c(1, 
                      1, 0))
                  }
                z[[paste0(1, suits()[j])]] <- bid
            }
            else if (n[v] > 5 & m < 16) {
                bid <- list(bid = gsub("^1", 2, y))
                for (k in setdiff(2:v - 1, j)) {
                  bid <- response.wrapper(bid, k, n, g[setdiff(1:4, 
                    c(v, j, k))], c(j, k, v), c(3, 4, 6 + as.numeric(g[setdiff(1:4, 
                    c(v, j, k))])))
                }
                z[[paste0(1, suits()[j])]] <- bid
            }
            else if (v < u) {
                bid <- list(bid = paste0(2 + level(m + r, 19), 
                  suits()[u]))
                if (m + r < 19) {
                  bid <- response.wrapper(bid, k, n, g[k], c(j, 
                    v, u, k), c(3, 5 + as.numeric(g[k]), 5, 4))
                }
                if (is.element(m + r, 16:18)) 
                  bid <- response(bid, "N", "N")
                z[[paste0(1, suits()[j])]] <- bid
            }
            else if (m + r > 15) {
                bid <- list(bid = paste0(2, suits()[u]))
                if (k < 3 & n[k] > 3) {
                  bid <- response(bid, k, k, 4)
                }
                else {
                  bid <- response(bid, k, "N", 3)
                }
                bid <- response(bid, u, ifelse(u < 3, u, "N"))
                if (n[j] > 2) {
                  bid <- response(bid, j, j, 4)
                }
                else {
                  bid <- response(bid, j, "N", 3)
                }
                if (m + r > 18) {
                  bid <- response(bid, "N", "N")
                  bid <- response(bid, v, "N")
                }
                z[[paste0(1, suits()[j])]] <- bid
            }
            else {
                bid <- list(bid = gsub("^1", 2, y))
                if (j > 1) 
                  for (l in 2:j - 1) if (n[j] > 2) {
                    bid <- response(bid, l, j)
                  }
                  else if (g[setdiff(1:4, c(v, j, l))]) {
                    bid <- response(bid, l, "N", 3)
                  }
                  else {
                    bid <- response(bid, l, v)
                  }
                z[[paste0(1, suits()[j])]] <- bid
            }
        }
    z
}

#' opener.21
#' 
#' opener's subsequent bids
#' @param x = internal representation of a bridge hand
#' @param y = opening bid
#' @keywords opener.21
#' @export
#' @family opener

opener.21 <- function (x, y) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    v <- match(gsub("\\d", "", y), suits())
    u <- max(setdiff(ord[1:2], v))
    g <- as.logical(stopped.suits(x))
    g <- g | n > 2
    z <- list()
    if (v < 4) 
        for (j in 3:v + 1) {
            if (n[j] > 2 & j == 2) {
                bid <- list(bid = paste0("4", suits()[j]))
            }
            else if (v < 3 & n[v] > 7) {
                bid <- list(bid = gsub("^1", 4, y))
            }
            else if (v < 3 & n[v] == 7 & n[u] < 4) {
                bid <- list(bid = gsub("^1", 2, y))
                bid <- response(bid, "N", v)
                for (k in v:3 + 1) {
                  p <- setdiff(1:4, c(v, j, k))
                  p <- ifelse(all(g[p]), "N", v)
                  p <- ifelse(k == j & j == 2 & n[j] > 1, j, 
                    p)
                  bid <- response(bid, k, p)
                  l <- grep(paste0(suits()[k], "$"), names(bid), 
                    value = T)
                  if (k < j & n[k] < 3 & n[j] > 1) 
                    bid[[l]] <- response(bid[[l]], k, j)
                }
                if (v == 2) 
                  if (g[setdiff(3:4, j)]) {
                    bid <- response(bid, 1, "N")
                    bid[["2S"]] <- response(bid[["2S"]], 1, ifelse(n[1] > 
                      2, "S", "N"))
                    bid[["2S"]] <- response(bid[["2S"]], 2, 2)
                    for (l in 3:4) bid[["2S"]] <- response(bid[["2S"]], 
                      l, v)
                  }
                  else {
                    bid <- response(bid, 1, v)
                    bid[["2S"]] <- response.wrapper(bid[["2S"]], 
                      1, n, F, c(1, j), c(3, 2), c(1, 1))
                  }
            }
            else if (identical(n[1:2], c(4, 4))) {
                bid <- list(bid = "2H")
                bid[["2S"]] <- list(bid = "4S")
                bid[["2N"]] <- bid[["3C"]] <- bid[["3D"]] <- list(bid = "3S")
            }
            else if (j != u & n[u] > 3) {
                p <- setdiff(1:4, c(u, v, j))
                bid <- list(bid = paste0(2 + as.numeric(j < u), 
                  suits()[u]))
                bid <- response.wrapper(bid, v, n, g[p], c(u, 
                  v, j), c(5, 5 + as.numeric(v < 3), 3))
                l <- grep(paste0(suits()[v], "$"), names(bid), 
                  value = T)
                if (length(l) == 0) 
                  bid <- response(bid, v, v)
                if (greater("2N", bid[["bid"]])) 
                  bid <- response.wrapper(bid, "N", n, T, c(u, 
                    v, j), c(5, 6, 3))
                l <- ifelse(g[p], "N", ifelse(n[v] > 5 & v < 
                  3, v, u))
                if (l == v | j > u) 
                  bid <- response(bid, u, l)
                bid <- response.wrapper(bid, j, n, g[p], c(u, 
                  v, j), c(5, 6 + as.numeric(g[p]), 2), c(0, 
                  0, 1))
                l <- grep(paste0(suits()[j], "$"), names(bid), 
                  value = T)
                if (n[v] > 6 & n[j] < 1) 
                  bid[[l]] <- response(bid[[l]], j, v)
                bid <- response.wrapper(bid, p, n, T, c(p, u, 
                  v, j), c(4, 5, 6, 3), c(1, 0, 0, 1))
                l <- grep(paste0(suits()[p], "$"), names(bid), 
                  value = T)
                if (length(l) == 1) 
                  if (greater("2N", bid[[l]][["bid"]])) {
                    bid[[l]] <- response(bid[[l]], "N", "N")
                    bid[[l]] <- response.wrapper(bid[[l]], p, 
                      n, T, c(p, u, v, j), c(3, 5, 7, 3 - as.numeric(p < 
                        j)), c(1, 0, 0, 0))
                  }
                  else if (bid[[l]][["bid"]] == "2N") {
                    bid[[l]] <- response.wrapper(bid[[l]], p, 
                      n, T, c(p, u, v, j), c(3, 5, 6, 3))
                  }
                  else if (any(bid[[l]][["bid"]] == paste0(3, 
                    suits()[-1]))) {
                    k <- match(gsub("^\\d", "", bid[[l]][["bid"]]), 
                      suits())
                    if (p < k) 
                      bid[[l]] <- response(bid[[l]], p, ifelse(n[p] > 
                        2 & p < 3, p, "N"))
                  }
            }
            else if (n[v] > 5 & !identical(n[ord][1:2], c(6, 
                5))) {
                bid <- list(bid = gsub("^1", 2, y))
                if (n[j] > 2 & j > 2) {
                  bid <- response(bid, "N", j)
                  for (k in setdiff(1:4, c(v, j))) {
                    h <- setdiff(1:4, c(v, j, k))
                    bid <- response(bid, k, ifelse(g[h], "N", 
                      j))
                    l <- names(bid)[grepl(paste0(suits()[k], 
                      "$"), names(bid))]
                    bid[[l]] <- response.wrapper(bid[[l]], k, 
                      n, g[h], c(k, j), c(3, 3))
                    for (p in c(v, j)) if (greater("3N", cheapest(bid[[l]][["bid"]], 
                      p))) 
                      bid[[l]] <- response(bid[[l]], p, ifelse(g[h], 
                        "N", j))
                  }
                  k <- setdiff(1:4, c(v, j))
                  for (l in c(v, j)) if (all(g[k]) & l > 2) {
                    bid <- response(bid, l, "N")
                  }
                  else if (all(!g[k])) {
                    bid <- response(bid, l, l)
                  }
                  else if (any(g[k] & greater("3N", cheapest(cheapest(bid[["bid"]], 
                    l), k)))) {
                    p <- g[k] & greater("3N", cheapest(cheapest(bid[["bid"]], 
                      l), k))
                    bid <- response(bid, l, max(k[p]))
                  }
                  else {
                    bid <- response(bid, l, l)
                  }
                }
                else if (n[v] > 6) {
                  bid <- response(bid, "N", v)
                  for (k in 1:4) {
                    h <- setdiff(1:4, c(v, j, k))
                    if (all(g[h])) {
                      h <- "N"
                    }
                    else if (any(g[h] & greater("3N", cheapest(cheapest(bid[["bid"]], 
                      k), h)))) {
                      h <- tail(h[g[h] & greater("3N", cheapest(cheapest(bid[["bid"]], 
                        k), h))], 1)
                    }
                    else {
                      h <- v
                    }
                    bid <- response(bid, k, h)
                    if (k < 3) {
                      w <- grep(suits()[k], names(bid), value = T)
                      for (p in 1:4) {
                        if (n[p] + as.numeric(p == k) > 3) {
                          l <- p
                        }
                        else if (all(g[setdiff(1:4, c(v, j, p, 
                          k))])) {
                          l <- "N"
                        }
                        else if (n[j] + as.numeric(p == j | (p == 
                          k & k < j)) > 2) {
                          l <- j
                        }
                        else {
                          l <- v
                        }
                        if (greater("3N", cheapest(bid[[w]][["bid"]], 
                          p)) | p != l) 
                          bid[[w]] <- response(bid[[w]], p, l)
                      }
                    }
                  }
                }
                else {
                  bid <- response(bid, "N", tail(suits()[n == 
                    3], 1))
                  for (k in setdiff(1:4, v)) if (k == j & j == 
                    2 & n[j] == 2) {
                    bid <- response(bid, k, k, 4)
                  }
                  else if (all(g[setdiff(1:4, c(v, j, k))])) {
                    bid <- response(bid, k, "N")
                    l <- names(bid)[grepl(paste0(suits()[k], 
                      "$"), names(bid))]
                    if (bid[[l]][["bid"]] == "2N") 
                      for (p in 1:4) {
                        h <- ifelse(p == k & n[k] > 2 & k < 3, 
                          k, "N")
                        bid[[l]] <- response(bid[[l]], p, h)
                      }
                  }
                  else if (k == j & n[j] > 1) {
                    bid <- response(bid, k, j)
                  }
                  else {
                    l <- suits()[n == 3]
                    bid <- response(bid, k, l)
                  }
                  h <- setdiff(1:4, c(v, j))
                  if (all(g[h])) {
                    bid <- response(bid, v, "N")
                    bid <- response(bid, j, "N")
                  }
                  else if (any(g[h] & 1:4 < v)) {
                    bid <- response(bid, v, (1:4)[g[h] & 1:4 < 
                      v])
                  }
                  else {
                    bid <- response(bid, v, v)
                  }
                }
            }
            else if (n[j] > 2 & any(!g[-j])) {
                bid <- list(bid = paste0("3", suits()[j]))
                if (n[v] > 5) {
                  if (greater("2N", bid[["bid"]])) 
                    bid <- response(bid, "N", v)
                  for (k in suits()[-j]) bid <- response(bid, 
                    k, v)
                }
                else {
                  bid <- response(bid, v, v)
                  for (k in setdiff(1:4, c(v, j))) {
                    h <- setdiff(1:4, c(v, j, k))
                    h <- ifelse(g[h], "N", j)
                    bid <- response(bid, k, h)
                    l <- grep(paste0(suits()[k], "$"), names(bid), 
                      value = T)
                    if (n[k] < 3) 
                      bid[[l]] <- response(bid[[l]], k, j)
                  }
                }
            }
            else {
                bid <- list(bid = "2N")
                h <- setdiff(1:4, c(v, j))
                h <- ifelse(all(g[h]), "N", v)
                bid <- response(bid, v, h)
                if (n[j] == 2 & j < 3) {
                  bid <- response(bid, j, j)
                }
                else {
                  bid <- response(bid, j, "N")
                }
                for (k in setdiff(1:4, c(v, j))) {
                  h <- setdiff(1:4, c(v, j, k))
                  if (all(g[h])) 
                    bid <- response(bid, k, "N")
                }
            }
            z[[paste0("2", suits()[j])]] <- bid
        }
    z
}

#' opener.2C
#' 
#' opener's subsequent bids after 2C
#' @param x = internal representation of a bridge hand
#' @keywords opener.2C
#' @export
#' @family opener

opener.2C <- function (x) 
{
    n <- shape.detl(x)
    m <- hcp(x) + len(x)
    ord <- order(n, decreasing = T)
    if (all(n[ord][-4] == rep(4, 3))) {
        v <- ord[3]
    }
    else {
        v <- ord[1]
    }
    z <- list()
    if (bal(x)) {
        bid <- list(bid = paste0(2 + level(m, 25), "N"))
        if (bid[["bid"]] == "2N") 
            bid <- c(bid, opener.2N(x))
    }
    else if (v > 2) {
        bid <- list(bid = cheapest("2D", v))
        bid <- response(bid, 2, ifelse(n[2] > 3, 2, ifelse(n[1] > 
            3, 1, "N")))
        bid <- response(bid, 1, ifelse(n[1] > 3, 1, "N"))
    }
    else if (v == 1) {
        bid <- list(bid = "2S")
        bid <- response(bid, 2, ifelse(n[2] > 3, 2, "N"))
        if (m > 24) {
            bid[["2N"]] <- list(bid = ifelse(n[1] > 5, "3S", 
                "3N"))
            bid[["3S"]] <- list(bid = "4S")
        }
    }
    else {
        bid <- list(bid = "2H")
        if (m > 24) 
            bid[["3H"]] <- list(bid = "4H")
        if (n[1] > 3) {
            bid[["2S"]] <- list(bid = paste0(3 + level(m, 25), 
                "S"))
            if (m > 24) 
                bid[["2N"]] <- list(bid = "3N")
        }
        else if (n[2] > 5 & m > 24) {
            bid[["2N"]] <- list(bid = "3H")
            bid[["2S"]] <- list(bid = "3H")
            bid[["2S"]] <- response(bid[["2S"]], 1, ifelse(n[1] == 
                3, 1, "N"))
        }
        else if (m > 24) {
            bid[["2S"]] <- list(bid = "3N")
            bid[["2N"]] <- list(bid = "3N")
        }
        else {
            bid[["2S"]] <- list(bid = "2N")
            bid[["2S"]] <- response(bid[["2S"]], 1, ifelse(n[1] > 
                2, 1, "N"))
        }
    }
    z[["2D"]] <- bid
    if (n[2] > 2) {
        bid <- list(bid = "4H")
    }
    else if (n[1] > 3) {
        bid <- list(bid = "2S")
        bid <- response(bid, 2, ifelse(n[2] > 1, 2, ifelse(n[1] > 
            4, 1, "N")))
    }
    else {
        bid <- list(bid = "3N")
    }
    z[["2H"]] <- bid
    if (n[1] > 2) {
        bid <- list(bid = "4S")
    }
    else if (n[2] > 3) {
        bid <- list(bid = "3H")
        bid <- response(bid, 1, ifelse(n[1] == 2, 1, "N"))
    }
    else {
        bid <- list(bid = "3N")
    }
    z[["2S"]] <- bid
    for (j in 3:4) if (n[2] > 3) {
        bid <- list(bid = "3H")
        bid <- response(bid, 1, ifelse(n[1] > 3, 1, "N"))
        z[[paste0(3, suits()[j])]] <- bid
    }
    else if (n[1] > 3) {
        z[[paste0(3, suits()[j])]] <- list(bid = "3S")
    }
    else {
        z[[paste0(3, suits()[j])]] <- list(bid = "3N")
    }
    z
}

#' opener.2N
#' 
#' opener's subsequent bids after 2N
#' @param x = internal representation of a bridge hand
#' @keywords opener.2N
#' @export
#' @family opener

opener.2N <- function (x) 
{
    n <- shape.detl(x)
    z <- list()
    z[["3C"]] <- list(bid = stayman(x, 3))
    if (all(n[1:2] < 4)) 
        for (j in 1:2) z[["3C"]] <- response(z[["3C"]], j, ifelse(n[j] == 
            3, j, "N"))
    z[["3H"]] <- list(bid = ifelse(n[2] > 2, "4H", "3N"))
    z[["3S"]] <- list(bid = ifelse(n[1] > 2, "4S", "3N"))
    z
}

#' opener.2Nresp
#' 
#' opener's subsequent bids after 1M
#' @param x = internal representation of a bridge hand
#' @param y = opening bid
#' @keywords opener.2Nresp
#' @export
#' @family opener

opener.2Nresp <- function (x, y) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    v <- match(gsub("\\d", "", y), suits())
    if (v < 3 & n[v] > 5) {
        bid <- list(bid = paste0(3, suits()[v]))
    }
    else if (v == 1 & n[2] > 3) {
        bid <- list(bid = "3H")
    }
    else {
        bid <- list(bid = "3N")
    }
    z <- list()
    z[["2N"]] <- bid
    z
}

#' opener.preempt
#' 
#' opener's subsequent bids after a preempt
#' @param x = internal representation of a bridge hand
#' @keywords opener.preempt
#' @export
#' @family opener

opener.preempt <- function (x) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    v <- match(gsub("^\\d", "", open(x)), suits())
    z <- list()
    for (j in setdiff(1:2, v)) {
        y <- cheapest(paste0(n[v] - 4, suits()[v]), j)
        z[[y]] <- list(bid = cheapest(y, ifelse(n[j] > 2, j, 
            v)))
    }
    z
}

#' resp
#' 
#' responder's bid
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords resp
#' @export
#' @family resp

resp <- function (x, y) 
{
    if (y == "1N") {
        z <- resp.1N(x)
    }
    else if (grepl("^1(S|H)$", y)) {
        z <- resp.1minorajor(x, y)
    }
    else if (grepl("^1(C|D)$", y)) {
        z <- resp.1minor(x, y)
    }
    else {
        z <- "?"
    }
    z
}

#' resp.1minor
#' 
#' responder's bid
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords resp.1minor
#' @export
#' @family resp

resp.1minor <- function (x, y) 
{
    n <- shape.detl(x)
    m <- n
    m[match(gsub("^\\d", "", y), suits())] <- 0
    h <- n[as.numeric(y == "1C") + 3]
    z <- hcp(x)
    if (z < 6) {
        z <- "P"
    }
    else if (z > 11 & max(m) > 4) {
        z <- head(suits()[m == max(m)], 1)
        z <- paste0(as.numeric(gsub("[^0-9]", "", y)) + senior(z, 
            y), z)
    }
    else if (max(n[1:2]) > 4) {
        z <- suits(2)[n[1:2] == max(n[1:2])]
        z <- paste0("1", head(z, 1))
    }
    else if (max(n[1:2]) == 4) {
        z <- suits(2)[n[1:2] == max(n[1:2])]
        z <- paste0("1", tail(z, 1))
    }
    else if (h > 4 & is.element(z, 6:11)) {
        z <- gsub("^1", 2 + level(z, 10), y)
    }
    else if (bal(x) & is.element(z, 12:18)) {
        z <- paste0(2 + level(z, 16), "N")
    }
    else if (bal(x) & z > 18) {
        z <- "6N"
    }
    else if (y == "1C" & n[3] > 4) {
        z <- "1D"
    }
    else if (is.element(z, 6:11)) {
        z <- "1N"
    }
    else if (y == "1C" & n[3] == 4) {
        z <- "1D"
    }
    else if (is.element(z, 12:15)) {
        z <- "2N"
    }
    else if (is.element(z, 16:18)) {
        z <- "3N"
    }
    else {
        z <- "6N"
    }
    z
}

#' resp.1minorajor
#' 
#' responder's response to 1M
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords resp.1minorajor
#' @export
#' @family resp

resp.1minorajor <- function (x, y) 
{
    n <- shape.detl(x)
    u <- shortness(n, y)
    h <- n[as.numeric(y == "1H") + 1]
    z <- hcp(x)
    if (grepl("^1(S|H)$", y) & h > 2 & u < 3) {
        z <- z + h - u
    }
    if (z < 6) {
        z <- "P"
    }
    else if (h > 2) {
        z <- gsub("^1", 2 + level(z, c(10, 12)), y)
    }
    else if (z > 11 & max(n) > 4) {
        z <- head(suits()[n == max(n)], 1)
        if (max(n) > 7 & any(z == suits(2))) {
            z <- paste0(4, z)
        }
        else {
            z <- paste0(as.numeric(gsub("[^0-9]", "", y)) + senior(z, 
                y), z)
        }
    }
    else if (y == "1H" & n[1] > 3) {
        z <- "1S"
    }
    else if (is.element(z, 12:15)) {
        z <- "2N"
    }
    else if (is.element(z, 16:18)) {
        z <- "3N"
    }
    else if (z > 18) {
        z <- "6N"
    }
    else {
        z <- "1N"
    }
    z
}

#' resp.1N
#' 
#' responder's response to 1N
#' @param x = internal representation of a bridge hand
#' @keywords resp.1N
#' @export
#' @family resp

resp.1N <- function (x) 
{
    n <- shape.detl(x)
    z <- hcp(x) + len(x)
    if (z < 8 & max(n[1:2]) < 5 & min(n[1:3]) > 3) {
        z <- "2C"
    }
    else if (z < 8 & max(n[1:3]) < 5) {
        z <- "P"
    }
    else if (z < 8) {
        z <- head(c("S", "H", "D")[n[1:3] == max(n[1:3])], 1)
        z <- paste0("2", z)
    }
    else if (z > 7 & any(n[1:2] == 4) & all(n[1:2] > 3)) {
        z <- "2C"
    }
    else if (z > 9 & min(n[1:2]) > 4) {
        z <- "3S"
    }
    else if (z > 9 & max(n[1:2]) > 5) {
        z <- head(c("S", "H")[n[1:2] == max(n[1:2])], 1)
        z <- paste0("4", z)
    }
    else if (z > 9 & max(n[1:2]) == 5 & min(n[1:2]) != 4) {
        z <- head(c("S", "H")[n[1:2] == 5], 1)
        z <- paste0("3", z)
    }
    else if (max(n[1:2]) > 3) {
        z <- "2C"
    }
    else if (is.element(z, 8:9)) {
        z <- "2N"
    }
    else {
        z <- "3N"
    }
    z
}

#' resp.stayman
#' 
#' Responder's response to opener after Stayman
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords resp.stayman
#' @export
#' @family resp

resp.stayman <- function (x, y) 
{
    n <- shape.detl(x)
    u <- shortness(n, y)
    h <- n[as.numeric(y == "2H") + 1]
    if (y == "2S") {
        r <- 0
    }
    else {
        r <- match(gsub("^\\d", "", y), suits())
        r <- n[2:r - 1]
    }
    z <- hcp(x)
    if (y != "2D" & h > 3 & u < 3) {
        z <- z + h - u
    }
    else {
        z <- z + len(x)
    }
    if (z < 8) {
        z <- "P"
    }
    else if (y != "2D" & h > 3 & is.element(z, 8:9)) {
        z <- gsub("^2", 3, y)
    }
    else if (y != "2D" & h > 3) {
        z <- gsub("^2", 4, y)
    }
    else if (y == "2S" & is.element(z, 8:9) & n[2] > 5) {
        z <- "3H"
    }
    else if (y == "2S" & is.element(z, 8:9)) {
        z <- "2N"
    }
    else if (y == "2S") {
        z <- "3N"
    }
    else if (z > 9 & any(n[1:2] > 5)) {
        z <- head(suits(2)[n[1:2] > 4], 1)
        z <- paste0("4", z)
    }
    else if (z > 9 & any(n[1:2] > 4)) {
        z <- head(suits(2)[n[1:2] > 4], 1)
        z <- paste0("3", z)
    }
    else if (any(r > 4)) {
        z <- r == max(r)
        z <- head(suits()[seq_along(z)][z], 1)
        z <- paste0("2", z)
    }
    else if (is.element(z, 8:9)) {
        z <- "2N"
    }
    else {
        z <- "3N"
    }
    z
}

#' resp.stayman.wrapper
#' 
#' responder's response to 1N
#' @param x = internal representation of a bridge hand
#' @keywords resp.stayman.wrapper
#' @export
#' @family resp

resp.stayman.wrapper <- function (x) 
{
    z <- paste0("2", suits(3))
    z <- split(z, z)
    z <- lapply(z, function(z) resp.stayman(x, z))
    z <- lapply(z, function(z) list(bid = z))
    z
}

#' responder
#' 
#' responder's response to 1N
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords responder
#' @export
#' @family responder

responder <- function (x, y) 
{
    if (y == "2C") {
        z <- responder.2C(x)
    }
    else if (grepl("^(2[DHS]|3[^N])$", y)) {
        z <- responder.preempt(x, y)
    }
    else if (y == "1N") {
        z <- responder.1N(x)
    }
    else if (y == "2N") {
        z <- responder.2N(x)
    }
    else if (grepl("^1(H|S)$", y)) {
        z <- responder.1major(x, y)
    }
    else if (grepl("^1(C|D)$", y)) {
        z <- responder.1minor(x, y)
    }
    else {
        z <- list(bid = "P")
    }
    z
}

#' responder.1major
#' 
#' responder's response to 1M
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords responder.1major
#' @export
#' @family responder

responder.1major <- function (x, y) 
{
    n <- shape.detl(x)
    m <- hcp(x)
    u <- n[as.numeric(y == "1H") + 1]
    u <- m + u - shortness(n, y)
    z <- list(bid = resp.1minorajor(x, y))
    if (u > 7 & z[["bid"]] == gsub("^1", "2", y)) {
        z[[gsub("^1", "3", y)]] <- list(bid = gsub("^1", "4", 
            y))
    }
    if (z[["bid"]] == "1S") 
        z <- c(z, responder.1Xresp(x, y, z[["bid"]]))
    if (z[["bid"]] == "1N") 
        z <- c(z, responder.1Nresp(x, y))
    if (z[["bid"]] == "2N") 
        z <- c(z, responder.2Nresp(x, y))
    if (any(z[["bid"]] == paste0(2, suits()[suits() < gsub("^1", 
        "", y)]))) 
        z <- c(z, responder.21(x, y, z[["bid"]]))
    z
}

#' responder.1minor
#' 
#' responder's response to 1m
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords responder.1minor
#' @export
#' @family responder

responder.1minor <- function (x, y) 
{
    v <- match(gsub("^\\d", "", y), suits())
    n <- shape.detl(x)
    m <- hcp(x)
    w <- stopped.suits(x)
    w[v] <- T
    u <- m + n[v] - shortness(n, y)
    z <- list(bid = resp.1minor(x, y))
    if (m > 7 & z[["bid"]] == gsub("^1", "2", y)) 
        z[["2N"]] <- list(bid = "3N")
    if (z[["bid"]] == gsub("^1", 2, y)) {
        for (j in 2:v - 1) if (all(ifelse(seq_along(w) == j, 
            T, w)[1:2])) {
            z[[paste0(2, suits()[j])]] <- list(bid = paste0(2 + 
                level(m, 8), "N"))
        }
        else if (j == 3 & any(ifelse(seq_along(w) == j, T, w)[1:2])) {
            bid <- list(bid = paste0(2, head(suits()[ifelse(seq_along(w) == 
                j, T, w)], 1)))
            if (m > 7) 
                bid[["2N"]] <- list(bid = "3N")
            if (u > 10) 
                bid[["3D"]] <- list(bid = paste0(4 + level(u, 
                  13), "D"))
            if (u > 8) 
                bid[[gsub("^1", 4, y)]] <- list(bid = gsub("^1", 
                  5, y))
            z[[paste0(2, suits()[j])]] <- bid
        }
        else {
            bid <- list(bid = gsub("^1", 3 + level(u, 11), y))
            if (is.element(u, 9:10)) 
                bid[[gsub("^1", 4, y)]] <- list(bid = gsub("^1", 
                  5, y))
            z[[paste0(2, suits()[j])]] <- bid
        }
        if (y == "1D") 
            if (all(w[1:3]) & m > 7) {
                z[["3C"]] <- list(bid = "3N")
            }
            else {
                z[["3C"]] <- list(bid = paste0(3 + level(u, c(11, 
                  13)), "D"))
            }
    }
    if (z[["bid"]] == gsub("^1", 3, y)) {
        if (u > 10) 
            z[[gsub("^1", 4, y)]] <- list(bid = gsub("^1", 5, 
                y))
        for (j in 2:v - 1) if (all(ifelse(seq_along(w) == j, 
            T, w)[1:2])) {
            z[[paste0(3, suits()[j])]] <- list(bid = "3N")
        }
        else if (j == 3 & any(ifelse(seq_along(w) == j, T, w)[1:2])) {
            bid <- list(bid = paste0(3, head(suits()[ifelse(seq_along(w) == 
                j, T, w)], 1)))
            z[[paste0(3, suits()[j])]] <- bid
        }
        else if (u > 11) {
            z[[paste0(3, suits()[j])]] <- list(bid = gsub("^1", 
                4 + level(u, 14), y))
        }
    }
    if (grepl("^1(D|H|S)$", z[["bid"]])) 
        z <- c(z, responder.1Xresp(x, y, z[["bid"]]))
    if (z[["bid"]] == "1N") 
        z <- c(z, responder.1Nresp(x, y))
    if (any(z[["bid"]] == paste0(2, suits()[suits() < gsub("^1", 
        "", y)]))) 
        z <- c(z, responder.21(x, y, z[["bid"]]))
    z
}

#' responder.1N
#' 
#' responder's response to 1N
#' @param x = internal representation of a bridge hand
#' @keywords responder.1N
#' @export
#' @family responder

responder.1N <- function (x) 
{
    n <- shape.detl(x)
    m <- hcp(x) + len(x)
    fcn <- function(z) {
        if (gsub(".$", "", z[["bid"]]) == 2) {
            z[["2N"]] <- list(bid = gsub("^2", "3", z[["bid"]]))
            z[["3N"]] <- list(bid = gsub("^2", "4", z[["bid"]]))
        }
        z
    }
    z <- list(bid = resp.1N(x))
    if (m > 7 & z[["bid"]] == "2C") {
        z <- c(z, resp.stayman.wrapper(x))
    }
    if (m > 9 & all(n[1:2] > 4)) {
        z[["3N"]] <- list(bid = "4H")
    }
    else if (all(n[1:2] > 4)) {
        z[["2D"]][["2N"]] <- list(bid = "3H")
        z[["2D"]][["3N"]] <- list(bid = "4H")
    }
    else if (any(n[1:2] > 5)) {
        z <- c(z[1], lapply(z[-1], fcn))
    }
    z
}

#' responder.1Nresp
#' 
#' responder's response to <y> 1N
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords responder.1Nresp
#' @export
#' @family responder

responder.1Nresp <- function (x, y) 
{
    v <- match(gsub("^\\d", "", y), suits())
    n <- shape.detl(x)
    m <- hcp(x)
    ord <- order(n, decreasing = T)
    r <- ord[1]
    z <- list()
    if (m > 8) 
        z[[paste0(3, suits()[v])]] <- list(bid = "3N")
    if (v > 1) 
        for (u in 2:v - 1) {
            z[[paste0(2, suits()[u])]] <- list(bid = paste0(2 + 
                level(m, 9), "N"))
        }
    if (v < 4) 
        for (u in v:3 + 1) {
            if (u == 2 & n[2] > 3 & m + n[2] - min(n) > 8) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(3 + 
                  level(m + n[2] - min(n), 12), suits()[u]))
            }
            else if (m > 8 & n[u] > 3) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(3, 
                  suits()[u]))
            }
            else if (m > 8 & n[r] > 5) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(3, 
                  suits()[r]))
            }
            else if (m > 8) {
                z[[paste0(2, suits()[u])]] <- list(bid = "2N")
            }
            else if (n[u] > 2) {
                z[[paste0(2, suits()[u])]] <- list(bid = "P")
            }
            else if (r < u & n[r] > 5) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(2, 
                  suits()[r]))
            }
            else if (n[v] > 1) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(2, 
                  suits()[v]))
            }
            else if (r < u & n[r] > 4) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(2, 
                  suits()[r]))
            }
            z[[paste0(3, suits()[u])]] <- list(bid = ifelse(u == 
                2 & n[u] > 3, "4H", "3N"))
        }
    z
}

#' responder.1Xresp
#' 
#' opener's subsequent bids after 1M
#' @param x = internal representation of a bridge hand
#' @param y = opener's initial bid
#' @param h = responder's initial response
#' @keywords responder.1Xresp
#' @export
#' @family responder

responder.1Xresp <- function (x, y, h) 
{
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    m <- hcp(x)
    v <- match(gsub("^\\d", "", y), suits())
    r <- match(gsub("^\\d", "", h), suits())
    j <- head(setdiff(ord, c(v, r)), 1)
    z <- list()
    if (r < 3 & m + n[r] - min(n) > 9) {
        z[[gsub("^1", 2, h)]] <- list(bid = gsub("^1", 3 + level(m + 
            n[r] - min(n), 12), h))
        z[[gsub("^1", 3, h)]] <- list(bid = gsub("^1", 4, h))
    }
    else if (r < 3 & m + n[r] - min(n) == 9) {
        z[[gsub("^1", 3, h)]] <- list(bid = gsub("^1", 4, h))
    }
    if (r > 1) 
        for (u in 2:r - 1) {
            k <- setdiff(1:4, c(u, v, r))
            if (n[u] > 3) {
                bid <- list(bid = paste0(2 + level(m + n[u] - 
                  min(n), c(9, 12)), suits()[u]))
            }
            else if (m > 11 & r < 3 & n[r] > 4) {
                bid <- list(bid = cheapest(paste0(1, suits()[u]), 
                  k))
                bid <- responder.4thSuit(bid, n, v, r, u, k)
            }
            else if (m > 11) {
                bid <- list(bid = "3N")
            }
            else if (n[r] > 5) {
                bid <- list(bid = paste0(2 + level(m, 9), suits()[r]))
            }
            else if (n[v] > 3) {
                bid <- list(bid = paste0(2 + level(m, 9), suits()[v]))
            }
            else {
                bid <- list(bid = paste0(1 + level(m, 9), "N"))
            }
            z[[paste0(1, suits()[u])]] <- bid
        }
    if (v - r > 1) 
        for (u in seq(r + 1, v - 1)) {
            k <- setdiff(1:4, c(u, v, r))
            if (m < 9) {
                bid <- list(bid = cheapest(cheapest(h, u), ifelse(n[v] > 
                  2, v, "N")))
            }
            else if ((n[k] > 3 & k < 3) | (n[r] > 4 & r < 3)) {
                bid <- list(bid = cheapest(cheapest(h, u), k))
                if (n[r] > 4 & r < 3) {
                  bid <- response(bid, r, r, 4)
                }
                else {
                  bid <- response(bid, r, "N", 3)
                }
                if (n[k] > 3 & k < 3) {
                  bid <- response(bid, k, k, 4)
                }
                else {
                  bid <- response(bid, k, "N", 3)
                }
            }
            else if (u < 3 & n[u] > 3) {
                bid <- list(bid = paste0(3 + level(m, 9), suits()[u]))
            }
            else {
                bid <- list(bid = "3N")
            }
            z[[paste0(2, suits()[u])]] <- bid
        }
    if (j < r & r > 2 & m > 11 & n[j] > 3) {
        bid <- list(bid = paste0(2, suits()[j]))
        bid <- response(bid, j, ifelse(j < 3, j, "N"))
        for (k in c("N", suits()[-j])) bid <- response(bid, k, 
            "N")
        z[[paste0(2, suits()[r])]] <- bid
    }
    else if (j < r & r > 2 & m > 11) {
        z[[paste0(2, suits()[r])]] <- list(bid = "3N")
    }
    if (m > 8 & r > 2) 
        z[[paste0(3, suits()[r])]] <- list(bid = "3N")
    if (v < 4) 
        for (u in v:3 + 1) {
            z[[paste0(3, suits()[u])]] <- list(bid = "3N")
        }
    if (r > 1) 
        for (u in 2:r - 1) if (n[u] > 3) {
            z[[paste0(2, suits()[u])]] <- list(bid = paste0(4, 
                suits()[u]))
        }
        else if (r < 3 & n[r] > 4) {
            z[[paste0(2, suits()[u])]] <- list(bid = paste0(3, 
                suits()[r]))
        }
        else {
            z[[paste0(2, suits()[u])]] <- list(bid = "3N")
        }
    if (m > 11 & r < 3 & n[r] > 5) {
        z[["1N"]] <- list(bid = paste0(4, suits()[r]))
    }
    else if (m > 10) {
        z[["1N"]] <- list(bid = paste0(2 + level(m, 12), "N"))
    }
    else if (n[r] > 5) {
        z[["1N"]] <- list(bid = paste0(2, suits()[r]))
    }
    if (n[r] > 5 & r < 3) {
        z[["2N"]] <- list(bid = paste0(4, suits()[r]))
    }
    else if (m > 6) {
        z[["2N"]] <- list(bid = "3N")
    }
    if (m > 11 & j < r & n[j] > 3) {
        bid <- list(bid = paste0(2, suits()[j]))
        bid <- response(bid, j, j)
        bid <- response(bid, "N", "N")
        bid <- response(bid, r, ifelse(r < 3 & n[r] > 4, r, "N"))
        bid <- response(bid, v, ifelse(v < 3 & n[v] > 0, v, "N"))
        bid <- response(bid, setdiff(1:4, c(j, r, v)), "N")
        z[[gsub("^1", 2, y)]] <- bid
    }
    else if (n[r] > 5 & is.element(m, 9:11)) {
        z[[gsub("^1", 2, y)]] <- list(bid = gsub("^1", 3, h))
    }
    else if (m > 8 & j < r & n[j] > 3) {
        z[[gsub("^1", 2, y)]] <- list(bid = cheapest(gsub("^1", 
            2, y), j))
    }
    else if (m > 11 & r < 3 & n[r] > 6) {
        z[[gsub("^1", 2, y)]] <- list(bid = gsub("^1", 4, h))
    }
    else if (m > 11) {
        z[[gsub("^1", 2, y)]] <- list(bid = "3N")
    }
    if (m > 8) 
        z[[gsub("^1", 3, y)]] <- list(bid = ifelse(n[v] > 1 & 
            v < 3, gsub("^1", 4, y), "3N"))
    if (v < 4) 
        for (u in v:3 + 1) {
            k <- setdiff(1:4, c(u, v, r))
            if (m > 11 & ((r < 3 & n[r] > 4) | (k < 3 & n[k] > 
                3))) {
                bid <- list(bid = cheapest(paste0(2, suits()[u]), 
                  k))
                bid <- responder.4thSuit(bid, n, v, r, u, k)
                z[[paste0(2, suits()[u])]] <- bid
            }
            else if (m > 8) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(2 + 
                  level(m, 12), "N"))
            }
            else if (n[v] >= n[u]) {
                z[[paste0(2, suits()[u])]] <- list(bid = paste0(2, 
                  suits()[v]))
            }
        }
    z
}

#' responder.21
#' 
#' opener's subsequent bids after 1M
#' @param x = internal representation of a bridge hand
#' @param y = opener's initial bid
#' @param h = responder's initial response
#' @keywords responder.21
#' @export
#' @family responder

responder.21 <- function (x, y, h) 
{
    v <- match(gsub("^\\d", "", y), suits())
    r <- match(gsub("^\\d", "", h), suits())
    n <- shape.detl(x)
    ord <- order(n, decreasing = T)
    u <- setdiff(ord[1:2], r)
    g <- as.logical(stopped.suits(x))
    g <- g | n > 2
    z <- list()
    k <- ifelse(r < 3 & n[r] > 5, r, "N")
    z[[cheapest(h, "N")]] <- list(bid = cheapest(cheapest(h, 
        "N"), k))
    k <- setdiff(2:r - 1, v)
    if (all(g[k])) {
        bid <- list(bid = "3N")
    }
    else if (any(k < r & g[k])) {
        l <- k[k < r & g[k]]
        bid <- list(bid = paste0(3, suits()[l]))
        if (n[l] > 4 & l < 3) 
            bid <- response(bid, r, l)
        if (n[v] < 2) {
            bid <- response(bid, v, r)
        }
        else if (cheapest(bid[["bid"]], v) != paste0(4, suits()[v])) {
            bid <- response(bid, v, v)
        }
    }
    else {
        bid <- list(bid = paste0(4, suits()[r]))
    }
    z[[cheapest(h, r)]] <- bid
    if (v < 3 & n[v] == 2) {
        bid <- list(bid = paste0(4, suits()[v]))
    }
    else if (all(g[setdiff(1:4, c(v, r))])) {
        bid <- list(bid = "2N")
        l <- ifelse(n[v] == 1 & v < 3, v, "N")
        bid <- response(bid, v, l)
        for (k in setdiff(1:4, v)) bid <- response(bid, k, "N")
    }
    else if (r == 2 & n[r] > 5) {
        bid <- list(bid = "3H")
        bid <- response.wrapper(bid, 1, n, all(g[3:4]), 1:4, 
            c(1, 7, 4, 4))
    }
    else if (n[u] > 3) {
        bid <- list(bid = cheapest(cheapest(h, v), u))
        if (u != v) {
            bid <- response.wrapper(bid, u, n, all(g[k]), c(u, 
                v, r), c(5, 2, 6))
            bid <- response.wrapper(bid, r, n, all(g[k]), c(u, 
                v, r), c(5, 5, 5))
            bid <- response.wrapper(bid, v, n, all(g[k]), c(u, 
                v, r), c(5, 1, 6))
            if (greater("2N", bid[["bid"]])) {
                bid <- response.wrapper(bid, "N", n, F, c(u, 
                  v, r), c(5, 2, 6))
                l <- match(bid[["2N"]][["bid"]], paste0(3, suits()))
                if (l > 1) 
                  for (p in 2:l - 1) if (p == v & n[v] > 0 & 
                    v < 3) {
                    bid[["2N"]] <- response(bid[["2N"]], p, p)
                  }
                  else {
                    bid[["2N"]] <- response(bid[["2N"]], p, "N")
                  }
            }
        }
        else {
            if (length(k) == 1) {
                bid <- response(bid, k, "N")
            }
            else for (l in k) if (g[setdiff(k, l)]) {
                bid <- response(bid, l, "N")
            }
            else if (n[v] > 1) {
                bid <- response(bid, l, v)
            }
            else {
                bid <- response(bid, l, r)
            }
        }
    }
    else if (any(g[k])) {
        l <- k[g[k]]
        bid <- list(bid = cheapest(cheapest(h, v), l))
        if (n[l] > 4 & l < 3) {
            bid <- response(bid, l, l)
        }
        else if (n[v] > 1) {
            bid <- response(bid, l, v)
        }
        else if (l < v) {
            bid <- response(bid, l, r)
        }
        else if (l > v & n[r] > 5) {
            bid <- response(bid, l, r)
        }
        if (n[l] > 4 & l < 3) {
            bid <- response(bid, r, l)
        }
        else if (l < v) {
            bid <- response(bid, r, r)
        }
        bid <- response(bid, v, ifelse(n[v] > 0, v, r))
        if (greater("2N", bid[["bid"]])) 
            bid <- response(bid, "N", "N")
    }
    else {
        l <- ifelse(n[r] > 5, r, u)
        bid <- list(bid = cheapest(cheapest(h, v), l))
        l <- ifelse(n[u] > 4, u, r)
        l <- ifelse(n[v] > 0, v, l)
        if (greater("3N", cheapest(bid[["bid"]], v))) 
            bid <- response(bid, v, l)
        if (length(k) == 1) {
            if (greater("3N", cheapest(bid[["bid"]], k))) 
                bid <- response(bid, k, "N")
        }
        else for (l in k) {
            p <- ifelse(n[v] > 2, v, r)
            p <- ifelse(g[setdiff(k, l)], "N", p)
            bid <- response(bid, l, p)
        }
    }
    z[[cheapest(h, v)]] <- bid
    for (j in setdiff(1:4, c(v, r))) {
        k <- setdiff(1:4, c(v, r, j))
        if (j < 3 & n[j] > 3) {
            bid <- list(bid = paste0(4, suits()[j]))
        }
        else if (n[r] > 5 & v == 1 & r == 2 & !g[k]) {
            bid <- list(bid = cheapest(cheapest(h, j), r))
            bid <- response.wrapper(bid, v, n, g[k], c(j, v, 
                r), c(4, 2, 7))
            if (n[r] > 6 & n[j] < 3) 
                bid <- response(bid, j, r)
        }
        else if (n[k] > 3 & k < 3) {
            bid <- list(bid = cheapest(cheapest(h, j), k))
            if (cheapest(bid[["bid"]], "N") == "2N") 
                bid <- response.wrapper(bid, "N", n, T, c(k, 
                  v, r), c(5, 3, 6))
            if (greater("3N", cheapest(cheapest(bid[["bid"]], 
                v), k))) {
                bid <- response.wrapper(bid, v, n, T, c(k, v, 
                  r), c(5, 2, 6), c(0, 1, 0))
                l <- grep(paste0(suits()[v], "$"), names(bid), 
                  value = T)
                if (greater("3N", bid[[l]][["bid"]])) 
                  for (p in c("N", suits())) if (greater("3N", 
                    cheapest(bid[[l]][["bid"]], p))) 
                    bid[[l]] <- response(bid[[l]], p, "N")
            }
            else {
                bid <- response.wrapper(bid, v, n, T, c(v, r), 
                  c(2, 6), c(1, 0))
            }
            if (!greater(cheapest(bid[["bid"]], r), "3N")) 
                bid <- response(bid, r, "N")
            bid <- response(bid, j, ifelse(j < 3 & n[j] > 2, 
                j, "N"))
        }
        else if (g[k]) {
            bid <- list(bid = cheapest(cheapest(h, j), "N"))
            if (bid[["bid"]] == "2N") {
                if (v < 3 & n[v] == 2) 
                  bid <- response(bid, v, v, 4)
                else bid <- response(bid, v, "N")
                if (j < 3 & n[j] == 3) 
                  bid <- response(bid, j, j)
                else bid <- response(bid, j, "N")
                bid <- response(bid, k, "N")
                bid <- response(bid, r, "N")
            }
        }
        else if (n[j] > 3 & (r > j | n[r] < 6)) {
            if (j < 3) {
                bid <- list(bid = paste0(4, suits()[j]))
            }
            else {
                bid <- list(bid = cheapest(cheapest(h, j), j))
            }
            bid <- response.wrapper(bid, v, n, g[k], c(j, v, 
                r), c(4, 2, 6))
        }
        else if (n[v] > 3) {
            bid <- list(bid = cheapest(cheapest(h, j), v))
            bid <- response(bid, j, ifelse(j < 3 & n[j] == 3, 
                j, "N"))
            bid <- response(bid, k, "N")
        }
        else if (n[r] > 5) {
            bid <- list(bid = cheapest(cheapest(h, j), r))
            bid <- response(bid, k, "N")
            bid <- response.wrapper(bid, j, n, g[k], c(j, v, 
                r), c(3, 3 - as.numeric(j < v), 7))
            bid <- response.wrapper(bid, v, n, g[k], c(j, v, 
                r), c(4, 2, 7), c(0, 1, 0))
            l <- grep(paste0(suits()[v], "$"), names(bid), value = T)
            if (n[v] == 0 & n[r] > 7) 
                bid[[l]] <- response(bid[[l]], v, r)
        }
        else if (n[v] > 2) {
            bid <- list(bid = cheapest(cheapest(h, j), v))
            if (v > 1) 
                for (i in 2:v - 1) {
                  l <- ifelse(g[setdiff(1:4, c(v, i, r))], "N", 
                    v)
                  bid <- response(bid, i, l)
                }
        }
        else if (n[v] == 2 & v < 3) {
            bid <- list(bid = cheapest(cheapest(h, j), v))
        }
        else {
            bid <- list(bid = cheapest(cheapest(h, j), "N"))
            if (v < 3 & n[v] == 2) {
                bid <- response(bid, v, v, 4)
            }
            else if (bid[["bid"]] == "2N") {
                bid <- response(bid, v, "N")
            }
            if (bid[["bid"]] == "2N") 
                for (l in setdiff(1:4, v)) {
                  bid <- response(bid, l, "N")
                }
        }
        z[[cheapest(h, j)]] <- bid
    }
    z
}

#' responder.2C
#' 
#' responder's response to 2N
#' @param x = internal representation of a bridge hand
#' @keywords responder.2C
#' @export
#' @family responder

responder.2C <- function (x) 
{
    n <- shape.detl(x)
    m <- hcp(x)
    ord <- order(n, decreasing = T)
    r <- ord[1]
    if (n[r] > 4 & good.suit(x, F)[r]) {
        z <- list(bid = cheapest("2D", r))
        if (r == 2 & n[1] > 3) {
            z <- response(z, 1, 1, 4)
        }
        else if (r == 2 & n[r] > 5) {
            z <- response(z, 1, 2)
            z[["2S"]] <- response(z[["2S"]], 1, ifelse(n[1] == 
                3, 1, "N"))
        }
        else if (r == 2) {
            z <- response(z, 1, "N", 3)
        }
        else if (r == 1) {
            z <- response(z, 2, ifelse(n[2] > 3, 2, ifelse(n[1] > 
                5, 1, "N")))
        }
        else if (r > 2) {
            z <- response(z, 2, ifelse(n[2] > 3, 2, ifelse(n[1] > 
                3, 1, "N")))
            z <- response(z, 1, ifelse(n[1] > 3, 1, "N"))
        }
    }
    else {
        z <- list(bid = "2D")
        if (n[2] > 2) {
            z <- response(z, 2, 2, 3 + level(m, 3))
        }
        else if (n[1] > 3) {
            z <- response(z, 2, 1)
            z[["2H"]] <- response(z[["2H"]], 2, ifelse(n[1] > 
                4, 1, "N"))
            if (m > 2) {
                z[["2H"]] <- response(z[["2H"]], 1, 1)
                if (n[1] > 4) {
                  z[["2H"]] <- response(z[["2H"]], "N", 1)
                  z[["2H"]] <- response(z[["2H"]], 2, ifelse(n[2] == 
                    2, 2, 1))
                }
                else {
                  z[["2H"]] <- response(z[["2H"]], "N", "N")
                  z[["2H"]] <- response(z[["2H"]], 2, ifelse(n[2] == 
                    2, 2, "N"))
                }
            }
        }
        else {
            z <- response(z, 2, "N", 2 + level(m, 3))
            if (m < 3) 
                z[["2H"]] <- response(z[["2H"]], 2, ifelse(n[2] == 
                  2, 2, "N"))
        }
        if (n[1] > 2) {
            z <- response(z, 1, 1, 3 + level(m, 3))
        }
        else {
            z <- response(z, 1, "N", 2 + level(m, 3))
            if (m < 3) 
                z[["2S"]] <- response(z[["2S"]], 1, ifelse(n[1] == 
                  2, 1, "N"))
        }
        z[["2N"]] <- responder.2N(x, 22)
        for (j in 3:4) if (n[2] > 3) {
            l <- paste0(3, suits()[j])
            z[[l]] <- list(bid = "3H")
            z[[l]] <- response(z[[l]], 1, ifelse(n[1] > 3, 1, 
                "N"))
        }
        else if (n[1] > 3) {
            z[[paste0(3, suits()[j])]] <- list(bid = "3S")
        }
        else {
            z[[paste0(3, suits()[j])]] <- list(bid = "3N")
        }
    }
    z
}

#' responder.2N
#' 
#' responder's response to 2N
#' @param x = internal representation of a bridge hand
#' @param y = points promised by opener
#' @keywords responder.2N
#' @export
#' @family responder

responder.2N <- function (x, y = 20) 
{
    n <- shape.detl(x)
    m <- hcp(x) + len(x)
    if (m + y < 25) {
        z <- list(bid = "P")
    }
    else if (all(n[1:2] > 4)) {
        z <- list(bid = "3S")
        z[["3N"]] <- list(bid = "4H")
    }
    else if (all(sort(n[1:2]) == c(4, 6))) {
        z <- list(bid = "3C")
        z[["3D"]] <- list(bid = paste0(4, suits(2)[order(n[1:2], 
            decreasing = T)][1]))
        z[["3H"]] <- list(bid = "4H")
        z[["3S"]] <- list(bid = "4S")
    }
    else if (all(sort(n[1:2]) == 4:5)) {
        z <- list(bid = "3C")
        z[["3D"]] <- list(bid = paste0(3, suits(2)[order(n[1:2], 
            decreasing = T)][1]))
        z[["3H"]] <- list(bid = "4H")
        z[["3S"]] <- list(bid = "4S")
    }
    else if (any(n[1:2] > 5)) {
        z <- list(bid = paste0(4, suits(2)[order(n[1:2], decreasing = T)][1]))
    }
    else if (any(n[1:2] > 4)) {
        z <- list(bid = paste0(3, suits(2)[order(n[1:2], decreasing = T)][1]))
    }
    else if (any(n[1:2] > 3)) {
        z <- list(bid = "3C")
        z[["3D"]] <- list(bid = "3N")
        z[["3H"]] <- list(bid = ifelse(n[2] > 3, "4H", "3N"))
        z[["3S"]] <- list(bid = ifelse(n[1] > 3, "4S", "3N"))
    }
    else {
        z <- list(bid = "3N")
    }
    z
}

#' responder.2Nresp
#' 
#' responder's response to <y> 2N
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords responder.2Nresp
#' @export
#' @family responder

responder.2Nresp <- function (x, y) 
{
    v <- match(gsub("^\\d", "", y), suits())
    n <- shape.detl(x)
    m <- hcp(x)
    ord <- order(n, decreasing = T)
    z <- list()
    z[[paste0(3, suits()[v])]] <- list(bid = ifelse(n[v] == 2, 
        paste0(4, suits()[v]), "3N"))
    if (v == 1) 
        z[["3H"]] <- list(bid = ifelse(n[2] > 3, "4H", "3N"))
    z
}

#' responder.4thSuit
#' 
#' responder's subsequent bids after fourth suit forcing
#' @param bid = auction
#' @param n = suit distribution
#' @param v = an integer (opener's first suit)
#' @param r = an integer (responder's first suit)
#' @param u = an integer (opener's second suit)
#' @param k = an integer (fourth suit)
#' @keywords responder.4thSuit
#' @export
#' @family responder

responder.4thSuit <- function (bid, n, v, r, u, k) 
{
    if (cheapest(bid[["bid"]], "N") == "2N") {
        bid <- response(bid, "N", "N")
    }
    if (r < 3 & n[r] > 4) {
        bid <- response(bid, r, r, 4)
    }
    else {
        bid <- response(bid, r, "N", 3)
    }
    if (v < 3 & n[v] > 1) {
        bid <- response(bid, v, v, 4)
    }
    else {
        bid <- response(bid, v, "N", 3)
    }
    if (u < 3 & n[u] > 2) {
        bid <- response(bid, u, u, 4)
    }
    else {
        bid <- response(bid, u, "N", 3)
    }
    if (k < 3 & n[k] > 3) {
        bid <- response(bid, k, k, 4)
    }
    else {
        bid <- response(bid, k, "N", 3)
    }
    z <- bid
    z
}

#' responder.preempt
#' 
#' responder's response to a preempt
#' @param x = internal representation of a bridge hand
#' @param y = string (opener's bid)
#' @keywords responder.preempt
#' @export
#' @family responder

responder.preempt <- function (x, y) 
{
    n <- shape.detl(x)
    m <- hcp(x)
    ord <- order(n, decreasing = T)
    v <- match(gsub("^\\d", "", y), suits())
    r <- setdiff(ord, v)[1]
    if (m + n[v] < 17) {
        z <- list(bid = "P")
    }
    else if (grepl("^2", y) & r < 3 & n[r] > 4) {
        z <- list(bid = cheapest(y, r))
        if (r < v) 
            z <- response(z, r, r)
        z[[gsub(2, 3, y)]] <- list(bid = ifelse(v < 3, paste0(4, 
            suits()[v]), "3N"))
    }
    else {
        z <- list(bid = ifelse(v < 3, paste0(4, suits()[v]), 
            "3N"))
    }
    z
}

#' response
#' 
#' response to partner's response
#' @param x = list object (bid)
#' @param y = suit of partner's response
#' @param n = suit of your subsequent response
#' @param w = minimum level of response
#' @keywords response
#' @export

response <- function (x, y, n, w) 
{
    if (any(y == 1:4)) 
        y <- suits()[y]
    if (any(n == 1:4)) 
        n <- suits()[n]
    z <- cheapest(x[["bid"]], y)
    x[[z]] <- list(bid = cheapest(z, n))
    if (!missing(w)) 
        if (gsub(".$", "", x[[z]][["bid"]]) < w) {
            x[[z]][["bid"]] <- gsub("^\\d", w, x[[z]][["bid"]])
        }
    z <- x
    z
}

#' response.wrapper
#' 
#' response
#' @param x = internal representation of a bridge hand
#' @param y = suit to respond in
#' @param n = detailed suit count
#' @param w = T/F depending on whether NT is ok
#' @param h = suit of response
#' @param u = threshold for each response
#' @param v = 1/0 depending on whether meeting threshold confirms fit
#' @keywords response.wrapper
#' @export

response.wrapper <- function (x, y, n, w, h, u, v) 
{
    if (missing(v)) 
        v <- rep(0, length(h))
    z <- x
    done <- F
    v <- v[order(h)]
    u <- u[order(h)]
    h <- h[order(h)]
    j <- 1
    while (!done & h[j] < 3) {
        if (n[h[j]] >= u[j]) {
            if (y != h[j] | cheapest(z[["bid"]], y) != paste0(4, 
                suits()[y])) {
                if (v[j]) {
                  z <- response(z, y, h[j], 4)
                  done <- T
                }
                else if (greater("3N", cheapest(cheapest(z[["bid"]], 
                  y), h[j])) | !w) {
                  z <- response(z, y, h[j])
                  done <- T
                }
            }
            else {
                done <- T
            }
        }
        j <- j + 1
    }
    if (!done & greater("3N", cheapest(x[["bid"]], y)) & w) {
        z <- response(z, y, "N")
        done <- T
    }
    while (!done & j <= length(h)) {
        if (n[h[j]] >= u[j] & (h[j] != y | greater("3N", cheapest(x[["bid"]], 
            y)))) {
            z <- response(z, y, h[j])
            done <- T
        }
        j <- j + 1
    }
    z
}

#' senior
#' 
#' 0 if <x> is senior to <y> or 1
#' @param x = string (suit)
#' @param y = string (opener's bid)
#' @keywords senior
#' @export

senior <- function (x, y) 
{
    as.numeric(x < gsub("^.*(.)$", "\\1", y))
}

#' shape
#' 
#' shape of the hand
#' @param x = internal representation of a bridge hand
#' @keywords shape
#' @export

shape <- function (x) 
{
    z <- shape.detl(x)
    z <- paste(sort(z, decreasing = T), collapse = "")
    z
}

#' shape.detl
#' 
#' count of S, H, D, C
#' @param x = internal representation of a bridge hand
#' @keywords shape.detl
#' @export

shape.detl <- function (x) 
{
    z <- x%/%13
    z <- sapply(split(z, z), length)
    z <- z[match(3:0, names(z))]
    z <- as.numeric(ifelse(is.na(z), 0, z))
    z
}

#' shortness
#' 
#' shortness outside partner's suit
#' @param x = integer vector (detail shape)
#' @param y = string (opener's bid)
#' @keywords shortness
#' @export

shortness <- function (x, y) 
{
    z <- gsub("^.*(.)$", "\\1", y)
    z <- min(x[-match(z, suits())])
    z
}

#' stayman
#' 
#' Opener's response to Stayman
#' @param x = internal representation of a bridge hand
#' @param y = level of response
#' @keywords stayman
#' @export

stayman <- function (x, y = 2) 
{
    z <- shape.detl(x)[1:2] > 3
    if (any(z)) {
        z <- tail(c("S", "H")[z], 1)
    }
    else {
        z <- "D"
    }
    z <- paste0(y, z)
    z
}

#' stopped.suits
#' 
#' T/F depending on whether suit is stopped
#' @param x = integer vector (internal representation of cards in a suit)
#' @keywords stopped.suits
#' @export

stopped.suits <- function (x) 
{
    z <- sapply(split(x%%13, x%/%13), stopper)
    z <- z[match(3:0, names(z))]
    z <- ifelse(is.na(z), F, z)
    z
}

#' stopper
#' 
#' T/F depending on whether suit is stopped
#' @param x = integer vector (internal representation of cards in a suit)
#' @keywords stopper
#' @export

stopper <- function (x) 
{
    z <- any(x > 10)
    z <- z | (length(x) > 2 & any(x == 10))
    z <- z | (length(x) > 3 & any(x == 9))
    z <- z | length(x) > 4
    z
}

#' suit.symbols
#' 
#' suit symbol
#' @param x = a character (S/H/D/C)
#' @keywords suit.symbols
#' @export

suit.symbols <- function (x) 
{
    z <- c("<U+2660>", "<U+2665>", "<U+2666>", "<U+2663>")
    z <- rep(z, 2)
    y <- c(suits(), 3:0)
    z <- z[match(x, y)]
    z
}

#' suits
#' 
#' character vector of length 4
#' @param x = integer (number of suits)
#' @keywords suits
#' @export

suits <- function (x = 4) 
{
    c("S", "H", "D", "C")[1:x]
}

#' txt.left
#' 
#' Returns the left <y> characters
#' @param x = a string vector
#' @param y = a positive integer
#' @keywords txt.left
#' @export
#' @family txt

txt.left <- function (x, y) 
{
    substring(x, 1, y)
}

#' txt.regexp
#' 
#' converts <x> to a regular expression by padding certain characters with \\\\
#' @param x = a string
#' @keywords txt.regexp
#' @export
#' @family txt

txt.regexp <- function (x) 
{
    gsub("([\\^$.?*|+()[{])", "\\\\\\1", x)
}

#' txt.right
#' 
#' Returns the right <y> characters
#' @param x = a string vector
#' @param y = a positive integer
#' @keywords txt.right
#' @export
#' @family txt

txt.right <- function (x, y) 
{
    substring(x, nchar(x) - y + 1, nchar(x))
}

#' txt.trim
#' 
#' trims off leading/trailing occurences of <y>
#' @param x = a string vector
#' @param y = a string
#' @keywords txt.trim
#' @export
#' @family txt

txt.trim <- function (x, y = " ") 
{
    txt.trim.right(txt.trim.left(x, y), y)
}

#' txt.trim.left
#' 
#' trims off leading occurences of <y>
#' @param x = a string vector
#' @param y = a string
#' @keywords txt.trim.left
#' @export
#' @family txt

txt.trim.left <- function (x, y) 
{
    gsub(paste0("^(", txt.regexp(y), ")+"), "", x)
}

#' txt.trim.right
#' 
#' trims off trailing occurences of <y>
#' @param x = a string vector
#' @param y = a string
#' @keywords txt.trim.right
#' @export
#' @family txt

txt.trim.right <- function (x, y) 
{
    gsub(paste0("(", txt.regexp(y), ")*$"), "", x)
}
