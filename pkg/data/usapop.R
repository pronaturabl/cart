##'  Population data of the USA.
##'
##' @format A data frame with 52 observations on the following variable.
##' \describe{
##' \item{\code{population}}{a numeric vector: estimated number of people living
##' in a specific U.S. state in 2009}
##' }
##' The state names are given in the data frame row names.
##' @source \link{http://en.wikipedia.org/wiki/List_of_U.S._States_by_population},
## retrieved 29 September 2010 19:04 UTC, Page Version ID: 383596724
##' @examples
##' data(usapop)
##' str(usapop)
##' @keywords datasets
usapop <- read.table("usapop.txt", header = TRUE, sep = "\t", row.names = 1)
