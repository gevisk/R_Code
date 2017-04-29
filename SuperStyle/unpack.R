########===============================================================####GvK#
####                                                                       ####
##          Unpack                                                           ##
#             Unpack all the values of a vector in a string                   #
########===============================================================########




# .unpack() : return ... as a list or a collapsed string if parser exist
# collapse : collapse argument of paste (only necessary if you want to collapse)
# select : subset the list, by default TRUE <=> all values
unpack <- function(..., collapse, select = TRUE) {
  if (!missing(collapse)) {
    return(paste(c(...)[select], collapse = collapse))
  }
  c(...)[select]
}

# csv style 
unpack(1, 4, 5, 1.2, 5, -2, collapse = ";")  # don't forget to specify collapse =
# same with a vector
x <- c(1, 4, 5, 1.2, 5, -2)
unpack(x, collapse = ";")  

# Creating formulas for models
data(iris)
library(MASS)
target <- "Species"

feat1 <- unpack(colnames(iris)[,1:4], collapse = "+")
feat2 <- unpack(colnames(iris)[,1:4], collapse = "*")
# we can unpack column only if they are numeric fo example : 
feat3 <- unpack(colnames(iris), collapse = "+",
                select=sapply(iris, is.numeric))
# without interactions
lda(as.formula(paste0(target, "~", feat1), data=iris)
# including interactions
lda(as.formula(paste0(target, "~", feat2), data=iris)
