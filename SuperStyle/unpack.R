########===============================================================####GvK#
####                                                                       ####
##          Unpack                                                           ##
#             Unpack all the values of a vector in a string                   #
########===============================================================########




# Input:
  v   : a vector
  sep : a character
# Output:
  a string of the concaneted values of v, separated by sep
unpack <- function(v, sep=' ') paste(v, collapse=sep)

# csv style
x <- c(1, 4, 5, 1.2, 5, -2)
unpack(x, ";")   # return "1;4;5;1.2;5;-2"

# Creating formulas for models
data(iris)
library(MASS)
target <- "Species"
features <- colnames(iris)[1:4]
lda(as.formula(paste(target, "~", unpack(features, "+"))), data=iris)
# including interactions
lda(as.formula(paste(target, "~", unpack(features, "*"))), data=iris)
