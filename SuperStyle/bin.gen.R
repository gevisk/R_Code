########================================================================########
######                                                                    ######
####    bin.gen                                                             ####
##      Generate random binary numbers                                        ##
########================================================================########


# Return a matrix with n binary numbers with x elements
bin.gen <- function(x, n=1) {
  matrix((1:x %in% sample(1:x, round(runif(1) * (x - 1) + 1, 0))) * 1,
         nrow=n,
         ncol=x)
}

# I wrote it for the implementation of a genetic algorithm and then realized the algorithm was already implemented
