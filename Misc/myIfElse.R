# just playing with the short-circuit evaluation of R in order to create my own if-else structure

myIfElse <- function(cond, true, false) {
  # If else, sans utiliser de if ou de else
  missing(false) && return(invisible(cond && true))
  invisible((cond && true) || false)
}

# Test if condition is TRUE
# As condition is TRUE, we should expect variable a to be created, but not b
myIfElse(TRUE, a <- 5, b <- 6)
if (!exists("a")) stop("a doesn't existed, TRUE doesn't execute the true instruction")
if (exists("b"))  stop("b exist but shouldn't. TRUE execute the false condition")
rm(a)

# Test if condition is FALSE without else
myIfElse(FALSE, a <- 5)  
if (exists("a")) stop("a exist but shouldn't. FALSE execute true condition")
# with this we can also check if R try to execute the false statement when 
# it is not given

# Test if condition is FALSE with else
myIfElse(FALSE, a <- 5, b <- 6)
# redondant with the precedent block
if (exists("a"))  stop("a exist but shouldn't. FALSE execute the true condition") 
if (!exists("b")) stop("b doesn't existed, FALSE doesn't execute the false instruction")
rm(b)
