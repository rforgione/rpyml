sigmoid <- function(z) {
    sig <- 1/(1+exp(-z))
    sig
}