load_data <- function() {
    X <- as.matrix(read.table("data/x.dat"))
    y <- as.matrix(read.table("data/y.dat"))
    
    all <- list(X, y)
    
    all
}