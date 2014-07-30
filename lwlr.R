lwlr <- function (X_train, y_train, x, tau) {
    
    # initialize variables
    
    lambda <- .0001
    m <- dim(X_train)[1]
    n <- dim(X_train)[2]
    theta <- matrix(0,nrow=n, ncol=1)
    grad <- matrix(1, nrow=n, ncol=1)
    
    w <- exp(-rowSums((X_train - repmat(t(x), m, 1))^2) / (2*tau))
    
    while(sqrt(sum(grad^2)) > 1e-6) {
        h <- sigmoid(X_train %*% theta)
        grad <- t(X_train) %*% (w * (y_train - h)) - (lambda * theta)
        H <- -t(X_train) %*% ((as.matrix(diag(as.numeric(w*h*(1-h))))) %*% X_train) - (lambda*diag(n))
        theta <- theta - solve(H) %*% grad
    }
    
    pred <- as.double(t(theta) %*% x > 0)
}

repmat <- function(X, m, n) {
    ##R equivalent of repmat (matlab)
    mx <- dim(X)[1]
    nx <- dim(X)[2]
    
    matrix(t(matrix(X, mx, nx*n)), mx*m, nx*n, byrow=T)
}

sigmoid <- function(z) {
    sig <- 1/(1+exp(-z))
    sig
}
