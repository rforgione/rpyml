source("lwlr.R")

plot_lwlr <- function(X, y, tau, res) {
    x <- matrix(0, 2, 1)
    pred <- matrix(0, res, res)
    for (i in 1:res){
        for (j in 1:res) {
            x[1] <- 2*(i-1)/(res - 1) - 1
            x[2] <- 2*(j-1)/(res - 1) - 1
            pred[i,j] <- lwlr(X, y, x, tau)
        }
    }

plot.new()
image(pred, col=c('blue', 'red'), xlab="X1", ylab="X2", xaxt='n', yaxt='n', main=paste("LWLR: tau =", tau))
par(new=T)
plot((res/2)*(1+X[y==0,1])+0.5, (res/2)*(1+X[y==0,2])+0.5, xlab="", ylab="", xaxt='n', yaxt='n', xlim=c(1,res), ylim=c(1,res))
par(new=T)
points((res/2)*(1+X[y==1,1])+0.5, (res/2)*(1+X[y==1,2])+0.5, pch=4)
par(new=F)

}