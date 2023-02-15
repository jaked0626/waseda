library("tidyverse")

testdata <- read_csv("./testdata.csv")
# remove duplicate index row
testdata[1] <- NULL

X <- as.matrix(testdata[, 1:4], mode="numeric")
y <- as.vector(unlist(testdata[, 5]), mode="numeric")
eta <- 0.1


##### implements perceptron algorithm #####
perceptron <- function(X, y, eta, normalize=FALSE) {

    l <- nrow(X)
    n <- length(X[1, ])
    w <- numeric(n)
    b <- 0
    margin <- numeric(l)
    R <- max(apply(X, 1, function(x) norm(x, type="2")))
    k <- 0
    while(1) {
        k_temp <- k 
        for (i in seq(1, l, 1)) {
            gamma_ <- as.numeric(y[i] * (w %*% X[i,] + b))
            margin[i] <- gamma_
            if (gamma_ <= 0) {
                w <- w + eta * as.numeric(y[i]) * X[i, ]
                b <- b + eta * as.numeric(y[i]) * R^2
                k <- k + 1
            }
        }
        # end if no mistakes 
        if (k_temp == k) {
            break
        }
    }
    # print number of mistakes
    print(sprintf("Number of mistakes: %i", k))
    print(sprintf("Novikoff upper bound for mistakes: %f", (2 * R / min(margin))^2))
    print(sprintf("gamma: %f", min(margin)))
    # return normalized weight matrix if normalize is TRUE
    if (normalize) {
        b <- b / norm(w, type="2")
        w <- w / norm(w, type="2")
    }

    return (c(w, b))
}

##### implements dual perceptron algorithm #####
dual_perceptron <- function(X, y) {
    b <- 0; k <- 0; l <- length(y); a <- numeric(l);
    R <- max(apply(X, 1, function(x) norm(x, type="2")))

    margin <- numeric(l); margin.sum <- 0;

    while( margin.sum < 100){
        for(i in 1:l){
            margin[i] <- y[i]*(sum( (a*y*X)%*%X[i, ] )+b);

            if (margin[i] <=0) { 
                a[i] <- a[i]+1; b <- b+y[i]*R^2; k<-k+1  
            }
        }
        margin.sum <- sum(sign(margin));
    }

    w <- t(rep(1,100))%*%(a*y*X);
    b <- b / norm(w, type="2");
    w <- w / norm(w, type="2");
    gam <- min(margin);

    return(data.frame("weight"=w, "bias"=b, "margin"=gam, "Fault"=k))
}