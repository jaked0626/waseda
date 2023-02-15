bag <- 0:9
trial <- 1000
n <- 1 

## 0 から 9 までの数字が書かれた 10 枚の架空のカードを作成し “bag” と名付ける。
bag <- 0:9

## Parameters: n = 標本サイズ
##             T = 標本数
bag.experiment <- function(n=10, T=500){
  means <- rep(NA, T)
  s.mat <<- matrix(NA, ncol=n, nrow=T)
  for(i in 1:T){
    s <- sample(bag, n, replace=TRUE)
    s.mat[i,] <<- s
    means[i] <- mean(s)
    par(family="HiraKakuPro-W3", cex.lab=1.4, cex.axis=1.2, cex.main=1.6, mex=1.4,
        bg="black", fg="white", col.main="white", col.axis="white", col.lab="white")
    hist(means[1:i], axes=FALSE, freq=FALSE, xlim=c(0,9),
         xlab="sample mean", ylab="probability density",
         main=paste("sample size=", n, ", number of the sample=", i),
         col="gray")
    axis(1, 0:9)
    abline(v=4.5, lwd=2, col="red")
  }
  axis(2)
}

# シミュレーション結果を表示するためには次のコマンドを入力する。

quartz()
bag.experiment(n=100)

# n = 10, 100, ... と設定を変えて実行してみると、その違いが明確にわかる