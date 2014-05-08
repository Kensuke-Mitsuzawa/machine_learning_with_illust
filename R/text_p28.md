# repmat関数を定義する
from  http://d.hatena.ne.jp/a_bicky/20100509/1273385433

```r
repmat <- function(A, m, n = m) {
    if (missing(m)) 
        stop("Requires at least 2 inputs.")
    
    if (is.vector(A)) 
        A <- t(matrix(A))
    
    d <- dim(A)
    nr <- d[1]
    nc <- d[2]
    
    if (length(m) <= 2) {
        m[2] <- ifelse(is.na(m[2]), n, m[2])
        
        if (length(d) > 2) {
            ## 2次元になるまで再帰で次元を削減
            ret <- array(dim = c(nr * m[1], nc * m[2], d[-(1:2)]))
            for (i in 1:d[length(d)]) {
                sep <- paste(rep(",", length(d) - 1), collapse = "")
                eval(parse(text = sprintf("ret[%si] <- repmat(A[%si], m[1], m[2])", 
                  sep, sep)))
            }
        } else {
            tmpA <- matrix(rep(t(A), m[1]), nrow = nr * m[1], byrow = TRUE)
            return(matrix(rep(tmpA, m[2]), nrow = nr * m[1], ncol = nc * m[2]))
        }
        ret
    } else {
        if (length(d) > 2) 
            stop("Doesn't support these arguments.")  # MATLABではサポートされている
        
        tmpA <- matrix(rep(t(A), m[1]), nrow = nr * m[1], byrow = TRUE)
        tmpA <- matrix(rep(tmpA, m[2]), nrow = nr * m[1], ncol = nc * m[2])
        array(rep(tmpA, prod(m[-(1:2)])), c(nr * m[1], nc * m[2], m[-(1:2)]))
    }
}
```


#機械学習p28のR実装


```r
n <- 50
N <- 1000

# 等間隔でベクトルを作成 xはn*1行列にする
x <- t(matrix(seq(-3, 3, length = n), 1))
X <- t(matrix(seq(-3, 3, length = N), 1))

pix <- 3.14 * x
y <- (sin(pix)/pix) + 0.1 * x + (0.05 * rnorm(n))


hh <- 2 * (0.3^2)
# n*1の行列の作成
t0 <- t(matrix(rnorm(n), 1))
e <- 0.1

for (o in 1:(n * 1000)) {
    # 一様分布のスカラ値＊ｎ
    i <- ceiling(runif(1) * n)
    # ガウスカーネルの戻り値 kiはn*1行列になるはず
    ki <- exp(-(x - x[i])^2/hh)
    t <- t0 - e * ki * as.numeric(t(ki) %*% t0 - y[i])
    if (norm(t - t0) < 1e-06) {
        print("stop iteration because of convergence")
        print(o)
        break
    }
    t0 <- t
}
```

```
## [1] "stop iteration because of convergence"
## [1] 30971
```

```r

K <- exp(-(repmat(X^2, 1, n) + repmat(t(x^2), N, 1) - 2 * X %*% t(x))/hh)
F <- K %*% t
```




```r
library(ggplot2)
x_axis <- c(-2.8, 0.5, 1.2, 2.8)
for_ggplot <- data.frame(value = F, x_axis = X)
syokichi <- data.frame(value = y, x_axis = x)

p1 <- ggplot(data = for_ggplot, aes(x = x_axis, y = value))
p1 + geom_line() + geom_line(data = syokichi, aes(x = x_axis, y = value, colour = "red")) + 
    labs(y = "確率的勾配法で学習した最小２乗学習．学習後が黒．学習前が赤")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

