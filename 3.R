plotLevy <- function(n = 100) {
  plot(cumsum(rt(n, df=2)), cumsum(rt(n, df=2)), 
       type = "o", pch = 16, lwd = 2,
       xlab = "", ylab = "", 
       axes = FALSE,
       main = "Levy flight")
  box()
}

dLevy <- function (y, m = 0, s = 1) {
  return (sqrt(s/(2*pi*(y-m)^3))*exp(-s/(2*(y-m))))
}

mLevy <- function (x, m = 0, s = 1) {
  return (integrate(dLevy, lower=m, upper=y, y=y, m=m, s=s)$value)
}

plotLevy()

par(mar=c(2,2,2,2))
op<-par(mfrow=c(1,2))

x <- seq(1, 5, length=100)
plot(x, dLevy(x), type="l", lty=2, main="Densitatea")
lines(x, dLevy(x), lwd=2, col="red")
plot(x, mLevy(x), type="l", lty=2, main="Masa")
lines(x, mLevy(x), lwd=2, col="red")

# intergrand f
f <- function(r,x = 1) x*exp(-r)   # order of arguments reversed
# integral
h <- function(x) integrate(dLevy, lower=1, upper=x, m = 1)$value
g <- Vectorize(h)
plot(x,g(x), col="red")

par(op)
