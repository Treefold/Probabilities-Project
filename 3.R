library(rmutil)

plorBrow <- function (n = 100) {
  plot(cumsum(rnorm(n)), cumsum(rnorm(n)), 
       type = "o", pch = 16, lwd = 2,
       xlab = "", ylab = "", 
       axes = FALSE,
       main = "Brownian Motion")
  box()
}

plotLevy <- function(n = 100) {
  plot(cumsum(rt(n, df=2)), cumsum(rt(n, df=2)), 
       type = "o", pch = 16, lwd = 2,
       xlab = "", ylab = "", 
       axes = FALSE,
       main = "Levy flight")
  box()
}

plotLevy()
plorBrow()

par(mar=c(2,2,2,2))
op<-par(mfrow=c(2,2))

x <- seq(1, 5, length=1000)
plot (x, dlevy(x, 2/3), type="l", lty=2)
lines(x, dlevy(x), lwd=2, col="red")
lines(x, dlevy(x, 1/3), lwd=2, col="blue")
lines(x, dlevy(x, 2/3), lwd=2, col="green")
legend("topright", 
       legend = c("m=0,    d = 0", 
                  "m=1/3, d = 0",
                  "m=2/3, d = 0"), 
       col = c("red", "blue", "green"), 
       pch = c(19,19, 19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8)

plot (x, dlevy(x, 1/2, 1), type="l", lty=2)
lines(x, dlevy(x, 1/2, 1), lwd=2, col="red")
lines(x, dlevy(x, 1/2, 2), lwd=2, col="blue")
lines(x, dlevy(x, 1/2, 3), lwd=2, col="green")
legend("topright", 
       legend = c("m=1/2, d = 1", 
                  "m=1/2, d = 2",
                  "m=1/2, d = 3"), 
       col = c("red", "blue", "green"), 
       pch = c(19,19, 19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8)

plot (c(1,5), c(plevy(1, 1/2, 3), plevy(5, 1/3)), lty=2)
lines(x, plevy(x), lwd=2, col="red")
lines(x, plevy(x, 1/3), lwd=2, col="blue")
lines(x, plevy(x, 2/3), lwd=2, col="green")
legend("bottomright", 
       legend = c("m=0,    d = 0", 
                  "m=1/3, d = 0",
                  "m=2/3, d = 0"), 
       col = c("red", "blue", "green"), 
       pch = c(19,19, 19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8)

plot (c(1,5), c(0, plevy(5, 1/2, 1)), lty=2)
lines(x, plevy(x, 1/2, 1), lwd=2, col="red")
lines(x, plevy(x, 1/2, 2), lwd=2, col="blue")
lines(x, plevy(x, 1/2, 3), lwd=2, col="green")
legend("topleft", 
       legend = c("m=1/2, d = 1", 
                  "m=1/2, d = 2",
                  "m=1/2, d = 3"), 
       col = c("red", "blue", "green"), 
       pch = c(19,19, 19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8)

legend()
par(op)


