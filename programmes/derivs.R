library(pracma)

phim <- function (x, m, s) {
  ((x-m)*exp(-(x-m)^2/(2*s*s)))/(sqrt(2*pi)*s^3)
}

phis <- function (x, m, s) {
  ((((x-m)^2)*exp(-(x-m)^2/(2*s^2)))/(sqrt(2*pi)*s^4))-(exp(-(x-m)^2/(2*s^2))/(sqrt(2*pi)*s^2))
}

E<-function (a, m, s) {exp(-((a-m)**2)/(2*s**2))}
PHIm<-function (a, m, s) {-E(a,m,s)/(sqrt(2*pi)*s)}
PHIs<-function (a, m, s) {-((a-m)*E(a,m,s))/(sqrt(2*pi)*s**2)}

esigma <- function(x, a, m, s) {
 n <- length(x)
 a11 <- sum(((phim(x,m,s)/dnorm(x,m,s))+(PHIm(a,m,s)/(1-pnorm(a,m,s))))**2)/n
 a12 <- sum(((phis(x,m,s)/dnorm(x,m,s))+(PHIs(a,m,s)/(1-pnorm(a,m,s))))*((phim(x,m,s)/dnorm(x,m,s))+(PHIm(a,m,s)/(1-pnorm(a,m,s)))))/n
 a21 <- a12
 a22 <- sum(((phis(x,m,s)/dnorm(x,m,s))+(PHIs(a,m,s)/(1-pnorm(a,m,s))))**2)/n
 mx <- c(a11, a12, a21, a22)
 dim(mx) <- c(2, 2)
 inv(mx)[1,1]
}
