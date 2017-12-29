# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

plotContour = function(mu1, mu2, s11, s22, rho) {

  x1  = seq(-20, 20, length = 41)
  x2 = x1

  bvnd = function(x1, x2) {
    term1<-1 / (2 * pi * sqrt(s11 * s22 * (1 - rho ^ 2)))
    term2<--1 / (2*(1 - rho ^ 2))
    term3<-(x1 - mu1) ^ 2 / s11
    term4<-(x2 - mu2) ^ 2 / s22
    term5<--2 * rho * ((x1 - mu1) * (x2 - mu2)) / (sqrt(s11) * sqrt(s22))
    term1 * exp(term2 * (term3 + term4 - term5))
  }

  z = outer(x1, x2, bvnd)
  contour(z, col = 'red', drawlabel=FALSE, main="Density estimation :cont Plot")
  contour(z**2, col = "green")
}

plotPersp = function(mu1, mu2, s11, s22, rho) {

  x1  = seq(-20, 20, length = 41)
  x2 = x1

  bvnd = function(x1, x2) {
    term1<-1 / (2 * pi * sqrt(s11 * s22 * (1 - rho ^ 2)))
    term2<--1 / (2*(1 - rho ^ 2))
    term3<-(x1 - mu1) ^ 2 / s11
    term4<-(x2 - mu2) ^ 2 / s22
    term5<--2 * rho * ((x1 - mu1) * (x2 - mu2)) / (sqrt(s11) * sqrt(s22))
    term1 * exp(term2 * (term3 + term4 - term5))
  }

  z = outer(x1, x2, bvnd)
  persp(z, main = "Density estimation:perspective plot")
}


