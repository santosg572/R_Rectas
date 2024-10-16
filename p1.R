
cuadrado <- function(x0=0, y0=0, del1=0){
 points(c(x0, x0+del1),c(y0, y0), type='l')
 points(c(x0+del1, x0+del1),c(y0, y0+del1), type='l')
 points(c(x0+del1, x0),c(y0+del1, y0+del1), type='l')
 points(c(x0, x1),c(y0, y0+del1), type='l')
}


x1 = 1
y1 = 1
del1 = 1

plot(c(0,4), c(0,4))

for (x1 in seq(1,1.8,.2)){
  for (y1 in seq(0,.8,.2)){
    cuadrado(x1, y1, .2)
  }
}

