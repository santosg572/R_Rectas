Traslacion <- function(x=0, y=0, t=0){
  res = c(x + t[1], y + t[2])
}

Traslada_Puntos <- function(x=0, y=0, pi=0, pf=0){
  t = c(pf[1] - pi[1], pf[2] - pi[2])
  np1 = length(x)

  xn = c()
  yn = c()
  for (i in 1:np1){
    print('datos iniciales')
    print(c(x[i], y[i]))
    p = Traslacion(x[i], y[i], t)
    print('datos finales')
    print(p)
    xn = c(xn, p[1])
    yn = c(yn, p[2])
  }
  ret = list(x=xn, y = yn)
}

Rota_Puntos <- function(x=0, y=0, pr=0, teta){
  pi = pr
  pf = c(0,0)
  pt = Traslada_Puntos(x, y, pi, pf)

  np1 = length(x)

  xn = c()
  yn = c()
  for (i in 1:np1){
    p = Rota(teta, pt$x[i], pt$y[i])
    xn = c(xn, p[1])
    yn = c(yn, p[2])
  }

  pf = pr
  pi = c(0,0)
  pt = Traslada_Puntos(xn, yn, pi, pf)
  ret = list(x=pt$x, y = pt$y)
}



Gen_Cuadrado <- function(x0=0, y0=0, del1=0){
  x = c(x0, x0+del1, x0+del1, x0)
  y = c(y0, y0, y0+del1, y0+del1)
  res = list(x=x, y=y)
}



Graf_Cuadrado <- function(x, y){
  for (i in 1:4){
    points(c(x[i], x[i+1]),c(y[i], y[i+1]), type='l')
  }
  points(c(x[4], x[1]),c(y[4], y[1]), type='l')
}

Rota <- function(teta=0, x=0, y=0){
  rot = matrix(c(cos(teta), sin(teta), -sin(teta), cos(teta)), ncol=2)
  res = rot %*% c(x, y)
}

Rota_Vertices <- function(teta=0, x=0, y=0, p=0){
  np1 = length(x)
  xn = c()
  yn = c()
  for (i in 1:np1){
    r = Rota(teta, x[i]-p[1], y[i]-p[2])
    xn = c(xn, r[1] + p[1])
    yn = c(yn, r[2] + p[2])
  }
}

