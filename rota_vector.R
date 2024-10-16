
rm(list=ls())

source('funciones_geometricas.R')

plot(c(-5,-5), c(5,5), xlim=c(-5,5), ylim=c(-5,5))

v = c(1,0)
c = c(0,0)

for (i in 1:30) {
  print(i)
  points(c(c[1], v[1]), c(c[2], v[2]), type='l', lwd=3)
  Sys.sleep(2)  # Pause for 2 second in each iteration
  points(c(c[1], v[1]), c(c[2], v[2]), type='l', lwd=4, col='white')
  v = Rota(pi/8, v[1], v[2])
}



