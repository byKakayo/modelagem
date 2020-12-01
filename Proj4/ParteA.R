f <- function(a, b, x) a*cos(b*x)

#r <- as.matrix()
#for (x in 0:pi) {
#  r <- rbind(r, ())
#}

number_of_cycles = 2
max_y = 40

x = 1:500
a = number_of_cycles * 2*pi/length(x)

y = max_y * sin(x*a)
noise1 = max_y * 1/10 * sin(x*a*10)

plot(x, y, type="l", col="red", ylim=range(-1.5*max_y,1.5*max_y,5))
points(x, y + noise1, col="green", pch=20)
points(x, noise1, col="yellow", pch=20)