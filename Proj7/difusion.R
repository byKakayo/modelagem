difusion <- function(psi, L, n, dt, d) {
  for (t in 1:n) {
    for (x in 1:L) {
      for (y in 1:L) {
        psi[y, x] = psi[y, x] + d*(psi[y, x-1] + psi[y, x+1] +
                    psi[y-1, x] + psi[y+1, x] - 4*psi[y, x])*dt 
      }
    }
    t = t + dt
  }
  
  return(psi)
}