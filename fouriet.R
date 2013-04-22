set.seed(1711)
PI = acos(-1)
int <- function(a, b, rev = FALSE) {
  if (a == b) {
    return (a:b)
  }
  if (a < b) {
    if (!rev)
      return (a:b)
    else
      return (c())
  } else {
    if (rev)
      return (a:b)
    else
      return (c())
  }
}
getck <- function (f, k) {
  n = length(f)
  arg = -2 * PI * k * int(0, n - 1) / (n - 1)  
  return (sum(f * complex(real = cos(arg), imaginary = sin(arg))) / n)
}
calc <- function (f1, f2, a, b, n) {  
  F1 = f1(a + (b - a) * int(0, n - 1) / (n - 1))
  F2 = f2(a + (b - a) * int(0, n - 1) / (n - 1))
  sigma1 = sqrt(0.5)
  sigma2 = sqrt(0.5)
  F1 = F1 + runif(n, 0, sigma1)
  F2 = F2 + runif(n, 0, sigma2)  
  plot(F1, type = "l",col = "BLUE")
  lines(F2, col = "RED")  
  P = 10
  c1 = sapply(int(-P, P), function(x) {return (getck(F1, x))})    
  c2 = sapply(int(-P, P), function(x) {return (getck(F2, x))})  
  R = vector(length = P + 1)
  for (p in int(0, P)) {
    R[p + 1] = sum(Mod(c1 - c2)[(P + 1 - p):(P + 1 + p)])
  }
  print(R)
}
f1 = function(x) {return (x)}
beta = 1.75
f2 = function(x) {return (x + beta * x * exp(-2 * x))}

calc(f1, f2, 0, 2 * PI, 100)
closeAllConnections()