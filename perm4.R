set.seed(1711)
graphics.off()
closeAllConnections()
FROM = 0
TO = 50
N = 400
n = 4
GOOD <- FALSE
values <- array(0, c(2 * n, N))
X <- array(0, c(N))
for (j in int(1, N)) {
  X[j] <- FROM + (TO - FROM) * (j - 1) / (N - 1)
}
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
fourparametricfunction <- function(a, b, c, d) {
  return (
    function(x) {
      return (a + (b - a) / (1 + (x / c)^(-2 - d)))
    }
  )
}

nextcomb <- function(n, p) {  
  k <- length(p)  
  GOOD <<- FALSE
  for (i in int(k, 1, TRUE)) {
    if (p[i] < n - k + i) {
      GOOD <<- TRUE
      p[i] <- p[i] + 1      
      for (j in int(i + 1, k)) {
        p[j] <- p[i] + j - i
      } 
      break
    }
  }
  return (p)
}

calcvalue <- function(p, type) {
  a <- c(p, 1)
 # print(sort(a))
  b <- (1:(2 * n))[-a]
  res <- 0
  #print(sort(b))
  if (type == 0) {
    ls1 <- c()
    ls2 <- c()
    for (i in int(1, N)) {    
      mean1 <- mean(values[a[1:n], i])
      mean2 <- mean(values[b[1:n], i])      
      ls1 <- c(ls1, mean1)
      ls2 <- c(ls2, mean2)      
      res <- res + abs(mean1 - mean2)
    }
   # lines(X, ls1, col = "green", type = 'l')
  #  lines(X, ls2, col = "magenta", type = 'l')
    return (res)
  }
  if (type == 1) {
    for (i in int(1, N)) {    
      mean1 <- mean(values[a[1:n], i])
      mean2 <- mean(values[b[1:n], i])      
      res <- res + sum(abs(values[a[1:n], i] - mean1)) + sum(abs(values[b[1:n], i] - mean2))
    }
    return (-res)
  }
  
}

plotgroup <- function (st, col, flag = FALSE) {  
  if (flag) {
    plot(X, values[st,], col = col, type = 'l')
  }
  for (i in int(st, st + n - 1)) {
    lines(X, values[i,], col = col)
  }
}
for (D in seq(0, 2, 0.5)) {    
  for (sigma in seq(0, 2, 0.2)) {  
    #sigma <- 2
    #D <- 2
  	cat(sprintf("%f %f ", D, sigma))
    for (type in int(0, 1)) {      
      meanp <- 0
      C <- 10
      for (cnt in int(1, C)) {  
        firstgroup <- list()
        for (i in int(1, n)) {
          firstgroup <- c(firstgroup, fourparametricfunction(50, 300, runif(1, 11, 13), runif(1, 4, 6)))
        }
        secondgroup <- list()
        for (i in int(1, n)) {
          secondgroup <- c(secondgroup, fourparametricfunction(50, 300, runif(1, 11, 13) + D, runif(1, 4, 6)))
        }
        for (i in int(1, n)) {
        	for (j in int(1, N)) {        		
        		values[i, j] <- firstgroup[[i]](X[j]) + rnorm(1, 0, sigma)
        		values[n + i, j] <- secondgroup[[i]](X[j]) + rnorm(1, 0, sigma)
        	}
        }     
        #plotgroup(1, "blue", TRUE)
        #plotgroup(n + 1, "red")        
        currentcomb <- int(1, n - 1)
        T2 <- calcvalue(currentcomb + 1, type)
        
        cnt <- 0
        pv <- 0
        while (TRUE) {
          Tc <- calcvalue(currentcomb + 1, type)     
          cnt <- cnt + 1
          if (Tc > T2) {
            pv <- pv + 1
          }    
          currentcomb <- nextcomb(2 * n - 1, currentcomb)
          if (!GOOD) {
            break
          }
        }
        pv <- pv / cnt 
        meanp <- meanp + pv / C
      }        
      if (type != 1)
        cat(sprintf("%f ", meanp))
      else
        cat(sprintf("%f\n", meanp))
    }
    #stop("=)")
  }
}





