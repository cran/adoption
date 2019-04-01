GoldenbergDistance <- function(param, dist, W, Goldenberg_C) {
## set.seed(99) ;  Print(param);  param <- c(0.1, 0.2)
  
  .Call(C_GoldenbergDistance, param, dist, W, Goldenberg_C)
  return(NULL)
  
  neighbour <- dist <= param[1];  diag(neighbour) <- FALSE;  w <- neighbour / Goldenberg_C

#  Print(param)
  set.seed(99)
  if (length(param) > 1 && param[2] > 0) {
    idx <- which(neighbour)[rbinom(sum(neighbour), 1, prob=param[2]) == 1]
    w[idx] <- -1 / Goldenberg_C
  }
 
  stopifnot(all(abs(w - W) < 1e-15))
 # print("ok!")
}

VarDistance <- function(param, dist, W)  {
## set.seed(99) ;  Print(param);  param <- c(0.1, 0.2)
  
  .Call(C_VarDistance, param, dist, W)
  return(NULL)
}
