library(quantreg)

load('natality_processed.RData') 

Y <- d$weight
X <- as.matrix(d[,-1])

tau <- seq(0.05, 0.95, 0.05)

# The i'th column of a coefficient matrix is
# the quantile coefficients obtained at
# tau[i] quantile.
rqcoef <- function(X, Y, tau) {
  n <- nrow(X); p <- ncol(X)
  res <- matrix(NA, p+1, length(tau))
  for (i in 1:length(tau)) {
    temp <- try(rq(Y ~ as.matrix(X), tau = tau[i], 
                   method = 'fn')$coefficients, T)
    if (length(temp) == 1) {
      res[,i] <- rq(Y ~ as.matrix(X), tau = tau[i])$coefficients
    } else {
      res[,i] <- temp
    }
  }
  return(res)
}

coef<- rqcoef(d[,-1], d[,1], tau)

write.csv(coef, "coef.csv")



