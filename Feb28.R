library(quantreg)

load('natality_processed.RData')

table(d$ed.hs, d$ed.smcol)

lm1 <- lm(weight ~ ., data = d)
summary(lm1)

# Today's task:
# 1. Downolad the data
# 2. Visualization (e.g. visualize the effect of race)

Y <- d$weight
X <- as.matrix(d[,-1])

qr.1 <- rq(Y ~ X, tau = 0.5)
names(qr.1)

coef <- qr.1$coefficients
coef

# summary(qr.1)

# different optimization algorithms for quantile regression
system.time(
  qr.1 <- rq(Y ~ X, tau = 0.5)
  )

system.time(
  qr.1 <- rq(Y ~ X, tau = 0.5, method = "fn")
  )

# computation efficiency in R
# try subsampling
s <- sample(nrow(d), 5000)
d.s <- d[s,]

# try vectorized computation
system.time(
  for (i in 1:nrow(d)) {
  d$boy[i] <- d$boy[i] + 1
  }
)

# today's task 2
# Acquire the quantile regression coefficients at quantile levels
# 0.05, 0.1, 0.15, ..., 0.9, 0.95
# Make a plot for the race variable, x-axis is quantile level
# y-axis is coefficient for the race variable

# system.time(
# d$boy <- d$boy + 1
# }

# Acquire the quantile regression coefficients at quantile levels
quantiles <- seq(0.05, 0.95, by = 0.05)

# Initialize matrix to store coefficients
coefficients <- matrix(NA, nrow = length(quantiles), ncol = ncol(X), 
                       dimnames = list(quantiles, colnames(X)))

# Loop through quantiles and fit quantile regression models
for (i in seq_along(quantiles)) {
  qr_temp <- try(rq(Y ~ X, tau = quantiles[i]), silent = TRUE)  # Handle errors gracefully
  if (!inherits(qr_temp, "try-error")) {
    coefficients[i, ] <- qr_temp$coefficients
  }
}

# Extract the 'race' coefficient
race_coef <- coefficients[, "race"]

# Plot
plot(quantiles, race_coef, type = "l", xlab = "Quantile Level", 
     ylab = "Coefficient for Race Variable", main = "Quantile Regression Coefficient for Race Variable")
