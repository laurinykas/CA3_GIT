
#Step 1
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


#Step 2
nll_lm <- function(data, par) {
  beta <- par[1:4]                 
  sigma <- exp(par[5])          
  
  X <- model.matrix(y ~ x1 + x2 + x3, data = data)
  y <- data$y
  epsilon <- y - X %*% beta
  
  nll <- -sum(dnorm(epsilon, mean = 0, sd = sigma, log = TRUE))
  
  return(nll)
}


#Step 3

beta_init <- c(mean(df$y), 0, 0, 0)         
log_sigma_init <- log(sd(df$y))             
par_init <- c(beta_init, log_sigma_init)

lower_bounds <- c(rep(-.Machine$double.xmax, 4), log(.Machine$double.xmin))
upper_bounds <- c(rep(.Machine$double.xmax, 4), log(.Machine$double.xmax))


optim_results <- optim(
  par = par_init,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  upper = upper_bounds,
  lower = lower_bounds,
  hessian = TRUE
)


beta_hat <- optim_results$par[1:4]
sigma_hat <- exp(optim_results$par[5])


cat("Estimated beta coefficients:\n")
print(beta_hat)
cat("\nEstimated sigma:\n")
print(sigma_hat)


#Step 4 
# Customization 
#Handles better numerical instablity



#Task5

X <- model.matrix(y ~ x1 + x2 + x3, data = df)

y <- df$y

beta_hat_matrix <- solve(t(X) %*% X) %*% t(X) %*% y

beta_hat_matrix <- as.numeric(beta_hat_matrix)

beta_hat- beta_hat_matrix #Small diff


# TASK 6

n <- nrow(X)
p <- ncol(X)

residuals <- y - X %*% beta_hat_matrix
RSS <- sum(residuals^2)

sigma_hat_ols <- sqrt(RSS / (n - p))

sigma_hat- sigma_hat_ols #Sig. diff.
#Step 7 

standard_errors <- sqrt(diag(solve(optim_results$hessian)[1:4, 1:4]))
cat("\nStandard errors of the regression coefficients:\n")
print(standard_errors)

lm_fit <- lm(y ~ x1 + x2 + x3, data = df)

beta_hat_lm <- coef(lm_fit)
cat("\nEstimated coefficients from lm():\n")
print(beta_hat_lm)

sigma_hat_lm <- summary(lm_fit)$sigma
cat("\nEstimated sigma from lm():\n")
print(sigma_hat_lm)

