#1. normal  Solution 
data1 <- c(10, 15, 20,9.48, 17.15, 11.68, 10.00, 11.88, 12.13, 14.93, 9.73, 7.61, 7.21,10.28, 7.03, 11.01, 12.36, 13.28, 6.58, 11.07, 12.56, 8.18, 9.17)
n <- length(data1)
sigma2 <- 1 
tau_values <- c(10, 15, 20)
prior_var <- 2
posterior_means <- numeric(length(data1))
posterior_vars <- numeric(length(data1))
x <- seq(0, 30, length.out=1000)
plot(x, dnorm(x, mean=tau_values[1], sd=sqrt(prior_var)), type="l", col="red",xlab="μ", ylab="Density", main="Prior and Posterior Distributions", ylim=c(0, 0.5))
legend("topright", legend=c("Prior", "Posterior"), col=c("red", "blue"), lty=1)
for (i in seq_along(data1)) {
tau <- data1[i]
# Posterior parameters
posterior_var <- 1 / (1/prior_var + n/sigma2)
posterior_mean <- posterior_var * (tau/prior_var + sum(data1)/sigma2)
posterior_means[i] <- posterior_mean
posterior_vars[i] <- posterior_var
lines(x, dnorm(x, mean=tau_values, sd=sqrt(prior_var)), col="red")
lines(x, dnorm(x, mean=posterior_mean, sd=sqrt(posterior_var)), col="blue")
}
#Display posterior means
results1 <- data.frame(tau=data1, posterior_mean=posterior_means, posterior_var=posterior_vars)
print(results1)
________________________________________________
#2
# Given dataset exponential
data_exp <- c(3.29, 7.53, 0.48, 2.03, 0.36, 0.07, 4.49, 1.05, 9.15,3.67, 2.22, 2.16, 4.06, 11.62, 8.26, 1.96, 9.13, 1.78, 3.81, 17.02)
n <- length(data_exp)
sum_x <- sum(data_exp)
bayes_estimator <- sum_x / (n - 1)
posterior_samples <- rep(NA, 10000)
for (i in 1:10000) {
  u <- runif(1) 
  posterior_samples[i] <- -sum_x / (n - 1) * log(1 - u)  }

# Compute 95% HPD Interval
hpd_interval <- quantile(posterior_samples, c(0.025, 0.975))

# Print results
cat("Bayes Estimator for θ:", bayes_estimator, "\n")
cat("95% HPD Interval for θ:", hpd_interval, "\n")

# Plot posterior distribution manually
hist(posterior_samples, probability = TRUE, breaks = 50, col = "skyblue",
     main = "Posterior Distribution of θ", xlab = "θ", border = "black")
lines(density(posterior_samples), col = "red", lwd = 2)  
____________________________________________
#3.child Iq
x <- 115  
sigma2 <- 100 
mu0 <- 100  
tau2 <- 225
posterior_var <- 1 / (1/tau2 + 1/sigma2)
posterior_mean <- posterior_var * (mu0/tau2 + x/sigma2)
loss_a1 <- function(theta) {
  ifelse(theta < 90, 0,
  ifelse(theta <= 110, theta - 90, 2*(theta - 90)))
}
loss_a2 <- function(theta) {
  ifelse(theta < 90, 90 - theta,
  ifelse(theta <= 110, 0, theta - 110))
}
loss_a3 <- function(theta) {
  ifelse(theta < 90, 2*(110 - theta),
  ifelse(theta <= 110, 110 - theta, 0))
}
expected_loss <- function(loss_fn) {
  integrate(function(theta) {
    loss_fn(theta) * dnorm(theta, mean=posterior_mean, sd=sqrt(posterior_var))
  }, lower=-Inf, upper=Inf)$value
}
loss1 <- expected_loss(loss_a1)
loss2 <- expected_loss(loss_a2)
loss3 <- expected_loss(loss_a3)
#Bayes decision is the action with minimum expected loss
bayes_decision <- which.min(c(loss1, loss2, loss3))
actions <- c("a1: below average", "a2: average", "a3: above average")
results3 <- list(
  posterior_dist = paste0("N(", round(posterior_mean, 2), ", ", round(posterior_var, 2), ")"),
  expected_losses = c(loss1, loss2, loss3),
  bayes_decision = actions[bayes_decision]
)
print(results3)
x_vals <- seq(50, 150, length.out=1000)
plot(x_vals, dnorm(x_vals, posterior_mean, sqrt(posterior_var)), type="l",xlab="True IQ (θ)", ylab="Density", main="Posterior Distribution of True IQ")
abline(v=c(90, 110), col="gray", lty=2)
text(posterior_mean, max(dnorm(x_vals, posterior_mean, sqrt(posterior_var))),
paste("Bayes decision:", actions[bayes_decision]), pos=3)
_____________________________________________________________________________
#4.
# Given data
theory_mean <- 82.4
theory_sd <- 1.1
experiment_mean <- 82.1
experiment_sd <- 1.7
posterior_precision <- (1/theory_sd^2) + (1/experiment_sd^2)
posterior_mean <- ((theory_mean/theory_sd^2) + (experiment_mean/experiment_sd^2)) / posterior_precision
posterior_sd <- sqrt(1/posterior_precision)
cat("Posterior Distribution:\n")
cat(sprintf("Mean: %.2f GeV\n", posterior_mean))
cat(sprintf("Standard Deviation: %.2f GeV\n", posterior_sd))
# Part B: Bayes Factor Calculation for m < 83.0 GeV
# Function to calculate Bayes factor
calculate_bayes_factor <- function() {
#Prior probability (from theory)
prior_prob <- pnorm(83.0, theory_mean, theory_sd)
# Posterior probability
post_prob <- pnorm(83.0, posterior_mean, posterior_sd)
#Complement probabilities
prior_comp <- 1 - prior_prob
post_comp <- 1 - post_prob
#Bayes factor for H0: m < 83 vs H1: m >= 83
BF <- (post_prob/post_comp) / (prior_prob/prior_comp)
return(BF)
}
bayes_factor <- calculate_bayes_factor()
cat("\nBayes Factor for m < 83.0 GeV:\n")
cat(sprintf("BF = %.3f\n", bayes_factor))
# Visualization (Base R only)
# Create sequence for plotting
x <- seq(78, 87, length.out = 1000)
# Calculate densities
prior_dens <- dnorm(x, theory_mean, theory_sd)
likelihood_dens <- dnorm(x, experiment_mean, experiment_sd)
posterior_dens <- dnorm(x, posterior_mean, posterior_sd)
# Plot distributions
plot(x, prior_dens, type = "l", col = "blue", lwd = 2, ylim = c(0, max(c(prior_dens, likelihood_dens, posterior_dens))),xlab = "Mass (GeV)", ylab = "Density", main = "Prior, Likelihood, and Posterior Distributions")
lines(x, likelihood_dens, col = "red", lwd = 2)
lines(x, posterior_dens, col = "green", lwd = 2)
abline(v = 83.0, lty = 2, col = "gray")
# Add legend
legend("topright", legend = c("Prior (Theory)", "Likelihood (Experiment)", "Posterior", "83.0 GeV threshold"),col = c("blue", "red", "green", "gray"), lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1))
#Add probability annotations
text(83.5, 0.2, sprintf("Prior P(m<83) = %.3f", pnorm(83.0, theory_mean, theory_sd)), pos = 4)
text(83.5, 0.15, sprintf("Post P(m<83) = %.3f", pnorm(83.0, posterior_mean, posterior_sd)), pos = 4)
text(83.5, 0.1, sprintf("Bayes Factor = %.3f", bayes_factor), pos = 4)
_____________________________________________________________________________
_____________________________________________________________________________
#Regression 
#residual delivery time 
# Input data
y <- c(16.68,11.50,12.03,14.88,13.75,18.11,8.00,17.83,79.24,21.50,
       40.33,21.00,13.50,19.75,24.00,29.00,15.35,19.00,9.50,35.10,
       17.90,52.32,18.75,19.83,10.75)
x1 <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
x2 <- c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,
        776,200,132,36,770,140,810,450,635,150)

# Create data frame
delivery_data <- data.frame(y, x1, x2)

# Fit linear model
model <- lm(y ~ x1 + x2, data = delivery_data)

# Basic model summary
summary(model)

# Calculate residuals
residuals <- resid(model)

# Standardized residuals
standardized_res <- residuals / sd(residuals)

# Studentized residuals (externally studentized)
n <- length(y)
p <- length(coef(model))
sigma_hat <- summary(model)$sigma
h <- hatvalues(model) # Leverage values
studentized_res <- residuals / (sigma_hat * sqrt(1 - h))

# PRESS residuals
press_res <- residuals / (1 - h)

# Calculate influence measures
cooks_d <- cooks.distance(model)
dffits <- residuals * sqrt(h) / (sigma_hat * (1 - h))
dfbetas <- dfbeta(model)

# Leverage points (high leverage)
leverage <- h
high_leverage <- which(leverage > 2*p/n) # Common threshold

# Influential observations
influential <- which(cooks_d > 4/(n - p - 1)) # Common threshold

# Create results data frame
results <- data.frame(
  Obs = 1:n,
  Residual = round(residuals, 4),
  Standardized_Residual = round(standardized_res, 4),
  Studentized_Residual = round(studentized_res, 4),
  PRESS_Residual = round(press_res, 4),
  Leverage = round(leverage, 4),
  Cooks_Distance = round(cooks_d, 4),
  DFFITS = round(dffits, 4)
)

# Print results
print(results)

# Identify special points
cat("\nHigh Leverage Points (observations with h >", round(2*p/n, 3), "):\n")
print(results[high_leverage, ])

cat("\nInfluential Observations (Cook's D >", round(4/(n - p - 1), 3), "):\n")
print(results[influential, ])

# Plot diagnostics
par(mfrow = c(2, 2))
plot(model, which = 1:4)
_____________________________________________-
#2.
# Data
x <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8, 9, 10, 11, 12, 13, 14, 15)
y <- c(6.3, 11.1, 20.0, 24.0, 26.1, 30.0, 33.8, 34.0, 38.1, 39.9, 42.0, 46.1, 53.1, 52.0, 52.5, 48.0, 42.8, 27.8, 21.9)

# Step 1: Compute mean of x and y
x_mean <- sum(x) / length(x)
y_mean <- sum(y) / length(y)

# Step 2: Compute centered x and squared centered x
x_centered <- x - x_mean
x_centered_sq <- x_centered^2

# Step 3: Compute regression coefficients (b0, b1, b2) manually
# Create matrices
X <- cbind(1, x_centered, x_centered_sq)  # Design matrix
XT_X <- t(X) %*% X  # X'X
XT_Y <- t(X) %*% y  # X'Y

# Solve for (b0_cap, b1_cap, b2_cap)
beta_hat <- solve(XT_X, XT_Y)  # (X'X)^(-1) X'Y

b0_cap <- beta_hat[1]  # Estimate of b0
b1_cap <- beta_hat[2]  # Estimate of b1
b2_cap <- beta_hat[3]  # Estimate of b2

# Step 4: Compute residuals and variance
y_hat <- X %*% beta_hat  # Predicted values
residuals <- y - y_hat
RSS <- sum(residuals^2)  # Residual Sum of Squares
n <- length(y)
p <- 3  # Number of parameters (b0, b1, b2)
sigma_sq <- RSS / (n - p)  # Estimate of variance

# Step 5: Compute standard error of b2_cap
XT_X_inv <- solve(XT_X)  # (X'X)^(-1)
se_b2 <- sqrt(sigma_sq * XT_X_inv[3, 3])  # Standard error of b2_cap

# Step 6: Perform t-test for H0: b2 = 0
t_stat <- b2_cap / se_b2
df <- n - p  # Degrees of freedom
p_value <- 2 * pt(-abs(t_stat), df)  # Two-tailed test

cat("Estimated coefficients:\n")
cat("b0_cap =", b0_cap, "\n")
cat("b1_cap =", b1_cap, "\n")
cat("b2_cap =", b2_cap, "\n\n")

cat("Hypothesis Test for H0: b2 = 0\n")
cat("t-statistic =", t_stat, "\n")
cat("p-value =", p_value, "\n")

# Decision rule at alpha = 0.05
if (p_value < 0.05) {
  cat("Reject H0: There is significant evidence that b2 is not zero.\n")
} else {
  cat("Fail to reject H0: There is no significant evidence that b2 is different from zero.\n")
}
______________________________________________________________________________________
__________________________________________________________________________
#`logistics
data=cbind(c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,1.8369, 1.861, 1.8839),c(59, 60, 62, 56, 63, 59, 62, 60), 
           c(6, 13, 18, 28, 52, 53, 61, 60))
colnames(data) <-c("logarithm ofCS2 concentration ", "no of beetles","no of killed")
data
N<-length(data)
ni<-data[,2]
xi<-data[,1]
yi<-data[,3]
logistic<-glm(cbind(yi,ni-yi) ~ xi, family=binomial(logit), data=data.frame(data))
logistic
propi <- yi/ni
# Fitted logistic model
fit.logis <- Vectorize(function(d)  plogis(logistic$coefficients[1] +logistic$coefficients[2]*d))
curve(fit.logis,1.6,2, xlab = "Dose", ylab = "Proportion", main = "Fitted Model", lwd = 2, cex.axis = 1.5, cex.lab = 1.5)
points(xi,propi,pch=10,col="red",lwd=2)
logistic$coefficients[1]
_________________________________________________
#poisson 
Y<-c(2,3,6,7,8,9,10,12,15)
X<-c(-1,-1,0,0,0,0,1,1,1)
pois<-glm(Y~X,family = "poisson")
summary(pois)
pois$fitted.values
par(mfrow = c(2,3))
plot(pois, which = 1:6)
par(mfrow = c(1,1))
________________________________________________________________
#Nonlinear
x <- c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42)
y <- c(0.490, 0.475, 0.450, 0.437, 0.433, 0.455, 0.423, 0.407, 0.407, 0.407, 0.405, 0.393, 0.405, 0.400, 0.395, 0.400, 0.390, 0.390)
model <- function(x, alpha, beta) {
  alpha + (0.49 - alpha) * exp(-beta * (x - 8))
}
# Sum of squares function to minimize
ssq <- function(params) {
  alpha <- params[1]
  beta <- params[2]
  y_pred <- model(x, alpha, beta)
  sum((y - y_pred)^2)
}
initial_guess <- c(alpha = 0.4, beta = 0.1)
result <- optim(initial_guess, ssq, method = "L-BFGS-B",lower = c(0, 0), upper = c(0.49, Inf))
alpha_est <- result$par[1]
beta_est <- result$par[2]
# Calculate fitted values
y_fitted <- model(x, alpha_est, beta_est)
# Results
cat("Estimated parameters:\n")
cat("α =", alpha_est, "\n")
cat("β =", beta_est, "\n")
# Plot results
plot(x, y, main = "Nonlinear Regression Fit", xlab = "X", ylab = "Y", pch = 19)
lines(x[order(x)], y_fitted[order(x)], col = "red", lwd = 2)
legend("topright", legend = c("Data", "Fitted Model"),col = c("black", "red"), pch = c(19, NA), lty = c(NA, 1))










