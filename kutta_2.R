n<-10^4
x<-rnorm(n,0,1)
f<-exp(-x^2)
p<-mean(f)

o<-rexp(n,rate=1)
p<-runif(n,rate=1)
f_val<-exp(-(o+p))
i_esti<-mean(f_val)
iesti

u<-runif(n,0,1)
v1<-sqrt(1-u^2)
v2<-u^2
cor_u_v1<-cor(u,v1)
cor_u_v2<-cor(v2,v1)

#rejection 20x*(1-x)^3
# Define the target density function
f <- function(x) {
  20 * x * (1 - x)^3
}
# Find the maximum of f(x) to determine our bounding constant
# We can find the maximum analytically by taking derivative:
# f'(x) = 20(1-x)^3 - 60x(1-x)^2
# Setting f'(x) = 0 => x = 1/4
x_max <- 1/4
c <- f(x_max)  # c ≈ 2.109375
# Define the proposal density (uniform on [0,1])
# g(x) = 1 for 0 < x < 1
# Rejection sampling algorithm
rejection_sampling <- function(n) {
  samples <- numeric(n)
  accepted <- 0
  
 while (accepted < n) {
    # Step 1: Generate Y from g (uniform)
    Y <- runif(1)
    
   # Step 2: Generate U from uniform(0,1)
    U <- runif(1)
    # Step 3: Accept or reject
    if (U <= f(Y)/(c * 1)) {  # Since g(Y) = 1
      accepted <- accepted + 1
      samples[accepted] <- Y
    }
  }
  
  return(samples)
}

# Generate 10,000 samples
set.seed(123)  # For reproducibility
n_samples <- 10000
samples <- rejection_sampling(n_samples)

# Plot the results to verify
hist(samples, breaks = 50, freq = FALSE, main = "Empirical vs Theoretical Density")
curve(f(x), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Empirical", "Theoretical"), 
       col = c("black", "red"), lty = c(1, 1))

# Print acceptance rate (should be about 1/c)
cat("Theoretical acceptance rate:", round(1/c, 4), "\n")
cat("Empirical acceptance rate:", round(n_samples/(n_samples * c), 4), "\n")
_______________________________________________-
#Rejection (kx^0.5e^-x)

# Define the target density function
target_density <- function(x) {
  if (x > 0) {
    return(x^(1/2) * exp(-x))
  } else {
    return(0)
  }
}

# Define the proposal density function (exponential with rate 1)
proposal_density <- function(x) {
  return(dexp(x, rate = 1))
}

# Define the constant M
M <- optimize(function(x) target_density(x) / proposal_density(x), interval = c(0, 10), maximum = TRUE)$objective

# Rejection sampling function
rejection_sampling <- function(n) {
  samples <- numeric(n)
  for (i in 1:n) {
    accept <- FALSE
    while (!accept) {
      # Sample from the proposal distribution
      x <- rexp(1, rate = 1)
      # Calculate the acceptance probability
      u <- runif(1)
      if (u < target_density(x) / (M * proposal_density(x))) {
        samples[i] <- x
        accept <- TRUE
      }
    }
  }
  return(samples)
}

# Generate samples
set.seed(123)
samples <- rejection_sampling(1000)

# Plot the histogram of the samples
hist(samples, breaks = 30, freq = FALSE, main = "Rejection Sampling for Gamma(3/2, 1)", xlab = "x")
curve(dgamma(x, shape = 3/2, rate = 1), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = "Gamma(3/2, 1)", col = "red", lwd = 2)
__________________________________________________________
#Rejection z(0,1)

# Define the target density function
f <- function(x) {
  (2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# Define the proposal density function (e.g., exponential distribution with rate 1)
g <- function(x) {
  dexp(x, rate = 1)
}

# Find the maximum value of f(x) / g(x) to determine M
x_vals <- seq(0, 10, length.out = 1000)
ratio <- f(x_vals) / g(x_vals)
M <- max(ratio)

# Rejection sampling algorithm
n_samples <- 10000
samples <- numeric(n_samples)
accepted <- 0

while (accepted < n_samples) {
  # Sample from the proposal distribution
  x <- rexp(1, rate = 1)
  
  # Sample from a uniform distribution
  u <- runif(1)
  
  # Accept or reject the sample
  if (u < f(x) / (M * g(x))) {
    accepted <- accepted + 1
    samples[accepted] <- x
  }
}

# Since we generated the absolute value, we need to assign random signs
signs <- sample(c(-1, 1), n_samples, replace = TRUE)
normal_samples <- samples * signs

# Check the results
hist(normal_samples, breaks = 50, prob = TRUE, main = "Histogram of Generated Normal Samples")
curve(dnorm(x), add = TRUE, col = "red")
_____________________________________________________________________-
#  find value sqrt(2/pi)
# Set a random seed for reproducibility
set.seed(123)

# Generate 1 million standard normal random variables
n <- 10^6
z <- rnorm(n)  # No libraries needed - rnorm is in base R

# Calculate the mean of absolute values
mean_abs_z <- mean(abs(z))

# Theoretical value
theoretical_value <- sqrt(2/pi)

# Compare results
cat("Simulated E[|Z|]:", mean_abs_z, "\n")
cat("Theoretical value (√(2/π)):", theoretical_value, "\n")
cat("Difference:", abs(mean_abs_z - theoretical_value), "\n")
__________________________________________________________-

##target n(0,1)and proposed n(5,2)
# Set seed for reproducibility
set.seed(123)

# Define the target and proposal distributions
target_dist <- function(x) dnorm(x, mean = 0, sd = 1)
proposal_dist <- function(x) dnorm(x, mean = 5, sd = sqrt(2))

# Generate samples from the proposal distribution
n_samples <- 10000
samples <- rnorm(n_samples, mean = 5, sd = sqrt(2))

# Calculate the importance weights
weights <- target_dist(samples) / proposal_dist(samples)

# Estimate the expected value E(x^2)
expected_value <- mean(samples^2 * weights)

# Print the estimated expected value
cat("Estimated E(x^2):", expected_value, "\n")

# Create a density plot to compare the target and proposal distributions
x_values <- seq(-5, 10, length.out = 1000)
target_density <- target_dist(x_values)
proposal_density <- proposal_dist(x_values)

# Plot the densities using base R
plot(x_values, target_density, type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "Density", main = "Comparison of Target and Proposal Distributions")
lines(x_values, proposal_density, col = "red", lwd = 2)
legend("topright", legend = c("Target N(0,1)", "Proposal N(5,2)"),
       col = c("blue", "red"), lwd = 2)
____________________________________________________________________-
##Big question
# Discrete hazard rate method for geometric random variable
simulate_geometric <- function(p) {
  X <- 1
  while (TRUE) {
    U <- runif(1) # Generate a random number U
    if (U < p) {
      return(X) # Stop if U < p
    }
    X <- X + 1 # Increment X
  }
}

# Example usage
set.seed(123) # For reproducibility
p <- 0.3 # Parameter for geometric distribution
n_simulations <- 1000 # Number of simulations
results <- replicate(n_simulations, simulate_geometric(p))

# Compare with theoretical mass function
theoretical_probs <- dgeom(0:max(results), prob = p)
empirical_probs <- table(results) / n_simulations

# Print results
cat("Theoretical probabilities:\n", theoretical_probs, "\n")
cat("Empirical probabilities:\n", empirical_probs, "\n")

# Plot results
plot(0:max(results), theoretical_probs, type = "h", col = "blue", lwd = 2,
     xlab = "X", ylab = "Probability", main = "Geometric Distribution")
lines(as.numeric(names(empirical_probs)), empirical_probs, type = "h", col = "red", lwd = 2)
legend("topright", legend = c("Theoretical", "Empirical"), col = c("blue", "red"), lwd = 2)
___________________________________________________________________-
#Antithetic & montecarlo x/2^x-1
h <- function(x) {
  return(x/2^(x-1))
}
monte_carlo <- function(k) {
  samples <- rexp(k, rate = 1)  #
  estimates <- h(samples)       #
  return(mean(estimates))       #
}
antithetic_variates <- function(k) {
  u <- runif(k / 2)             
  samples1 <- -log(u)           
  samples2 <- -log(1 - u)       #
  estimates1 <- h(samples1)     # 
  estimates2 <- h(samples2)     #
  return(mean(c(estimates1, estimates2)))  
}
# Number of samples
k <- c(1000,2000,60000)
mc_estimates <- replicate(100, monte_carlo(k))  
mc_mean <- mean(mc_estimates) 
mc_var <- var(mc_estimates)    
av_estimates <- replicate(100, antithetic_variates(k)) 
av_mean <- mean(av_estimates)  
av_var <- var(av_estimates)    
# Compare computational efficiency
time_mc <- system.time(replicate(100, monte_carlo(k)))  # Time for Monte Carlo
time_av <- system.time(replicate(100, antithetic_variates(k)))  # Time for Antithetic Variates

# Print results
cat("Monte Carlo Mean:", mc_mean, "\n")
cat("Monte Carlo Variance:", mc_var, "\n")
cat("Antithetic Variates Mean:", av_mean, "\n")
cat("Antithetic Variates Variance:", av_var, "\n")
cat("Monte Carlo Time:", time_mc[3], "seconds\n")
cat("Antithetic Variates Time:", time_av[3], "seconds\n")

# Visualization using base R
# Create a density plot for Monte Carlo and Antithetic Variates estimates
plot(density(mc_estimates), col = "blue", lwd = 2, main = "Comparison of Monte Carlo and Antithetic Variates Estimates",
     xlab = "Estimate", ylab = "Density", xlim = range(c(mc_estimates, av_estimates)))
lines(density(av_estimates), col = "red", lwd = 2)
legend("topright", legend = c("Monte Carlo", "Antithetic Variates"), col = c("blue", "red"), lwd = 2)
________________________________________________________________________

#Suppose Xis a random variable following the Exponential distribution with parameter λ.
Use Monte Carlo simulation to estimate the expectation of the function h(x) = e^-x

set.seed(123)  # For reproducibility

# Parameters
lambda <- 2    # Parameter for exponential distribution
k <- 10000     # Number of samples
true_value <- lambda / (1 + lambda)  # Theoretical value of E[e^-X]

# Basic Monte Carlo estimation
basic_mc <- function(k, lambda) {
  start_time <- Sys.time()
  X <- rexp(k, rate = lambda)
  hX <- exp(-X)
  estimate <- mean(hX)
  variance <- var(hX)/k
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  
  list(estimate = estimate, 
       variance = variance,
       time = as.numeric(time_taken),
       samples = hX)
}

# Control Variates estimation
control_variates <- function(k, lambda) {
  start_time <- Sys.time()
  X <- rexp(k, rate = lambda)
  hX <- exp(-X)
  
  # Use X itself as control variate (since we know E[X] = 1/lambda)
  c <- -cov(hX, X) / var(X)  # Optimal coefficient
  Y <- hX + c * (X - 1/lambda)
  
  estimate <- mean(Y)
  variance <- var(Y)/k
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  
  list(estimate = estimate, 
       variance = variance,
       time = as.numeric(time_taken),
       samples = Y,
       c = c)
}

# Run both methods
basic_result <- basic_mc(k, lambda)
cv_result <- control_variates(k, lambda)

# Print results
cat("True value:", true_value, "\n\n")
cat("Basic Monte Carlo:\n")
cat("  Estimate:", basic_result$estimate, "\n")
cat("  Variance:", basic_result$variance, "\n")
cat("  Time (s):", basic_result$time, "\n\n")

cat("Control Variates:\n")
cat("  Estimate:", cv_result$estimate, "\n")
cat("  Variance:", cv_result$variance, "\n")
cat("  Time (s):", cv_result$time, "\n")
cat("  Optimal c:", cv_result$c, "\n\n")

cat("Variance reduction:", 100*(1 - cv_result$variance/basic_result$variance), "%\n")

# Visualization of variance reduction
par(mfrow = c(1, 2))

# Plot basic MC estimates
plot(cumsum(basic_result$samples) / (1:k), type = "l", 
     ylim = range(c(basic_result$estimate, cv_result$estimate, true_value)),
     main = "Basic Monte Carlo Convergence",
     xlab = "Number of samples", ylab = "Estimate")
abline(h = true_value, col = "red", lty = 2)
legend("topright", legend = c("MC Estimate", "True Value"), 
       col = c("black", "red"), lty = c(1, 2))

# Plot control variates estimates
plot(cumsum(cv_result$samples) / (1:k), type = "l",
     ylim = range(c(basic_result$estimate, cv_result$estimate, true_value)),
     main = "Control Variates Convergence",
     xlab = "Number of samples", ylab = "Estimate")
abline(h = true_value, col = "red", lty = 2)
legend("topright", legend = c("CV Estimate", "True Value"), 
       col = c("black", "red"), lty = c(1, 2))
___________________________________________________________________________-
#AR(1) Also, visualize the simulated
data using a time series plot of one MCMC.
set.seed(123)
n <- 1000
phi <- 0.8
sigma <- 1
y <- numeric(n)
y[1] <- rnorm(1, 0, sigma / sqrt(1 - phi^2))
for (t in 2:n) {
  y[t] <- phi * y[t-1] + rnorm(1, 0, sigma)
}
plot(y,type="l",col="blue",main="simulated AR(1) data")
grid()
acf(y)
____________________________________________________________________
#Run the MCMC simulation using MH algorithm for 10,000 iterations to generate
samples of the parameters.
#Estimate the variance of the posterior mean using the nonoverlapping batch mean
(NBM ) method. Also, provide the necessary plots.

phi_true <- 0.7  # True value for phi
sigma2_true <- 1 # True value for variance of epsilon
n <- 1000        # Number of observations

# Generate synthetic AR(1) data
epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2_true))
y <- numeric(n)
y[1] <- epsilon[1]  # Set initial value for y_1
for (t in 2:n) {
  y[t] <- phi_true * y[t-1] + epsilon[t]
}
# Metropolis-Hastings MCMC for AR(1) model with estimation of sigma2
mcmc_iterations <- 10000 # Number of MCMC iterations

# Initialize parameters for MCMC
phi_start <- 0.5    # Initial guess for phi
sigma2_start <- 1   # Initial guess for sigma^2
phi_values <- numeric(mcmc_iterations)  # To store the sampled phi values
sigma2_values <- numeric(mcmc_iterations) # To store the sampled sigma^2 values
phi_values[1] <- phi_start
sigma2_values[1] <- sigma2_start

# Metropolis-Hastings Algorithm for parameter estimation
proposal_sd_phi <- 0.05  # Standard deviation for proposal distribution of phi
proposal_sd_sigma2 <- 0.1 # Standard deviation for proposal distribution of sigma^2

for (i in 2:mcmc_iterations) {
  
  # Propose a new value for phi from a normal distribution centered around the current phi
  phi_proposal <- rnorm(1, mean = phi_values[i-1], sd = proposal_sd_phi)
  
  # Propose a new value for sigma^2 (scale parameter) from a normal distribution
  sigma2_proposal <- abs(rnorm(1, mean = sigma2_values[i-1], sd = proposal_sd_sigma2)) # Ensures positivity
  
  # Calculate the likelihood for the current and proposed values of phi and sigma^2
  log_likelihood_current <- sum(dnorm(y[2:n], mean = phi_values[i-1] * y[1:(n-1)], sd = sqrt(sigma2_values[i-1]), log = TRUE))
  log_likelihood_proposal <- sum(dnorm(y[2:n], mean = phi_proposal * y[1:(n-1)], sd = sqrt(sigma2_proposal), log = TRUE))
  
  # Calculate the acceptance ratio
  acceptance_ratio_phi <- min(1, exp(log_likelihood_proposal - log_likelihood_current))
  
  # Accept or reject the proposal for phi
  if (runif(1) < acceptance_ratio_phi) {
    phi_values[i] <- phi_proposal
  } else {
    phi_values[i] <- phi_values[i-1]  # Retain the previous value if the proposal is rejected
  }
  
  # Update the sigma^2 using the residuals from the AR(1) model
  residuals <- y[2:n] - phi_values[i] * y[1:(n-1)]
  log_likelihood_current_sigma2 <- -n / 2 * log(sigma2_values[i-1]) - sum(residuals^2) / (2 * sigma2_values[i-1])
  log_likelihood_proposal_sigma2 <- -n / 2 * log(sigma2_proposal) - sum(residuals^2) / (2 * sigma2_proposal)
  
  # Calculate the acceptance ratio for sigma^2
  acceptance_ratio_sigma2 <- min(1, exp(log_likelihood_proposal_sigma2 - log_likelihood_current_sigma2))
  
  # Accept or reject the proposal for sigma^2
  if (runif(1) < acceptance_ratio_sigma2) {
    sigma2_values[i] <- sigma2_proposal
  } else {
    sigma2_values[i] <- sigma2_values[i-1]  # Retain the previous value if the proposal is rejected
  }
}
mean(phi_values)
mean(sigma2_values)
# Function to compute the NBM variance estimator
nbm_variance <- function(chain, batch_size) {
  n <- length(chain)
  B <- n %/% batch_size  # Number of batches
  
  # Ensure the number of samples is a multiple of batch_size
  chain <- chain[1:(B * batch_size)]
  
  # Compute batch means
  batch_means <- sapply(1:B, function(j) mean(chain[((j-1)*batch_size + 1):(j*batch_size)]))
  
  # Compute overall mean
  overall_mean <- mean(batch_means)
  
  # Compute variance of batch means
  nbm_var <- (batch_size / (B - 1)) * sum((batch_means - overall_mean)^2)
  
  return(nbm_var)
}

# Set batch size
batch_size <- 500

# Compute NBM variance for phi and sigma^2
phi_nbm_var <- nbm_variance(phi_values, batch_size)
sigma2_nbm_var <- nbm_variance(sigma2_values, batch_size)

# Print results
cat("NBM Variance Estimate for Phi:", phi_nbm_var, "\n")
cat("NBM Variance Estimate for Sigma^2:", sigma2_nbm_var, "\n")

# Visualization: Trace plots and histogram
par(mfrow = c(2, 2))

# Trace plot for phi
plot(phi_values, type = "l", col = "blue", main = "Trace Plot of Phi", ylab = "Phi", xlab = "Iteration")

# Histogram of phi
hist(phi_values, col = "lightblue", main = "Histogram of Phi", xlab = "Phi", breaks = 30)

# Trace plot for sigma^2
plot(sigma2_values, type = "l", col = "red", main = "Trace Plot of Sigma^2", ylab = "Sigma^2", xlab = "Iteration")

# Histogram of sigma^2
hist(sigma2_values, col = "pink", main = "Histogram of Sigma^2", xlab = "Sigma^2", breaks = 30)
____________________________________________________________-
# Generate random samples from a Normal distribution using the Metropolis-Hastings
(M-H) algorithm and estimate its parameters. Then, implement a burn-in period to
remove initial bias and compare results using the Mean Squared Error (MSE).
Do it for different sample sizes (n=50,100,250)

# Function for Metropolis-Hastings Algorithm to sample from N(mu, sigma^2)
metropolis_hastings <- function(n, mu, sigma, proposal_sd, burn_in = 0) {
  samples <- numeric(n + burn_in)  # Store samples
  samples[1] <- rnorm(1, mean = mu, sd = sigma)  # Initialize with a random value
  
  for (i in 2:(n + burn_in)) {
    # Propose a new sample from a normal distribution centered at the current value
    proposal <- rnorm(1, mean = samples[i-1], sd = proposal_sd)
    
    # Compute acceptance probability
    acceptance_ratio <- dnorm(proposal, mean = mu, sd = sigma) / 
                        dnorm(samples[i-1], mean = mu, sd = sigma)
    
    # Accept or reject the proposed value
    if (runif(1) < acceptance_ratio) {
      samples[i] <- proposal
    } else {
      samples[i] <- samples[i-1]
    }
  }
  
  # Apply burn-in period (removing first 'burn_in' samples)
  return(samples[(burn_in + 1):(n + burn_in)])
}

# Function to compute Mean Squared Error (MSE)
mse <- function(estimated, true_value) {
  return(mean((estimated - true_value)^2))
}

# True parameters of the normal distribution
mu_true <- 5
sigma_true <- 2

# Proposal standard deviation
proposal_sd <- 1

# Burn-in period
burn_in_period <- 500

# Different sample sizes
sample_sizes <- c(50, 100, 250)

# Store results
results <- data.frame(Sample_Size = integer(), MSE_Without_Burnin = numeric(), MSE_With_Burnin = numeric())

# Run M-H algorithm for different sample sizes and compare MSE
for (n in sample_sizes) {
  # Without burn-in
  samples_no_burn <- metropolis_hastings(n, mu_true, sigma_true, proposal_sd, burn_in = 0)
  mu_est_no_burn <- mean(samples_no_burn)
  mse_no_burn <- mse(mu_est_no_burn, mu_true)
  
  # With burn-in
  samples_with_burn <- metropolis_hastings(n, mu_true, sigma_true, proposal_sd, burn_in = burn_in_period)
  mu_est_with_burn <- mean(samples_with_burn)
  mse_with_burn <- mse(mu_est_with_burn, mu_true)
  
  # Store results
  results <- rbind(results, data.frame(Sample_Size = n, 
                                       MSE_Without_Burnin = mse_no_burn, 
                                       MSE_With_Burnin = mse_with_burn))
}

# Print results
print(results)

# Visualization
par(mfrow = c(1, 2))

# Plot of samples without burn-in
plot(samples_no_burn, type = "l", col = "blue", main = "Trace Plot (No Burn-in)", xlab = "Iteration", ylab = "Sampled Value")

# Plot of samples with burn-in
plot(samples_with_burn, type = "l", col = "red", main = "Trace Plot (With Burn-in)", xlab = "Iteration", ylab = "Sampled Value")
________________________________________________________________________
#Rayleigh and Chi_square
sigma <- 2  
rayleigh_density <- function(x, sigma) {
  if (x < 0) return(0)
  (x / sigma^2) * exp(-x^2 / (2 * sigma^2))
}
mh_rayleigh_varying <- function(n_samples, sigma, burn_in = 1000) {
  samples <- numeric(n_samples + burn_in)
  current <- 1  # Initial value
  accept_count <- 0
  for (i in 1:(n_samples + burn_in)) {
    # Proposal: Chi-square with df = current state (rounded to integer)
    df_proposal <- max(1, round(current))
    proposal <- rchisq(1, df = df_proposal)
    # Calculate acceptance probability
    target_ratio <- rayleigh_density(proposal, sigma) / rayleigh_density(current, sigma)
    proposal_ratio <- dchisq(current, df = max(1, round(proposal)))/dchisq(proposal, df = df_proposal)
    alpha <- min(1, target_ratio * proposal_ratio)
    # Accept/reject
    if (runif(1) < alpha) {
      current <- proposal
      if (i > burn_in) accept_count <- accept_count + 1
    }
    samples[i] <- current
  }
  # Return post-burn-in samples and acceptance rate
  list(samples = samples[(burn_in + 1):length(samples)], acceptance_rate = accept_count / n_samples)
}
# Part b: Metropolis-Hastings with fixed Chi-square proposal
mh_rayleigh_fixed <- function(n_samples, sigma, df_fixed = 4, burn_in = 1000) {
  samples <- numeric(n_samples + burn_in)
  current <- 1  # Initial value
  accept_count <- 0
  for (i in 1:(n_samples + burn_in)) {
    # Proposal: Chi-square with fixed df
    proposal <- rchisq(1, df = df_fixed)
    # Calculate acceptance probability
    target_ratio <- rayleigh_density(proposal, sigma) / rayleigh_density(current, sigma)
    proposal_ratio <- dchisq(current, df = df_fixed) / dchisq(proposal, df = df_fixed)
    alpha <- min(1, target_ratio * proposal_ratio)
    # Accept/reject
    if (runif(1) < alpha) {
      current <- proposal
      if (i > burn_in) accept_count <- accept_count + 1
    }
    samples[i] <- current
  }
  # Return post-burn-in samples and acceptance rate
  list(samples = samples[(burn_in + 1):length(samples)], 
       acceptance_rate = accept_count / n_samples)
}
# Generate samples
n_samples <- 10000
result_varying <- mh_rayleigh_varying(n_samples, sigma)
result_fixed <- mh_rayleigh_fixed(n_samples, sigma)

# a) Acceptance rate with varying proposal
cat("Acceptance rate with varying proposal:",result_varying$acceptance_rate, "\n")

# b) Acceptance rate with fixed proposal
cat("Acceptance rate with fixed proposal:",result_fixed$acceptance_rate, "\n")
cat("\nComment on acceptance rates:
The varying proposal has acceptance rate of", round(result_varying$acceptance_rate, 3), 
"while the fixed proposal has", round(result_fixed$acceptance_rate, 3), ".
The optimal acceptance rate for MH algorithms is typically around 0.234 for high-dimensional 
problems, though for 1D problems it can be higher. The fixed proposal rate is more satisfactory 
as it's closer to the optimal range. The varying proposal has too high acceptance which may 
indicate the proposals are too small, leading to slow exploration of the space.\n")
# d) Reduce bias and autocorrelation
reduce_autocorrelation <- function(samples, thin = 10) {
  # Thinning: keep every 'thin'-th sample
  thinned <- samples[seq(1, length(samples), by = thin)]
  # Remove burn-in (already done in our functions)
  # No additional burn-in needed here
  thinned
}
# Apply thinning
thinned_varying <- reduce_autocorrelation(result_varying$samples)
thinned_fixed <- reduce_autocorrelation(result_fixed$samples)
cat("\nAfter thinning (keeping every 10th sample), we have", length(thinned_fixed), "samples.\n")
# e) Visualizations to verify Rayleigh distribution
verify_distribution <- function(samples, sigma) {
  # Histogram with theoretical density
  hist(samples, prob = TRUE, breaks = 30, main = "Sample vs Theoretical Rayleigh",
       xlab = "x", col = "lightblue")
  # Theoretical Rayleigh density curve
  x <- seq(0, max(samples), length.out = 100)
  lines(x, (x / sigma^2) * exp(-x^2 / (2 * sigma^2)), col = "red", lwd = 2)
  legend("topright", legend = c("Sample", "Theoretical"), col = c("lightblue", "red"), lwd = c(NA, 2), pch = c(15, NA))
  # Q-Q plot
  theoretical_quantiles <- qrayleigh <- function(p, sigma) {
    sigma * sqrt(-2 * log(1 - p))
  }
  sorted_samples <- sort(samples)
  n <- length(sorted_samples)
  probs <- (1:n) / (n + 1)
  theo_quantiles <- qrayleigh(probs, sigma)
  
  plot(theo_quantiles, sorted_samples, main = "Q-Q Plot", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  abline(0, 1, col = "red")
}
# Create visualizations for fixed proposal results (better acceptance rate)
par(mfrow = c(1, 2))
verify_distribution(thinned_fixed, sigma)
_______________________________________________________________________________________
#Gibbs sampling given data 
x <- c(0.156422697, 1.00337381, 0.438915231, 0.304314185, 0.056541623, 
       0.056532097, 0.019946256, 0.670410288, 0.306360718, 0.410416687, 
       0.006933103, 1.167852492, 0.595476514, 0.079562542, 0.066892996, 
       0.067537141, 0.12091791, 0.247975944, 0.188512356, 0.114740998, 
       0.315456958, 0.050078176, 0.115171837, 0.152092406, 0.202978229, 
       0.512645337, 0.074245287, 0.240676385, 0.29916824, 0.015854617, 
       0.311777674, 0.062320417, 0.02242131, 0.991229265, 1.123543447, 
       0.550777191, 0.121095953, 0.034259105, 0.384250254, 0.193363614)
n <- length(x)
sum_x <- sum(x)
n_iter <- 10000
a_samples <- numeric(n_iter)
b_samples <- numeric(n_iter)
# Initial values
a <- 1
b <- 1
# Gibbs sampling loop
for (i in 1:n_iter) {
  # Sample a | b, x ~ Gamma(n+1, sum_x + 1) since prior on a: exp(-a), likelihood: a^n exp(-a b sum x)
  shape_a <- n + 1
  rate_a <- b * sum_x + 1
  a <- qgamma(runif(1), shape=shape_a, rate=rate_a)
# Sample b | a, x ~ Gamma(n+1, a*sum_x + 1) since prior on b: exp(b), likelihood: b^n exp(-a b sum x)
  shape_b <- n + 1
  rate_b <- a * sum_x + 1
  b <- qgamma(runif(1), shape=shape_b, rate=rate_b)
  a_samples[i] <- a
  b_samples[i] <- b
}
# Plotting the results
plot(a_samples, type='l', main="Trace plot of a", xlab="Iteration", ylab="a")
plot(b_samples, type='l', main="Trace plot of b", xlab="Iteration", ylab="b")
hist(a_samples, breaks=40, main="Posterior of a", xlab="a", col="lightblue",prob=T)
lines(density(a_samples), col = "red", lwd = 2)
hist(b_samples, breaks=40, main="Posterior of b", xlab="b", col="lightgreen",prob=T)
lines(density(b_samples), col = "red", lwd = 2)
plot(a_samples, b_samples, pch=20, col=c("red","blue"),main="Joint Posterior of (a, b)", xlab="a", ylab="b")
____________________________________________________________________________________________________________________________
#Bootstrap_Weibull_parametric & nonparametric
time <- c(120, 150, 200, 170, 180, 220, 140, 160, 210, 190)
n <- length(time)
weibull_loglik <- function(params, x) {
  shape <- params[1]
  scale <- params[2]
  if (shape <= 0 || scale <= 0) return(-Inf)
  ll <- n*log(shape) - n*shape*log(scale) + (shape - 1)*sum(log(x)) - sum((x/scale)^shape)
  return(-ll)  # negative for minimization
}
best_ll <- Inf
best_shape <- NA
best_scale <- NA
for (s in seq(0.1, 5, by=0.01)) {
  for (l in seq(50, 300, by=1)) {
    ll <- weibull_loglik(c(s, l), time)
    if (ll < best_ll) {
      best_ll <- ll
      best_shape <- s
      best_scale <- l
    }
  }
}

mle_shape <- best_shape
mle_scale <- best_scale
B <- 1000
shape_boot <- numeric(B)
for (b in 1:B) {
  u <- runif(n)
  sample_b <- mle_scale * (-log(1 - u))^(1 / mle_shape)
  best_ll_b <- Inf
  for (s in seq(0.1, 5, by=0.01)) {
    for (l in seq(50, 300, by=1)) {
      ll <- weibull_loglik(c(s, l), sample_b)
      if (ll < best_ll_b) {
        best_ll_b <- ll
        shape_boot[b] <- s
      }
    }
  }
}

# Estimate bias and standard error
bias_shape <- mean(shape_boot) - mle_shape
se_shape <- sqrt(sum((shape_boot - mean(shape_boot))^2) / (B - 1))
cat("MLE shape =", mle_shape, "\n")
cat("Bias =", bias_shape, "\n")
cat("Standard Error =", se_shape, "\n")
#nonparametric
income <- c(45, 50, 52, 47, 60, 58, 55, 53, 49, 65)
n <- length(income)
B <- 1000
mean_boot <- numeric(B)
for (b in 1:B) {
  sample_b <- income[sample(1:n, n, replace=TRUE)]
  mean_boot[b] <- sum(sample_b) / n
}
# 95% Confidence Interval
sorted_means <- sort(mean_boot)
lower <- sorted_means[round(0.025 * B)]
upper <- sorted_means[round(0.975 * B)]

cat("Bootstrap Mean Estimate =", mean(income), "\n")
cat("95% CI: [", lower, ",", upper, "]\n")
#jackkinfe
rainfall <- c(8.2, 10.4, 12.3, 9.5, 11.0, 10.8, 9.7, 13.2, 10.1, 12.0)
n <- length(rainfall)
means_jack <- numeric(n)
for (i in 1:n) {
  jack_sample <- rainfall[-i]
  means_jack[i] <- sum(jack_sample) / (n - 1)
}
jack_mean <- sum(means_jack) / n
se_jack <- sqrt((n - 1) * sum((means_jack - jack_mean)^2) / n)
cat("Jackknife Mean Estimate =", mean(rainfall), "\n")
cat("Jackknife SE =", se_jack, "\n")
_____________________________________________________________________
#Shopping data form customer
# Parameters for the 3 shopper types (Budget, Moderate, Premium)
mu_true <- c(50, 150, 300)      
sigma_true <- c(15, 30, 50)     
pi_true <- c(0.4, 0.35, 0.25)   
n <- 1000
z_true <- sample(1:3, size = n, replace = TRUE, prob = pi_true)
spending <- rnorm(n, mean = mu_true[z_true], sd = sigma_true[z_true])
spending <- pmax(spending, 0)
# Step 2: Implement EM algorithm for Gaussian Mixture Model
em_gmm <- function(data, k = 3, max_iter = 100, tol = 1e-6) {
  n <- length(data)
  mu <- runif(k, min = min(data), max = max(data))
  sigma <- rep(sd(data)/k, k)
  pi <- rep(1/k, k)
  log_lik <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    ## E-step: Compute responsibilities
    # Matrix of likelihoods for each data point and component
    lik <- matrix(NA, nrow = n, ncol = k)
    for (j in 1:k) {
      lik[, j] <- pi[j] * dnorm(data, mean = mu[j], sd = sigma[j])
    }
    
    # Responsibilities (posterior probabilities)
    gamma <- lik / rowSums(lik)
    
    ## M-step: Update parameters
    # Effective number of points in each component
    N_k <- colSums(gamma)
    
    # Update mixing proportions
    pi_new <- N_k / n
    
    # Update means
    mu_new <- colSums(gamma * data) / N_k
    
    # Update standard deviations
    sigma_new <- sqrt(colSums(gamma * (sweep(matrix(data, n, k), 2, mu_new)^2)) / N_k)
    
    # Compute log-likelihood for convergence check
    log_lik[iter] <- sum(log(rowSums(lik)))
    
    # Check for convergence
    if (iter > 1 && abs(log_lik[iter] - log_lik[iter - 1]) < tol) {
      break
    }
    
    # Update parameters
    mu <- mu_new
    sigma <- sigma_new
    pi <- pi_new
  }
  
  return(list(mu = mu, sigma = sigma, pi = pi, 
              log_lik = log_lik[1:iter], gamma = gamma))
}

# Run EM algorithm
results <- em_gmm(spending)

## Step 3: Results and visualization

# Print estimated parameters
cat("Estimated parameters:\n")
cat("Means:", round(results$mu, 2), "\n")
cat("SDs:", round(results$sigma, 2), "\n")
cat("Mixing proportions:", round(results$pi, 3), "\n\n")

# Compare with true parameters
cat("True parameters:\n")
cat("Means:", mu_true, "\n")
cat("SDs:", sigma_true, "\n")
cat("Mixing proportions:", pi_true, "\n\n")

# Plot the results
par(mfrow = c(1, 2))

# Histogram of data with estimated densities
hist(spending, breaks = 30, prob = TRUE, col = "lightblue",
     main = "Spending Distribution with Estimated Components",
     xlab = "Total Spend")

# Add estimated components
x <- seq(0, max(spending), length.out = 1000)
for (j in 1:3) {
  lines(x, results$pi[j] * dnorm(x, mean = results$mu[j], sd = results$sigma[j]), 
        col = j + 1, lwd = 2)
}

legend("topright", legend = paste("Component", 1:3), 
       col = 2:4, lwd = 2)

# Plot of log-likelihood over iterations
plot(results$log_lik, type = "b", col = "blue",
     main = "Log-Likelihood During EM Iterations",
     xlab = "Iteration", ylab = "Log-Likelihood")

# Class assignments based on maximum responsibility
z_est <- apply(results$gamma, 1, which.max)

# Confusion matrix (comparing true vs estimated classes)
cat("Confusion matrix (true vs estimated classes):\n")
print(table(True = z_true, Estimated = z_est))
__________________________________________________________________
#censor data at 7 
data <- c(4.2, 5.8, 6.1, 7.1, 3.9, 5.5, 7.1, 7.1, 4.8)  # Replace >7 with a value greater than 7 for initial estimation
censored_indices <- c(4, 7, 8)  # Indices of censored data
censoring_limit <- 7  # Censoring threshold

# EM algorithm function
em_algorithm <- function(data, censored_indices, max_iter = 100, tol = 1e-6) {
  # Initial estimates
  mu <- mean(data)  # Initial mean
  sigma <- sd(data)  # Initial standard deviation
  
  for (iter in 1:max_iter) {
    # E-step: Calculate expected values for censored data
    expected_values <- data
    for (i in censored_indices) {
      expected_values[i] <- mu + sigma * dnorm(censoring_limit, mean = mu, sd = sigma) / (1 - pnorm(censoring_limit, mean = mu, sd = sigma))
    }
    
    # M-step: Update parameters
    mu_new <- mean(expected_values)
    sigma_new <- sqrt(sum((expected_values - mu_new)^2) / length(expected_values))
    
    # Check for convergence
    if (abs(mu_new - mu) < tol && abs(sigma_new - sigma) < tol) {
      break
    }
    
    mu <- mu_new
    sigma <- sigma_new
  }
  
  return(list(mu = mu, sigma = sigma))
}
# Run the EM algorithm
results <- em_algorithm(data, censored_indices)

# Print the estimated parameters
cat("Estimated Mean (μ):", round(results$mu, 2), "\n")
cat("Estimated Standard Deviation (σ):", round(results$sigma, 2), "\n")





