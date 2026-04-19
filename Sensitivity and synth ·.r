library(cmdstanr)
library(loo)
library(ggplot2)

df <- read.csv("data_processed/pm25_daily_clean_2022_2025.csv")
stan_data <- list(N = nrow(df), y = df$y, month = df$month)

#fit
mod_exp <- cmdstan_model("model2_sensitivity_exp.stan")
fit_exp <- mod_exp$sample(data = stan_data, seed = 123, chains = 4,
                          iter_warmup = 1000, iter_sampling = 2000, refresh = 500)

mod_g42 <- cmdstan_model("model2_sensitivity_gamma42.stan")
fit_g42 <- mod_g42$sample(data = stan_data, seed = 123, chains = 4,
                          iter_warmup = 1000, iter_sampling = 2000, refresh = 500)

# nu posteriors compare
nu_baseline <- as.vector(fit2$draws("nu"))
nu_exp      <- as.vector(fit_exp$draws("nu"))
nu_g42      <- as.vector(fit_g42$draws("nu"))

cat(sprintf("Gamma(2,0.1): mean=%.2f, median=%.2f, q5=%.2f, q95=%.2f\n",
            mean(nu_baseline), median(nu_baseline),
            quantile(nu_baseline, 0.05), quantile(nu_baseline, 0.95)))
cat(sprintf("Exp(0.1):     mean=%.2f, median=%.2f, q5=%.2f, q95=%.2f\n",
            mean(nu_exp), median(nu_exp),
            quantile(nu_exp, 0.05), quantile(nu_exp, 0.95)))
cat(sprintf("Gamma(4,0.2): mean=%.2f, median=%.2f, q5=%.2f, q95=%.2f\n",
            mean(nu_g42), median(nu_g42),
            quantile(nu_g42, 0.05), quantile(nu_g42, 0.95)))

# Plot
df_sens <- data.frame(
  nu    = c(nu_baseline, nu_exp, nu_g42),
  Prior = rep(c("Gamma(2,0.1)", "Exp(0.1)", "Gamma(4,0.2)"),
              c(length(nu_baseline), length(nu_exp), length(nu_g42)))
)

p_sens <- ggplot(df_sens, aes(x = nu, fill = Prior)) +
  geom_density(alpha = 0.4) +
  xlim(3, 12) +
  labs(title = expression("Prior Sensitivity: Posterior of "*nu),
       x = expression(nu), y = "Density") +
  theme_bw()



#SYNTHETIC DATA CORRECTNESS TEST 
mod2 <- cmdstan_model("studentt_model.stan")

n_reps <- 50
N_sim  <- 200
months_sim <- rep(1:12, length.out = N_sim)

# Storage
coverage_mu    <- logical(n_reps)
coverage_sigma <- logical(n_reps)
coverage_nu    <- logical(n_reps)

set.seed(42)
for (i in 1:n_reps) {
  cat(sprintf("Synthetic test %d/%d\n", i, n_reps))
  
  # Draw from prior
  mu_true    <- rnorm(1, 2, 1)
  beta_true  <- rnorm(12, 0, 1)
  beta_true  <- beta_true - mean(beta_true)  # enforce sum-to-zero
  sigma_true <- rexp(1, 1)
  nu_true    <- rgamma(1, 2, 0.1)
  if (nu_true < 1) nu_true <- 1.5  # respect lower bound
  
  # Simulate data
  y_sim <- numeric(N_sim)
  for (t in 1:N_sim) {
    y_sim[t] <- mu_true + beta_true[months_sim[t]] +
      rt(1, df = nu_true) * sigma_true
  }
  
  sim_data <- list(N = N_sim, y = y_sim, month = months_sim)
  
  # Fit
  fit_sim <- tryCatch({
    mod2$sample(data = sim_data, seed = i, chains = 2,
                iter_warmup = 500, iter_sampling = 1000,
                refresh = 0, show_messages = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(fit_sim)) next
  
  # Check coverage
  mu_draws    <- as.vector(fit_sim$draws("mu"))
  sigma_draws <- as.vector(fit_sim$draws("sigma"))
  nu_draws    <- as.vector(fit_sim$draws("nu"))
  
  coverage_mu[i]    <- (quantile(mu_draws, 0.05) <= mu_true) &
    (mu_true <= quantile(mu_draws, 0.95))
  coverage_sigma[i] <- (quantile(sigma_draws, 0.05) <= sigma_true) &
    (sigma_true <= quantile(sigma_draws, 0.95))
  coverage_nu[i]    <- (quantile(nu_draws, 0.05) <= nu_true) &
    (nu_true <= quantile(nu_draws, 0.95))
}

cat(sprintf("mu:    %.0f%%\n", 100 * mean(coverage_mu)))
cat(sprintf("sigma: %.0f%%\n", 100 * mean(coverage_sigma)))
cat(sprintf("nu:    %.0f%%\n", 100 * mean(coverage_nu)))


#MCSE 
print(fit1$summary(c("mu", "sigma"), c("mean", "mcse_mean")))

print(fit2$summary(c("mu", "sigma", "nu"), c("mean", "mcse_mean")))

# DIVERGENT TRANSITIONS CHECK

fit1$cmdstan_diagnose()
fit2$cmdstan_diagnose()
