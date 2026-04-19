library(cmdstanr)
library(bayesplot)
library(loo)
library(ggplot2)


df <- read.csv("data_processed/pm25_daily_clean_2022_2025.csv")
df$date <- as.Date(df$date)

stan_data <- list(
  N = nrow(df),
  y = df$y,
  month = df$month
)

mod1 <- cmdstan_model("base_model.stan")

fit1 <- mod1$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500
)

mod2 <- cmdstan_model("studentt_model.stan")

fit2 <- mod2$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500
)

log_lik1 <- fit1$draws("log_lik", format = "matrix")
log_lik2 <- fit2$draws("log_lik", format = "matrix")

loo1 <- loo(log_lik1)
loo2 <- loo(log_lik2)

print(loo1)
print(loo2)
print(loo_compare(loo1, loo2))

# ADVI (Model 1)
fit1_vi <- mod1$variational(
  data = stan_data,
  seed = 123,
  algorithm = "meanfield",
  output_samples = 1000
)

# ADVI (Model 2)
fit2_vi <- mod2$variational(
  data = stan_data,
  seed = 123,
  algorithm = "meanfield",
  output_samples = 1000
)

# compare nu
nu_hmc <- as.vector(fit2$draws("nu"))
nu_advi <- as.vector(fit2_vi$draws("nu"))

df_nu <- data.frame(
  nu = c(nu_hmc, nu_advi),
  Method = rep(c("HMC", "ADVI"), c(length(nu_hmc), length(nu_advi)))
)

p_nu <- ggplot(df_nu, aes(x = nu, fill = Method)) +
  geom_density(alpha = 0.5) +
  labs(title = expression("Posterior of "*nu*": HMC vs ADVI"),
       x = expression(nu), y = "Density") +
  theme_bw()


#compare sigma
sigma_hmc <- as.vector(fit2$draws("sigma"))
sigma_advi <- as.vector(fit2_vi$draws("sigma"))

df_sigma <- data.frame(
  sigma = c(sigma_hmc, sigma_advi),
  Method = rep(c("HMC", "ADVI"), c(length(sigma_hmc), length(sigma_advi)))
)

p_sigma <- ggplot(df_sigma, aes(x = sigma, fill = Method)) +
  geom_density(alpha = 0.5) +
  labs(title = expression("Posterior of "*sigma*": HMC vs ADVI"),
       x = expression(sigma), y = "Density") +
  theme_bw()


# Simulate y_rep(Model 1)
mu1_draws <- as.vector(fit1$draws("mu"))
sigma1_draws <- as.vector(fit1$draws("sigma"))
beta1_draws <- fit1$draws("beta", format = "matrix")

n_rep <- 500 
y_rep_m1 <- matrix(NA, nrow = n_rep, ncol = stan_data$N)

set.seed(42)
for (i in 1:n_rep) {
  for (t in 1:stan_data$N) {
    y_rep_m1[i, t] <- rnorm(1,
                            mean = mu1_draws[i] + beta1_draws[i, stan_data$month[t]],
                            sd   = sigma1_draws[i])
  }
}

# Simulate y_rep (Model 2) 
mu2_draws <- as.vector(fit2$draws("mu"))
sigma2_draws <- as.vector(fit2$draws("sigma"))
nu2_draws <- as.vector(fit2$draws("nu"))
beta2_draws <- fit2$draws("beta", format = "matrix")

y_rep_m2 <- matrix(NA, nrow = n_rep, ncol = stan_data$N)

for (i in 1:n_rep) {
  for (t in 1:stan_data$N) {
    y_rep_m2[i, t] <- mu2_draws[i] + beta2_draws[i, stan_data$month[t]] +
      rt(1, df = nu2_draws[i]) * sigma2_draws[i]
  }
}

# Compare max values 
max_obs <- max(stan_data$y)
max_m1 <- apply(y_rep_m1, 1, max)
max_m2 <- apply(y_rep_m2, 1, max)

df_ppc <- data.frame(
  max_y  = c(max_m1, max_m2),
  Model  = rep(c("Model 1 (Normal)", "Model 2 (Student-t)"), each = n_rep)
)

p_ppc <- ggplot(df_ppc, aes(x = max_y, fill = Model)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
  geom_vline(xintercept = max_obs, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = max_obs + 0.15, y = Inf, label = "Observed max",
           vjust = 2, hjust = 0, size = 3.5) +
  labs(title = "Posterior Predictive Check: Distribution of max(y_rep)",
       x = expression("max(y"["rep"]*")"), y = "Count") +
  theme_bw()



