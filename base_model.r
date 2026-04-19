library(cmdstanr)
library(loo)
library(ggplot2)
df <- read.csv("data_processed/pm25_daily_clean_2022_2025.csv")

stan_data <- list(
  N     = nrow(df),
  y     = df$y,
  month = df$month
)

mod1 <- cmdstan_model("base_model.stan")

fit1 <- mod1$sample(
  data    = stan_data,
  seed    = 123,
  chains  = 4,
  iter_warmup   = 1000,
  iter_sampling = 2000,
  refresh = 500
)

# diagonal
fit1$summary()
fit1$cmdstan_diagnose()

# trace plot
library(bayesplot)
mcmc_trace(fit1$draws(c("mu", "sigma", "beta[7]", "beta[8]")))

#studentt_model
mod2 <- cmdstan_model("studentt_model.stan")

fit2 <- mod2$sample(
  data    = stan_data,
  seed    = 123,
  chains  = 4,
  iter_warmup   = 1000,
  iter_sampling = 2000,
  refresh = 500
)

fit2$summary()
fit2$cmdstan_diagnose()

mcmc_trace(fit2$draws(c("mu", "sigma", "nu", "beta[7]", "beta[8]")))

#LOO-CV comparison
loo1 <- loo(fit1$draws("log_lik"))
loo2 <- loo(fit2$draws("log_lik"))
print(loo_compare(loo1, loo2))

# ── PPC: max(y_rep) ──
y_rep1 <- fit1$draws("y_rep", format = "matrix")
y_rep2 <- fit2$draws("y_rep", format = "matrix")
obs_max <- max(df$y)

df_ppc <- data.frame(
  max_yrep = c(apply(y_rep1, 1, max), apply(y_rep2, 1, max)),
  Model = rep(c("Model 1 (Normal)", "Model 2 (Student-t)"),
              c(nrow(y_rep1), nrow(y_rep2)))
)
ggplot(df_ppc, aes(x = max_yrep, fill = Model)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  geom_vline(xintercept = obs_max, linetype = "dashed") +
  labs(title = "Posterior Predictive Check: Distribution of max(y_rep)", x = "max(y_rep)") +
  theme_bw()

#ADVI comparison
fit2_vi <- mod2$variational(data = stan_data, algorithm = "meanfield",
                            output_samples = 1000)
# Compare nu and sigma between HMC and ADVI
nu_hmc   <- as.vector(fit2$draws("nu"))
nu_advi  <- as.vector(fit2_vi$draws("nu"))
sigma_hmc  <- as.vector(fit2$draws("sigma"))
sigma_advi <- as.vector(fit2_vi$draws("sigma"))

df_compare <- data.frame(
  value  = c(nu_hmc, nu_advi, sigma_hmc, sigma_advi),
  Method = rep(c("HMC", "ADVI", "HMC", "ADVI"),
               c(length(nu_hmc), length(nu_advi),
                 length(sigma_hmc), length(sigma_advi))),
  Param  = rep(c("nu", "nu", "sigma", "sigma"),
               c(length(nu_hmc), length(nu_advi),
                 length(sigma_hmc), length(sigma_advi)))
)
library(gridExtra)

# nu
df_nu <- data.frame(
  value = c(nu_hmc, nu_advi),
  Method = rep(c("HMC", "ADVI"), c(length(nu_hmc), length(nu_advi)))
)
p_nu <- ggplot(df_nu, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.4) +
  labs(title = expression("Posterior of "*nu*": HMC vs ADVI"),
       x = expression(nu), y = "Density") +
  theme_bw()

#sigma
df_sigma <- data.frame(
  value = c(sigma_hmc, sigma_advi),
  Method = rep(c("HMC", "ADVI"), c(length(sigma_hmc), length(sigma_advi)))
)
p_sigma <- ggplot(df_sigma, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.4) +
  labs(title = expression("Posterior of "*sigma*": HMC vs ADVI"),
       x = expression(sigma), y = "Density") +
  theme_bw()


