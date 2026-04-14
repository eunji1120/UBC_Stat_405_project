library(cmdstanr)
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
