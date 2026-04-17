library(cmdstanr)
df <- read.csv("data_processed/pm25_daily_clean_2022_2025.csv")

stan_data <- list(
  N     = nrow(df),
  y     = df$y,
  month = df$month
)

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
