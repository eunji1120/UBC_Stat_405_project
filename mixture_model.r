library(cmdstanr)
library(bayesplot)
df <- read.csv("data_processed/pm25_daily_clean_2022_2025.csv")

stan_data <- list(
  N     = nrow(df),
  y     = df$y,
  month = df$month
)

mod2 <- cmdstan_model("mixture_model.stan")

fit2 <- mod2$sample(
  data    = stan_data,
  seed    = 123,
  chains  = 4,
  iter_warmup   = 2000,   
  iter_sampling = 3000,   
  adapt_delta   = 0.95,   
  refresh = 500
)

# diagnose
fit2$summary()
fit2$cmdstan_diagnose()

# trace plots
mcmc_trace(fit2$draws(c("mu0", "mu1", "delta", "sigma0", "sigma1", 
                        "pi_month[7]", "pi_month[8]")))

