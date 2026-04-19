# Bayesian Heavy-Tailed Modeling of PM2.5 in BC's Southern Interior

STAT 405 Final Project — Eun Ji Hwang

## Files

**Stan models:**
- `base_model.stan` — Model 1: Normal seasonal baseline
- `studentt_model.stan` — Model 2: Student-t seasonal
- `mixture_model.stan` — Exploratory Gaussian mixture (Appendix F)
- `model2_sensitivity_exp.stan` — Sensitivity analysis: Exp(0.1) prior on ν
- `model2_sensitivity_gamma42.stan` — Sensitivity analysis: Gamma(4,0.2) prior on ν

**R scripts (run in this order):**
1. `the pm2.5 plot and log(pm2.5)plot.r` — EDA and Figure 1
2. `base_model.r` — Fit Model 1 (HMC)
3. `studentt_model.r` — Fit Model 2 (HMC)
4. `hmc_advi.r` — LOO-CV, PPC, and ADVI comparison (Figures 3-4)
5. `sensitivity_and_synth.r` — Prior sensitivity and synthetic data calibration
6. `mixture_model.r` — Mixture model attempt

**Data:**
- `data_raw/` — Raw PM2.5 data from BC Air Data Archive
- `data_processed/` — Cleaned daily data used in analysis

## Requirements
R packages: `cmdstanr`, `loo`, `bayesplot`, `ggplot2`, `gridExtra`
