//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower=0> N;
  vector[N] y;
  array[N] int<lower=1, upper=12> month;
}

parameters {
  real mu0;
  real<lower=0> delta;
  real<lower=0> sigma0;
  real<lower=0> sigma1;
  vector[12] alpha;
}
transformed parameters {
  real mu1 = mu0 + delta;
}

model {
  mu0 ~ normal(1.5, 0.5);
  delta ~ lognormal(0, 0.5);
  sigma0 ~ exponential(1);
  sigma1 ~ exponential(1);
  alpha ~ normal(0, 1);

  for (n in 1:N) {
    real log_pi = log_inv_logit(alpha[month[n]]);
    real log_1m_pi = log1m_inv_logit(alpha[month[n]]);
    target += log_sum_exp(
      log_pi   + normal_lpdf(y[n] | mu1, sigma1),
      log_1m_pi + normal_lpdf(y[n] | mu0, sigma0)
    );
  }
}

generated quantities {
  vector[12] pi_month;
  for (m in 1:12)
    pi_month[m] = inv_logit(alpha[m]);
}

