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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
  array[N] int<lower=1, upper=12> month;
}
parameters {
  real mu;
  vector[12] beta;
  real<lower=0> sigma;
  real<lower=1> nu;
}
model {
  mu ~ normal(2, 1);
  beta ~ normal(0, 1);
  sigma ~ exponential(1);
  nu ~ gamma(4, 0.2);
  sum(beta) ~ normal(0, 0.001);
  for (n in 1:N)
    y[n] ~ student_t(nu, mu + beta[month[n]], sigma);
}
