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
  real mu;
  vector[12] beta;
  real<lower=0> sigma;
}



model {
  // priors
  mu ~ normal(2, 1);
  beta ~ normal(0, 1);
  sigma ~ exponential(1);
  
  // soft sum-to-zero constraint
  //since without this the mu and betas are shifting freely
  sum(beta) ~ normal(0, 0.001); 

  // likelihood
  for (n in 1:N) {
    y[n] ~ normal(mu + beta[month[n]], sigma);
  }
}



