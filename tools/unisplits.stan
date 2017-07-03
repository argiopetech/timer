data {
  int<lower=0> N;
  int num_y[N];
  vector[sum(num_y)] y;

  real mu_prior;
  real tau_prior;
}
parameters {
  vector[N] theta;
  vector<lower=0>[N] sigma;

  real mu;
  real<lower=0> tau;
}
model {
  for (n in 1:N) {
    theta[n] ~ normal(mu, tau);
    sigma[n] ~ cauchy(0, 5);

    y[(sum(num_y[1:n-1]) + 1):(sum(num_y[1:n]))] ~ normal(theta[n], sigma[n]);
  }

  mu ~ normal(mu_prior, tau_prior);
  tau ~ cauchy(0, 5);

  y ~ normal(mu, tau);
}
generated quantities {
  vector[N] theta_pred;
  vector[N] tots;
  
  real mu_pred;

  for (n in 1:N) {
    theta_pred[n] = normal_rng(theta[n], sigma[n]);
  }

  for (n in 1:N) {
    tots[n] = sum(theta_pred[1:n]);
  }

  mu_pred = normal_rng(mu, tau);
}
