data {
  int < lower = 1 > N; // Sample size
  vector[N] y; // Sample
}

parameters {
  real mu; // mean
  real < lower = 0 > sigma1;
  real < lower = 0 > sigma2;
}

model {
  // Priors
  target += normal_lpdf(mu | 0, 100);
  target += inv_gamma_lpdf(sigma1 | 0.01, 0.01);
  target += inv_gamma_lpdf(sigma2 | 0.01, 0.01);
  // Likelihood
  for(n in 1:N){
    if(y[n] <  mu)
    {
      target += normal_lpdf( (y[n]-mu)/sigma1 | 0 , 1) + log(2) - log(sigma1 + sigma2);
    }
    if(y[n] >=  mu){
      target += normal_lpdf( (y[n]-mu)/sigma2 | 0 , 1) + log(2) - log(sigma1 + sigma2);
    }
  }
}

generated quantities {
} // The posterior predictive distribution"
