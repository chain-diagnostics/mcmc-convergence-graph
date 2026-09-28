data {

  int<lower=1> N;
  vector[N] x;
  vector[N] y;

}

parameters {
  real a;
  real b;
  real<lower=0> gamma;
}

model {
  a     ~ normal(0, 1);
  b     ~ normal(0, 1);
  gamma ~ normal(0, 0.2);
  y ~ cauchy(a * x + b, gamma);

}
