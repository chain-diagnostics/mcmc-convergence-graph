
transformed data {
  matrix[2, 2] Sigma = [[0.15, 0.14], [0.14, 0.15]];
  vector[2] mu = [0, 0]';
}

parameters {
  vector<lower=-1, upper=1>[2] x;
}

model {
  target += multi_normal_lpdf(x | mu, Sigma);
}
