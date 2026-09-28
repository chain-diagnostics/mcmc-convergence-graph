
transformed data {
  vector[2] mu1 = [0.5, 0]';
  vector[2] mu2 = [-0.5, 0]';

  matrix[2, 2] Sigma = diag_matrix(rep_vector(0.008, 2));
  real log_w = log(1.0 / 2.0);
}

parameters {
  vector<lower=-1, upper=1>[2] x;
}

model {
  target += log_sum_exp({
    log_w + multi_normal_lpdf(x | mu1, Sigma),
    log_w + multi_normal_lpdf(x | mu2, Sigma)

  });
}
