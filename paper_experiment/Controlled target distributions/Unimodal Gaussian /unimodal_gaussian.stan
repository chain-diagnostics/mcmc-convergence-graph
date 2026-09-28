parameters {
  vector<lower=-1, upper=1>[2] x;
}

model {
  target += normal_lpdf(x | 0, sqrt(0.15));
}
