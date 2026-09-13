library(rstan)
library(mcmcConvergenceGraph)


fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/multimodal_gaussian/multimodal_gaussian.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/multimodal_gaussian"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

arr <- as.array(fit) #this gives us a 3D array iterations*chains*parameters (and post-warmup only)
print(arr)

c1 <- monitor(
  arr[,c(1,2,8),"x[1]",drop =FALSE],
  warmup = 0,
  probs =c(0.025,0.5,0.975),
)
as.data.frame(c1)

c2 <- monitor(
  arr[,c(4,6,7),"x[1]",drop =FALSE],
  warmup = 0,
  probs =c(0.025,0.5,0.975),
)
as.data.frame(c2)

c3 <- monitor(
  arr[,c(3,5),"x[1]",drop =FALSE],
  warmup = 0,
  probs =c(0.025,0.5,0.975),
)
as.data.frame(c3)

c4 <- monitor(
  arr[,c(1, 2, 4, 6, 7, 8),"x[2]",drop =FALSE],
  warmup = 0,
  probs =c(0.025,0.5,0.975),
)
as.data.frame(c4)

c5 <- monitor(
  arr[,c(3,5),"x[2]",drop =FALSE],
  warmup = 0,
  probs =c(0.025,0.5,0.975),
)
as.data.frame(c5)
