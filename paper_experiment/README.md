# Paper experiments

Install the package, then run each experiment from its own folder.
Fit the model first, then run the `*_pairwise.R` script.

```r
install.packages(c("devtools", "rstan"))
devtools::install_github("chain-diagnostics/mcmc-convergence-graph")

setwd("paper_experiment/Controlled target distributions/uniform")
source("uniform_base.R")
source("uniform_pairwise.R")
```

In RStudio: open the script and use Session → Set Working Directory → To Source File Location.

The pharmacokinetic scripts also need `bayesplot` and `ggplot2`.
