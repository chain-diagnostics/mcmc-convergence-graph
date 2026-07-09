# pairwiserhat

`pairwiserhat` is an R package for graph-based pairwise R-hat diagnostics for
MCMC chains.

The proposed diagnostic is designed to identify multimodality without
automatically treating it as a sampling failure. Instead of reporting only one
global R-hat value across all chains, `pairwiserhat` compares chains pair by pair
and uses these comparisons to summarize the structure of MCMC behaviour.

The package provides tools for computing pairwise R-hat values, constructing
pairwise R-hat matrices, building chain-level diagnostic graphs, combining graph
information across parameters, summarizing connected components and isolated
chains, visualizing graph structure, and exporting diagnostic summaries.

The package is intended both for users who want a convenient diagnostic summary
after fitting Bayesian models or working with their own MCMC draws, and for
developers who want reusable functions for pairwise R-hat computation,
visualization, and diagnostic reporting.


## Installation

`pairwiserhat` is currently available as a development package from GitHub.

You can install it with:

```r

install.packages("devtools")

devtools::install_github("ccgu-uppsala/pairwiserhat")
```

## License

MIT
