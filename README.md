# mcmcConvergenceGraph

`mcmcConvergenceGraph` is an R package for the MCMC convergence graph
diagnostic. The diagnostic compares MCMC chains pair by pair using pairwise
R-hat values and summarises the resulting chain-agreement structure as a graph.

Instead of reporting only one global R-hat value across all chains, the package
builds a graph whose nodes are chains and whose edges mark pairs whose pairwise
R-hat falls below a chosen threshold. The resulting graph can be used to
identify groups of chains that appear to explore the same posterior region, as
well as isolated chains and other patterns of chain disagreement.

The package provides tools for computing pairwise R-hat values, constructing
pairwise R-hat matrices, building chain-level diagnostic graphs, combining graph
information across parameters, summarising connected components and isolated
chains, visualising graph structure, and exporting diagnostic summaries.

The user-facing wrapper is `mcmcgraph()`.

The package is intended both for users who want a convenient diagnostic summary
after fitting Bayesian models or working with their own MCMC draws, and for
developers who want reusable functions for pairwise R-hat computation,
visualisation, and diagnostic reporting.


## Installation

`mcmcConvergenceGraph` is currently available as a development package from
GitHub.

You can install it with:

```r
install.packages("devtools")

devtools::install_github("InfectionMedicineProteomics/mcmc-convergence-graph")
```

The organisation is [https://github.com/InfectionMedicineProteomics](https://github.com/InfectionMedicineProteomics)
and the repository is [https://github.com/InfectionMedicineProteomics/mcmc-convergence-graph](https://github.com/InfectionMedicineProteomics/mcmc-convergence-graph).


## License

MIT
