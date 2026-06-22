pairwise_rhat_clusters <- function(graph) {
  components <- igraph::components(graph)

  membership <- components$membership

  chain_names <- igraph::V(graph)$name

  all_components <- split(
    chain_names,
    membership
  )

  all_components <- unname(all_components)

  multi_chain_clusters <- all_components[
    lengths(all_components) > 1
  ]

  isolated_chains <- unlist(
    all_components[lengths(all_components) == 1],
    use.names = FALSE
  )

  list(
    clusters = multi_chain_clusters,
    n_clusters = length(multi_chain_clusters),
    isolated_chains = isolated_chains,
    n_isolated = length(isolated_chains),
    all_components = all_components,
    n_components = length(all_components),
    membership = membership
  )
}
