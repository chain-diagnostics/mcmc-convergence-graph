pairwise_rhat_clusters <- function(graph) {
  components <- igraph::components(graph)
  membership <- components$membership
  chain_names <- igraph::V(graph)$name
  clusters <- split(
    chain_names,
    membership
  )
  clusters <- unname(clusters)
  isolated_chains <- clusters[
    lengths(clusters) == 1
  ]
  isolated_chains <- unlist(
    isolated_chains,
    use.names = FALSE
  )
  list(
    clusters = clusters,
    n_clusters = length(clusters),
    isolated_chains = isolated_chains,
    n_isolated = length(isolated_chains),
    membership = membership
  )
}
