#' Summarize clusters in a pairwise R-hat graph
#'
#' Finds connected components in a pairwise R-hat graph adn separates them into
#' muti-chain clusters and isolated chains. In this diagnostic, single chain
#' components are counted as isolated chains rather than as clusters.
#'
#' @param graph An 'igraph' object
#'
#' @returns A list containing multi-chain clusters. the number of multi-chain
#' clusters, isolated chains, the number of isolated chains, all connected
#' components, the total number of connected components, and the component
#' membership vector.
#'
#' @export


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
