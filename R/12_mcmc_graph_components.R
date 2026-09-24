#' Summarize connected components in a pairwise R-hat graph
#'
#' Finds connected components in a pairwise R-hat graph and separates them into
#' the multi-chain components \eqn{\mathcal{Q}} and the isolated chains
#' \eqn{\mathcal{I}}. Single-chain components are counted in \eqn{I}, not in
#' \eqn{K}.
#'
#' @param graph An `igraph` object.
#'
#' @return A list with \eqn{\mathcal{Q}} (`Q`), \eqn{K = |\mathcal{Q}|} (`K`),
#'   isolated chain identifiers (`isolated_chains`), \eqn{I = |\mathcal{I}|}
#'   (`I`), all connected components, and the component membership vector.
#'
#' @export
#'
#' @examples
#' graph <- igraph::make_graph(
#'   edges = c(
#'     "chain1", "chain2",
#'     "chain3", "chain4"
#'   ),
#'   directed = FALSE
#' )
#'
#' mcmc_graph_components(graph)

#takes one igraph and returns K, I and sets of chains
#input can be G_rho,s, G_union, G_intersection
mcmc_graph_components <- function(graph) {
  #finds who can reach whom by walking edges, output is a list:membership, csize:size of each component, no:how many component
  components <- igraph::components(graph)
  #named integers vector, one per node, e.g.chain1->component1
  membership <- components$membership

  chain_names <- igraph::V(graph)$name
  #cuts the name vector into groups that share the same membership id
  #e.g.list("1" = c("1","3","4"), "2" = "2"). one vector per component
  all_components <- split(
    chain_names,
    membership
  )
  #drop the list name
  all_components <- unname(all_components)
  #keep group with more than one chain
  multi_chain_clusters <- all_components[
    lengths(all_components) > 1
  ]
  #output: cahracter vector of isolated chain names.
  isolated_chains <- unlist(
    all_components[lengths(all_components) == 1],
    use.names = FALSE
  )
  #pack everything into the list above and return it
  list(
    Q = multi_chain_clusters,
    K = length(multi_chain_clusters),
    isolated_chains = isolated_chains,
    I = length(isolated_chains),
    all_components = all_components,
    membership = membership
  )
}


