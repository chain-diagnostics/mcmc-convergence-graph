library(rstan)
library(mcmcConvergenceGraph)
library(ggplot2)

fit_path <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_compartment_pk_infer_vc/three_compartment_pk_infer_vc.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/three_compartment_pk"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)
arr <- as.array(fit)

pars <- c("k12", "k13", "k21", "k31", "k10", "VC", "sigma_add", "sigma_prop")

# Groups from G_intersection on this RDS. Colour is attached to the
# chain set, not to the left or right peak of any parameter.
res <- mcmc_graph_summary(
  draws      = fit,
  parameters = c("k10", "k12", "k21", "k13", "k31", "VC"),
  rho        = 1.05,
  save_csv   = FALSE
)

ids_from <- function(names) as.integer(sub(".*:", "", names))
group_ids <- lapply(res$intersection_clusters$all_components, ids_from)
group_ids <- group_ids[order(vapply(group_ids, min, integer(1)))]

group_labels <- vapply(
  group_ids,
  function(ids) paste0("(", paste(ids, collapse = ", "), ")"),
  character(1)
)

component_colours <- c("#f1a340", "#998ec3")
names(component_colours) <- group_labels

n_iter <- dim(arr)[1]
chain_group <- integer(dim(arr)[2])
for (g in seq_along(group_ids)) {
  chain_group[group_ids[[g]]] <- g
}

cat("orange:", group_labels[1], "\n")
cat("purple:", group_labels[2], "\n")

param_labels <- c(
  k12        = "k[12]",
  k13        = "k[13]",
  k21        = "k[21]",
  k31        = "k[31]",
  k10        = "k[10]",
  VC         = "V[C]",
  sigma_add  = "sigma[add]",
  sigma_prop = "sigma[prop]"
)

for (par in pars) {
  plot_data <- data.frame(
    value = as.vector(arr[, , par]),
    group = factor(
      group_labels[rep(chain_group, each = n_iter)],
      levels = group_labels
    )
  )

  p <- ggplot(plot_data, aes(x = value, colour = group, fill = group)) +
    geom_density(alpha = 0.45, linewidth = 0.7, adjust = 1.1) +
    scale_colour_manual(values = component_colours, name = "Chain group") +
    scale_fill_manual(values = component_colours, name = "Chain group") +
    labs(x = NULL, y = "Density", title = parse(text = param_labels[[par]])) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(output_dir, paste0("marginal_", par, ".pdf")),
    plot     = p,
    width    = 5,
    height   = 4
  )
}
