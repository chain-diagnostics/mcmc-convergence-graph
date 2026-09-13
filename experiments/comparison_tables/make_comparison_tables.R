baseline_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
pairwise_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat"

models <- c(
  "uniform",
  "gaussian_ball",
  "three_cluster",
  "funnel",
  "linear_with_noise",
  "same_slope_diff_intercept",
  "diff_slope_same_intercept",
  "diff_slope_diff_intercept",
  "three_compartment_pk"
)

model_titles <- c(
  uniform                     = "Uniform distribution",
  gaussian_ball               = "Single Gaussian ball",
  three_cluster               = "Three-cluster Gaussian mixture",
  funnel                      = "Neal's funnel",
  linear_with_noise           = "Noisy linear line",
  same_slope_diff_intercept   = "Shared slope",
  diff_slope_same_intercept   = "Shared intercept",
  diff_slope_diff_intercept   = "Free slope and intercept",
  three_compartment_pk        = "Three-compartment PK"
)

rho <- 1.05

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("\\[", "{[}", x)
  x <- gsub("\\]", "{]}", x)
  x
}

format_param_latex <- function(name) {
  paste0("\\texttt{", latex_escape(name), "}")
}

compact_group_token <- function(token) {
  token <- trimws(token)
  # "(1, 2, 3, 4, 5, 6, 7, 8)" -> "(1--8)" when contiguous
  m <- regexec("^\\(([0-9, ]+)\\)$", token)
  g <- regmatches(token, m)[[1]]
  if (length(g) >= 2) {
    nums <- as.integer(strsplit(g[2], ",\\s*")[[1]])
    if (length(nums) >= 3 && all(diff(nums) == 1L)) {
      return(paste0("(", nums[1], "--", nums[length(nums)], ")"))
    }
    return(paste0("(", paste(nums, collapse = ","), ")"))
  }
  token
}

format_group_string <- function(clusters, isolated) {
  parts <- character(0)

  if (nzchar(clusters)) {
    cluster_tokens <- strsplit(clusters, "\\s*\\|\\s*")[[1]]
    parts <- c(parts, vapply(cluster_tokens, compact_group_token, character(1)))
  }

  if (nzchar(isolated)) {
    isolated_split <- strsplit(isolated, ",\\s*")[[1]]
    parts <- c(parts, paste0("\\{", isolated_split, "\\}"))
  }

  paste(parts, collapse = " $|$ ")
}

format_rhat <- function(x) {
  if (is.na(x)) {
    return("-")
  }

  formatted <- formatC(x, format = "f", digits = 3)

  if (x > rho) {
    return(paste0("\\textcolor{red}{", formatted, "}"))
  }

  formatted
}

safe_param_name <- function(name) {
  gsub("[^A-Za-z0-9_]", "_", name)
}

make_one_table <- function(model) {
  cls_path <- file.path(baseline_dir, model, "classical_rhat_summary.csv")
  pw_path  <- file.path(pairwise_dir, model, "mcmc_graph_summary.csv")

  cls <- utils::read.csv(cls_path, row.names = 1, check.names = FALSE)
  pw  <- utils::read.csv(
    pw_path,
    check.names       = FALSE,
    stringsAsFactors  = FALSE,
    colClasses        = c(
      parameter           = "character",
      n_clusters          = "integer",
      n_isolated          = "integer",
      isolated_chains     = "character",
      clusters            = "character",
      pairwise_rhat_value = "numeric"
    )
  )

  params <- pw$parameter

  classical_rhat <- cls[params, "Rhat"]

  df <- data.frame(
    parameter           = params,
    classical_rhat      = classical_rhat,
    pairwise_rhat_value = pw$pairwise_rhat_value,
    n_clusters          = pw$n_clusters,
    n_isolated          = pw$n_isolated,
    chain_groups        = mapply(
      format_group_string,
      pw$clusters,
      pw$isolated_chains,
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )

  df
}

df_to_latex_body <- function(df) {
  lines <- character(nrow(df))

  for (i in seq_len(nrow(df))) {
    lines[i] <- paste0(
      format_param_latex(df$parameter[i]),   " & ",
      format_rhat(df$classical_rhat[i]),     " & ",
      format_rhat(df$pairwise_rhat_value[i])," & ",
      df$n_clusters[i], " & ",
      df$n_isolated[i], " & ",
      df$chain_groups[i], " \\\\"
    )
  }

  paste(lines, collapse = "\n\\hline\n")
}

make_latex_table <- function(model, df) {
  label <- paste0("tab:cmp-", model)

  body <- df_to_latex_body(df)

  paste0(
    "\\begin{table}[H]\n",
    "\\centering\n",
    "\\label{", label, "}\n",
    "\\begin{tabular}{|l|c|c|c|c|l|}\n",
    "\\hline\n",
    "\\multicolumn{6}{|c|}{\\textbf{", model_titles[[model]], "}} \\\\\n",
    "\\hline\n",
    "Parameter & Classical $\\hat{R}$ & Pairwise $\\hat{R}$ & $K$ & $I$ & Chain groups \\\\\n",
    "\\hline\n",
    body, "\n",
    "\\hline\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )
}

combined_tex_parts <- character(0)

for (m in models) {
  df <- make_one_table(m)

  utils::write.csv(
    df,
    file.path(pairwise_dir, m, "comparison_table.csv"),
    row.names = FALSE
  )

  tex <- make_latex_table(m, df)

  writeLines(tex, file.path(pairwise_dir, m, "comparison_table.tex"))

  combined_tex_parts <- c(combined_tex_parts, tex)

  cat("=== ", m, " ===\n", sep = "")
  print(df, row.names = FALSE)
  cat("\n")
}

combined_tex <- paste(combined_tex_parts, collapse = "\n")
writeLines(combined_tex, file.path(pairwise_dir, "comparison_tables.tex"))

cat("wrote:", file.path(pairwise_dir, "comparison_tables.tex"), "\n")

# Combined Level-1 table (controlled test posteriors)
level1_models <- c(
  "uniform",
  "gaussian_ball",
  "linear_with_noise",
  "funnel",
  "three_cluster"
)

level1_blocks <- character(0)

for (m in level1_models) {
  df <- make_one_table(m)
  level1_blocks <- c(
    level1_blocks,
    paste0(
      "\\multicolumn{6}{|c|}{\\textbf{", model_titles[[m]], "}} \\\\\n",
      "\\hline\n",
      df_to_latex_body(df), "\n",
      "\\hline"
    )
  )
}

level1_tex <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{MCMC convergence graph diagnostic on five controlled test posteriors (Level~1).}\n",
  "\\label{tab:cmp-level1}\n",
  "\\small\n",
  "\\setlength{\\tabcolsep}{4pt}\n",
  "% Needs \\usepackage{tabularx}\n",
  "\\begin{tabularx}{\\textwidth}{|l|c|c|c|c|X|}\n",
  "\\hline\n",
  "Parameter & Classical $\\hat{R}$ & Pairwise $\\hat{R}$ & Clusters ($K$) & Isolated ($I$) & Chain groups \\\\\n",
  "\\hline\n",
  paste(level1_blocks, collapse = "\n"), "\n",
  "\\end{tabularx}\n",
  "\\end{table}\n"
)

level1_path <- file.path(pairwise_dir, "level1_comparison_table.tex")
writeLines(level1_tex, level1_path)
cat("wrote:", level1_path, "\n")

# Combined Level-2 table (Gaussian mixture linear regression)
level2_models <- c(
  "same_slope_diff_intercept",
  "diff_slope_same_intercept",
  "diff_slope_diff_intercept"
)

level2_blocks <- character(0)

for (m in level2_models) {
  df <- make_one_table(m)
  level2_blocks <- c(
    level2_blocks,
    paste0(
      "\\multicolumn{6}{|c|}{\\textbf{", model_titles[[m]], "}} \\\\\n",
      "\\hline\n",
      df_to_latex_body(df), "\n",
      "\\hline"
    )
  )
}

level2_tex <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{MCMC convergence graph diagnostic on three Gaussian mixture ",
  "linear regressions (Level~2).}\n",
  "\\label{tab:cmp-level2}\n",
  "\\small\n",
  "\\setlength{\\tabcolsep}{4pt}\n",
  "% Needs \\usepackage{tabularx}\n",
  "\\begin{tabularx}{\\textwidth}{|l|c|c|c|c|X|}\n",
  "\\hline\n",
  "Parameter & Classical $\\hat{R}$ & Pairwise $\\hat{R}$ & ",
  "Clusters ($K$) & Isolated ($I$) & Chain groups \\\\\n",
  "\\hline\n",
  paste(level2_blocks, collapse = "\n"), "\n",
  "\\end{tabularx}\n",
  "\\end{table}\n"
)

level2_path <- file.path(pairwise_dir, "level2_comparison_table.tex")
writeLines(level2_tex, level2_path)
cat("wrote:", level2_path, "\n")

# Combined Level-3 table (systems-biology / PK)
level3_models <- c("three_compartment_pk")

level3_blocks <- character(0)

for (m in level3_models) {
  df <- make_one_table(m)
  level3_blocks <- c(
    level3_blocks,
    paste0(
      "\\multicolumn{6}{|c|}{\\textbf{", model_titles[[m]], "}} \\\\\n",
      "\\hline\n",
      df_to_latex_body(df), "\n",
      "\\hline"
    )
  )
}

level3_tex <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{MCMC convergence graph diagnostic on the three-compartment ",
  "pharmacokinetic model (Level~3).}\n",
  "\\label{tab:cmp-level3}\n",
  "\\small\n",
  "\\setlength{\\tabcolsep}{4pt}\n",
  "% Needs \\usepackage{tabularx}\n",
  "\\begin{tabularx}{\\textwidth}{|l|c|c|c|c|X|}\n",
  "\\hline\n",
  "Parameter & Classical $\\hat{R}$ & Pairwise $\\hat{R}$ & ",
  "Clusters ($K$) & Isolated ($I$) & Chain groups \\\\\n",
  "\\hline\n",
  paste(level3_blocks, collapse = "\n"), "\n",
  "\\end{tabularx}\n",
  "\\end{table}\n"
)

level3_path <- file.path(pairwise_dir, "level3_comparison_table.tex")
writeLines(level3_tex, level3_path)
cat("wrote:", level3_path, "\n")

