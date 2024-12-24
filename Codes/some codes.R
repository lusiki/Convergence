

# Function to add significance stars based on p-value
add_significance_stars <- function(p_value) {
  if (p_value <= 0.001) {
    return("***")
  } else if (p_value <= 0.01) {
    return("**")
  } else if (p_value <= 0.05) {
    return("*")
  } else if (p_value <= 0.1) {
    return(".")
  } else {
    return("")
  }
}

# Function to apply different estimators (fdGPH, fdSperio) and calculate significance
gph_test_results <- function(data, group_name, bandwidth, estimator) {
  if (estimator == "fdGPH") {
    # Apply the GPH estimator with a specified bandwidth
    result <- fracdiff::fdGPH(data$diff_log_gdp_per_capita, bandw.exp = bandwidth)
    d_value <- result$d
  } else if (estimator == "fdSperio") {
    # Apply the Sperio estimator
    result <- fracdiff::fdSperio(data$diff_log_gdp_per_capita)
    d_value <- result$d
  }
  
  # Calculate the number of observations (n)
  n <- length(data$diff_log_gdp_per_capita)
  
  # Asymptotic standard error of the estimated d value
  se_d <- 1 / sqrt(n)
  
  # Z-score and p-value for significance testing
  z_score <- d_value / se_d
  p_value <- 2 * (1 - pnorm(abs(z_score)))  # Two-tailed p-value
  
  # Add significance stars
  significance <- add_significance_stars(p_value)
  
  # Return a tibble with group name, estimated d value, significance, and bandwidth
  tibble(
    group_name = group_name,
    gph_d = paste0(round(d_value, 4), significance),  # Append significance stars to d
    bandwidth = bandwidth
  )
}

# Define bandwidths and methods
bandwidths <- c(0.4 , 0.5, 0.6, 0.7, 0.8, 0.9)
methods <- c("fdGPH", "fdSperio")

# Apply the test to different groups and bandwidths

# GPH Estimator (fdGPH)
gph_croatia_vs_nms8_fdGPH <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_nms8, "Croatia vs NMS8", .x, "fdGPH"))
gph_croatia_vs_nms12_fdGPH <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_nms12, "Croatia vs NMS12", .x, "fdGPH"))
gph_croatia_vs_se4_fdGPH <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_se4, "Croatia vs SE4", .x, "fdGPH"))
gph_croatia_vs_eu15_fdGPH <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_eu15, "Croatia vs EU15", .x, "fdGPH"))

gph_results_long_fdGPH <- bind_rows(gph_croatia_vs_nms8_fdGPH, gph_croatia_vs_nms12_fdGPH, gph_croatia_vs_se4_fdGPH, gph_croatia_vs_eu15_fdGPH)
gph_results_wide_fdGPH <- gph_results_long_fdGPH %>%
  tidyr::pivot_wider(names_from = bandwidth, values_from = gph_d, names_prefix = "Bandwidth_")

# Sperio Estimator (fdSperio)
gph_croatia_vs_nms8_fdSperio <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_nms8, "Croatia vs NMS8", .x, "fdSperio"))
gph_croatia_vs_nms12_fdSperio <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_nms12, "Croatia vs NMS12", .x, "fdSperio"))
gph_croatia_vs_se4_fdSperio <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_se4, "Croatia vs SE4", .x, "fdSperio"))
gph_croatia_vs_eu15_fdSperio <- purrr::map_df(bandwidths, ~ gph_test_results(diff_croatia_vs_eu15, "Croatia vs EU15", .x, "fdSperio"))

gph_results_long_fdSperio <- bind_rows(gph_croatia_vs_nms8_fdSperio, gph_croatia_vs_nms12_fdSperio, gph_croatia_vs_se4_fdSperio, gph_croatia_vs_eu15_fdSperio)
gph_results_wide_fdSperio <- gph_results_long_fdSperio %>%
  tidyr::pivot_wider(names_from = bandwidth, values_from = gph_d, names_prefix = "Bandwidth_")

# Create and display tables for each estimator

# GPH Estimator Table
kable(
  gph_results_wide_fdGPH, 
  caption = "GPH Estimator (fdGPH) Results for Income Convergence by Bandwidth with Significance",
  col.names = c("Group Name", paste0("GPH d (", bandwidths, ")")),
  digits = 4,
  align = 'c'
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Estimation Results by Bandwidth" = length(bandwidths)))

# Sperio Estimator Table
kable(
  gph_results_wide_fdSperio, 
  caption = "GPH Estimator (fdSperio) Results for Income Convergence by Bandwidth with Significance",
  col.names = c("Group Name", paste0("GPH d (", bandwidths, ")")),
  digits = 4,
  align = 'c'
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Estimation Results by Bandwidth" = length(bandwidths)))
