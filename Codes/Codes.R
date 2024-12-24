#### LIBS ----
library(eurostat)
library(tidyverse)
library(zoo)
library(scales)
library(tseries)
library(urca)
library(fracdiff)
library(arfima)
library(gt)

#### DATA----
# Define groups of countries
nms8 <- c("CZ", "EE", "HU", "LV", "LT", "PL", "SK", "SI")
nms12 <- c(nms8, "BG", "RO", "CY", "MT")
se4 <- c("EL", "PT", "ES", "IT")
eu15 <- c("AT", "BE", "DK", "FI", "FR", "DE", "EL", "IE", "IT", "LU", "NL", "PT", "ES", "SE", "UK")
croatia <- "HR"

# Get seasonally adjusted quarterly GDP data for European countries from 2000 onwards
gdp_quarterly <- get_eurostat("namq_10_gdp", time_format = "date")

# Filter the dataset for GDP (current prices, million euros, seasonally adjusted) and from 2000 onwards
gdp_quarterly <- gdp_quarterly %>%
  filter(na_item == "B1GQ", unit == "CP_MEUR", s_adj == "SCA", TIME_PERIOD >= as.Date("2000-01-01"))

# Select relevant columns and convert to a more readable format
gdp_quarterly <- gdp_quarterly %>%
  select(geo, TIME_PERIOD, values) %>%
  rename(country = geo, date = TIME_PERIOD, gdp = values)

# Get annual population data for European countries
pop_data <- get_eurostat("demo_r_pjanaggr3", time_format = "date")

# Filter population data to include only the necessary columns and years from 2000 onwards
pop_data <- pop_data %>%
  filter(geo %in% c(nms8, nms12, se4, eu15, croatia), sex == "T", age == "TOTAL", TIME_PERIOD >= as.Date("2000-01-01")) %>%
  select(geo, TIME_PERIOD, values) %>%
  rename(country = geo, year = TIME_PERIOD, population = values)

# Define a function to interpolate annual population data to quarterly frequency
interpolate_population <- function(pop_data) {
  pop_data %>%
    group_by(country) %>%
    complete(year = seq(min(year), max(year), by = "year")) %>%
    mutate(population = zoo::na.approx(population, na.rm = FALSE)) %>%
    mutate(year_quarter = as.yearqtr(year) + seq(0, 0.75, by = 0.25)) %>%
    complete(year_quarter = seq(min(year_quarter), max(year_quarter), by = 0.25)) %>%
    mutate(population = zoo::na.spline(population, na.rm = FALSE)) %>%
    select(country, year_quarter, population)
}

# Interpolate the population data to quarterly frequency
pop_data_quarterly <- interpolate_population(pop_data)

# Merge GDP data with interpolated population data
merge_gdp_pop <- function(gdp_data, pop_data_quarterly) {
  gdp_data %>%
    mutate(year_quarter = as.yearqtr(date)) %>%
    left_join(pop_data_quarterly, by = c("country" = "country", "year_quarter" = "year_quarter")) %>%
    mutate(gdp_per_capita = gdp * 1e6 / population,   # Convert GDP to euros
           log_gdp_per_capita = log(gdp_per_capita)) %>%  # Calculate log GDP per capita
    select(country, date, log_gdp_per_capita)
}

# Filter and merge data for each group and Croatia
gdp_croatia <- gdp_quarterly %>% filter(country == croatia)
gdp_croatia <- merge_gdp_pop(gdp_croatia, pop_data_quarterly)

gdp_nms8 <- gdp_quarterly %>% filter(country %in% nms8)
gdp_nms8 <- merge_gdp_pop(gdp_nms8, pop_data_quarterly) %>% group_by(date) %>% summarise(log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE))

gdp_nms12 <- gdp_quarterly %>% filter(country %in% nms12)
gdp_nms12 <- merge_gdp_pop(gdp_nms12, pop_data_quarterly) %>% group_by(date) %>% summarise(log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE))

gdp_se4 <- gdp_quarterly %>% filter(country %in% se4)
gdp_se4 <- merge_gdp_pop(gdp_se4, pop_data_quarterly) %>% group_by(date) %>% summarise(log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE))

gdp_eu15 <- gdp_quarterly %>% filter(country %in% eu15)
gdp_eu15 <- merge_gdp_pop(gdp_eu15, pop_data_quarterly) %>% group_by(date) %>% summarise(log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE))

# Add group names to each data frame before calculating differences
gdp_nms8 <- gdp_nms8 %>% mutate(group = "NMS8")
gdp_nms12 <- gdp_nms12 %>% mutate(group = "NMS12")
gdp_se4 <- gdp_se4 %>% mutate(group = "SE4")
gdp_eu15 <- gdp_eu15 %>% mutate(group = "EU15")

# Calculate the differences between Croatia's log GDP per capita and the groups
calculate_differences <- function(croatia_data, group_data) {
  croatia_data <- croatia_data %>% select(date, log_gdp_per_capita) %>% rename(log_gdp_per_capita_croatia = log_gdp_per_capita)
  combined_data <- left_join(group_data, croatia_data, by = "date")
  combined_data %>%
    mutate(diff_log_gdp_per_capita = log_gdp_per_capita - log_gdp_per_capita_croatia) %>%
    select(date, group, diff_log_gdp_per_capita)
}

diff_croatia_vs_nms8 <- calculate_differences(gdp_croatia, gdp_nms8)
diff_croatia_vs_nms12 <- calculate_differences(gdp_croatia, gdp_nms12)
diff_croatia_vs_se4 <- calculate_differences(gdp_croatia, gdp_se4)
diff_croatia_vs_eu15 <- calculate_differences(gdp_croatia, gdp_eu15)

# Combine data for differences
combine_for_plot_diff <- function(...) {
  combined <- bind_rows(...)
  return(combined)
}

diff_croatia_vs_groups <- combine_for_plot_diff(diff_croatia_vs_nms8, diff_croatia_vs_nms12, diff_croatia_vs_se4, diff_croatia_vs_eu15)

# Create a combined data frame for plotting
combine_for_plot <- function(df1, df2, group_name) {
  df1 <- df1 %>% mutate(group = "Croatia")
  df2 <- df2 %>% mutate(group = group_name)
  combined <- bind_rows(df1, df2)
  return(combined)
}

croatia_vs_nms8 <- combine_for_plot(gdp_croatia, gdp_nms8, "NMS8")
croatia_vs_nms12 <- combine_for_plot(gdp_croatia, gdp_nms12, "NMS12")
croatia_vs_se4 <- combine_for_plot(gdp_croatia, gdp_se4, "SE4")
croatia_vs_eu15 <- combine_for_plot(gdp_croatia, gdp_eu15, "EU15")

#### VIZUALIZATION ----

# Define a custom date labeling function
quarterly_labels <- function(x) {
  format(x, "%Yq%q")
}

# Create separate plots using ggplot2
plot_comparison <- function(data, title) {
  data <- data %>% filter(format(date, "%Y") < "2024")
  quarter_breaks <- seq.Date(from = min(data$date), to = max(data$date), by = "15 months")
  
  ggplot(data, aes(x = date, y = log_gdp_per_capita, color = group, linetype = group)) +
    geom_line(size = 1.2) +  # Adjust the size parameter for thicker lines
    labs(title = title, x = "Datum", y = "Log BDP per capita") +
    theme_minimal() +
    scale_color_manual(values = c("Croatia" = "black", 
                                  "NMS8" = "gray20", 
                                  "NMS12" = "gray40", 
                                  "SE4" = "gray60", 
                                  "EU15" = "gray80")) +
    scale_linetype_manual(values = c("Croatia" = "solid", 
                                     "NMS8" = "dashed", 
                                     "NMS12" = "dotted", 
                                     "SE4" = "dotdash", 
                                     "EU15" = "longdash")) +
    scale_x_date(breaks = quarter_breaks,  # Use quarter breaks
                 labels = function(x) paste0(format(x, "%Y"), "q", as.numeric(format(x, "%m")) %/% 3 + 1),
                 expand = expansion(mult = c(0, 0.05))) +  # Add a small expansion to the right
    coord_cartesian(xlim = c(min(data$date), max(data$date) + 15)) +  # Ensure the last quarter is shown by adding buffer
    geom_vline(xintercept = as.numeric(quarter_breaks), color = "gray80", linetype = "solid") +  # Add vertical lines
    theme(legend.position = "bottom") +  # Place legend at the bottom
    guides(color = guide_legend(title = NULL), linetype = guide_legend(title = NULL))  # Remove legend titles
}

# Plot for Croatia vs NMS8
plot_nms8 <- plot_comparison(croatia_vs_nms8, "")

# Plot for Croatia vs NMS12
plot_nms12 <- plot_comparison(croatia_vs_nms12, "")

# Plot for Croatia vs SE4
plot_se4 <- plot_comparison(croatia_vs_se4, "")

# Plot for Croatia vs EU15
plot_eu15 <- plot_comparison(croatia_vs_eu15, "")

# Display the plots
print(plot_nms8)
print(plot_nms12)
print(plot_se4)
print(plot_eu15)

# Visualize the differences in log GDP per capita
plot_diff_comparison <- function(data, title) {
  data <- data %>% filter(format(date, "%Y") < "2024")
  quarter_breaks <- seq.Date(from = min(data$date), to = max(data$date), by = "15 months")
  
  ggplot(data, aes(x = date, y = diff_log_gdp_per_capita, color = group, linetype = group)) +
    geom_line(size = 1.2) +
    labs(title = title, x = "Date", y = "Razlika (log) BDP per capita") +
    theme_minimal() +
    scale_color_manual(values = c("NMS8" = "gray20", 
                                  "NMS12" = "gray40", 
                                  "SE4" = "gray60", 
                                  "EU15" = "gray80")) +
    scale_linetype_manual(values = c("NMS8" = "dashed", 
                                     "NMS12" = "dotted", 
                                     "SE4" = "dotdash", 
                                     "EU15" = "longdash")) +
    scale_x_date(breaks = quarter_breaks,  # Use quarter breaks
                 labels = function(x) paste0(format(x, "%Y"), "q", as.numeric(format(x, "%m")) %/% 3 + 1),
                 expand = expansion(mult = c(0, 0.05))) +  # Add a small expansion to the right
    coord_cartesian(xlim = c(min(data$date), max(data$date) + 15)) +  # Ensure the last quarter is shown by adding buffer
    geom_vline(xintercept = as.numeric(quarter_breaks), color = "gray80", linetype = "solid") +  # Add vertical lines
    theme(legend.position = "none")  # Format labels as 2000 q1
}

# Plot for differences
plot_diff_nms8 <- plot_diff_comparison(diff_croatia_vs_nms8, "")
plot_diff_nms12 <- plot_diff_comparison(diff_croatia_vs_nms12, "")
plot_diff_se4 <- plot_diff_comparison(diff_croatia_vs_se4, "")
plot_diff_eu15 <- plot_diff_comparison(diff_croatia_vs_eu15, "")

# Display the plots
print(plot_diff_nms8)
print(plot_diff_nms12)
print(plot_diff_se4)
print(plot_diff_eu15)

# Combine data for comparisons
combine_for_plot <- function(...) {
  combined <- bind_rows(...)
  return(combined)
}

croatia_vs_nms8 <- combine_for_plot(gdp_croatia %>% mutate(group = "Croatia"), gdp_nms8 %>% mutate(group = "NMS8"))
croatia_vs_nms12 <- combine_for_plot(gdp_croatia %>% mutate(group = "Croatia"), gdp_nms12 %>% mutate(group = "NMS12"))
croatia_vs_se4 <- combine_for_plot(gdp_croatia %>% mutate(group = "Croatia"), gdp_se4 %>% mutate(group = "SE4"))
croatia_vs_eu15 <- combine_for_plot(gdp_croatia %>% mutate(group = "Croatia"), gdp_eu15 %>% mutate(group = "EU15"))

# Combine data for differences
combine_for_plot_diff <- function(...) {
  combined <- bind_rows(...)
  return(combined)
}

diff_croatia_vs_groups <- combine_for_plot_diff(diff_croatia_vs_nms8, diff_croatia_vs_nms12, diff_croatia_vs_se4, diff_croatia_vs_eu15)

# Add a column to indicate the type of data (comparison or difference)
croatia_vs_nms8 <- croatia_vs_nms8 %>% mutate(type = "Usporedba", group_name = "NMS8")
croatia_vs_nms12 <- croatia_vs_nms12 %>% mutate(type = "Usporedba", group_name = "NMS12")
croatia_vs_se4 <- croatia_vs_se4 %>% mutate(type = "Usporedba", group_name = "SE4")
croatia_vs_eu15 <- croatia_vs_eu15 %>% mutate(type = "Usporedba", group_name = "EU15")

diff_croatia_vs_nms8 <- diff_croatia_vs_nms8 %>% mutate(type = "Razlika", group_name = "NMS8")
diff_croatia_vs_nms12 <- diff_croatia_vs_nms12 %>% mutate(type = "Razlika", group_name = "NMS12")
diff_croatia_vs_se4 <- diff_croatia_vs_se4 %>% mutate(type = "Razlika", group_name = "SE4")
diff_croatia_vs_eu15 <- diff_croatia_vs_eu15 %>% mutate(type = "Razlika", group_name = "EU15")

# Combine all data
all_data <- bind_rows(croatia_vs_nms8, croatia_vs_nms12, croatia_vs_se4, croatia_vs_eu15,
                      diff_croatia_vs_nms8, diff_croatia_vs_nms12, diff_croatia_vs_se4, diff_croatia_vs_eu15)

plot_combined <- function(data) {
  quarter_breaks <- seq.Date(from = min(data$date), to = max(data$date), by = "15 months")
  
  ggplot(data, aes(x = date, y = ifelse(type == "Usporedba", log_gdp_per_capita, diff_log_gdp_per_capita), color = group, linetype = group)) +
    geom_line(size = 1.2) +
    labs(title = "", x = "Datum", y = "(log) BDP per capita", color = "GEO", linetype = "GEO") +
    theme_minimal() +
    scale_color_manual(values = c("Croatia" = "black", 
                                  "NMS8" = "gray20", 
                                  "NMS12" = "gray40", 
                                  "SE4" = "gray60", 
                                  "EU15" = "gray80")) +
    scale_linetype_manual(values = c("Croatia" = "solid", 
                                     "NMS8" = "dashed", 
                                     "NMS12" = "dotted", 
                                     "SE4" = "dotdash", 
                                     "EU15" = "longdash")) +
    scale_x_date(breaks = quarter_breaks, 
                 labels = function(x) paste0(format(x, "%Y"), "q", as.numeric(format(x, "%m")) %/% 3 + 1),
                 expand = expansion(mult = c(0, 0.05))) +  
    coord_cartesian(xlim = c(min(data$date), max(data$date) + 15)) +  
    geom_vline(xintercept = as.numeric(quarter_breaks), color = "gray80", linetype = "solid") +  
    theme(legend.position = "bottom", 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  
    guides(color = guide_legend(title = "GEO:"), linetype = guide_legend(title = "GEO:")) +  
    facet_wrap(~group_name + type, scales = "free_y", ncol = 2, 
               labeller = labeller(group_name = function(labels) paste0("GEO: ", labels), 
                                   type = function(labels) paste(labels)))
}

# Plot the combined data
plot_combined_graph <- plot_combined(all_data)

# Print the plot
print(plot_combined_graph)

#### ESTIMATION----





# Function to compute Ljung-Box test p-values for every second lag
compute_ljung_box_every_second_lag <- function(data, max_lag = 20) {
  results <- list()
  for (lag in seq(2, max_lag, by = 2)) {  # Loop through every second lag
    p_value <- Box.test(data$diff_log_gdp_per_capita, lag = lag, type = "Ljung-Box")$statistic
    results[[lag]] <- tibble(lag = lag, p_value = p_value)
  }
  bind_rows(results) %>% mutate(group = data$group[1])
}

# Apply the function to the data
ljung_box_results_second_lags <- diff_croatia_vs_groups %>%
  group_by(group) %>%
  do(compute_ljung_box_every_second_lag(., max_lag = 20)) %>%
  ungroup()

# Pivot the results for a nicer table format
ljung_box_table_second_lags <- ljung_box_results_second_lags %>%
  pivot_wider(names_from = lag, values_from = p_value, names_prefix = "lag_") %>%
  arrange(group)

# Display the results as a table
print(ljung_box_table_second_lags)

# Create a nice table visualization using gt
ljung_box_gt_second_lags <- ljung_box_table_second_lags %>%
  gt() %>%
  tab_header(
    title = "Ljung-Box Test P-values for Every Second Lag",
    subtitle = "P-values for every second lag up to 20 for each group"
  ) %>%
  cols_label(
    group = "Group",
    lag_2 = "Lag 2", lag_4 = "Lag 4", lag_6 = "Lag 6", lag_8 = "Lag 8", lag_10 = "Lag 10",
    lag_12 = "Lag 12", lag_14 = "Lag 14", lag_16 = "Lag 16", lag_18 = "Lag 18", lag_20 = "Lag 20"
  ) %>%
  fmt_number(
    columns = starts_with("lag_"),
    decimals = 3
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12,
    column_labels.font.size = 12
  )

# Print the gt table
print(ljung_box_gt_second_lags)


# Plot ACF values as bar charts with facets
acf_facet_plot <- ggplot(acf_results, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "black", color = "white") +
  labs(title = "ACF dijagram", x = "Lag", y = "ACF") +
  theme_minimal() +
  facet_wrap(~group, scales = "free_y")

# Print the ACF facet plot
print(acf_facet_plot)


# Calculate the differences between Croatia's log GDP per capita and the groups
calculate_differences <- function(croatia_data, group_data) {
  croatia_data <- croatia_data %>% select(date, log_gdp_per_capita) %>% rename(log_gdp_per_capita_croatia = log_gdp_per_capita)
  combined_data <- left_join(group_data, croatia_data, by = "date")
  combined_data %>%
    mutate(diff_log_gdp_per_capita = log_gdp_per_capita - log_gdp_per_capita_croatia) %>%
    select(date, group, diff_log_gdp_per_capita) %>%
    filter(!is.na(diff_log_gdp_per_capita))
}

diff_croatia_vs_nms8 <- calculate_differences(gdp_croatia, gdp_nms8)
diff_croatia_vs_nms12 <- calculate_differences(gdp_croatia, gdp_nms12)
diff_croatia_vs_se4 <- calculate_differences(gdp_croatia, gdp_se4)
diff_croatia_vs_eu15 <- calculate_differences(gdp_croatia, gdp_eu15)

# Perform the ADF test under three different specifications
adf_test_three_specifications <- function(data, group_name) {
  no_const <- ur.df(data$diff_log_gdp_per_capita, type = "none", selectlags = "AIC")@teststat[1]
  const <- ur.df(data$diff_log_gdp_per_capita, type = "drift", selectlags = "AIC")@teststat[1]
  const_trend <- ur.df(data$diff_log_gdp_per_capita, type = "trend", selectlags = "AIC")@teststat[1]
  
  tibble(
    group_name = group_name,
    no_const = no_const,
    const = const,
    const_trend = const_trend
  )
}

adf_croatia_vs_nms8 <- adf_test_three_specifications(diff_croatia_vs_nms8, "RH-NMS8")
adf_croatia_vs_nms12 <- adf_test_three_specifications(diff_croatia_vs_nms12, "RH-NMS12")
adf_croatia_vs_se4 <- adf_test_three_specifications(diff_croatia_vs_se4, "RH-SE4")
adf_croatia_vs_eu15 <- adf_test_three_specifications(diff_croatia_vs_eu15, "RH-EU15")

# Combine ADF test results
adf_results <- bind_rows(adf_croatia_vs_nms8, adf_croatia_vs_nms12, adf_croatia_vs_se4, adf_croatia_vs_eu15)

# Print the ADF test results in the required format
adf_results <- adf_results %>%
  select(group_name, no_const, const, const_trend) %>%
  rename(
    "Bez kons/tr" = no_const,
    "Konst" = const,
    "Konst/tr" = const_trend
  )

# Display the ADF test results table
print(adf_results)


# Perform fractional integration tests: Robinson's estimator, GPH estimator, P-GPH estimator


# Robinson's estimator
apply_robinson_estimator <- function(data, group_name) {
  result <- arfima::arfima(data$diff_log_gdp_per_capita)
  
  # Extract the fractional differencing parameter (dfrac)
  d.f <- result$modes[[1]]$dfrac
  se.d.f <- result$modes[[1]]$se[1]  # Assuming the standard error for dfrac is the first element in the se vector
  
  # Check if the standard error is available
  if (is.null(se.d.f)) {
    se.d.f <- NA
    p.value <- NA
  } else {
    # Calculate the t-statistic for d.f
    t.stat <- d.f / se.d.f
    
    # Calculate the p-value for the t-statistic (two-tailed test)
    p.value <- 2 * pt(-abs(t.stat), df = length(data$diff_log_gdp_per_capita) - 1)
  }
  
  tibble(
    group_name = group_name,
    robinson_d = d.f,
    p_value = p.value
  )
}

# Calculate differences for each group
diff_croatia_vs_nms8 <- calculate_differences(gdp_croatia, gdp_nms8)
diff_croatia_vs_nms12 <- calculate_differences(gdp_croatia, gdp_nms12)
diff_croatia_vs_se4 <- calculate_differences(gdp_croatia, gdp_se4)
diff_croatia_vs_eu15 <- calculate_differences(gdp_croatia, gdp_eu15)

# Apply Robinson estimator to each group
robinson_croatia_vs_nms8 <- apply_robinson_estimator(diff_croatia_vs_nms8, "Croatia vs NMS8")
robinson_croatia_vs_nms12 <- apply_robinson_estimator(diff_croatia_vs_nms12, "Croatia vs NMS12")
robinson_croatia_vs_se4 <- apply_robinson_estimator(diff_croatia_vs_se4, "Croatia vs SE4")
robinson_croatia_vs_eu15 <- apply_robinson_estimator(diff_croatia_vs_eu15, "Croatia vs EU15")

# Combine Robinson test results
robinson_results <- bind_rows(robinson_croatia_vs_nms8, robinson_croatia_vs_nms12, robinson_croatia_vs_se4, robinson_croatia_vs_eu15)

# Print the Robinson test results
print(robinson_results)

# GPH estimator
gph_test_results <- function(data, group_name) {
  result <- fracdiff::fdGPH(data$diff_log_gdp_per_capita)
  #tibble(
  #  group_name = group_name,
  #  gph_d = result$d
  #)
}

gph_croatia_vs_nms8 <- gph_test_results(diff_croatia_vs_nms8, "Croatia vs NMS8")
gph_croatia_vs_nms12 <- gph_test_results(diff_croatia_vs_nms12, "Croatia vs NMS12")
gph_croatia_vs_se4 <- gph_test_results(diff_croatia_vs_se4, "Croatia vs SE4")
gph_croatia_vs_eu15 <- gph_test_results(diff_croatia_vs_eu15, "Croatia vs EU15")

# Combine GPH test results
gph_results <- bind_rows(gph_croatia_vs_nms8, gph_croatia_vs_nms12, gph_croatia_vs_se4, gph_croatia_vs_eu15)

# Print the GPH test results
print(gph_results)

# P-GPH estimator
pgph_test_results <- function(data, group_name) {
  result <- urca::ur.pp(data$diff_log_gdp_per_capita, type = "Z-alpha", model = "trend")
  
}

pgph_croatia_vs_nms8 <- pgph_test_results(diff_croatia_vs_nms8, "Croatia vs NMS8")
pgph_croatia_vs_nms12 <- pgph_test_results(diff_croatia_vs_nms12, "Croatia vs NMS12")
pgph_croatia_vs_se4 <- pgph_test_results(diff_croatia_vs_se4, "Croatia vs SE4")
pgph_croatia_vs_eu15 <- pgph_test_results(diff_croatia_vs_eu15, "Croatia vs EU15")

# Combine P-GPH test results
pgph_results <- bind_rows(pgph_croatia_vs_nms8, pgph_croatia_vs_nms12, pgph_croatia_vs_se4, pgph_croatia_vs_eu15)

# Print the P-GPH test results
print(pgph_results)

# Combine all results into a single data frame for easy comparison
all_results <- bind_rows(
  robinson_results %>% mutate(test = "Robinson"),
  gph_results %>% mutate(test = "GPH"),
  pgph_results %>% mutate(test = "P-GPH")
)

# Print all results
print(all_results)


