# Load Matilda
# remotes::install_github("jk-brown/matilda")
library(matilda)
library(ggplot2)
library(dplyr)
library(tidyr)

# Configure Hector core
ini <- system.file("input/hector_ssp245.ini", package = "hector")
c_ssp245 <- newcore(ini, name = "SSP 2-4.5")

# Generate parameter values and remove ECS
set.seed(1)
param_values <- generate_params(c_ssp245, draws = 500)
param_values$ECS = NULL

# Parameter histograms
values_long <- param_values %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "Column", 
    values_to = "Value"
  )
ggplot(values_long, aes(x = Value, fill = Column)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(x = "Value", y = "Density") +
  facet_wrap(~ Column, scales = "free", ncol = 1) +
  ggtitle("Density Curves for Each Column") +
  guides(fill = FALSE) +
  scale_y_continuous(n.breaks = 3)

# Define the different evidence scenarios
evidence_scenarios <- c("Baseline" = 3.2, "No Process" = 3.3, "UL + EC" = 3.4)

# Store results for each evidence scenario
results_list <- list()
probs_list <- list()
start <- Sys.time()
for (scenario in names(evidence_scenarios)) {
  # Scenario name for storage
  scenario_name <- paste(scenario, evidence_scenarios[scenario], sep = ", ECS = ")
  
  # Replace the ECS column with the static mean value
  param_values$ECS <- evidence_scenarios[scenario]
  
  # Running Matilda analysis:
  
  # Running Hector iteratively
  results <-iterate_hector(core = c_ssp245,
                           params = param_values)
  
  # Screening Hector results using score_ramp algorithm
  co2_sd <- sd(matilda:::metricdata_co2$co2_ppm) # Standard deviation of observed co2 data
  scores <- score_hruns(results, criterion_co2_obs(), score_ramp, w1 = 0, w2 = co2_sd)
  results_scored <- merge(results, scores, by = "run_number")
  
  # Store results
  results_scored$evidence_scenario <- rep(scenario_name)
  results_list[[scenario]] <- results_scored
  
  # Defining and calculating output metrics
  my_metric <- new_metric(GLOBAL_TAS(), 2000:2100, mean)
  metric_values <- metric_calc(results, my_metric)
  
  # Weighted probabilistic projections
  bins <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, Inf)
  probability <- prob_calc(metric_values$metric_result,
                            bins = bins,
                            scores = scores$weights)
  
  # Store results
  probabilitydf <- as.data.frame(probability)
  probabilitydf$evidence_scenario <- rep(scenario_name)
  probs_list[[scenario]] <- probabilitydf
}
end <- Sys.time()
# Plotting CO2 concentration

results_merge <- do.call(rbind, results_list)

ggplot(subset(results_list[["Baseline"]],
              year > 1956 & year < 2100
              & variable == CONCENTRATIONS_CO2())) +
  geom_line(aes(x = year, y = value,
                group = run_number,
                color = weights, 
                alpha = weights),
            linewidth = 1) +
  scale_color_gradient(high = "dodgerblue4", low = "lightblue1") +
  scale_alpha_continuous(range = c(0.1, 1)) +
  geom_line(data = matilda:::metricdata_co2,
            aes(year, co2_ppm),
            color = "red",
            linewidth = 1,
            linetype = "longdash") +
  facet_wrap(~evidence_scenario) +
  ylab(expression(CO[2]~Concentration~(ppm))) +
  theme_light() +
  guides(alpha = "none")

# Plotting probabilities as stacked bar graph

probs_merge <- do.call(rbind, probs_list)
colnames(probs_merge) <- c("Warming", "Score" ,"Probability", "Scenario")

ggplot(probs_merge, aes(fill = Warming, y = Probability, x = Scenario)) +
  geom_bar(position = position_fill(reverse = T), 
           stat = "identity",
           width = 0.6) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1)) +
  scale_fill_manual(values = c("dodgerblue", 
                               "skyblue1", 
                               "mistyrose", # change to red
                               "lightcoral",
                               "red",
                               "darkred"),
                    labels = c("1 to 1.5 C", 
                               "1.5 to 2 C", 
                               "2 to 2.5 C", 
                               "2.5 to 3 C", 
                               "3.5 to 4 C",
                               ">4 C")) +
  coord_flip() +
  theme_light() 

# Plotting weighted median with 5-95% CI:

# Function to calculate weighted confidence intervals

weighted_confidence_interval <- function(data, weights, level = 0.95) {
  # Calculate the weighted mean
  weighted_mean <- sum(data * weights) / sum(weights)
  
  # Calculate the standard error of the weighted mean
  weighted_se <- sqrt(sum(weights * (data - weighted_mean)^2) / (sum(weights) * (length(data) - 1)))
  
  # Calculate the t-score for the desired confidence level
  t_score <- qt((1 + level) / 2, df = length(data) - 1)
  
  # Calculate the margin of error
  margin_of_error <- t_score * weighted_se
  
  # Calculate the confidence interval
  lower_bound <- weighted_mean - margin_of_error
  upper_bound <- weighted_mean + margin_of_error
  
  return(c(lower_bound, upper_bound))
}

# Subset data to include warming variable
results_subset <- subset(results_merge, variable == GLOBAL_TAS() &
                       year > 2035 &
                       year < 2101)

# Calculate confidence intervals using dplyr
intervals <- results_subset %>%
  group_by(evidence_scenario, year) %>%
  summarise(
    mean_temp = weighted.mean(value, weights),
    CI_10 = weighted_confidence_interval(value, weights, level = 0.90)[1],
    CI_90 = weighted_confidence_interval(value, weights, level = 0.90)[2],
    CI_33 = weighted_confidence_interval(value, weights, level = 0.66)[1],
    CI_66 = weighted_confidence_interval(value, weights, level = 0.66)[2]
  )

# Plot the data using ggplot2
colors <- c("#F69320", "#003466", "#DF0000")

ggplot(data = intervals) +
  geom_line(aes(x = year, y = mean_temp, color = evidence_scenario)) +
  geom_ribbon(aes(x = year, ymin = CI_10, ymax = CI_90, fill = evidence_scenario), alpha = 0.2) +
  geom_ribbon(aes(x = year, ymin = CI_33, ymax = CI_66, fill = evidence_scenario), alpha = 0.4) +
  labs(title = "Median Temperature Projections (5-95% CI)",
       x = "Year", y = "Temperature (\u00B0C)", color = "Evidence Scenario", fill = "Evidence Scenario") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_light()

print(end-start)

