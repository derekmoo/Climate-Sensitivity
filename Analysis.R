# Load Matilda
# remotes::install_github("jk-brown/matilda")
library(matilda)
library(ggplot2)
library(dplyr)
library(tidyr)

# Configure Hector core
ini <- system.file("input/hector_ssp245.ini", package = "hector")
c_ssp245 <- newcore(ini)

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

# Generate and store parameter sets for each evidence scenario
results <- list()

for (scenario in names(evidence_scenarios)) {
  
  # Replace the ECS column with the static mean value
  param_values$ECS <- evidence_scenarios[scenario]
  
  # CALL MATILDA
  
  # results[[scenario]] <- scenario_results
}

# # Running Matilda analysis for each line of evidence
# ######
# 
# params <- list_df_params[["Baseline"]]
# 
# # Running Hector Iteratively
# results <-iterate_hector(core = c_ssp245,
#                          params = params)
# head(results)
# 
# ggplot(results, aes(x = year, y = value, group = run_number, color = as.factor(run_number))) +
#   geom_line() +
#   facet_wrap(~variable, scales = "free_y")
# 
# # Screening Hector Results
# scores <- score_hruns(results, criterion_co2_obs(), score_ramp, w1 = 3, w2 = 12)
# 
# results_scored <- merge(results, scores, by = "run_number")
# 
# ggplot(data = results_scored) +
#   geom_line(aes(x = year, y = value, group = run_number, color = weights)) +
#   scale_color_continuous() +
#   facet_wrap(~variable, scales = "free_y")
# 
# # Defining and calculating output metrics
# 
# my_metric <- new_metric(GLOBAL_TAS(), 2000:2100, mean)
# 
# metric_values <- metric_calc(results, my_metric)
# 
# # Weighted probabilistic projections
# 
# bins <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, Inf)
# 
# prob_calc(metric_values$metric_result,
#           bins = bins,
#           scores = scores$weights)
