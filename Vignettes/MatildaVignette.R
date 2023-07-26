# Load Matilda
# remotes::install_github("jk-brown/matilda")
library(matilda)
library(ggplot2)

# Configure Hector Core
ini <- system.file("input/hector_ssp245.ini", package = "hector")
c_ssp245 <- newcore(ini)

# Generate Parameter Values
set.seed(1)
param_values <- generate_params(c_ssp245, draws = 10)

# Running Hector Iteratively
results <-iterate_hector(core = c_ssp245,
                         params = param_values)
head(results)

ggplot(results, aes(x = year, y = value, group = run_number, color = as.factor(run_number))) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

# Screening Hector Results
scores <- score_hruns(results, criterion_co2_obs(), score_ramp, w1 = 2, w2 = 20)

results_scored <- merge(results, scores, by = "run_number")

ggplot(data = results_scored) +
  geom_line(aes(x = year, y = value, group = run_number, color = weights)) +
  scale_color_continuous() +
  facet_wrap(~variable, scales = "free_y")

scores_bayes <- score_hruns(results, criterion_co2_obs(), score_bayesian, e = 1)

results_scored_bayes <- merge(results, scores_bayes, by = "run_number")

ggplot(data = results_scored_bayes) +
  geom_line(aes(x = year, y = value, group = run_number, color = weights)) +
  scale_color_continuous() +
  facet_wrap(~variable, scales = "free_y")

# Defining and calculating output metrics

my_metric <- new_metric(GLOBAL_TAS(), 2000:2100, mean)

metric_values <- metric_calc(results, my_metric)

# Weighted probabilistic projections

bins <- c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, Inf)

prob_calc(metric_values$metric_result, 
          bins = bins, 
          scores = scores$weights)
