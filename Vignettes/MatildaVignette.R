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











#' Scored Hector runs
#'
#' @param inifile a path to ini file of emissions pathway
#' @param draws number of random draws to generate model parameters
#' @param metric a metric defining data to filter from Hector output
#' @param crit scoring criterion to use
#' @param ssp_name An optional name to identify the core (string)
#'
#' @return Hector results with added column scoring each run.
scored_hector_runs <- function(inifile, ssp_name, draws, crit) {
  # initiate and core
  core = newcore(inifile, name = ssp_name)
  # generate parameters
  params = generate_params(core, draws)
  # running Hector
  h_result = iterate_hector(core, params)
  # score Hector runs 
  scores = score_hruns(h_result, crit, score_ramp, w1 = 2, w2 = 20)
  # merge scores with Hector Results
  scored_hector = merge(h_result, scores, "run_number")
  # shut down the core
  shutdown(core)
  # return
  return(scored_hector)  
}

# Establishing ini files for each scenario
ini_126 <- system.file("input/hector_ssp126.ini", package = "hector")
ini_245 <- system.file("input/hector_ssp245.ini", package = "hector")
ini_370 <- system.file("input/hector_ssp370.ini", package = "hector")
ini_585 <- system.file("input/hector_ssp585.ini", package = "hector")

# Running hector for each scenario
set.seed(1)
hector_126 <- scored_hector_runs(ini_126, ssp_name = "ssp126", 50, criterion_co2_obs())
hector_245 <- scored_hector_runs(ini_245, ssp_name = "ssp245", 50, criterion_co2_obs())
hector_370 <- scored_hector_runs(ini_370, ssp_name = "ssp370", 50, criterion_co2_obs())
hector_585 <- scored_hector_runs(ini_585, ssp_name = "ssp585", 50, criterion_co2_obs())

# Merging hector results for plotting
hector_merge <- rbind(hector_126,
                      hector_245,
                      hector_370,
                      hector_585)

# Plotting CO2 projections
plot_co2 <- ggplot(subset(hector_merge,
                          year > 1990 & year < 2100
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
            linewidth = 1) +
  facet_wrap(~variable + scenario) +
  ylab(expression(CO[2]~Concentration~(ppm))) +
  theme_light() +
  guides(alpha = "none")

plot_co2







#' Calculating probabilities 
#'
#' @param h_result A hector result
#' @param metric A metric to filter results of interest
#' @param criterionA criterion used to score Hector runs
#' @param bins Bins for computing probabilities - defaults for global_tas
#'
#' @return A data frame of probabilities 
probabilities <- function(h_result,
                          metric,
                          crit,
                          bins = c(1, 1.5, 2, 2.5, 3, 3.5, 4, Inf)) {
  # calculating metrics
  metrics = metric_calc(h_result, metric)
  # calculating scores
  scores = score_hruns(h_result, crit, score_ramp, w1 = 2, w2 = 20)
  # merging metrics and scores
  metric_scores = merge(metrics, scores, by = "run_number")
  # calculating probability
  probability = prob_calc(metric_scores$metric_result, bins,
                          metric_scores$weights)
  # coercing probability to data frame for plotting
  probs = as.data.frame(probability)
  # return
  return(probs)
}

# defining metric of mean global_tas for 1990:2100
metric_global_tas <- new_metric(GLOBAL_TAS(), years = 1990:2100, op = mean)

# Calculating probabilities for each hector run
probs_126 <- probabilities(hector_126, metric_global_tas, criterion_co2_obs())
probs_126$scenario <- rep("ssp_126")
probs_245 <- probabilities(hector_245, metric_global_tas, criterion_co2_obs())
probs_245$scenario <- rep("ssp_245")
probs_370 <- probabilities(hector_370, metric_global_tas, criterion_co2_obs())
probs_370$scenario <- rep("ssp_370")
probs_585 <- probabilities(hector_585, metric_global_tas, criterion_co2_obs())
probs_585$scenario <- rep("ssp_585")

#combining dfs with scenario types and probs
results_all <- rbind(probs_126,
                     probs_245,
                     probs_370,
                     probs_585)
colnames(results_all) <- c("Warming", "Score" ,"Probability", "Scenario")

# Plotting probabilities as stacked bar graph
ggplot(results_all, aes(fill = Warming, y = Probability, x = Scenario)) +
  geom_bar(position = position_fill(reverse = T), 
           stat = "identity",
           width = 0.6) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1)) +
  scale_fill_manual(values = c("dodgerblue", 
                               "skyblue1", 
                               "mistyrose",
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