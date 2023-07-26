library(hector)
library(ggplot2)

# Basic Run
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)
run(core)

results <- fetchvars(core, 2000:2300)
head(results)

ggplot(results) +
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")

# Setting Parameters
beta <- fetchvars(core, NA, BETA())
setvar(core, NA, BETA(), 0.40, "(unitless)")
fetchvars(core, NA, BETA())
reset(core)
run(core)
results_40 <- fetchvars(core, 2000:2300)
head(results_40)
results[["beta"]] <- 0.36
results_40[["beta"]] <- 0.4
compare_results <- rbind(results, results_40)

ggplot(compare_results) +
  aes(x = year, y = value, color = factor(beta)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_legend(title = expression(beta)))

# Sensitivity Analysis

run_with_param <- function(core, parameter, value) {
  setvar(core, NA, parameter, value, getunits(parameter))
  reset(core)
  run(core)
  result <- fetchvars(core, 2000:2300)
  result[["parameter_value"]] <- value
  result[["parameter_units"]] <- getunits(parameter)
  result
}

run_with_param_range <- function(core, parameter, values) {
  mapped <- Map(function(x) run_with_param(core, parameter, x), values)
  Reduce(rbind, mapped)
}

sensitivity_beta <- run_with_param_range(core, BETA(), seq(0, 1, length.out = 5))

ggplot(sensitivity_beta) +
  aes(x = year, y = value, color = parameter_value, group = parameter_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = expression(beta))) +
  scale_color_viridis_c()
