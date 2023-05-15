library(tidyverse)
library(here)

# prior tibble
priors <- tibble(
  uniform = c(1, 1), # uniform
  left = c(2, 8), # skewed left
  right = c(9, 1)  # skewed right
)

# likelihood parameters (10 successes, 10 failures)
likelihood <- c(10, 10)

# generate values for x-axis
x <- seq(0, 1, length.out = 100)

# calculate densities
prior_densities <- map(priors, ~ dbeta(x, .x[1], .x[2]))
likelihood_density <- dbeta(x, likelihood[1], likelihood[2])
posterior_densities <- map(priors, ~ dbeta(x, .x[1] + likelihood[1], .x[2] + likelihood[2]))

# combine the densities into a single data frame
data <- bind_rows(
  map2_df(
    prior_densities, 
    names(priors),
    ~ tibble(x, y = .x, prior = paste0(.y), type = "Prior")
  ),
  crossing(
    tibble(x, y = likelihood_density), 
    tibble(prior = names(priors)), 
    type = "Likelihood"
  ),
  map2_df(
    posterior_densities, 
    names(priors), 
    ~ tibble(x, y = .x, prior = paste0(.y), type = "Posterior"))
)

# set order of levels for plotting
data$type <- factor(data$type, levels = c("Prior", "Likelihood", "Posterior"))

# plotting
prior_plot <- ggplot(data, aes(x = x, y = y, colour = type)) +
  geom_line(linewidth = 2) +
  facet_wrap(~ prior) +
  scale_colour_manual(values = c("#95d5d0", "grey", "#ffa07a")) +
  labs(
    x = "Parameter Value",
    y = "Density",
    title = "Relationship between Prior, Likelihood, and Posterior"
  ) +
  theme_bw() +
  theme(
    strip.text = element_blank(),
    legend.title = element_blank(),
    # positioning of legend
    legend.position = c(1, 1), # make legend top right
    legend.justification = c(1, 1), # make legend touch panel border
    legend.box.just = "top", 
    # background colour and alpha for box and fill
    legend.background = element_rect(fill = alpha("white", 0.4)), 
    legend.box.background = element_rect(colour = "black", fill = "transparent")
  )

ggsave(
  here(
    "09_bayesian-estimation-and-model-comparison", 
    "resources",
    "prior-plot.png"
  ),
  prior_plot,
  width = 12,
  height = 8
)