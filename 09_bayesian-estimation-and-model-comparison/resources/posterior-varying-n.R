library(tidyverse)
library(here)

# prior tibble
prior <- c(4, 4)

# likelihood parameters (10 successes, 10 failures)
likelihoods <- tibble(
  `n = 5` = c(1, 4),
  `n = 20` = c(4, 16),
  `n = 2000` = c(400, 1600)
)
  

# generate values for x-axis
x <- seq(0, 1, length.out = 100)

# calculate densities
prior_density <- dbeta(x, prior[1], prior[2])
likelihood_density <- map(likelihoods, ~ dbeta(x, .x[1], .x[2]))
posterior_densities <- map(likelihoods, ~ dbeta(x, prior[1] + .x[1], prior[2] + .x[2]))


# combine the densities into a single data frame
data <- bind_rows(
  crossing(
    tibble(x, y = prior_density),
    tibble(sample = names(likelihoods)),
    type = "Prior"
  ),
  map2_df(
    likelihood_density, 
    names(likelihoods),
    ~ tibble(x, y = .x, sample = paste0(.y), type = "Likelihood")
  ),
  map2_df(
    posterior_densities, 
    names(likelihoods), 
    ~ tibble(x, y = .x, sample = paste0(.y), type = "Posterior")
  )
)

# set order of levels for plotting
data$type <- factor(data$type, levels = c("Prior", "Likelihood", "Posterior"))
data$sample <- factor(data$sample, levels = c("n = 5", "n = 20", "n = 2000"))

# plotting
sample_plot <- ggplot(data, aes(x = x, y = y, colour = type, linetype = type)) +
  geom_line(linewidth = 2) +
  scale_linetype_manual(values = c("solid", "solid", "longdash")) +
  facet_wrap(~ sample) +
  scale_colour_manual(values = c("#95d5d0", "grey", "#ffa07a")) +
  labs(
    x = "Parameter Value",
    y = "Density",
    title = "Relationship between Sample Size, Prior, Likelihood, and Posterior"
  ) +
  theme_bw() +
  theme(
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
    "sample-plot.png"
  ),
  sample_plot,
  width = 12,
  height = 8
)