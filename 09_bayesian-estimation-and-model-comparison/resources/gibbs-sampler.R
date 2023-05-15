library(tidyverse)
library(here)
library(gganimate)

# Data simulation ----

# define parameters
x_mean <- 50
y_mean <- 75
x_sd <- 35
y_sd <- 55
corr <- .6
covariance <- x_sd * y_sd * corr

# get the vcov
varcorr <- matrix(
  c(x_sd^2, covariance, covariance, y_sd^2),
  ncol = 2
)

# sample fro mthe bivariate normal
set.seed(1892)
samples <- MASS::mvrnorm(
  n = 200,
  mu = c(x = x_mean, y = y_mean),
  Sigma = varcorr
)
samples_df <- as_tibble(samples)

# plot the result
bivariate_plot <- ggplot(samples_df, aes(x = x, y = y)) +
  geom_density_2d(colour = "#95d5d0") +
  coord_cartesian(xlim = c(-200, 200), ylim = c(-200, 200)) +
  theme_bw()

ggsave(
  here(
    "09_bayesian-estimation-and-model-comparison", 
    "resources",
    "bivariate-plot.png"
  ), 
  bivariate_plot
)

# Gibbs sampler ----

# Define the likelihood function
likelihood <- function(data, x, y) {
  mu <- c(x, y)
  sigma <- varcorr
  return(prod(mvtnorm::dmvnorm(data, mean = mu, sigma = sigma)))
}

# Define the prior distribution for x and y
prior <- function(x, y) {
  return(dnorm(x, mean = x_mean, sd = x_sd) * dnorm(y, mean = y_mean, sd = y_sd))
}

# Define the posterior distribution (up to a constant)
posterior <- function(data, x, y) {
  return(likelihood(data, x, y) * prior(x, y))
}

# Implement the Gibbs sampling algorithm
gibbs_sampling <- function(iterations, data, initial_state) {
  current_state <- initial_state
  accepted_states <- matrix(0, nrow = iterations, ncol = 2)
  accepted_states[1, ] <- current_state
  
  for (i in 2:iterations) {
    # Sample x from the conditional distribution given y
    x <- rnorm(
      1, 
      mean = x_mean + varcorr[1, 2] * (current_state[2] - y_mean) / y_sd^2,
      sd = sqrt(varcorr[1, 1] - varcorr[1, 2]^2 / y_sd^2)
    )
    
    # Sample y from the conditional distribution given x
    y <- rnorm(
      1, 
      mean = y_mean + varcorr[1, 2] * (x - x_mean) / x_sd^2,
      sd = sqrt(varcorr[2, 2] - varcorr[1, 2]^2 / x_sd^2)
    )
    
    current_state <- c(x, y)
    accepted_states[i, ] <- current_state
  }
  
  return(accepted_states)
}

# Set the number of iterations and initial state
iterations <- 1000
initial_state <- c(x_mean, y_mean)

# Run the Gibbs sampling algorithm
chain <- gibbs_sampling(iterations, samples, initial_state)

# Create a data frame with the chain of accepted states
chain_df <- data.frame(iteration = 1:iterations, x = chain[, 1], y = chain[, 2])

# plot the animation
p <- ggplot(chain_df, aes(x, y, group = 1)) +
  geom_point(colour = "#ffa07a", size = 2) +
  labs(title = "Approximate Posterior Distribution") +
  coord_cartesian(xlim = c(-200, 200), ylim = c(-200, 200)) +
  theme_bw()

# create the animation using gganimate
animation <- p + 
  transition_states(iteration) +
  shadow_mark()

anim <- animate(animation, nframes = iterations, renderer = gifski_renderer())

anim_save(
  here(
    "09_bayesian-estimation-and-model-comparison", 
    "resources",
    "gibbs-sampling.gif"
  ), 
  anim
)
