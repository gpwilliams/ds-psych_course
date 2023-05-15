# Animate Bayesian updating ----
# Dr Glenn Williams, 2023-05-12

library(ggplot2)
library(gganimate)
library(gifski)
library(here)

# set up initial parameters
mu <- seq(70, 130, by = 1) # grid of possible means
prior <- rep(1/length(mu), length(mu)) # flat prior
n <- 1 # number of observations from means
mu_obs <- rnorm(n, mean = 100, sd = 10) # sample from a normal distribution

# define function for updating the posterior
create_posterior <- function(prior, mu_obs, mu){
  likelihood <- dnorm(mu_obs, mean = mu, sd = 10)
  posterior_unscaled <- likelihood * prior
  # return posterior
  posterior_unscaled / sum(posterior_unscaled)
}

# create data frame for plotting
df <- tibble(
  mu = mu, 
  prior = prior, 
  posterior = prior, 
  time = 1
)

# loop through each sample, updating the posterior
for (i in 2:20) {
  prior <- df |> filter(time == i-1) |> pull(posterior)
  posterior <- create_posterior(prior, mu_obs, mu)
  df <- rbind(df, tibble(mu = mu, prior = prior, posterior = posterior, time = i))
  mu_obs <- rnorm(n, mean = 100, sd = 10)
}

# create plot and animation with prior and posterior after each observation
bayes_anim <- ggplot(df, aes(x = mu)) +
  geom_line(aes(y = prior, colour = "Prior"), linewidth = 3, alpha = 0.5) +
  geom_line(aes(y = posterior, colour = "Posterior"), linewidth = 3) +
  scale_colour_manual(values = c("Prior" = "#95d5d0", "Posterior" = "#ffa07a")) +
  ylim(0, max(df$posterior)) +
  labs(
    x = "μ",
    y = "Density",
    title = "Bayesian Updating using Each Posterior as a Prior",
    subtitle = "Sample: {frame}",
    caption = "Sampling from μ ~ N(100, 10)"
  ) + 
  theme_bw() +
  theme(legend.title = element_blank()) +
  transition_manual(time) +
  ease_aes("linear")

anim <- animate(bayes_anim, renderer = gifski_renderer())
anim_save(
  here(
    "09_bayesian-estimation-and-model-comparison", 
    "resources",
    "bayesian-updating.gif"
  ), 
  anim, 
  animation_fps = 24
)