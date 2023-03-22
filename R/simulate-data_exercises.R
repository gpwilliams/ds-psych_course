library(tidyverse)
library(here)

demo_data <- tibble(
  subject = c(seq(1, 6), 8),
  heightWEIGHT = c("NA_70", "170-65", "161.80", "165_NONE", "NA__77", "189_90", "190.105"),
  age = c(18, 29, 190, NA, 22, 28, 55)
)

split_demo_data <- split(demo_data, demo_data$subject)

iwalk(
  split_demo_data, 
  ~write_csv(
    ., 
    file = here(
      "data", 
      "tidying-and-merging", 
      "height-weights", 
      paste0("participant_", .y, ".csv")
    )
  )
)

# IQ test scores
test_data <- tibble(
  subject = c(1, 3, 4, 4, 5, 6, 7),
  IQ = c(150, 160, 155, NA, 190, 120, 140)
)

write_csv(
  test_data, 
  here("data", "tidying-and-merging", "height-weights_demographics.csv")
)

