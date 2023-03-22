# make fake data

library(tidyverse)
library(here)

# read data ----
stroop <- read_csv(here("data", "arabic_stroop.csv")) |> 
  janitor::clean_names() |> 
  select(subject_id, word, colour, stroop, language, rt)

demo <- read_csv(here("data", "arabic_stroop_demographics.csv")) |> 
  janitor::clean_names() |> 
  select(subject_id, participant_age, participant_gender, arabic_reading, english_reading)

rt_ids <- c(4515754, 4515860, 4515875, 4515896)
demo_ids <- c(4517644, 4515860, 4515875, 4515896) 

# aggregate data ----
stroop_agg <- stroop |> 
  filter(rt > 500 & rt < 2000) |> 
  group_by(subject_id, stroop) |> 
  summarise(rt = mean(rt, na.rm = TRUE)) |> 
  filter(subject_id %in% rt_ids)

stroop_agg$subject_id <- c(1, 1, 2, 2, 3, 3, 4, 4)

demo_reduced <- demo |> 
  filter(subject_id %in% demo_ids) |> 
  select(subject_id:participant_gender) |> 
  rename(age = participant_age, gender = participant_gender)

demo_reduced$subject_id <- c(5, 2, 3, 4)

reading_reduced <- demo |> 
  filter(subject_id %in% demo_ids) |> 
  select(subject_id, arabic_reading, english_reading)

reading_reduced$subject_id <- c(5, 2, 3, 4)

# make new rows of data [for bind_rows]
stroop_new <- tibble(
  subject_id = c(6, 6),
  stroop = c("Incongruent", "Neutral"),
  rt = c(1317.213, 1148.765)
)

# make inconsistent codes [for intersect]
reading_inconsistent <- reading_reduced
reading_inconsistent$arabic_reading <- c(6, 5, 7, 6)
reading_inconsistent$english_reading <- c(6, 5, 4, 5)

# add a duplicate row [for union()]
reading_dupe <- reading_reduced 
reading_dupe[5,] <- reading_dupe[4,] 

# write to file ----

split_stroop <- split(stroop_agg, stroop_agg$subject_id)

iwalk(
  split_stroop, 
  ~write_csv(
    ., 
    file = here(
      "data", 
      "tidying-and-merging", 
      "stroop", 
      paste0("participant_", .y, ".csv")
    )
  )
)

write_csv(demo_reduced, here("data", "tidying-and-merging", "stroop_demographics.csv"))
write_csv(reading_reduced, here("data", "tidying-and-merging", "reading_coder-1.csv"))
write_csv(reading_inconsistent, here("data", "tidying-and-merging", "reading_coder-2.csv"))
write_csv(stroop_new, here("data", "tidying-and-merging", "stroop_new.csv"))
write_csv(reading_dupe, here("data", "tidying-and-merging", "reading_dupe.csv"))

# for finding dupes ----

union(reading_reduced, reading_dupe) # unique rows only
intersect(reading_reduced, reading_inconsistent) # identicals
setdiff(reading_reduced, reading_inconsistent) # non-identicals
union(reading_reduced, reading_inconsistent) # unique rows only
