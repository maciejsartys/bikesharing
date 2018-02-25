# tidyverse packages
library(readr)
library(dplyr)
library(tibble)
library(forcats)

# ml packages
library(MLmetrics)

#data import
train_data_file <- "data/train.csv"
test_data_file <- "data/test.csv"
sample_submission_file <- "data/sampleSubmission.csv"
submissions_dir <- "./submissions/"

train.df <- read_csv(train_data_file)
test.df <- read_csv(test_data_file)
sample_submission.df <- read_csv(sample_submission_file)

# categorical variables as factors conversion
train.df %>% mutate(season =
                      as_factor(as.character(season),
                             levels = 1:4,
                             labels = c("spring", "summer", "fall", "winter")
                             ),
                    weather =
                      as_factor(as.character(weather),
                             levels = 1:4,
                             labels = c("clear", "mist", "light rain/snow", "heavy rain/snow")
                             )
                    ) -> train.df

#baseline dummy model

mean_count <- round(mean(train.df$count))

train.df %>% select(datetime) %>% mutate(
  count = mean_count) -> train_pred
RMSLE(train_pred$count, train.df$count)
#1.569198

test.df %>% select(datetime) %>% mutate(
  datetime = as.character(datetime),
  count = mean_count) -> test_pred
write_csv(test_pred, paste0(submissions_dir, '01_dummy_mean.csv'))