library(tidyverse)
library(fastDummies)

# read in the raw data
# create dummy columns
# remove the original columns
# change class into a factor
# create cv partitions
set.seed(43)
mailing <- read_csv('./mailing.csv') %>% 
  dummy_cols(.data = ., select_columns = c('rfaa2', 'pepstrfl')) %>% 
  select(-rfaa2, -pepstrfl) %>% 
  mutate(class = as.factor(class)) %>% 
  mutate(cv_part = sample(1:10, size = nrow(.), replace = T))

# take a look at it
glimpse(mailing)
summary(mailing)
head(mailing)

mailing_test <- filter(mailing, cv_part == 1) %>% 
  sample_n(1000, replace = T)

# the dataset is way too big for the things we're doing, and it's incredibly unbalanced
mail_1 <- filter(mailing, cv_part != 1, class == 1) %>% 
  sample_n(size = 2000, replace = T)
mail_2 <- filter(mailing, cv_part != 1, class == 0) %>% 
  sample_n(size = 2000, replace = T)
mailing_train <- bind_rows(mail_1, mail_2) %>% 
  select(-cv_part)
mailing_test <- select(mailing_test, -cv_part)

save(mailing_test, mailing_train, file = 'mailing_train_test.RData')



