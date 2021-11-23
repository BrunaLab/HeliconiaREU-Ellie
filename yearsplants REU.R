library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(tictoc)
library(zoo)
library(purrr)

ha <- read_rds(here("data", "model_data.rds")) # reading in data
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history)

sample500 <- make_samples(500)

combo_3yrs <- rollapply(1999:2009, by = 1, width = 3, FUN = as.numeric)
x <- nrow(combo_3yrs)
all_dfs_3yr <- map(1:x, ~sample500 %>% map(~.x %>% filter(year %in% combo_3yrs[.x,]))) 
#get_model_stats(all_dfs_3yr[[1]])
three.years <- get_results(all_dfs_3yr)

?df
