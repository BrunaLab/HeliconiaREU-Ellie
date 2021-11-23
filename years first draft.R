library(zoo)
library(purrr)
library(tidyverse)

# example data
df <- expand_grid(plant = letters[1:5], year = 1999:2009)
df

combo_3yrs <- rollapply(1999:2009, by = 1, width = 3, FUN = as.numeric)
combo_4yrs <- rollapply(1998:2009, by = 1, width = 4, FUN = as.numeric)

df %>% filter(year %in% combo_3yrs[1,]) #first 3 yrs
df %>% filter(year %in% combo_3yrs[2,]) #second 3 yrs

#list of all 3 yr combos data frames
all_dfs_3yr <- map(1:nrow(combo_3yrs), ~df %>% filter(year %in% combo_3yrs[.x, ])) 

#could then map() your get_results() function to them.  Somethign like this maybe:
map(all_dfs_3yr, ~get_results(data = .x))