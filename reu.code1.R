library(purrr)
library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(dplyr)
ha <- read_rds(here("data", "ha_data_subset.rds"))
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history)

pops_to_sample <- 5
plants_per_sample <- 25
out <- vector("list", pops_to_sample)

for (i in 1:pops_to_sample) {
  plants <- unique(ha$ha_id_number)
  
  plant_sample <- sample(plants, plants_per_sample)
  
  out[[i]] <- 
    ha %>%
    filter(ha_id_number %in% plant_sample)
}
out

str(out)




m <- gam(surv ~
           s(log_size_prev) + 
           te(spei_history, L, 
              bs = "cr"), 
         family = binomial, 
         data = out[[5]],
         method = "REML")
summary(m)


summary.gam <- map_df(out, ~ .x %>% summary(m))


summary.gam %>% map_dbl(~.x$m)