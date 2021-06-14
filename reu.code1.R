library(purrr)
library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(dplyr)
ha <- read_rds(here("data", "ha_data_subset.rds"))
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history)

pops_to_sample <- 3
plants_per_sample <- 100
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


for (i in seq_along(out)){ 

m <- gam(surv ~
           s(log_size_prev) + 
           te(spei_history, L, 
              bs = "cr"), 
         family = binomial, 
         data = out[[i]],
         method = "REML")
summary(m)

r.sq.gam <- print(summary(m)$r.sq)
}

