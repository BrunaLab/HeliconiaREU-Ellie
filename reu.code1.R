# all packages needed 
library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(dplyr)

ha <- read_rds(here("data", "ha_data_subset.rds")) # reading in data
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history) # mutating the spei_history column, creates one that just contains the first value labeled "spei"
# im not sure if the for loop should go here or line 13
plants_per_sample <- seq(5000,500,by = -500) # this gets an error for taking too many samples because it isnt looping
# for (something in plants_per_sample??? maybe) {

pops_to_sample <- 1 # population samples created (is one an okay number for this?)

out <- vector("list", pops_to_sample) # create vector to store sample

for (i in 1:pops_to_sample) { # looping sampling process
  plants <- unique(ha$ha_id_number)
  
  plant_sample <- sample(plants, plants_per_sample)
  
  out[[i]] <- 
    ha %>%
    filter(ha_id_number %in% plant_sample) # to get unique individuals
}
out

str(out) # another way to look at sample pops


r.sq.gam <- vector("numeric", length = length(out))
for (i in seq_along(out)){ # loop for GAM

m <- gam(surv ~
           s(log_size_prev) + # 1D smooth for non-linear effect of size in previous year
           te(spei_history, L, # tensor product 2D smooth for lagged SPEI effect
              bs = "cr"), # use "cr" basis for each marginal dimension (this can be changed in dif situations)
         family = binomial, # because response is 1 or 0
         data = out[[i]], 
         method = "REML")

summary(m)$r.sq # pulling out r squared value

r.sq.gam[i] <- summary(m)$r.sq # storing r squared value
}
r.sq.gam
# } i think this is where the loop would end and repeat