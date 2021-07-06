library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(tictoc)

ha <- read_rds(here("data", "model_data.rds")) # reading in data
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history) # mutating the spei_history column, creates one that just contains the first value labeled "spei"
# im not sure if the for loop should go here or line 13

model_stats <- function(data) { #creates the gam and extracts the desired parameters
  m <- gam(surv ~
             s(log_size_prev) + 
             te(spei_history, L, 
                bs = "cr"), 
           family = binomial, 
           data = data, 
           method = "REML")
  
  df <- tibble(r2 = summary(m)$r.sq, edf = summary(m)$edf[[1]], 
               rmse = sqrt(mean(residuals.gam(m,type="response")^2)), 
               pvalue = summary(m)$s.pv[[2]])
  # should i use [[1]] for the pvalue? not sure which one is important
  return(df)
}


results <- function(sample_list) { # stores results
  df_list <- vector("list", length(sample_list)) # create vector to store sample
  for (i in seq_along(sample_list)) {
    tic(i)
    
    df_list[[i]] <- model_stats(sample_list[[i]])
    toc(log = TRUE, quiet = TRUE)
  }
  out <- bind_rows(df_list)
  return(out)
}


samples <- function(df, plants_per_sample, pops_to_sample) { # creates random samples
  out <- vector("list", pops_to_sample) # create vector to store sample
  
  for (i in 1:pops_to_sample) { # looping sampling process
    plants <- unique(ha$ha_id_number)
    
    plant_sample <- sample(plants, plants_per_sample)
    
    out[[i]] <- 
      ha %>%
      filter(ha_id_number %in% plant_sample) # to get unique individuals
  }
  return(out)
}

# use of the functions
seq(5000,500,by=-500)
sample_5000 <- samples(plants_per_sample = 5000, pops_to_sample = 2)
sample_4500 <- samples(plants_per_sample = 4500, pops_to_sample = 2)
sample_4000 <- samples(plants_per_sample = 4000, pops_to_sample = 2)
sample_3500 <- samples(plants_per_sample = 3500, pops_to_sample = 2)
sample_3000 <- samples(plants_per_sample = 3000, pops_to_sample = 2)
sample_2500 <- samples(plants_per_sample = 2500, pops_to_sample = 2)
sample_2000 <- samples(plants_per_sample = 2000, pops_to_sample = 2)
sample_1500 <- samples(plants_per_sample = 1500, pops_to_sample = 2)
sample_1000 <- samples(plants_per_sample = 1000, pops_to_sample = 2)
sample_500 <-  samples(plants_per_sample = 500, pops_to_sample = 15)

tic.clearlog()
results(sample_500)# extracts parameters in a tibble
log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
timing_sum <- sum(timings)
