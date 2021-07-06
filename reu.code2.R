library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(tictoc)

ha <- read_rds(here("data", "model_data.rds")) # reading in data
#ERS: short description of `ha` would be nice.  Include that two of the columns are matrix-columns.
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history) # mutating the spei_history column, creates one that just contains the first value labeled "spei"
# im not sure if the for loop should go here or line 13 #ERS: is this an old comment?

model_stats <- function(data) { #creates the gam and extracts the desired parameters
  m <- bam(surv ~
             s(log_size_prev) + 
             te(spei_history, L, 
                bs = "cr"), 
           family = binomial, 
           data = data, 
           method = "fREML")
  #ERS: Using te() is fine for now, but would be good to do this with the DLNM package like so:
  # s(spei_history, L, bs = "cb", xt = list(bs = "cr"))
  
  df <- tibble(r2 = summary(m)$r.sq, edf = summary(m)$edf[[1]], 
               rmse = sqrt(mean(residuals.gam(m,type="response")^2)), 
               pvalue = summary(m)$s.pv[[2]])
  # should i use [[1]] for the pvalue? not sure which one is important
  return(df)
}


#ERS this actually fits the models AND extracts the results.  That's probably the best way to do this so there isn't a giant list of model objects in memory, but make sure the comments (and maybe function name?) reflect this.  I was not expecting this function to be the one that takes the longest based on the name.
get_results <- function(sample_list) { # stores results
  df_list <- vector("list", length(sample_list)) # create vector to store sample
  for (i in seq_along(sample_list)) {
    tic(i)
    
    df_list[[i]] <- model_stats(sample_list[[i]])
    toc(log = TRUE, quiet = TRUE)
  }
  out <- bind_rows(df_list)
  return(out)
}


make_samples <- function(df, plants_per_sample, n_samples) { # creates random samples
  out <- vector("list", n_samples) # create vector to store sample
  
  for (i in 1:n_samples) { # looping sampling process
    plants <- unique(ha$ha_id_number)
    
    plant_sample <- sample(plants, plants_per_sample)
    
    out[[i]] <- 
      ha %>%
      filter(ha_id_number %in% plant_sample) # to get unique individuals
  }
  return(out)
}

# use of the functions
# seq(5000,500,by=-500)
# sample_5000 <- samples(plants_per_sample = 5000, n_samples = 2)
# sample_4500 <- samples(plants_per_sample = 4500, n_samples = 2)
# sample_4000 <- samples(plants_per_sample = 4000, n_samples = 2)
tic()
sample_3500 <- samples(plants_per_sample = 3500, n_samples = 25)
results(sample_3500)
toc()
# sample_3000 <- samples(plants_per_sample = 3000, n_samples = 2)
# sample_2500 <- samples(plants_per_sample = 2500, n_samples = 2)
# sample_2000 <- samples(plants_per_sample = 2000, n_samples = 2)
# sample_1500 <- samples(plants_per_sample = 1500, n_samples = 2)
# sample_1000 <- samples(plants_per_sample = 1000, n_samples = 2)
sample_500 <-  samples(plants_per_sample = 500, n_samples = 15)

tic.clearlog()
results(sample_500)# extracts parameters in a tibble
log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
timing_sum <- sum(timings)
