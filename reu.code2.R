
# Overview ----------------------------------------------------------------

# TODO: Needs a description of the code and what it is for

# TODO: file names need to be changed to be more informative
# see the tidyverse style guide: https://style.tidyverse.org/files.html#names


# Load Required packages --------------------------------------------------

# TODO: I find it useful to put in a reminder of what packages are being used for
# (obs. not necessary for things like tidyverse, just the ones that have a
# specific purpose or task, like tictoc)

library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(tictoc) # used for timing how long it takes to _____


# read in the data  -------------------------------------------------------

ha <- read_rds(here("data", "model_data.rds"))


# prepare data for analyses -----------------------------------------------

# mutating the spei_history column, creates one that just contains 
# the first value labeled "spei"
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history) 


# Function that does XYZ  -------------------------------------------------

# im not sure if the for loop should go here or line 13

#creates the gam and extracts the desired parameters
model_stats <- function(data) { 
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

# Function that does XYZ  -------------------------------------------------

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

# Function that does XYZ  -------------------------------------------------

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


# Section Description Needed ----------------------------------------------



# use of the functions
seq(5000,500,by=-500)
sample_5000 <- samples(plants_per_sample = 5000, pops_to_sample = 10)
sample_4500 <- samples(plants_per_sample = 4500, pops_to_sample = 25)
sample_4000 <- samples(plants_per_sample = 4000, pops_to_sample = 25)
sample_3500 <- samples(plants_per_sample = 3500, pops_to_sample = 25)
sample_3000 <- samples(plants_per_sample = 3000, pops_to_sample = 25)
sample_2500 <- samples(plants_per_sample = 2500, pops_to_sample = 25)
sample_2000 <- samples(plants_per_sample = 2000, pops_to_sample = 25)
sample_1500 <- samples(plants_per_sample = 1500, pops_to_sample = 25)
sample_1000 <- samples(plants_per_sample = 1000, pops_to_sample = 25)
sample_500 <-  samples(plants_per_sample = 500, pops_to_sample = 25)


# TODO? make it generic, allows you to convert to a function to run as loops
plants_per_sample = 500
pops_to_sample = 25
model_output_all_n <-  samples(plants_per_sample, pops_to_sample)





# Timing how long it takes to run GAM and extract key results -------------

tic.clearlog()
extracted_GAM_results <- results(sample_5000)# extracts parameters in a tibble
log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
timing_sum <- sum(timings)
timing_sum
timing_avg <- mean(timings)
timing_avg
timing_sd <- sd(timings)