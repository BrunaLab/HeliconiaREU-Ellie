
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
library(purrr) 
library(future)
library(furrr)

# read in the data  -------------------------------------------------------

ha <- read_rds(here("data", "model_data.rds"))


# prepare data for analyses -----------------------------------------------

# mutating the spei_history column, creates one that just contains 
# the first value labeled "spei"
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history) 


# Function that does XYZ  -------------------------------------------------


#creates the gam and extracts the desired parameters
model_stats <- function(data) { 
  m <- gam(surv ~
             s(log_size_prev) + 
             te(spei_history, L, 
                bs = "cr"), 
           family = binomial, 
           data = data, 
           method = "REML")
  
  tibble(r2 = summary(m)$r.sq, 
         edf = summary(m)$edf[[2]], 
         rmse = sqrt(mean(residuals.gam(m,type="response")^2)), 
         pvalue = summary(m)$s.pv[[2]])
}

# Function that does XYZ  -------------------------------------------------

#ERS this actually fits the models AND extracts the results.  That's probably the best way to do this so there isn't a giant list of model objects in memory, but make sure the comments (and maybe function name?) reflect this.  I was not expecting this function to be the one that takes the longest based on the name.

get_results <- function(sample_list) { # stores results
  df_list <- vector("list", length(sample_list)) # create vector to store sample
  for (i in seq_along(sample_list)) {
    # tic(i)
    
    df_list[[i]] <- model_stats(sample_list[[i]])
    # toc(log = TRUE, quiet = TRUE)
  }
  out <- bind_rows(df_list)
  return(out)
}



# Function that does XYZ  -------------------------------------------------

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



# Section Description Needed ----------------------------------------------



# Create samples

n_samp <- 500
sample_5000 <- make_samples(plants_per_sample = 5000, n_samples = n_samp)
names(sample_5000) <- paste0("plants_5000_", seq_along(sample_5000))
sample_4500 <- make_samples(plants_per_sample = 4500, n_samples = n_samp) 
names(sample_4500) <- paste0("plants_4500_", seq_along(sample_4500))
sample_4000 <- make_samples(plants_per_sample = 4000, n_samples = n_samp)  
names(sample_4000) <- paste0("plants_4000_", seq_along(sample_4000))
sample_3500 <- make_samples(plants_per_sample = 3500, n_samples = n_samp)
names(sample_3500) <- paste0("plants_3500_", seq_along(sample_3500))
sample_3000 <- make_samples(plants_per_sample = 3000, n_samples = n_samp) 
names(sample_3000) <- paste0("plants_3000_", seq_along(sample_3000))
sample_2500 <- make_samples(plants_per_sample = 2500, n_samples = n_samp) 
names(sample_2500) <- paste0("plants_2500_", seq_along(sample_2500))
sample_2000 <- make_samples(plants_per_sample = 2000, n_samples = n_samp)
names(sample_2000) <- paste0("plants_2000_", seq_along(sample_2000))
sample_1500 <- make_samples(plants_per_sample = 1500, n_samples = n_samp) 
names(sample_1500) <- paste0("plants_1500_", seq_along(sample_1500))
sample_1000 <- make_samples(plants_per_sample = 1000, n_samples = n_samp) 
names(sample_1000) <- paste0("plants_1000_", seq_along(sample_1000))
sample_500 <-  make_samples(plants_per_sample = 500, n_samples = n_samp)
names(sample_500) <- paste0("plants_500_", seq_along(sample_500))

# combine into one big list to map over
big_list <- c(sample_5000, sample_4500, sample_4000, sample_3500, sample_3000,
              sample_2500, sample_2000, sample_1500, sample_1000, sample_500)

# map the model_stats() function over the list
plan(multisession, workers = 3) 

tic()
example <- furrr::future_map_dfr(big_list, model_stats # should this be get_results?
                                 , .id = "sample_id")
toc()

plan(sequential)

# Examine results
example
plot(example$r2)
example$r2

example <- example %>%
  separate(sample_id, into = c("trash","n_plants", "rep")) %>% 
  select(-trash) %>% 
  mutate(n_plants = as.numeric(n_plants))
example
plot(example$n_plants, example$r2)
hist(example$r2)
qqplot(example$r2,example$n_plants) # seems very useful, like boxplot
# ?qqplot


