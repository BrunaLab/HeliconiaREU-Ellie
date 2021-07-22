library(tidyverse)
library(mgcv)
library(here)
library(furrr)


ha <- read_rds(here("data", "model_data.rds"))

#Pull a sample from the dataset, fit the DLNM, extract the relevant stats.
get_results <- function(plants_per_sample) {
  plants <- unique(ha$ha_id_number)
  plant_sample <- sample(plants, plants_per_sample)
  
  ha_sub <- 
    ha %>%
    filter(ha_id_number %in% plant_sample) # to get unique individuals
  
  m <- gam(surv ~
             s(log_size_prev) + 
             te(spei_history, L, 
                bs = "cr"), 
           family = binomial, 
           data = ha_sub, 
           method = "REML")
  
  tibble(r2 = summary(m)$r.sq, 
         edf = summary(m)$edf[[2]], 
         rmse = sqrt(mean(residuals.gam(m,type="response")^2)), 
         pvalue = summary(m)$s.pv[[2]])
}

#needs a name! runs get_results n_samples times
foo <- function(plants_per_sample, n_samples) {
  out <- get_results(plants_per_sample)
  i = 1
  while (i < n_samples) {
    out <- rbind(out, get_results(plants_per_sample))
    i <- i + 1
  }
  return(out)
}

#do this for 500, 1000, 1500, etc. plants per sample
x <- seq(500, 5000, by = 500)
names(x) <- paste0("plants_", x)

# 500 samples each for every number of plants in `x`
plan(multisession, workers = 3)

final_output <- future_map_dfr(.x = x, .f= ~foo(.x, n_samples = 500), .id = "experiment", .options = furrr_options(seed = TRUE))

plan(sequential)

final_output

write_csv(final_output, here("num_plants_experiment.csv"))
