get_model_stats <- function(data) {
  m <- gam(surv ~
             s(log_size_prev) + 
             te(spei_history, L, 
                bs = "cr"), 
           family = binomial, 
           data = data, 
           method = "REML")
  
  df <- tibble(r2 = summary(m)$r.sq,
               # rmsep = ,
               # edf = ,
               # pval = 
                 )
  return(df)
}


get_results <- function(sample_list) {
  df_list <- vector("list", length(sample_list)) # create vector to store sample
  for (i in seq_along(sample_list)) {
    df_list[[i]] <- get_model_stats(sample_list[[i]])
  }
  out <- bind_rows(df_list)
  return(out)
}


make_samples <- function(df, plants_per_sample, pops_to_sample = 3) {
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

# Examples:
sample_500 <- make_samples(ha=ha, plants_per_sample = 500, pops_to_sample = 2)
sample_1000 <- make_samples(ha=ha, plants_per_sample = 1000, pops_to_sample = 2)

get_results(sample_500)
