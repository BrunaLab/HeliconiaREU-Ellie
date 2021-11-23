get_model_stats <- function(data) {
  m <- gam(surv ~
             s(log_size_prev) + 
             te(spei_history, L, 
                bs = "cr"), 
           family = binomial, 
           data = data, 
           method = "REML")
  
  df <- tibble(r2 = summary(m)$r.sq,  edf = summary(m)$edf[[2]], 
               rmse = sqrt(mean(residuals.gam(m,type="response")^2)), 
               pvalue = summary(m)$s.pv[[2]])
                 
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


