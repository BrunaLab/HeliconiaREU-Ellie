# Example DLNM using mgcv package

# Load packages -----------------------------------------------------------

library(tidyverse)
library(mgcv)
library(gratia) #useful for visualizing GAM model output
library(here) #for reproducible file paths, will talk about next week in lab meeting

# Read in data ------------------------------------------------------------

ha <- read_rds(here("data", "ha_data_subset.rds"))
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history)

# Two of the columns of `ha` are matrix-columns.  This is weird.  I wrote a bit about matrix columns here: https://www.ericrscott.com/post/matrix-columns/

# `ha`: Subset of Heliconia demographic data and SPEI weather history
# 
# - `ha_id_number` (factor): unique plant ID number
# - `plot` (factor): plot ID
# - `habitat` (factor): CF = continuous forest, 1-ha = fragment
# - `year` (numeric): year of survey (surveys conducted in Feb)
# - `ht` (numeric): height in cm
# - `ht_prev` (numeric): height in cm in previous year
# - `shts` (integer): number of shoots
# - `shts_prev` (integer): number of shoots in prev year
# - `size` (numeric): shts * ht
# - `size_prev` (numeric): shts * ht in previous year
# - `log_size` (numeric): natural log of size
# - `log_size_prev` (numeric): natural log of size in prev year
# - `flwr` (integer): is the plant flowering? 1 = yes, 0 = no
# - `surv` (integer): 1 = alive, 0 = dead
# - `spei` (numeric): SPEI value for the month of the survey (Feb)
# - `spei_history` (c("matrix", "array")): a matrix-column containing SPEI history back 36 months for each observation
# - `L` (c("matrix", "array")): literally just 0:36


# survival example --------------------------------------------------------

m <- gam(surv ~
           s(log_size_prev) + # 1D smooth for non-linear effect of size in previous year
           te(spei_history, L, # tensor product 2D smooth for lagged SPEI effect
              bs = "cr"), # use "cr" basis for each marginal dimension
         family = binomial, # because response is 1 or 0
         data = ha,
         method = "REML")

summary(m)
# The lagged effect of SPEI is not quite significant with this data subset.

gratia::draw(m)
# Draws the penalized functions for log(size_prev) and spei_history * lag


# DLNM package ------------------------------------------------------------

# I used the `dlnm` package to fit these models.  It creates a "crossbasis function" which is almost exactly the same as the te() tensor product above. 

library(dlnm)

m2 <- gam(surv ~
           s(log_size_prev) + # 1D smooth for non-linear effect of size in previous year
           s(spei_history, L,
             bs = "cb", # use crossbasis smooth from dlnm package
             xt = list(bs = "cr")), # use "cr" basis for each marginal dimension
         family = binomial, # because response is 1 or 0
         data = ha,
         method = "REML")

summary(m2)
# gives slightly different p-value

draw(m2)
# surface also looks slightly different.  It looks more symmetrical around SPEI = 0.