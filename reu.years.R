library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(tictoc)
library(zoo)
library(purrr)
ha <- read_rds(here("data", "model_data.rds")) # reading in data
ha <- ha %>% mutate(spei = spei_history[,1], .before = spei_history)
str(ha)

combo_1yrs <- rollapply(1999:2009, by = 1, width = 1, FUN = as.numeric)
all_dfs_1yr<- map(combo_1yrs, ~ha %>% filter(year==.x))
#get_model_stats(all_dfs_1yr[[1]])
one.years <- get_results(all_dfs_1yr)


combo_2yrs <- rollapply(1999:2009, by = 1, width = 2, FUN = as.numeric)
x <- nrow(combo_2yrs)
all_dfs_2yr <- map(1:x, ~ha %>% filter(year %in% combo_2yrs[.x,])) 
#get_model_stats(all_dfs_2yr[[1]])
two.years <- get_results(all_dfs_2yr)

combo_3yrs <- rollapply(1999:2009, by = 1, width = 3, FUN = as.numeric)
x <- nrow(combo_3yrs)
all_dfs_3yr <- map(1:x, ~ha %>% filter(year %in% combo_3yrs[.x,])) 
#get_model_stats(all_dfs_3yr[[1]])
three.years <- get_results(all_dfs_3yr)

combo_4yrs <- rollapply(1999:2009, by = 1, width = 4, FUN = as.numeric)
x <- nrow(combo_4yrs)
all_dfs_4yr <- map(1:x, ~ha %>% filter(year %in% combo_4yrs[.x,])) 
#get_model_stats(all_dfs_4yr[[1]])
four.years <- get_results(all_dfs_4yr)

combo_5yrs <- rollapply(1999:2009, by = 1, width = 5, FUN = as.numeric)
x <- nrow(combo_5yrs)
all_dfs_5yr <- map(1:x, ~ha %>% filter(year %in% combo_5yrs[.x,])) 
#get_model_stats(all_dfs_5yr[[1]])
five.years <- get_results(all_dfs_5yr)

combo_6yrs <- rollapply(1999:2009, by = 1, width = 6, FUN = as.numeric)
x <- nrow(combo_6yrs)
all_dfs_6yr <- map(1:x, ~ha %>% filter(year %in% combo_6yrs[.x,]))
#get_model_stats(all_dfs_6yr[[1]])
six.years <- get_results(all_dfs_6yr)

combo_7yrs <- rollapply(1999:2009, by = 1, width = 7, FUN = as.numeric)
x <- nrow(combo_7yrs)
all_dfs_7yr <- map(1:x, ~ha %>% filter(year %in% combo_7yrs[.x,])) 
#get_model_stats(all_dfs_7yr[[1]])
seven.years <- get_results(all_dfs_7yr)

combo_8yrs <- rollapply(1999:2009, by = 1, width = 8, FUN = as.numeric)
x <- nrow(combo_8yrs)
all_dfs_8yr <- map(1:x, ~ha %>% filter(year %in% combo_8yrs[.x,])) 
#get_model_stats(all_dfs_8yr[[1]])
eight.years <- get_results(all_dfs_8yr)

combo_9yrs <- rollapply(1999:2009, by = 1, width = 9, FUN = as.numeric)
x <- nrow(combo_9yrs)
all_dfs_9yr <- map(1:x, ~ha %>% filter(year %in% combo_9yrs[.x,])) 
#get_model_stats(all_dfs_9yr[[1]])
nine.years <- get_results(all_dfs_9yr)

one.years$Year <- "1"
two.years$Year <- "2"
three.years$Year <- "3"
four.years$Year <- "4"
five.years$Year <- "5"
six.years$Year <- "6"
seven.years$Year <- "7"
eight.years$Year <- "8"
nine.years$Year <- "9"

fulldata.years <- rbind(one.years,two.years,three.years,four.years,five.years,six.years,seven.years,
               eight.years,nine.years)

r2boxplot <- ggplot(fulldata.years, aes(x = Year, y= r2))+ geom_boxplot(aes(fill=Year), 
            outlier.color = "red") + scale_fill_brewer(palette="OrRd") + 
            labs(title = "Figure 1", x = "Years in Sequence", y = "R Squared") + 
            theme_classic() + theme(legend.position = "none", axis.text.x = element_text(angle = 45,hjust=1)) 
rmseboxplot <- ggplot(fulldata.years, aes(x = Year, y= rmse))+ geom_boxplot(aes(fill=Year), 
            outlier.color = "red") + scale_fill_brewer(palette="OrRd") + 
            labs(title = "Figure 1", x = "Years in Sequence", y = "Root Mean Squared Error") + 
            theme_classic() + theme(legend.position = "none", axis.text.x = element_text(angle = 45,hjust=1))
edfboxplot <- ggplot(fulldata.years, aes(x = Year, y= edf))+ geom_boxplot(aes(fill=Year), 
            outlier.color = "red") + scale_fill_brewer(palette="OrRd") + 
            labs(title = "Figure 1", x = "Years in Sequence", y = "Estimated Degrees of Freedom") + 
            theme_classic() + theme(legend.position = "none", axis.text.x = element_text(angle = 45,hjust=1))
pvalueboxplot <- ggplot(fulldata.years, aes(x = Year, y= pvalue))+ geom_boxplot(aes(fill=Year), 
                outlier.color = "red") + scale_fill_brewer(palette="OrRd") + 
                labs(title = "Figure 1", x = "Years in Sequence", y = "P Value") + 
                theme_classic() + theme(legend.position = "none", axis.text.x = element_text(angle = 45,hjust=1))

