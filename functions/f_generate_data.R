library(tidyverse)
library(lubridate)

f_generate_data <- function() {
  
  tests <- tibble(
    test_name = c("Test A", "Test B", "Test C"), 
    start_date = c("2019-05-01", "2019-09-01", "2020-02-01")
    ) %>% 
    mutate(
      start_date = as.Date(start_date),
      end_date = start_date + days(13)
    )
  
  variations <- tibble(variation = c("Variation A", "Variation B", "Control"))
  
  df_params <- crossing(tests, variations) %>% 
    group_by(test_name) %>% 
    mutate(trials_param = round(rnorm(1, mean = 200, sd = 10))) %>% 
    group_by(test_name, variation) %>% 
    mutate(success_param = round(rnorm(1, mean = 40, sd = 4))) %>% 
    ungroup()
  
  df_daily <- df_params %>% 
    mutate(
      date = map2(start_date, end_date, seq, "day")
    ) %>% 
    unnest()
  
  df_daily <- df_daily %>% 
    group_by(test_name, date, variation) %>% 
    mutate(
      trials = round(rpois(1, trials_param)), 
      successes = round(rpois(1, success_param))) %>% 
    select(-trials_param, -success_param) %>% 
    ungroup()
  
  return(df_daily)
}
