## code to prepare `sample_matches` dataset goes here
?play_singles_match
library(dplyr)

sample_matches <- data.frame(
  g_max = rep(3, 10),
  f_score = 11,
  serve1 = stats::rnorm(10, 0, 0.1), 
  serve2 = stats::rnorm(10, 0, 0.1) 
  
           ) %>% 
  mutate(return1 = serve1 + stats::rnorm(10, 0.01, 0.05),
         return2 = serve2 + stats::rnorm(10, 0.01, 0.05)
  ) %>% 
  as_tibble()

sample_matches

usethis::use_data(sample_matches, overwrite = TRUE)
