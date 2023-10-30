# library(dplyr)
# #Example entries
# entries <- data.frame(a_serve = rnorm(16, sd = 0.5)) %>% 
#   as_tibble() %>% 
#   arrange(-a_serve) %>% 
#   mutate(a_return = a_serve + 0.05,
#          seed = 1:16
#          ) %>% 
#   select(seed, everything())
#   
# entries

single_elim_1x <- R6::R6Class(
  "single_elimination_singles_tournament",
  
  public = list(
    entries = NULL,
    matches = NULL,
    results = NULL,
    initialize = function(entries){
      rounds = log2(nrow(entries))
      
      if(rounds != ceiling(rounds)){
        rounds <- ceiling(rounds)
        null_seeds <- (nrow(entries) + 1):(2^rounds)
        null_entries <- data.frame(seed = null_seeds) %>% 
          mutate(a_serve = NA,
                 a_return = NA
                 ) 
        
        entries <- bind_rows(entries, null_entries)
      }
      
      n_entries = nrow(entries)
      
      matches = data.frame(id = 1:(n_entries/2)) %>% 
        mutate(round = 1, 
               side = 1
               ) %>% 
        as_tibble()
      
    }
      #Use purrr::pmap to play all the matches, set output
  )
  
  
)