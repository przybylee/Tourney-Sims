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
library(dplyr)
library(devtools)
load_all()

#parameters for the entries and skill levels
n_entries <- 8
std_serve <- 0.5
std_return <- 0.01

serve <- rnorm(n_entries, 0, std_serve)
return <- serve + rnorm(n_entries, 0.1, std_return)

entries <- data.frame(
  serve = serve, 
  return = return
) %>% 
  arrange(-(serve + return)) %>% 
  mutate(seed = 1:n_entries) %>% 
  select(seed, serve, return) %>% 
  as_tibble()

entries
N <- nrow(entries)
rounds <- ceiling(log2(N))

standings <- entries %>% 
  mutate(pos = seed, wins = 0, losses = 0)
round <- 1
N_matches_round <- 2^(rounds - 1)

round_matches <- data.frame(
  round = round,
  match = 1:N_matches_round
) %>% 
  mutate(pos1 = match, pos2 = 2*N_matches_round + 1 - match) %>% 
  inner_join(standings %>% 
               select(pos, seed, serve, return) %>% 
               rename_with(~paste0(.x, "1"))
             ) %>% 
  inner_join(standings %>%
               select(pos, seed, serve, return) %>% 
               rename_with(~paste0(.x, "2"))
  ) %>% 
  as_tibble() %>% 
  mutate(g_max = 3, f_score = 11)

id_cols <- c("round", "match", "seed1", "pos1", "seed2", "pos2")
#Play matches in round_matches
r_results <- play_many_singles_matches(round_matches, extra_cols = id_cols) %>% 
  mutate(pos_winner = ifelse(winner == 1, pos1, pos2),
         pos_loser = ifelse(winner == 1, pos2, pos1)
         )

#Update standings
standins <- standings %>% 
  left_join(select(r_results, pos_winner, match) %>% mutate(win = 1), 
            by = c("pos" = "pos_winner")
            ) %>% 
  left_join(select(r_results, pos_loser) %>% mutate(lose = 1),
            by = c("pos" = "pos_loser") 
            ) %>% 
  tidyr::replace_na(list(win = 0, lose = 0))
  
#Set up next round

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