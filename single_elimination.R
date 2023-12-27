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

id_cols <- c("round", "match", "seed1", "pos1", "seed2", "pos2")
#Function to build next round of matches
get_next_round_matches <- function(standings, round, N_matches){
  round_matches <- data.frame(
    round = round,
    match = 1:N_matches
  ) %>% 
    mutate(pos1 = match, pos2 = 2*N_matches + 1 - match) %>% 
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
  
  return(round_matches)
}

#Update standings
update_standings <- function(standings,  prev_results){
  output <- standings %>% 
    left_join(select(prev_results, pos_winner, match) %>% mutate(win = 1), 
              by = c("pos" = "pos_winner")
    ) %>% 
    left_join(select(prev_results, pos_loser) %>% mutate(lose = 1),
              by = c("pos" = "pos_loser") 
    ) %>% 
    tidyr::replace_na(list(win = 0, lose = 0)) %>% 
    mutate(pos = case_when(win == 1 ~ match,
                           lose == 1 ~ max(match, na.rm = TRUE) + 1,
                           TRUE ~ pos
                           ),
           wins = wins + win, 
           losses = losses + lose
           ) %>%
    select(-win, -lose, -match) %>% 
    arrange(pos, seed)
  
  return(output)
}

#If the number of matches played is greater than 1 when it should be one, award medals
update_standings_final <- function(standings, prev_results){
  output <- standings %>%
    left_join(select(prev_results, pos_winner, match) %>% mutate(win = 1), 
              by = c("pos" = "pos_winner")
    ) %>% 
    left_join(select(prev_results, pos_loser, match) %>% mutate(lose = 1),
              by = c("pos" = "pos_loser") 
    ) %>% 
    tidyr::replace_na(list(win = 0, lose = 0)) %>% 
    mutate(match = coalesce(match.x, match.y),
           wins = wins + win, 
           losses = losses + lose,
           pos2 = ifelse(pos > 4, pos, order(-wins, losses, match))
           ) %>% 
    select(-contains("match"), -win, -lose, -pos2)
  
  return(output)
}
  
play_round_and_update_standings <- function(
    standings, 
    round = NA_real_, 
    N_matches_round = ceiling(sum(standings$losses == 0)/2)
){
  #Set up the matches based on the standings
  round_matches <- get_next_round_matches(standings, round, N_matches_round)
  
  #If final round, set up the bronze medal match
  if(N_matches_round == 1){
    standings <- mutate(standings, pos = ifelse(pos == 3, row_number(), pos))
    
    bronze_match <- data.frame(
      round = round,
      match = 2
    ) %>% 
      mutate(pos1 = 3, pos2 = 4) %>% 
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
    
    round_matches <- rbind(round_matches, bronze_match)
  }
  
  #Play all the matches in the round
  r_results <- play_many_singles_matches(round_matches, extra_cols = id_cols) %>% 
    mutate(pos_winner = ifelse(winner == 1, pos1, pos2),
           pos_loser = ifelse(winner == 1, pos2, pos1)
           )
  
  #Update the standings
  if(N_matches_round == 1){
    standings <- update_standings_final(standings, r_results)
  }else{
    standings <- update_standings(standings, r_results)
    #Update number of matches
    N_matches_round <- N_matches_round/2
    #Update round
    round <- round + 1
  }
  
}

  
###############################
single_elim_1x <- R6::R6Class(
  "single_elimination_singles_tournament",
  
  public = list(
    entries = NULL,
    
    matches = NULL,
    
    results = NULL,
    
    standings = NULL,
    
    #Use the entries to create standings
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
