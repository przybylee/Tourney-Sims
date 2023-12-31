#Environments for simulating single elimination tournaments

#' @title Single elimination tournament simulator
#' 
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#'  
single_elim_1x <- R6::R6Class(
  "single_elimination_singles_tournament",
  
  public = list(
    #' @field entries The players in the tournament
    entries = NULL,
    
    #' @field match_results Tibble describing the results from all matches played
    match_results = NULL,
    
    #' @field standings Tibble describing the current standings
    standings = NULL,
    
    #' @field round The current round of the tournament
    round = NULL,
    
    #' @field rounds The number of rounds in the tournament
    rounds = NULL,
    
    #' @field N_matches_round The number of matches in the current round
    N_matches_round = NULL,
    
    #' @field id_cols Column names to join by when setting the next round of matches
    id_cols = c("round", "match", "seed1", "pos1", "seed2", "pos2"),
    
    #Use the entries to create standings
    
    #' @description 
    #' Create a new instance of the single elimination singles R6 class
    #'
    #' @param entries A tibble with the entries in the tournament
    #'
    #' @return A new instance of the single elimination singles R6 class
    #'
    initialize = function(entries = NULL){
      #Draw entries if not specified
      if(is.null(entries)){
        entries <- draw_singles_entries()
      }
      
      #Maybe add a distortion for the seeding here?
      
      self$entries <- data.frame(entries) %>% as_tibble()
      self$rounds <- ceiling(log2(nrow(self$entries)))
      self$round <- 1
      self$N_matches_round <- 2^(self$rounds - 1)
       
      #Initialize standings
      self$standings <-  self$entries %>% 
        left_join(data.frame(seed = 1:2^self$rounds), .) %>% 
        mutate(pos = seed, wins = 0, losses = 0) %>% 
        as_tibble()
      
    },
    
    #' @description
    #' Create a tibble with the next round of matches to be played based on 
    #' the current round and standings
    #' 
    get_next_round_matches = function(){
      round_matches <- data.frame(
        round = self$round,
        match = 1:self$N_matches_round
      ) %>% 
        mutate(pos1 = match, pos2 = 2*self$N_matches_round + 1 - match) %>% 
        inner_join(self$standings %>% 
                     select(pos, seed, serve, return) %>% 
                     rename_with(~paste0(.x, "1"))
        ) %>% 
        inner_join(self$standings %>%
                     select(pos, seed, serve, return) %>% 
                     rename_with(~paste0(.x, "2"))
        ) %>% 
        as_tibble() %>% 
        mutate(g_max = 3, f_score = 11)
      
      return(round_matches)
    },
    
    #' @description
    #' Update the tournament standings based on match results from the last round
    #' @param prev_results Tibble with the results from the last round of matches
    #'
    update_standings = function(prev_results){
      self$standings <- self$standings %>% 
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
      
      return(invisible())
    },
    
    #' @description 
    #' Update standings after the final round is played
    #' @param prev_results Tibble with the results from the last round of matches
    #' 
    #If the number of matches played is greater than 1 when it should be one, award medals
    update_standings_final = function(prev_results){
      self$standings <- self$standings %>%
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
      
      return(invisible())
    },
    
    #' @description Play round and update the standings
    #'
    play_round_and_update_standings = function(){
      
      round_matches <- self$get_next_round_matches()
      #If this is the last round, add a bronze medal match
      if(N_matches_round == 1){
        bronze_match <- standings %>% 
          filter(pos == 3) %>% 
          inner_join(standings, 
                     by = "pos", 
                     suffix = c("1", "2"), 
                     relationship = "many-to-many"
                     ) %>% 
          filter(seed1 > seed2) %>% 
          mutate(g_max = 3, f_score = 11, pos2 = 4, 
                 round = self$round, match = 2
                 ) %>% 
          select(round, match, pos1 = pos, pos2, seed1, seed2, 
                 serve1, return1, serve2, return2, g_max, f_score
                 )
        
        round_matches <- bind_rows(round_matches, bronze_match)
      }
      
      round_results <- round_matches %>% 
        play_many_singles_matches(extra_cols = self$id_cols) %>% 
        mutate(pos_winner = ifelse(winner == 1, pos1, pos2),
               pos_loser = ifelse(winner == 1, pos2, pos1)
        )
      
      #If this is not the last round update the standings as normal
      if(N_matches_round > 1){
        self$update_standings(round_results)
      } else {
        self$update_standings_final(round_results)
      }
      
      #Bind match results
      self$match_results <- bind_rows(self$match_results, round_results)
      
      #Update round and number of matches
      self$round <- self$round + 1
      self$N_matches_round <- self$N_matches_round/2
      
      return(invisible())
    }
    
  )
  
)
