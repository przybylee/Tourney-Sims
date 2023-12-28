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
    #' @field field The players in the tournament
    field = NULL,
    
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
    #' @export
    #'
    #' @examples #NONE
    initialize = function(entries){
      # #Draw entries if not specified
      # if(is.null(entries)){
      #   entries <- draw_singles_entries()
      # }
      
      #Maybe add a distortion for the seeding here?
      
      self$field <- data.frame(entries) %>% as_tibble()
      # self$rounds <- ceiling(log2(nrow(self$entries)))
      # self$round <- 1
      # self$N_matches_round <- 2^(self$rounds - 1)
      # 
      # #Initialize standings
      # self$standings <-  self$entries %>% 
      #   left_join(data.frame(seed = 1:2^self$rounds), .) %>% 
      #   mutate(pos = seed, wins = 0, losses = 0) 
      
    }
    
  )
  
  
)
