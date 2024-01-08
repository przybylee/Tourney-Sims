library(devtools)
load_all()

# Draw some players
entries <- draw_singles_entries(12)

entries

tourney <- single_elim_1x$new(entries)
tourney

names(tourney)
tourney$rounds

#Print the standings
tourney$standings

#Play the first round
tourney$play_round_and_update_standings()

#Print the scores
tourney$match_results

results <- tourney$play_tournament_report_results()

results
tourney$match_results %>% filter(round == 4)

#Play the tournament 100 times
sim_tourney <- function(id, entries){
  print(glue::glue("Simulating tournament {id}"))
  tourney <- single_elim_1x$new(entries)
  output <- tourney$play_tournament_report_results() %>% 
    mutate(trial = id) %>% 
    select(trial, everything())
  
  return(output)
}
  
sim_tourney(1, entries)

trials <- 1:100

trial_results <- purrr::map_dfr(trials, ~sim_tourney(.x, entries))

trial_results %>% 
  group_by(seed) %>% 
  summarise(pos1 = sum(pos == 1),
            pos2 = sum(pos == 2),
            pos3 = sum(pos == 3),
            pos4 = sum(pos == 4),
            pos5 = sum(pos == 5),
            pos6 = sum(pos == 6),
            pos7 = sum(pos == 7),
            pos8 = sum(pos == 8),
            pos9 = sum(pos == 9)
            
  )

