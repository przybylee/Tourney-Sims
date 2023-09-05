entries <- 16
round <- 1

build_round <- function(round, entries){
  matches <- data.frame(round = round, id = 1:(entries/2)) %>% 
    as_tibble() %>% 
    mutate(pos1 = id,
           pos2 = entries + 1 - id
           )
  
  return(matches)
}

build_bracket <- function(rounds = NULL, entries = 16){
  if(is.null(rounds)){rounds <- ceiling(log2(entries))}
  
  
}