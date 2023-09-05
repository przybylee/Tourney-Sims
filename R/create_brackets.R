# entries <- 16
# round <- 1

#' Created match rows for a round
#'
#' @param round Integer indciating the round of the tournament
#' @param entries Integer indicating the number of entries remaining in the 
#' tournament
#'
#' @return A dataframe with 4 fieldws
#' @export
#'
#' @examples
#' #none
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