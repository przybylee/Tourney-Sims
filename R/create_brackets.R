#' Draw a random field of singles players
#'
#' @param n_players Integer, number of players in the field 
#' @param return_effect Numeric, the log odds advantage for the returner
#' @param std_player The standard deviation of player effects
#' @param std_return The standard deviation of player return effects
#'
#' @description This function generates a field of random players for a singles
#' tournament.  Each player gets a serve rating and a return rating.  We assume 
#' the serve ratings for each player \eqn{j} are IID  with
#' \eqn{s_j = p_j ~ N(0, \sigma_p^2).} 
#' 
#' The return ratings are given by
#'  
#' \eqn{r_j = p_j + \rho + e_j}
#'  
#' where \eqn{\rho} is the fixed effect that represents the advantage for the 
#' returning player and each \eqn{e_j\sim N(0, \sigma_r^2)} are IID with 
#' \eqn{\sigma_r} as the standard deviation of player return effects.  These 
#' ratings are used by `play_singles_match()` to simulate any of the tournament 
#' matches.  Players are seeded based on the sum of their serve and return 
#' ratings.
#' 
#' @return A tibble with columns `seed`, `serve`, and `return`
#' @export
#'
#' @examples
#' #NONE
draw_singles_entries <- function(
    n_players = 8,
    return_effect = 0.1,
    std_player = 0.5,
    std_return = 0.01
    ){
  serve <- stats::rnorm(n_players, 0, std_player)
  return <- serve + stats::rnorm(n_players, return_effect, std_return)
  
  entries <- data.frame(
    serve = serve, 
    return = return
  ) %>% 
    arrange(-(serve + return)) %>% 
    mutate(seed = 1:n_players) %>% 
    select(seed, serve, return) %>% 
    as_tibble()
  
  return(entries)
}
