#' Draw a random field of singles players
#'
#' @param n_players Integer, number of players in the field 
#' @param return_effect Numeric, the log odds advantage for the returner
#' @param std_player The standard deviation of player effects
#' @param std_return The standard deviation of player return effects
#'
#' @description This function generates a field of random players for a singles
#' tournament.  Each player gets a serve rating and a return rating.  We assume 
#' the serve ratings for each player $j$ are IID  
#' $s_j = p_j\sim N(0, \sigma_p^2)$. The return ratings are given by
#' \begin{equation}
#' r_j = p_j + \rho + e_j
#' \end{equation}
#' where $\rho$ is the fixed effect that represents the advantage for the 
#' returning player and each $e_j\sim N(0, \sigma_r^2)$ are IID with $\simga_r$
#' as the standard deviation of player return effects.  These ratings are used 
#' by `play_singles_match()` to simulate any of the tournament matches.
#' 
#' @return A tibble with columns `seed`, `serve`, and `return`
#' @export
#'
#' @examples
#' NONE
draw_singles_entries <- function(
    n_players = 8,
    return_effect = 0.1,
    std_serve = 0.5,
    std_return = 0.01
    ){
  serve <- rnorm(n_players, 0, std_serve)
  return <- serve + rnorm(n_players, return_effect, std_return)
  
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