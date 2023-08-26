#' Simulate a single rally
#'
#' @param r1 Numeric, log odds rating of player1
#' @param r2 Numeric, log odds rating of player2
#' @param n Integer indicating the number of rallies to simulate
#'
#' @return A vector of integers indicating which player won the simulated 
#' rallies
#' @export
#'
#' @examples
#' play_rally(5, 1, 10)
play_rally <- function(r1 = 0, r2 = 0, n = 1){
  p <- logistic(r1, r2)
  winner <- ifelse(rbinom(n, 1, p) > 0, 1, 2)
  return(winner)
}

#' Simulate a singles game
#'
#' @param a_serve Numeric indicating the logodds rating of each player winning
#' a rally while serving
#' @param a_return Numeric indicating the logodds rating of each player winning 
#' a rally while receiving
#' @param f_score Integer indicating the number of points required to win the 
#' game
#' @param server Integer indicating which player serves first
#'
#' @return Numeric, 2-d indicating each player's score
#' @export
#'
#' @examples
#' #None
play_singles_game <- function(
    a_serve = c(0, 0),
    a_return = c(a_serve[1] + 0.01, a_serve[2] + 0.01),
    f_score = 11, 
    server = NA
){
  if(is.na(server)) server <- sample(1:2, size = 1)
  scores <- c(0,0)
  a <- matrix(c(a_serve, a_return), nrow = 2, ncol = 2)
  while(max(scores) < 11 | abs(scores[1] - scores[2]) < 2){
    #specify rally win probs
    a1 <- a[1, ifelse(server == 1, 1, 2)]
    a2 <- a[2, ifelse(server == 2, 1, 2)]
    #play the rally
    rally_outcome <- play_rally(a1, a2)
    #assign the point and server for the next rally
    if(rally_outcome == server){
      scores[rally_outcome] <- scores[rally_outcome] + 1
    }else{
      server <- rally_outcome
    }
    
  }
  return(scores)
}

#' Simulate a singles match
#'
#'
#' @param a_serve Numeric indicating the logodds rating of each player winning
#' a rally while serving
#' @param a_return Numeric indicating the logodds rating of each player winning 
#' a rally while receiving
#' @param g_max Integer indciating the maximum number of games in the match
#' @param f_score Integer indicating the minimum number of points required to
#' win a game
#'
#' @return A dataframe summarizing the simulated match
#' @export
#'
#' @examples
#' #none
play_singles_match <- function(
    a_serve = c(0, 0), 
    a_return = c(a_serve[1] + 0.01, a_serve[2] + 0.01), 
    g_max = 3, 
    f_score = 11){
  assertthat::assert_that(g_max %% 2 == 1, 
                          msg = "check g_max; invalid number of games"
  )
  #Choose who serves first
  server <- sample(1:2, 1)
  #initialize games and scores
  games <- c(0,0)
  scores <- ""
  while(max(games) < (g_max + 1)/2){
    game_i <- play_singles_game(a_serve, a_return, f_score, server = server)
    winner <- which.max(game_i)
    games[winner] <- games[winner] + 1
    scores <- paste(scores, paste(game_i[1], game_i[2], sep = "-"), " ", sep = "")
    server <- ifelse(server == 1, 2, 1)
  }
  
  winner <- which.max(games)
  #Drop the last space from scores
  #scores <-
  
  output <- data.frame(winner = winner, 
                       scores = scores, 
                       games1 = games[1], 
                       games2 = games[2],
                       a1serve = a_serve[1],
                       a1return = a_return[1],
                       a2serve = a_serve[2],
                       a2return = a_return[2])
  return(output)
}