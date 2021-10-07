# Probability constants from problem description
OFFSIDE_PROB = 0.15
LONGSHOT_PROB = 0.3
PASS_TO_SECTOR2_PROB = 0.6

# Run a monte carlo simulation
monte_carlo_soccer <- function(N) {
  # in theory we could run N simulations of the game here and give final prob
  # in practice we don't need to for the assignment- 1 game loop is enough
  game(minutes = 90)
}

# Simulate 1 soccer game
game <- function(minutes) {
  # Define any global vars for the game here
  team_1 <- list("player_number"=(1:10), "sector"=c(1,1,1,2,2,2,2,3,3,4), "offense_score"=c(1:10), "defense_score"=c(10:1))
  team_2 <- list("player_number"=(1:10), "sector"=c(4,4,3,3,3,2,2,2,1,1), "offense_score"=c(1:10), "defense_score"=c(10:1))
  
  # Flip a coin to determine initial game state
  if (runif(1, min=0, max=1) > 0.5) {
    game_state <- list("team_possession"=1, "ball_in_sector"=2, "score"=c(0,0))
  } else {
    game_state <- list("team_possession"=2, "ball_in_sector"=3, "score"=c(0,0))
  }
  
  for (i in 1:minutes) {
    # simulates a minute of gameplay and updates the game's state accordingly
    game_state <- minute(team_1, team_2, game_state)
  }

  # Print Score and Winner- Zoe
}

# Simulate 1 minute of a game
minute <- function(team_1, team_2, game_state) {
  #Goalie possession stuff- Zoe
  
  #Defense tries to take the ball- Cameron
    #End if successful
  #If in attacking sector 4- Cameron
    #close shot
  #Long shot (attacking sector 3)- Vikram
    #offsides
  #Pass ball forward- Vikram  
  
  
  return(game_state) 
}

# Define helper functions below
lst_sample <- function(x, size, replace = F) {
  if (length(x) == 1) return(rep(x, size))
  base::sample(x, size = size, replace = replace)
}