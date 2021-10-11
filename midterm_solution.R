# Probability constants from problem description
OFFSIDE_PROB = 0.15
LONGSHOT_PROB = 0.3
PASS_TO_SECTOR2_PROB = 0.6
GOALIE_SCORE = 7

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
    game_state <- list("team_possession"=1, "ball_in_sector"=2, "score"=c(0,0), "cur_min"=1, "goalie_with_ball"=FALSE)
  } else {
    game_state <- list("team_possession"=2, "ball_in_sector"=3, "score"=c(0,0), "cur_min"=1, "goalie_with_ball"=FALSE)
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
  
  # Try a long shot
  if (game_state$ball_in_sector == 3 & game_state$team_possession == 1 & runif(1,0,1) <= LONGSHOT_PROB) {
    offensive_player = lst_sample(team1$player_number,1,T)
    cat('Player ',offensive_player,' from team 1 takes a shot from sector 3')
    
    # Offense gets a penalty of min of two rolls b/c its a long shot
    offense_score = min(runif(2,1,team_1$offense_score[offense_player]))
    defense_score = runif(1,1,GOALIE_SCORE)
      
    if (offense_score > defense_score) {
      game_state$score[1] = game_state$score[1] + 1
      cat('Goal at minute ', game_state$cur_min, '!! The score is now ', game_state$score)
    }
    else {
      cat("... and he misses. Team 2 is now in possession")
    }

    game_state$goalie_with_ball = TRUE
    game_state$team_possession = 2
    game_state$cur_min = game_state$cur_min + 1
    game_state$ball_in_sector = 4
    return(game_state)
  }
  # Otherwise try to pass the ball forward
  else{
    offense_roll = ruinf(1,1,offense_score(team_1,team_2,game_state))
    defense_roll = runif(1,1,defense_score(team_1,team_2,game_state))
      
    if (offense_roll > defense_roll) {
      # Ball moves forward a sector
      game_state$ball_in_sector = 4  
      if (runif(1,0,1) > OFFSIDE_PROB) {
        # Offside penalty so team 2 gains possession
        game_state$team_possession = 2
      }
    }
    return(game_state)

  }
  # repeat for team 2 
  if (game_state$ball_in_sector == 2 & game_state$team_possession == 2 & runif(1,0,1) <= LONGSHOT_PROB) {
    offensive_player = lst_sample(team2$player_number,1,T)
    cat('Player ',offensive_player,' from team 2 takes a shot from sector 2')
    
    offense_score = min(runif(2,1,team_2$offense_score[offense_player]))
    defense_score = runif(1,1,GOALIE_SCORE)

    if (offense_score > defense_score) {
      game_state$score[2] = game_state$score[2] + 1
      cat('Goal at minute ', game_state$cur_min, '!! The score is now ', game_state$score)
    }
    else {
      cat("... and he misses. Team 1 is now in possession")
    }
    
    game_state$goalie_with_ball = TRUE
    game_state$team_possession = 1
    game_state$cur_min = game_state$cur_min + 1
    game_state$ball_in_sector = 1
    return(game_state)
  }
  else {
    offense_roll = ruinf(1,1,offense_score(team_1,team_2,game_state))
    defense_roll = runif(1,1,defense_score(team_1,team_2,game_state))
    
    if (offense_roll > defense_roll) {
      # Ball moves forward a sector
      game_state$ball_in_sector = 4  
      if (runif(1,0,1) > OFFSIDE_PROB) {
        # Offside penalty so team 2 gains possession
        game_state$team_possession = 2
      }
    }
    return(game_state)
  }
}

# Define helper functions below
lst_sample <- function(x, size, replace = F) {
  if (length(x) == 1) return(rep(x, size))
  base::sample(x, size = size, replace = replace)
}

offense_score <- function(team_1,team_2,game_state){
  if (game_state$team_possession==1){
    N <- which(team_1$sector == game_state$ball_in_sector)
    score = 0
    for (i in N){
      score <- score + team_1$offense_score[i]
    }
  } else {
    N <- which(team_2$sector == game_state$ball_in_sector)
    score = 0
    for (i in N){
      score <- score + team_2$offense_score[i]
    }
  }
  return(score)
}

defense_score <- function(team_1,team_2,game_state){
  if (game_state$team_possession==2){
    N <- which(team_1$sector == game_state$ball_in_sector)
    score = 0
    for (i in N){
      score <- score + team_1$defense_score[i]
    }
  } else {
    N <- which(team_2$sector == game_state$ball_in_sector)
    score = 0
    for (i in N){
      score <- score + team_2$defense_score[i]
    }
  }
  return(score)
}