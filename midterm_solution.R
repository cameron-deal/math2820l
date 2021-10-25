# Zoe, Cameron, Vikram

# Probability constants from problem description
OFFSIDE_PROB = 0.15
LONGSHOT_PROB = 0.3
PASS_TO_SECTOR2_PROB = 0.6
GOALIE_SCORE = 7

# Helper functions
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

# Simulate 1 minute of a game
minute <- function(team_1, team_2, game_state) {
  game_state$cur_minute = game_state$cur_minute + 1
  #cat("\nMinute", game_state$cur_minute, ": ")
  #cat(" Team Possession: ", game_state$team_possession, " ")
  #cat(" Ball_in_sector: ", game_state$ball_in_sector, " ")
  #Goalie possession stuff
  if (game_state$goalie_with_ball == TRUE){
    #pass to sector 2 with probability 0.6
    goalieKick = runif(1, 0, 1)
    if (goalieKick <= PASS_TO_SECTOR2_PROB & game_state$team_possession == 1){
      game_state$ball_in_sector = 2
    } 
    else if (goalieKick <= PASS_TO_SECTOR2_PROB & game_state$team_possession == 2){
      game_state$ball_in_sector = 3
    } 
    else { 
      #otherwise pass to sector (attacking) 3
      if (game_state$team_possession == 1){
        game_state$ball_in_sector = 3
      } else {
        game_state$ball_in_sector = 2
      }
     #defense tries to overtake ball
     offense_roll = runif(1,1,offense_score(team_1,team_2,game_state))
     defense_roll = runif(1,1,defense_score(team_1,team_2,game_state))
     #if successfully overtakes ball, change possession
     if (defense_roll > offense_roll) { 
        game_state$team_possession = 3 - game_state$team_possession
        cat('Intercept after the kick, team ', game_state$team_possession,
          ' is now in possession\n')
    }
    }
    game_state$goalie_with_ball = FALSE
  }
  
  #Defense tries to take the ball- Cameron
  off_sum <- offense_score(team_1,team_2,game_state)
  def_sum <- defense_score(team_1,team_2,game_state)
  #cat("off_sum = ", off_sum, "; def_sum = ", def_sum, "\n")
  off_team_roll <- runif(1, min=1, max=off_sum)
  def_team_roll <- runif(1, min=1, max=def_sum)
  #cat("off_team_roll = ", off_team_roll, "; def_team_roll = ", def_team_roll, "\n")
  if (def_team_roll>off_team_roll){
    game_state$team_possession = 3 - game_state$team_possession
    return(game_state)
  } 

  
  #If in attacking sector 4- Cameron
  #close shot- CHANGE the possession and sector variables
  if (game_state$team_possession==1 & game_state$ball_in_sector==4) {
    sector_offplayers <- team_1$offense_score[team_1$sector==game_state$ball_in_sector]
    off_score <- lst_sample(sector_offplayers, 1, replace = T)
    off_team_roll <- runif(1, min=1, max=off_score)
    goalie_roll <- runif(1, min=1, max=GOALIE_SCORE)
    cat("Player", team_1$player_number[team_1$offense_score==off_score], "from team 1 takes a shot from sector 4.\n")
  
    if(off_team_roll>goalie_roll) {
      game_state$score[1] <- game_state$score[1] + 1
      cat("It's a goal on minute", game_state$cur_minute, "!!! The score is now", game_state$score[1], ":", game_state$score[2], '\n')
      game_state$team_possession <- 2
      #game_state$cur_minute <- game_state$cur_minute + 1
      return(game_state)
    } else {
      game_state$team_possession <- 2
      #game_state$cur_minute <- game_state$cur_minute + 1
      cat("... and he misses. Team 2 is now in possession\n")
      game_state$goalie_with_ball = TRUE
      game_state$ball_in_sector = 4
      return(game_state)
    }
  } 
  
  if (game_state$team_possession==2 & game_state$ball_in_sector==1) {
    sector_offplayers <- team_2$offense_score[team_2$sector==game_state$ball_in_sector]
    off_score <- lst_sample(sector_offplayers, 1, replace = T)
    off_team_roll <- runif(1, min=1, max=off_score)
    goalie_roll <- runif(1, min=1, max=7)
    
    cat("Player", team_2$player_number[team_2$offense_score==off_score], "from team 2 takes a shot from sector 1.\n")
    
    if(off_team_roll > goalie_roll) {
      game_state$score[2] = game_state$score[2] + 1
      cat("It's a goal on minute", game_state$cur_minute, "!!! The score is now", game_state$score[1], ":", game_state$score[2], '\n')
    }
    else {
      cat("... and he misses. Team 2 is now in possession\n")
    }
    
    game_state$team_possession <- 1
    #game_state$cur_minute <- game_state$cur_minute + 1
    game_state$goalie_with_ball = TRUE
    game_state$ball_in_sector = 1
    return(game_state)
    
  } 
  #else {
   # if (game_state$team_possession==1) {game_state$team_possession==2} 
   # else {game_state$team_possession==1}
    #return(game_state)
  #}
  
  # Try a long shot
  if (game_state$ball_in_sector == 3 & game_state$team_possession == 1 
      & runif(1,0,1) <= LONGSHOT_PROB) {
    offensive_player = lst_sample(team_1$player_number[team_1$sector == 3],1,T)
    cat('Player ',offensive_player,' from team 1 takes a shot from sector 3\n')
    
    # Offense gets a penalty of min of two rolls b/c its a long shot
    offense_score = min(runif(2,1,team_1$offense_score[offensive_player]))
    defense_score = runif(1,1,GOALIE_SCORE)
      
    if (offense_score > defense_score) {
      game_state$score[1] = game_state$score[1] + 1
      cat("It's a goal at minute ", game_state$cur_minute, '!!! The score is now ', game_state$score, '\n')
    }
    else {
      cat("... and he misses. Team 2 is now in possession\n")
    }

    game_state$goalie_with_ball = TRUE
    game_state$team_possession = 2
    game_state$ball_in_sector = 4
    return(game_state)
  }
  # Otherwise try to pass the ball forward
  else if (game_state$team_possession == 1) {
    offense_roll = runif(1,1,offense_score(team_1,team_2,game_state))
    defense_roll = runif(1,1,defense_score(team_1,team_2,game_state))
    #cat("offense_roll: ", offense_roll, ";defense_roll: ", defense_roll, '\n')
      
    if (offense_roll > defense_roll) {
      if (game_state$ball_in_sector == 3 & runif(1,0,1) <= OFFSIDE_PROB) {
        # Offside penalty so team 2 gains possession
        game_state$team_possession = 2
        cat("Team 1 is offsides, team 2 now has the ball\n")
        game_state$ball_in_sector = 4
        return(game_state)
      }
      # Ball moves forward a sector
      game_state$ball_in_sector = game_state$ball_in_sector + 1 
    }
    return(game_state)

  }
  # repeat for team 2 
  if (game_state$ball_in_sector == 2 & game_state$team_possession == 2 
      & runif(1,0,1) <= LONGSHOT_PROB) {
    offensive_player = lst_sample(team_2$player_number[team_1$sector == 2],1,T)
    cat('Player ',offensive_player,' from team 2 takes a shot from sector 2\n')
    
    offense_score = min(runif(2,1,team_2$offense_score[offensive_player]))
    defense_score = runif(1,1,GOALIE_SCORE)

    if (offense_score > defense_score) {
      game_state$score[2] = game_state$score[2] + 1
      cat('Goal at minute ', game_state$cur_minute, '!! The score is now ', game_state$score, '\n')
    }
    else {
      cat("... and he misses. Team 1 is now in possession\n")
    }
    
    game_state$goalie_with_ball = TRUE
    game_state$team_possession = 1
    #game_state$cur_minute = game_state$cur_minute + 1
    game_state$ball_in_sector = 1
    return(game_state)
  }
  else if(game_state$team_possession == 2) {
    offense_roll = runif(1,1,offense_score(team_1,team_2,game_state))
    defense_roll = runif(1,1,defense_score(team_1,team_2,game_state))
    #cat("offense_roll= ", offense_roll, ";defense_roll= ", defense_roll, " ")
    
    if (offense_roll > defense_roll) {
      if (game_state$ball_in_sector == 2 & runif(1,0,1) <= OFFSIDE_PROB) {
        cat("Team 2 is offsides, team 1 now has the ball\n")
        # Offside penalty so team 1 gains possession
        game_state$team_possession = 1
        game_state$ball_in_sector = 1
        return(game_state)
      }
      # Ball moves forward a sector
      game_state$ball_in_sector = game_state$ball_in_sector - 1
    }
  }
  return(game_state)
}

# Simulate 1 soccer game
game <- function(minutes) {
  # Define any global vars for the game here
  team_1 <- list("player_number"=(1:10), "sector"=c(1,1,1,2,2,2,2,3,3,4), "offense_score"=c(1:10), "defense_score"=c(10:1))
  team_2 <- list("player_number"=(1:10), "sector"=c(4,4,3,3,3,2,2,2,1,1), "offense_score"=c(1:10), "defense_score"=c(10:1))
  
  # Flip a coin to determine initial game state
  if (runif(1, min=0, max=1) > 0.5) {
    game_state <- list("team_possession"=1, "ball_in_sector"=2, "score"=c(0,0), "cur_minute"=0, "goalie_with_ball"=FALSE)
  } else {
    game_state <- list("team_possession"=2, "ball_in_sector"=3, "score"=c(0,0), "cur_minute"=0, "goalie_with_ball"=FALSE)
  }
  
  for (i in 0:minutes) {
    # simulates a minute of gameplay and updates the game's state accordingly
    game_state <- minute(team_1, team_2, game_state)
  }
  
  # Print Score and Winner
  cat('End of the match. The score is  ')
  cat(game_state$score[1], ' : ', game_state$score[2], "\n")
  if (game_state$score[1] == game_state$score[2]){
    cat('It\'s a draw!')
  } else if (game_state$score[1] > game_state$score[2]){
    cat('Team 1 wins!')
  } else {
    cat('Team 2 wins!')
  }
}

# Run a monte carlo simulation
monte_carlo_soccer <- function(N) {
  # in theory we could run N simulations of the game here and give final prob
  # in practice we don't need to for the assignment- 1 game loop is enough
  game(minutes = 90)
}

monte_carlo_soccer();
