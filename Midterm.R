#####################################
## Name: Cameron Deal
## Assignment: Math 2820L Midterm
## Date Updated: 10/05/2021
#####################################

###Create Player Score Lists
team_1 <- list("player_number"=(1:10), "sector"=c(1,1,1,2,2,2,2,3,3,4), "offense_score"=c(1:10), "defense_score"=c(10:1))
team_2 <- list("player_number"=(1:10), "sector"=c(4,4,3,3,3,2,2,2,1,1), "offense_score"=c(1:10), "defense_score"=c(10:1))

###Defining lst_sample
lst_sample <- function(x, size, replace = F) {
  if (length(x) == 1) return(rep(x, size))
  base::sample(x, size = size, replace = replace)
}

###Flipping the coin
coin_flip <- runif(1, min=0, max=1)
if (coin_flip>0.5) {
  ball_possession=1
  sector=2
} else {ball_possession=2 
  sector=3}

###Big Loop? for (i in 1:90)

###score calculation function for each sector
offense_score <- function(possession, sector){
if (possession==1){
  N <- which(team_1$sector==sector)
  offense_score1 = 0
  for (i in N){
    offense_score1 <- offense_score1 + team_1$offense_score[i]
  }
} else {
  N <- which(team_2$sector==sector)
  offense_score1 = 0
  for (i in N){
    offense_score1 <- offense_score1 + team_2$offense_score[i]
  }
}
return(offense_score1)
}
###Test: offense_score(possession = 1, sector = 2)

defense_score <- function(possession, sector){
  if (possession==2){
    N <- which(team_1$sector==sector)
    defense_score1 = 0
    for (i in N){
      defense_score1 <- defense_score1 + team_1$defense_score[i]
    }
  } else {
    N <- which(team_2$sector==sector)
    defense_score1 = 0
    for (i in N){
      defense_score1 <- defense_score1 + team_2$defense_score[i]
    }
  }
  return(defense_score1)
}
###Test: defense_score(possession = 2, sector = 2)


###Possession flip function- NEED HELP- just stays stuck on the same values
possession_flip <- function(possession){
  if(possession==1) {
    ball_possession <- 2
  } else if(possession==2) {
    ball_possession <- 1
  }
}
ball_possession=1
possession_flip(possession = as.numeric(ball_possession))
ball_possession
possession_flip(possession = as.numeric(ball_possession))
ball_possession



###1. Defending Team attempts
off_sum <- offense_score(possession = ball_possession, sector = sector)
def_sum <- defense_score(possession = ball_possession, sector = sector)
off_team_roll <- runif(1, min=1, max=off_sum)
def_team_roll <- runif(1, min=1, max=def_sum)
if ((def_team_roll>off_team_roll) & (ball_possession==1)) {
  ball_possession <- 2
} else if ((def_team_roll>off_team_roll) & (ball_possession==2)) {
  ball_possession <- 1
} else ###Next
  
###2. Attacking team moves
if ((ball_possession==1 & sector=4) | (ball_possession==2 & sector=1)) {
  close_shot <-1
}
if (close_shot==1){
sector_players <- team_1$offense_score[team_1$sector==sector]
lst_sample(sector_players, 1, replace = T)
}

longshot_off_score <- sample(team)


###3. Probability that Long Shot Happens
if ((sector=3 & ball_possession=1) | (sector=2 & ball_possession=3)) {
  long_shot_prob <- runif(1, min=0, max=1)
  if (long_shot_prob<0.3) {
    #Insert long shot calcuation
  } else #Passsing stuff
}

###4.

#Output should be pass_successful =1

###5. Probability of offside
if (pass_successful == 1) {
  offside_prob <- runif(1, min=0, max=1)
}
  if (offside_prob>0.15) {
  ###No offside
    } else if (ball_possession==1 & offside_prob<0.15) {
     ball_possession <- 2
     sector <- 4
   } else {ball_possession <- 1
   sector <-1
   }

###6.



