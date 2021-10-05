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
  N <- length()
  for (i in 1:N)
}

}


###3. Long Shot calcuation- inputs are sectors and possession
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



