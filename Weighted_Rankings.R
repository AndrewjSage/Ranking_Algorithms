library(tidyverse)
library(MASS)
Games <- read.csv("Games.csv") 
X <- as.matrix(Games |> dplyr::select(A:E))
Y <- as.matrix(Games |> dplyr::select(Y))

# create diagonal weight matrix - first 3 games get weight 1/2, next 3 get weight 1, last 3 get weight 2
W <- diag(c(1/2, 1/2, 1/2, 1, 1, 1, 2, 2, 2)) 

# rankings using weights
b <- ginv(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y

#rankings not using weights
b_0 <- ginv(t(X)%*%X)%*%t(X)%*%Y

#------------------------------------------------------------ #

# Random games toy example

# Set seed
set.seed(123) 

# Number of teams to generate
numteams <- 10 # Maximum of 26

# Assign a letter to each team
teams <- LETTERS[1:numteams] # Teams A, B, C, ..., J 

# Number of games to generate 
numgames <- 30 

# Randomly select two different teams for each game 
games <- data.frame( 
  home_team = character(numgames), 
  away_team = character(numgames), 
  stringsAsFactors = FALSE 
) 

for (i in 1:numgames) { 
  
  # Select two different teams 
  matchup <- sample(teams, 2, replace = FALSE) 
  games$home_team[i] <- matchup[1] 
  games$away_team[i] <- matchup[2] 
  
} 

# Generate random margins of victory 
# Values range from -8 to +8 
games$margin <- sample( 
  c(-8:8), 
  numgames, 
  replace = TRUE
) 

# View the randomly generated games 
print(games) 

# X will have: +1 for the team, -1 for the opponent, and 0 for all other teams 

X <- matrix( 
  0, 
  nrow = numgames + 1, 
  ncol = numteams 
) 

# Set the team names to the letters previously established
colnames(X) <- teams

# Fill the matchup portion of X 
for (i in 1:numgames) { 
  team <- games$home_team[i] 
  opp <- games$away_team[i] 
  X[i, team] <- 1 
  X[i, opp] <- -1 
} 

# Set A + B + C + ... + J = 0 by adding a final row of ones. 
X[numgames + 1, ] <- 1 

# Show the X matrix
print(X)

# Set the final element of y to 0
y <- c(games$margin, 0) 

# Display the y-vector
print(y)

# Calculate the b values
b <- solve(t(X) %*% X) %*% t(X) %*% y 

# Create the rankings table
Rankings_Table <- data.frame( 
  Team = teams, 
  Rating = as.numeric(b) 
) 

# Sort from highest rating to lowest rating 
Rankings_Table <- Rankings_Table[
  order(-Rankings_Table$Rating),
] 

# Add ranking number 
Rankings_Table$Rank <- 1:nrow(Rankings_Table) 

# Reorder columns 
Rankings_Table <- Rankings_Table[ , c("Rank", "Team", "Rating")] 

# Display results
print(Rankings_Table)


#------------------------------------------------------------ #

# read in games data
Games_Data <- read.csv("NCAAF24.csv")
  
## Function to read scores from Games_Data data frame with team names and margin and calculate rankings

Weighted_Rankings <- function(Games_Data){
    
    #Preliminary work to set up X matrix
    Scores <- Games_Data |> dplyr::select(Team, Opp, Mar, Home)
    names(Scores) <- c("TEAM", "OPP",  "Mar", "Home")
    Scores <- rbind(Scores,Scores)
    h <- nrow(Scores)/2
    Scores$TEAM <- as.character(Scores$TEAM)
    Scores$OPP <- as.character(Scores$OPP)
    Scores$TEAM[(h+1):nrow(Scores)] <- Scores$OPP[1:h]
    Scores$OPP[(h+1):nrow(Scores)] <- Scores$TEAM[1:h]
    Scores$TEAM <- as.factor(Scores$TEAM)
    Scores$OPP <- as.factor(Scores$OPP)
    Scores <- Scores[!is.na(Scores$Mar),]
    games <- nrow(Scores)/2
    teams <- c(Scores$TEAM)
    opps <- c(Scores$OPP)
    numteams <- length(unique(teams))
    X <- matrix(0, games+1, numteams+1)
    
    # fill in X matrix with 0's and 1's
    for (i in 1:games){
      team <- teams[i]
      opp <- opps[i]
      X[i,team] <- 1
      X[i,opp] <- -1
    }
    X[games+1,1:numteams]=c(rep(1, numteams)) #add sum to zero constraint
    X[,numteams+1]=c(Scores$Home[1:games],0) #add homefield advantage
    y <- c(Scores$Mar[1:games],0) # margin of victory vector
  
    b <- ginv(t(X)%*%X)%*%t(X)%*%y  #calculate ratings using least squares
   
    
    
    # weight games by closeness of opponent's strengths.
    # games between closely matched competitors are rated more heavily than mismatches
    W <- diag(nrow(X))
    Winv <- W
    diag(Winv) <- 1/diag(W) # initially give each game equal weight
    b <- ginv(t(X)%*%Winv%*%X)%*%t(X)%*%Winv%*%y # calculate initial ratings
    b0 <- rep(1000, length(b)) # vector to store weighted ratings
    
   # loop to iteratively recalculate ratings and game weights until they reach convergence criteria
    while(sum((b-b0)^2)>0.0001){
      # fill in diagonal entries of weight matrix with difference in ratings between the teams that played in that game
      diag(W)[1:(nrow(W)-1)] <- X[-nrow(X),-ncol(X)]%*%b[-length(b)] 
      Winv <- W
      w <- diag(W)[1:(nrow(W)-1)]
      w <- abs(c(scale(w))) #standardize differences
      diag(Winv)[nrow(W)] <- 0
      diag(Winv)[1:(nrow(W)-1)] <- 1/(1+exp(w))  # use reciprocal exponential weighting function
      b0 <- b   # store previous ratings
      b <- ginv(t(X)%*%Winv%*%X)%*%t(X)%*%Winv%*%y # calculate new ratings using new weights
    }
    
    GameWeights <- diag(Winv)  # store final game weightings
    
    
    
    
    # organize into table
    Rankings_Table <- data.frame(sort(as.character(unique(Scores$TEAM))), b[1:numteams])
    names(Rankings_Table) <- c("Team", "Rating")
    Rankings_Table <- Rankings_Table[order(-Rankings_Table$Rating), ]
    Rankings_Table$Rnk <- (seq(1,nrow(Rankings_Table), 1))
    return(Rankings_Table)
  }


# calculate rankings
NCAAF24_Rankings <- Weighted_Rankings(Games_Data)
NCAAF24_Rankings


#------------------------------------------------------------ #

# read in games data
Soccer_Games <- read.csv("Owen_Work/International Soccer Games 2018-2022.csv")

## Function to read scores from Games_Data data frame with team names and margin and calculate rankings

Rankings <- function(Games_Data){
  
  #Preliminary work to set up X matrix
  Scores <- Games_Data |> dplyr::select(home_team, away_team, margin)
  names(Scores) <- c("TEAM", "OPP",  "Mar")
  Scores <- rbind(Scores,Scores)
  h <- nrow(Scores)/2
  Scores$TEAM <- as.character(Scores$TEAM)
  Scores$OPP <- as.character(Scores$OPP)
  Scores$TEAM[(h+1):nrow(Scores)] <- Scores$OPP[1:h]
  Scores$OPP[(h+1):nrow(Scores)] <- Scores$TEAM[1:h]
  Scores$TEAM <- as.factor(Scores$TEAM)
  Scores$OPP <- as.factor(Scores$OPP)
  Scores <- Scores[!is.na(Scores$Mar),]
  games <- nrow(Scores)/2
  teams <- c(Scores$TEAM)
  opps <- c(Scores$OPP)
  numteams <- length(unique(teams))
  X <- matrix(0, games+1, numteams+1)
  
  # fill in X matrix with 0's and 1's
  for (i in 1:games){
    team <- teams[i]
    opp <- opps[i]
    X[i,team] <- 1
    X[i,opp] <- -1
  }
  X[games+1,1:numteams]=c(rep(1, numteams)) #add sum to zero constraint
  y <- c(Scores$Mar[1:games],0) # margin of victory vector
  
  b <- ginv(t(X)%*%X)%*%t(X)%*%y  #calculate ratings using least squares
  
  # organize into table
  Rankings_Table <- data.frame(sort(as.character(unique(Scores$TEAM))), b[1:numteams])
  names(Rankings_Table) <- c("Team", "Rating")
  Rankings_Table <- Rankings_Table[order(-Rankings_Table$Rating), ]
  Rankings_Table$Rnk <- (seq(1,nrow(Rankings_Table), 1))
  return(Rankings_Table)
}


# calculate rankings
World22_Rankings <- Rankings(Soccer_Games)
World22_Rankings
