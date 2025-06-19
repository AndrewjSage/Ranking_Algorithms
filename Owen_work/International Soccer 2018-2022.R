library(tidyverse)
library(MASS)

# read in games data
Games_Data <- read.csv("New International Games 2018-2022.csv")

## Function to read scores from Games_Data data frame with team names and margin and calculate rankings

Rankings <- function(Games_Data){
  
  #Preliminary work to set up X matrix
  Scores <- Games_Data |> dplyr::select(home_team, away_team, home, margin)
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
  
  # organize into table
  Rankings_Table <- data.frame(sort(as.character(unique(Scores$TEAM))), b[1:numteams])
  names(Rankings_Table) <- c("Team", "Rating")
  Rankings_Table <- Rankings_Table[order(-Rankings_Table$Rating), ]
  Rankings_Table$Rnk <- (seq(1,nrow(Rankings_Table), 1))
  return(Rankings_Table)
}


# calculate rankings
World22_Rankings <- Rankings(Games_Data)
World22_Rankings
