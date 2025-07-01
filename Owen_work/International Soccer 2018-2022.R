library(tidyverse)
library(MASS)

# read in games data
Games_Data <- New.International.Games.2018.2022
## Function to read scores from Games_Data data frame with team names and margin and calculate rankings
 
 cup_games <- Games_Data %>% 
   filter(!(V7 %in% c("Friendly", "Friendly tournament", 
                      "African Nations Cup qualifier", "Arab Cup qualifier", 
                      "Asian Cup qualifier", "CONCACAF Champ qual", 
                      "CONCACAF Nations League q", "East Asian Championship qual",
                      "European Championship qual", "Southeast Asian Champ qual")))
 
 qualifiers <- Games_Data %>% 
   filter(V7 %in% c("African Nations Cup qualifier", "Arab Cup qualifier", 
            "Asian Cup qualifier", "CONCACAF Champ qual", 
        "CONCACAF Nations League q", "East Asian Championship qual",
       "European Championship qual", "Southeast Asian Champ qual"))
 
 friendlies <- Games_Data %>%
   filter(V7 %in% c("Friendly", "Friendly tournament"))
 
Rankings <- function(Games_Data){
  
  #Preliminary work to set up X matrix
  Scores <- Games_Data |> dplyr::select(V3, V4, V7, V9, V10)
  names(Scores) <- c("TEAM", "OPP", "Tournament", "Home", "Mar")
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
  tournaments <- c(Scores$Tournament)
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
  #for(i in 1:games){ #weighting games by match type
  #  tournament <- tournaments[i]
  #  friendlies <- c("Friendly", "Friendly tournament")
   # qualifiers <- c("African Nations Cup qualifier", "Arab Cup qualifier", 
        #            "Asian Cup qualifier", "CONCACAF Champ qual", 
            #        "CONCACAF Nations League q", "East Asian Championship qual",
             #       "European Championship qual", "Southeast Asian Champ qual")
    #if(tournament %in% friendlies){
    #  X[i, numteams+2] <- 1
   # }
    #else if(tournament %in% qualifiers){
    #  X[i, numteams+2] <- 2
    #}
    #else(X[i, numteams+2] <- 3)
  #}
 
  y <- c(Scores$Mar[1:games],0) # margin of victory vector
  for (i in 1:games){ #scale the margin vector to make winning more important and limit the weight of routs
    margin <- y[i]
    
    if(margin == 1){
      y[i] <- 2
    }
    if(margin == -1){
      y[i] <- -2
    }
    if(margin >= 2){
      y[i] <- 3 + log(y[i] - 1)
    }
    if(margin <= -2){
      y[i] <- -3 - log((-1 %*% y[i]) - 1)
    }
    
    #previous margin weightings
   # if(margin >= 3){
    #  y[i] <- (19 + margin) / 8
    #}
    #if(margin <= -3){
    #  y[i] <- (-19 + margin) / 8
    #}
    
  }
  b <- ginv(t(X)%*%X)%*%t(X)%*%y  #calculate ratings using least squares
  show(b)
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
