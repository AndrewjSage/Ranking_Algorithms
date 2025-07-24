library(tidyverse)
library(MASS)

# read in games data
Games_Data <- International.Soccer.Games.2018.2022
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

 Test_cup_subset <- cup_games[951:1748, ]
 
 Test_indices <- which(duplicated(rbind(Games_Data, Test_cup_subset))[(nrow(Games_Data) + 1):nrow(rbind(Games_Data, Test_cup_subset))])
 
 
 Test_Data <- dplyr::semi_join(Games_Data, Test_cup_subset, by = colnames(Games_Data))
 
 Sample_Data <- dplyr::anti_join(Games_Data, Test_Data, by = colnames(Games_Data))
 
 
Rankings <- function(Sample_Data){
  
  #Preliminary work to set up X matrix
  Scores <- Sample_Data |> dplyr::select(V3, V4, V7, V9, V10)
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
  
  X[games+1,1:numteams]=c(rep(1, numteams))#add sum to zero constraint
  X[,numteams+1]=c(Scores$Home[1:games],0) #add homefield advantage
  
  #set up weighted matrix W
  W <- matrix(0, games+1, games+1)
  W[games+1, games+1] <- 1
  
    friendly_games <- c("Friendly", "Friendly tournament")
   qualifier_games <- c("African Nations Cup qualifier", "Arab Cup qualifier", 
              "Asian Cup qualifier", "CONCACAF Champ qual", 
          "CONCACAF Nations League q", "East Asian Championship qual",
         "European Championship qual", "Southeast Asian Champ qual")
   # Add date column and process time weights
   dates <- as.Date(Sample_Data$V11)
   dates <- c(dates, dates)  # duplicated for rbind symmetry
   game_dates <- dates[1:games]
   
   most_recent_date <- max(game_dates)
   days_since <- as.numeric(most_recent_date - game_dates)
   
   # Set decay rate λ (can be tuned)
   lambda <- 0.001
   time_weights <- exp(-lambda * days_since)
   
   # Combined tournament and time weights
   for(i in 1:games){ 
     tournament <- tournaments[i]
     base_weight <- if (tournament %in% friendly_games) {
       1
     } else if (tournament %in% qualifier_games) {
       1.5
     } else {
       2
     }
     
     W[i, i] <- base_weight * time_weights[i]
   }
   
   W[games+1, games+1] <- 1  # keep constraint weight unchanged
 
  y <- c(Scores$Mar[1:games], 0)#add +1 to games here #margin of victory vector
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
    
  }
  
  b <- ginv(t(X)%*%W%*%X)%*%t(X)%*%W%*%y  #calculate ratings using weighted least squares
  
  # weight games by closeness of opponent's strengths.
  # games between closely matched competitors are rated more heavily than mismatches
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
  
  show(b)
  
  # organize into table
  Rankings_Table <- data.frame(sort(as.character(unique(Scores$TEAM))), b[1:numteams])
  names(Rankings_Table) <- c("Team", "Rating")
  Rankings_Table <- Rankings_Table[order(-Rankings_Table$Rating), ]
  Rankings_Table$Rnk <- (seq(1,nrow(Rankings_Table), 1))
  return(Rankings_Table)
}


# calculate rankings
World22_Rankings <- Rankings(Sample_Data)
World22_Rankings

#function to represent accuracy of future predictions
Predictions <- function(Test_Data, World22_Rankings){
  
  #preliminary work to set up prediction loop
  Test <- Test_Data |> dplyr::select(V3, V4, V9, V10)
  names(Test) <- c("TEAM", "OPP", "Home", "Mar")
  home_countries <- c(Test$TEAM)
  away_countries <- c(Test$OPP)
  World22_Teams <- c(World22_Rankings$Team)
  World22_Ratings <- c(World22_Rankings$Rating)
  true_results <- c(Test$Mar)
  
  p <- 0
  
  for(i in 1:769){
    
    #home country ranking
    home <- home_countries[i]
    home_row <- which(World22_Teams == home)
    home_ranking <- World22_Ratings[home_row]
    
    #away country ranking
    away <- away_countries[i]
    away_row <- which(World22_Teams == away)
    away_ranking <- World22_Ratings[away_row]
    
    #true result
    true_result <- true_results[i]
    
    p <- p + (((home_ranking - away_ranking) - true_result)^2)
  }
  
  return(p)
}

Rankings_Accuracy <- Predictions(Test_Data, World22_Rankings)
show(Rankings_Accuracy)
  
