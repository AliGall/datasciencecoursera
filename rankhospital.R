rankhospital <- function(state, outcome, num) { 

  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank 
  ## num can take values "best", "worst", or an integer indicating the ranking
  
  if (num == "best") { ## change to numerical value (we will deal with "worst" below)
    num <- 1
  } 
  
  ## Read outcome data
  
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  all_data[, 11] <- suppressWarnings(as.numeric(all_data[, 11])) ## col 11 = heart attack rates
  all_data[, 17] <- suppressWarnings(as.numeric(all_data[, 17])) ## col 17 = heart failure rates
  all_data[, 23] <- suppressWarnings(as.numeric(all_data[, 23])) ## col 23 = pneumonia rates
  
  ## create table of valid outcomes
  
  valid_outcomes <- data.frame(illness = c("heart attack", "heart failure", "pneumonia"),
                               deaths = c(11, 17, 23) )   ## relevant data column in the csv file
  
  ## Check that state and outcome are valid
  
  ## first, create a subset of this state's data
  
  state_data <- all_data[all_data$State == state, ] 
  
  if(nrow(state_data) == 0) ## if subset is empty then this state is invalid
    stop("invalid state")
  
  ## Check that this outcome appears in valid_outcomes table
  
  outcome_index <- which(apply(valid_outcomes, 1, function(x) any(x == outcome)))  ## find row in valid_outcomes 
  
  if (length(outcome_index) == 0 ) ## if it's not found in valid_outcomes
    stop("invalid outcome")
  
  ## sort the data according to the outcome we are interested in
  
  if (num == "worst") {
    
    ## sort by descending death rate, ascending hospital name
    state_data <- state_data[ order(-state_data[,(valid_outcomes$deaths[outcome_index])], state_data[,2]), ] 
    num <- 1
  }
    
  else {
    
    ## sort by ascending death rate, ascending hospital name
    state_data <- state_data[ order(state_data[,(valid_outcomes$deaths[outcome_index])], state_data[,2]), ] 
  }
  
  ## Return the hospital name at the requested ranking
  
  state_data$Hospital.Name[num]
  
}