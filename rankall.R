rankall <- function(outcome, num = "best") { ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  
  ## Read outcome data
  
  all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  all_data[, 11] <- suppressWarnings(as.numeric(all_data[, 11])) ## col 11 = heart attack rates
  all_data[, 17] <- suppressWarnings(as.numeric(all_data[, 17])) ## col 17 = heart failure rates
  all_data[, 23] <- suppressWarnings(as.numeric(all_data[, 23])) ## col 23 = pneumonia rates
  
  ## create table of valid outcomes
  
  valid_outcomes <- data.frame(illness = c("heart attack", "heart failure", "pneumonia"),
                               deaths = c(11, 17, 23) )   ## relevant data column in the csv file
  
  
  
  ## Check that this outcome appears in valid_outcomes table
  
  outcome_index <- which(apply(valid_outcomes, 1, function(x) any(x == outcome)))  ## find row in valid_outcomes 
  
  if (length(outcome_index) == 0 ) ## if it's not found in valid_outcomes
    stop("invalid outcome")
  
  if (num == "best") {
    
    num <- 1
  }
  
  if (num == "worst") {
  
    ## sort the data by state, outcome DESCENDING, hospital name
    all_data <- all_data[ order(all_data[,7], -all_data[,(valid_outcomes$deaths[outcome_index])], all_data[,2]), ] 
    num <- 1
    
  } 
  
  else {
    
    ## sort the data by state, outcome ASCENDING, hospital name
    all_data <- all_data[ order(all_data[,7], all_data[,(valid_outcomes$deaths[outcome_index])], all_data[,2]), ] 
  
}  
  
  ## split data according by state
  
 state_data <- split(all_data, all_data$State)
 
 
 ## sapply(MyList,"[", 2,1 ) ## selects first element of 2nd row of each matrix
 
 
 results <- lapply(state_data, "[", num, 2)  ## 2nd element of NUMth row of each list
 res2 <- unlist(results) 
  
  ## return dataframe containing state and hospital name for given rank
  
data.frame(hospital=res2, state=names(results), row.names=names(results))
 
}
