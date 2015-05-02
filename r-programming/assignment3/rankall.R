
rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome <- outcome
        rank <- num
        
        ## Check that state and outcome are valid
        if(outcome == 'heart attack'){
                outcome_data <- subset(data, select = c(7,2,11))
        } else if (outcome == 'heart failure'){
                outcome_data <- subset(data, select = c(7,2,17))
        } else if (outcome == 'pneumonia'){
                outcome_data <- subset(data, select = c(7,2,23))
        } else {
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        outcome_data[,3] <- as.numeric(outcome_data[,3])
        outcome_data <- na.omit(outcome_data)
        outcome_data_by_state <- split(outcome_data,outcome_data$State)
        
        hospital_data <- data.frame(hospital = character(0), state = character(0))
        
        ranking <- function(x,rank){
                
                state_ordered_outcome <- x[ order(x[,3],x[,2]),]
               
                if(num == "best" ){
                        hospital_by_rank <- state_ordered_outcome$Hospital.Name[1]
                } else if (num == "worst"){
                        hospital_by_rank <- state_ordered_outcome$Hospital.Name[nrow(state_ordered_outcome)]
                }else hospital_by_rank <- state_ordered_outcome$Hospital.Name[num]
                
                temp_hospital_data <- data.frame(hospital=hospital_by_rank,state=x$State[1])
                hospital_data <<- rbind(hospital_data,temp_hospital_data)
        }
        
        sapply(outcome_data_by_state,ranking)
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospital_data
}
