
rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        state <- state
        outcome <- outcome
        rank <- num
        
        ## Check that state and outcome are valid
        state_data <- subset(data, data$State == state)
        
        if nrow(state_data == 0){
                stop("invalid state")
        }
        
        if(outcome == 'heart attack'){
                state_data <- subset(state_data, select = c(7,2,11))
        } else if (outcome == 'heart failure'){
                state_data <- subset(state_data, select = c(7,2,17))
        } else if (outcome == 'pneumonia'){
                state_data <- subset(state_data, select = c(7,2,23))
        } else {
                stop("invalid outcome")
        }
        
        state_data[,3] <- as.numeric(state_data[,3])
        state_data <- na.omit(state_data)
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        state_data_ordered <- state_data[ order(state_data[,3],state_data[,2]),]
        
        if(num == "best" ){
                hospital_by_rank <- state_data_ordered$Hospital.Name[1]
        } else if (num == "worst"){
                hospital_by_rank <- state_data_ordered$Hospital.Name[nrow(state_data)]
        }else hospital_by_rank <- state_data_ordered$Hospital.Name[num]
        
        hospital_by_rank
        
}
